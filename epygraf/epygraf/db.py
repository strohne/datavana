import pandas as pd
from sqlalchemy import create_engine, text
import mysql
import re
import mysql.connector
import os
import pymysql
from pymysql import cursors


def setup(host="localhost", port=3306, username="root", password="root", database=""):

    """
    Set up environment variables for connecting to a database.

    :param host: (str) The hostname or IP address of the database server.
                  Default is 'localhost'.
    :param port: (int) The port number for the database server. Default is 3306.
    :param username: (str) The username for connecting to the database.
                      Default is 'root'.
    :param password: (str) The password for connecting to the database.
                      Default is 'root'.
    :param database: (str) The name of the database to connect to.
                     Default is an empty string.
    :return: None

    """
    settings = {
        "epi_host": host,
        "epi_port": str(port),
        "epi_username": username,
        "epi_password": password,
        "epi_dbname": database
    }

    for key, value in settings.items():
        os.environ[key] = str(value)


def connect(db=None):
    
    """
    Connect to a database using the provided or environment-based parameters.

    :param db: (str or None) The name of the database to connect to. If None,
               uses the database name from environment variables.
    :return: pymysql.connections.Connection
        A connection to the MySQL database.
    """
    if db is None:
        db = os.environ.get("epi_dbname")

    con = pymysql.connect(
        host=os.environ.get("epi_host"),
        port=int(os.environ.get("epi_port")),
        user=os.environ.get("epi_username"),
        password=os.environ.get("epi_password"),
        database=db
    )

    return con


def table(table, db=None, deleted=False, cond=None):
    
    """
    Retrieve data from a table based on the specified conditions.

    :param table: (str) The name of the table to retrieve data from.
    :param db: (pymysql.connections.Connection or None) The MySQL database connection.
               If None, a new connection will be established using the
               default settings or environment variables.
    :param deleted: (bool) Flag indicating whether to include deleted
                    records. Default is False.
    :param cond: (str or list or None) Filter condition(s) to apply to the query.
                 Each condition should be a string that represents a SQL condition.
                 If provided as a string, no additional formatting is applied.
                 If provided as a list, conditions are joined using 'AND'.
                 Default is None.
    :return: pandas.DataFrame
        A DataFrame containing the retrieved data.
    """
    #TODO: what if the db parameter is a string (the database name?)
    if db is None:
        con = connect()  # Establish a new connection
    else:
        con = db  # Use the provided connection

    cursor = con.cursor(cursors.DictCursor)  # Use DictCursor

    # Construct SQL
    sql = f"SELECT * FROM {table}"

    # Add deleted = 0 to the conditions vector
    if not deleted:
        if cond:
            cond = f"deleted = 0 AND {cond}"
        else:
            cond = "deleted = 0"

    # Add the condition(s) to the query
    if cond:
        if isinstance(cond, list):
            cond_str = " AND ".join(cond)
        else:
            cond_str = cond

        sql += f" WHERE {cond_str}"

    # Execute SQL query
    cursor.execute(sql)

    # Fetch the result as a DataFrame
    result = pd.DataFrame(cursor.fetchall())

    # Close the cursor and connection if a new connection was established
    if db is None:
        cursor.close()
        con.close()

    return result


def name(con):
    
    """
    Retrieve the database name from the MySQL connection.

    :param con: (pymysql.connections.Connection) The MySQL database connection.
    :return: str or None
        The name of the connected database, or None if no database is selected.

    """
    with con.cursor() as cursor:
        cursor.execute("SELECT DATABASE();")
        result = cursor.fetchone()
        return result[0] if result else None


def databases(epi=False):
    
    """
    Retrieve a list of databases, optionally filtered by prefix.

    :param epi: (bool) If True, filter databases to those starting with 'epi_'.
                If False, return all databases. Default is False.
    :return: list
        A list of database names.
    """
    con = connect()
    with con.cursor() as cursor:
        cursor.execute("SHOW DATABASES;")
        dbs = cursor.fetchall()

    if epi:
        dbs = [db[0] for db in dbs if db[0].startswith("epi_")]

    return dbs


def condition(table, field, values):
    
    """
    Create a filter condition for a field based on specified values.

    :param table: (str) The name of the table.
    :param field: (str) The name of the field to filter.
    :param values: (list) A list of values to use in the filter condition.
                   Supported value types: int, float, str.
    :return: str
        A filter condition statement.
    """
    if all(isinstance(val, (int, float)) for val in values):
        value_str = f"({','.join(map(str, values))})"
    elif all(isinstance(val, str) for val in values):
        value_str = f"('{','.join(values)}')"
    else:
        raise ValueError("Unsupported value types in the list")

    statement = f"{table}.{field} IN {value_str}"
    return statement


def geolocations(db=None, itemtype="geolocations"):
    
    """
    Retrieve geolocations data for a given database.

    :param db: (str or None) The name of the database. If None, the default database will be used.
    :param itemtype: (str) The item type for geolocations. Default is "geolocations".
    :return: pandas.DataFrame
        A DataFrame containing the retrieved geolocations data.
        
    """
    # TODO: Use connect()
    engine = create_engine(f"mysql+pymysql://{os.environ.get('epi_username')}:{os.environ.get('epi_password')}@{os.environ.get('epi_host')}:{os.environ.get('epi_port')}/{db}")

    sql = f"""
        SELECT
          articles_id,
          id AS item_id,
          sortno,
          published,
          CAST(JSON_VALUE(`value`, '$.lat') AS DOUBLE) AS lat,
          CAST(JSON_VALUE(`value`, '$.lng') AS DOUBLE) AS lng
        FROM items WHERE
          itemtype = '{itemtype}' AND
          deleted=0
    """

    table = pd.read_sql_query(sql, engine)

    return table


def get_codings(db):
    
    """
    Retrieve codings data from the specified database.

    This function retrieves codings data from the 'items', 'properties', 'types', 'articles', and 'links' tables
    for the specified database. It joins the data based on relationships and provides a DataFrame with codings information.

    :param db: (str or None) The name of the database. If None, the default database will be used.
    :return: pandas.DataFrame
        A DataFrame containing the retrieved codings data.
    """
    #TODO: rename to get_annotations()

    con = connect(db)  # Use the connect function to get the connection

    # Check if the result is a tuple (connection, databasename)
    if isinstance(con, tuple):
        con, databasename = con
    else:
        databasename = db

    #TODO: Filter data in SQL query, avoid deleted==0 etc. below
    #TODO: JOIN data in SQL query, avoid pd.merge() below

    items = table("items", con)
    properties = table("properties", con)
    types = table("types", con)
    articles = table("articles", con)
    links = table("links", con)

    if isinstance(db, str):
        con.close()

    # Join data
    items = items.merge(
        articles[['id', 'articletype']],
        left_on='articles_id',
        right_on='id'
    ).merge(
        properties[['id', 'propertytype', 'norm_data', 'lemma', 'name', 'unit']],
        left_on='properties_id',
        right_on='id'
    ).merge(
        types.query("scope == 'properties'")[['name', 'category', 'caption']],
        left_on='propertytype',
        right_on='name'
    )

    # Prepare data
    # TODO: make articletype filter configurable
    items = items[
        (items['deleted'] == 0) &
        (items['articletype'] == "object") &
        (~items['properties_id'].isna())
    ]

    links = links.merge(
        articles[articles['articletype'] == "object"][['id']],
        left_on='root_id',
        right_on='id'
    )

    codings = pd.concat([
        items[['articles_id', 'properties_id']],
        links.rename(columns={'root_id': 'articles_id', 'to_id': 'properties_id'})[['articles_id', 'properties_id']]
    ]).groupby(['articles_id', 'properties_id']).size().reset_index(name='count')

    codings['db'] = databasename

    return codings


def get_codes(db):
    
    """
    Retrieve codes for a given database.

    This function retrieves codes from the 'properties' table for the specified database.
    It includes information about properties, their types, and hierarchical relationships.

    :param db: (str or None) The name of the database. If None, the default database will be used.
    :return: pandas.DataFrame
        A DataFrame containing the retrieved codes.
    """
    con = connect(db)  # Use the connect function to get the connection

    # Check if the result is a tuple (connection, databasename)
    if isinstance(con, tuple):
        con, databasename = con
    else:
        databasename = db

    properties = table("properties", con)

    if isinstance(db, str):
        con.close()

    # Filter properties without related_id (top-level properties)
    codes = properties[properties['related_id'].isna()][[
        'id', 'parent_id', 'propertytype', 'lemma', 'name',
        'norm_data', 'norm_iri', 'level', 'lft', 'rght'
    ]]

    # Remove duplicates in the 'propertytype' column
    codes = codes.drop_duplicates('propertytype')

    # Create propertytypes DataFrame with unique property types
    propertytypes = codes.assign(
        level=-1,
        id=lambda x: -x.groupby('propertytype').cumcount() - 1
    )

    # Merge propertytypes with codes and handle parent_id
    codes = codes.merge(
        propertytypes[['id', 'propertytype']],
        left_on='propertytype',
        right_on='propertytype',
        suffixes=('', '_propertytypes')
    ).assign(
        parent_id=lambda x: x['id_propertytypes'].combine_first(x['parent_id'])
    ).drop(columns=['id_propertytypes'])

    # Concatenate propertytypes and codes, adjust hierarchical structure
    codes = pd.concat([
        propertytypes,
        codes
    ]).groupby('propertytype').apply(lambda x: fix_lft_rght(x)).reset_index(drop=True)

    # Fill missing lemma with name
    codes['lemma'] = codes['lemma'].fillna(codes['name'])

    # Add database name to the DataFrame
    codes['db'] = databasename

    return codes

def fix_lft_rght(df):
    if df["level"].iloc[0] == -1:
        df["lft"] = df["lft"].min() - 1
        df["rght"] = df["rght"].max() + 1
    return df



  

