import os
import pandas as pd
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

# Initial setup using defaults
setup()

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


def table(table, db=None, deleted=False, filter=None):
    """
    Retrieve data from a table.

    :param table: (str) The table name.
    :param db: (pymysql.connections.Connection or string) The database connection object or the database name.
        If None, uses the database name from the settings.
    :param deleted: (bool) Flag indicating whether to include deleted records.
                           Default is False.
    :param filter: (str or list or None) Filter condition(s) to apply to the query.
                 Each condition should be a string that represents a SQL condition.
                 If provided as a string, no additional formatting is applied.
                 If provided as a list, conditions are joined using 'AND'.
                 Default is None.
    :return: pandas.DataFrame
        A DataFrame containing the retrieved data.
    """

    # Get database connection
    if isinstance(db, pymysql.connections.Connection):
        con = db
    else:
        con = connect(db)

    cursor = con.cursor(cursors.DictCursor)  # Use DictCursor

    # Construct SQL expression
    sql = f"SELECT * FROM {table}"

    # Add conditions to the query
    if filter is None:
        filter = []
    elif not isinstance(filter, list):
        filter = [filter]


    # Add deleted = 0 to the conditions
    if not deleted:
        filter.append(f"{table}.deleted = 0")

    # Convert to SQL string
    if len(filter) > 0:
        filter = " AND ".join(filter)
        sql += f" WHERE {filter}"

    # Execute SQL query
    cursor.execute(sql)

    # Fetch the result as a DataFrame
    result = pd.DataFrame(cursor.fetchall())

    # Close the cursor
    cursor.close()

    # Close the connection if a new connection was established
    if not isinstance(db, pymysql.connections.Connection):
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
    # TODO: Use connect(), see annotations()
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


def annotations(db, tables=['items', 'links'], filter={}):
    
    """
    Retrieve article annotations from items and links.

    Data from the items, properties, types, articles, and links tables will be joined.

    :param db: (str or None) The name of the database. If None, the default database will be used.
    :param filter: (dict) Filter by itemtype, linktype, propertytype, articletype, or projecttype by assigning
                          the respective keys with a list of allowed values.
    :param tables: (list or str) Add 'items' or 'links' to the list to retrieve annotations from the respective tables.
    :return: pandas.DataFrame
        A DataFrame containing annotations
    """

    # Defaults
    tables = [tables] if not isinstance(tables, list) else tables

    # Get connection
    db = os.environ.get("epi_dbname") if db is None else db
    con = connect(db)
    cursor = con.cursor(cursors.DictCursor)

    # Filter construction method
    def typeFilter(filter, tablename, colname):
        filter = filter.get(colname, [])
        filter = [filter] if not isinstance(filter, list) else filter
        if len(filter) > 0:
            filter = ['"' + x + '"' for x in filter]
            filter = "AND " + tablename + "." + colname + " IN (" + ",".join(filter) + ")"
            return filter
        else:
            return ""

    items = pd.DataFrame()
    if ('items' in tables):
        # Construct items SQL expression
        sql_items = f"""
           SELECT
                items.itemtype, items.sortno, items.properties_id ,
                properties.lemma, properties.name, properties.norm_iri AS properties_iri, 
                properties.propertytype,            
                items.articles_id, articles.signature as articles_signature, articles.norm_iri AS articles_iri,
                articles.articletype,
                projects.id AS projects_id, projects.signature AS projects_signature, projects.norm_iri AS projects_iri,
                projects.projecttype
           FROM items 
           INNER JOIN properties ON items.properties_id = properties.id AND properties.deleted=0 {typeFilter(filter,'properties','propertytype')}
           INNER JOIN articles ON items.articles_id = articles.id AND articles.deleted=0 {typeFilter(filter,'articles','articletype')}
           INNER JOIN projects ON articles.projects_id = projects.id AND projects.deleted=0 {typeFilter(filter,'projects','projecttype')}
           WHERE items.deleted=0 {typeFilter(filter,'items','itemtype')}
        """


        # Execute SQL query
        cursor.execute(sql_items)
        items = pd.DataFrame(cursor.fetchall())

    links = pd.DataFrame()
    if ('links' in tables):
        # Construct links SQL expression
        sql_links = f"""
               SELECT
                    links.from_tagname AS linktype, links.to_id AS properties_id,
                    properties.lemma, properties.name, properties.norm_iri AS properties_iri, 
                    properties.propertytype,            
                    links.root_id AS articles_id, articles.signature as articles_signature, articles.norm_iri AS articles_iri,
                    articles.articletype,
                    projects.id AS projects_id, projects.signature AS projects_signature, projects.norm_iri AS projects_iri,
                    projects.projecttype
               FROM links
               INNER JOIN properties ON links.to_id = properties.id AND links.to_tab='properties' AND properties.deleted=0  {typeFilter(filter,'properties','propertytype')}
               INNER JOIN articles ON links.root_id = articles.id AND links.root_tab='articles' AND articles.deleted=0  {typeFilter(filter,'articles','articletype')}           
               INNER JOIN projects ON articles.projects_id = projects.id AND projects.deleted=0  {typeFilter(filter,'projects','projecttype')}
               WHERE links.deleted=0  {typeFilter(filter,'links','from_tagname')}
            """

        # Execute SQL query
        cursor.execute(sql_links)
        links = pd.DataFrame(cursor.fetchall())

    # Close the cursor
    cursor.close()
    con.close()

    annotations =  pd.concat([items, links])
    annotations['db'] = db

    return annotations


def properties(db):
    
    """
    Retrieve properties

    The result includes the properties and their hierarchical relationships.

    TODO: revise

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



  

