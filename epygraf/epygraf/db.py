import pandas as pd
from sqlalchemy import create_engine, text
import re
from typing import Union, DataFrame, Optional

settings = {
    'host': 'localhost',
    'port': 3306,
    'username': 'root',
    'password': 'root',
    'database': "epi_all"
}

def setup(host="localhost", port=3306, username="root", password="root", database=""):
    
    """
    Save database connection settings to environment variables.
    Environment variables are prefixed with "epi_" and used in db_connect()
    to establish the connection.

    :param host: (str) host
    :param port: (int) port
    :param username: (str) username
    :param password: (str) password
    :param database: (str) database
    :return: None
    """
    settings['host'] = host
    settings['port'] = port
    settings['username'] = username
    settings['password'] = password
    settings['database'] = database

def connect(db=None):
   
    """
    Get a connection to a database.
    Before you can use this function, call db_setup once
    to set the connection parameters.
    All parameters are stored in the environment.

    :param db: (str or None) Name of the database as string.
               Leave empty to use the database name from the environment settings.
    :return: (pymysql.connections.Connection) A connection to the database.
    """
    db_str = 'mysql+pymysql://' + \
             settings['username'] + ':' + \
             settings['password'] + '@' + \
             settings['host'] + ':' + \
             str(settings['port']) + '/' + \
             db

    db_enginge = create_engine(db_str)

    return (db_enginge)


def unconnect(db_engine):
    """
    Closes a database connection
    
    Parameters:
    - db_engine The database engine created by connect()
    """
    db_engine.dispose()

def table(table="", db="epi_all"):
    """
    Get all rows in a table filtered by deleted=0.
    
    Parameters:
    - table The table name
    - db The database engine obtained from connect()
    Returns:
    A pandas dataframe
    
    """

    query = 'SELECT * FROM ' + table + ' WHERE deleted = 0'    
    
    with connect(db).connect() as con:
        db_rows = pd.read_sql(text(query), con=con)

    return(db_rows)


def db_name(con):
    with con.cursor() as cursor:
        cursor.execute("SHOW DATABASES;")
        result = cursor.fetchone()
        return result[0] if result else None

def db_databases(epi=False):
    con = db_connect()
    with con.cursor() as cursor:
        cursor.execute("SHOW DATABASES;")
        dbs = cursor.fetchall()

    if epi:
        dbs = [db[0] for db in dbs if db[0].startswith("epi_")]

    return dbs

def db_condition(table, field, value):
    if all(isinstance(val, (int, float)) for val in value):
        value_str = f"({','.join(map(str, value))})"
    elif all(isinstance(val, str) for val in value):
        value_str = f"('{\"','\".join(value)}')"
    else:
        raise ValueError("Unsupported value types in the list")

    statement = f"{table}.{field} in {value_str}"
    return statement

def db_get_codings(db):
    con, databasename = db_connect(db)
    
    items = db_table("items", con)
    properties = db_table("properties", con)
    types = db_table("types", con)
    articles = db_table("articles", con)
    links = db_table("links", con)

    if isinstance(db, str):
        dbDisconnect(con)

    items = items.merge(
        articles[['id', 'articletype']],
        left_on='articles_id',
        right_on='id',
        suffixes=('', '_articles')
    ).merge(
        properties[['id', 'propertytype', 'norm_data', 'lemma', 'name', 'unit']],
        left_on='properties_id',
        right_on='id',
        suffixes=('', '_properties')
    ).merge(
        types.query('scope == "properties"')[['name', 'category', 'caption']],
        left_on='propertytype',
        right_on='name',
        suffixes=('', '_types')
    )

    items = items[
        (items['deleted'] == 0) &
        (items['articletype'] == 'object') &
        (items['properties_id'].notna())
    ]

    links = links[
        (links['deleted'] == 0) &
        (links['root_tab'] == 'articles') &
        (links['to_tab'] == 'properties')
    ].merge(
        articles.query('articletype == "object"')[['id']],
        left_on='root_id',
        right_on='id',
        suffixes=('', '_articles')
    )

    codings = pd.concat([
        items[['articles_id', 'properties_id']],
        links[['articles_id', 'to_id']]
    ]).groupby(['articles_id', 'properties_id']).size().reset_index(name='count')
    
    codings['db'] = databasename

    return codings

def db_get_codes(db):
    con, databasename = db_connect(db)

    properties = db_table("properties", con)

    if isinstance(db, str):
        dbDisconnect(con)

    codes = properties[properties['related_id'].isna()][[
        'id', 'parent_id', 'propertytype', 'lemma', 'name',
        'norm_data', 'norm_iri', 'level', 'lft', 'rght'
    ]]

    propertytypes = codes.drop_duplicates('propertytype')[[
        'propertytype', 'propertytype'
    ]].assign(
        level=-1,
        id=lambda x: -x.groupby('propertytype').cumcount() - 1
    )

    codes = codes.merge(
        propertytypes[['id', 'propertytype']],
        left_on='propertytype',
        right_on='propertytype',
        suffixes=('', '_propertytypes')
    ).assign(
        parent_id=lambda x: x['id_propertytypes'].combine_first(x['parent_id'])
    ).drop(columns=['id_propertytypes'])

    codes = pd.concat([
        propertytypes,
        codes
    ]).groupby('propertytype').apply(lambda x: fix_lft_rght(x)).reset_index(drop=True)

    codes['lemma'] = codes['lemma'].fillna(codes['name'])

    codes['db'] = databasename

    return codes

def db_geolocations(db, itemtype="geolocations"):
    con, _ = db_connect(db)
    
    sql = f"""
        SELECT
          articles_id,
          id AS item_id,
          sortno,
          published,
          CAST(JSON_VALUE(`value`, '$.lat') AS DOUBLE)  AS lat,
          CAST(JSON_VALUE(`value`, '$.lng') AS DOUBLE) AS lng
        FROM items WHERE
          itemtype = '{itemtype}' AND
          deleted=0
    """

    table = pd.read_sql_query(sql, con)

    if isinstance(db, str):
        dbDisconnect(con)

    return table


  

