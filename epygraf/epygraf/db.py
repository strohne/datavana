import pandas as pd
from sqlalchemy import create_engine, text

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


  

