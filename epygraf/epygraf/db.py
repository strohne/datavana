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
    Prepare the database connection parameters
    """
    settings['host'] = host
    settings['port'] = port
    settings['username'] = username
    settings['password'] = password
    settings['database'] = database

def connect(db = "epi_all"):
    """
    Create an engine used to connect to the database
    
    Parameters:
    - db The database name
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
