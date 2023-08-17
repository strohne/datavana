import pandas as pd
import os
from sqlalchemy import create_engine, text
from urllib.parse import urlparse, urlunparse

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


  
def api_setup(apiserver, apitoken, verbose=False): 
    
    """ 
    Save API connection settings to environment variables
    """
    
    settings = dict(locals())
    settings = {f"epi_{key}": str(value) for key, value in settings.items()}
    os.environ.update(settings)


def api_buildurl(endpoint, query=None, database=None, extension="json"):
    
    """
    Build base URL
    """

    # Get server and token from the global settings
    server = os.getenv("epi_apiserver")
    token = os.getenv("epi_apitoken")
    verbose = os.getenv("epi_verbose") == "TRUE"

    parsed_url = urlparse(server)
    parsed_query = dict(parsed_url.query)

    parsed_query["token"] = token
    
    # Add query parameters
    if query is not None:
        parsed_query.update(query)

    if not endpoint.startswith("/"):
        endpoint = "/" + endpoint

    if extension is None:
        extension = ""
    elif extension and not extension.startswith("."):
        extension = "." + extension

    if database is not None:
        path = f"epi/{database}{endpoint}{extension}"
    else:
        path = f"{endpoint}{extension}"

    parsed_url = parsed_url._replace(path=path, query="")

    url = urlunparse(parsed_url)

    if verbose:
        print(url)

    return url

