import pandas as pd
import os
from sqlalchemy import create_engine, text
from urllib.parse import urlparse, urlunparse


def setup(apiserver, apitoken, verbose=False): 
    
    """ 
    Save API connection settings to environment variables
    """
    
    settings = dict(locals())
    settings = {f"epi_{key}": str(value) for key, value in settings.items()}
    os.environ.update(settings)


def build_url(endpoint, query=None, database=None, extension="json"):
    
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
