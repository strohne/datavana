import os
from urllib.parse import urlparse, urlunparse, urlencode
import pandas as pd
import io
import requests
from epygraf import base

def setup(apiserver, apitoken, verbose=False): 
    
    """
    Save API connection settings to environment variables.

    :param apiserver: (str) URL of the Epigraf server
                        (including https-protocol)
    :param apitoken: (str) Access token
    :param verbose: (bool) Show debug messages and the built URLs
                    (default is False)
    """
    settings = dict(locals())
    settings = {f"epi_{key}": str(value) for key, value in settings.items()}
    settings['epi_verbose'] = str(verbose).upper()
    os.environ.update(settings)


def build_url(endpoint, query=None, database=None, extension="json"):
    
    """
    Build base URL.

    :param endpoint: (str) The endpoint, e.g. articles/import
    :param query: (dict or None) Query parameters for the endpoint
    :param database: (str or None) The database name
    :param extension: (str) Extension added to the URL path, defaults to json.
    :return: (str) The built URL
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
    
    parsed_url = parsed_url._replace(path=path, query=urlencode(parsed_query))

    url = urlunparse(parsed_url)

    if verbose:
        print(url)

    return url


def table(endpoint, params=None, db=None, maxpages=1, silent=False):

    """
    Download tabular data.

    :param endpoint: (str) The endpoint path (e.g. "articles/index" or "articles/view/1")
    :param params: (dict) A named dictionary of query parameters
    :param db: (str) The database name
    :param maxpages: (int) Maximum number of pages to request.
                    Set to 1 for non-paginated tables.
    :param silent: (bool) Whether to output status messages
    :return: (pandas.DataFrame) The downloaded tabular data
    """
    verbose = True if os.getenv("epi_verbose") == "TRUE" else False

    data = pd.DataFrame()
    page = 1

    fetchmore = True
    while fetchmore:
        if params is None:
            params = {}
        params["page"] = page
        url = build_url(endpoint, params, db, "csv")
        ext = ".csv"

        if (not silent):
            if maxpages == 1:
                print(f"Fetching data from {endpoint}.")
            else:
                print(f"Fetching page {page} from {endpoint}.")
        message = None

        try:
            if verbose:
                resp = requests.get(url, cookies={'XDEBUG_SESSION' : 'XDEBUG_ECLIPSE'})
            else:
                resp = requests.get(url)

            if resp.status_code == 200:
                body = resp.text
                rows = pd.read_csv(io.StringIO(body), delimiter=";", dtype=str)
            elif resp.status_code == 404:
                message = "No more data found."
                rows = pd.DataFrame()
            else:
                rows = pd.DataFrame()
                message = f"Error {resp.status_code}: {resp.text}"

        except Exception as e:
            message = str(e)
            rows = pd.DataFrame()

        if message is not None:
            print(message)

        if not rows.empty:
            data = pd.concat([data, rows], ignore_index=True)
            fetchmore = page < maxpages
            page += 1
        else:
            fetchmore = False

    if not silent:
        print(f"Fetched {data.shape[0]} records from {endpoint}.")

    # Convert columns to appropriate types
    data = data.convert_dtypes()
    data = to_epitable(data, {'endpoint': endpoint, 'params': params, 'db':db})
    return data



# Create and execute a job
def job_create(endpoint, params, database, payload=None):

    """
    Create and execute a job.

    :param endpoint: (str) The endpoint supporting job creation
    :param params: (dict) Query parameters
    :param database: (str) The selected database
    :param payload: (object or None) The data posted to the job endpoint
    :return: None
    """
    server = os.getenv("epi_apiserver")
    verbose = True if os.getenv("epi_verbose") == "TRUE" else False
    
    print(f"Creating job on server {server}")
    
        # 1. Create job
    url = build_url(endpoint, params, database)
    
    if verbose:
        resp = requests.post(url, json=payload, cookies={"XDEBUG_SESSION": "XDEBUG_ECLIPSE"})
    else:
        resp = requests.post(url, json=payload)
    
    body = resp.json()
    job_id = body.get("job_id", None)
    
    error = False
    message = None
    
    # Request error
    if resp.status_code != 200:
        error = True
        message = body.get("error", {}).get("message", None)
    
    # Job error
    elif not body.get("success", True):
        error = True
        message = body.get("message", None)
    
    # No job ID
    elif job_id is None:
        error = True
        message = "No job ID found."
    
    if error:
        raise Exception(f"Could not create job: {message}")
    
    if message is not None:
        print(message)
    
    # 2. Execute job
    job_execute(job_id)

# Execute a job
def job_execute(job_id):

    """
    Execute a job.

    :param job_id: (str) The job ID
    :return: (bool) Whether the job was finished without error.
    """

    verbose = True if os.getenv("epi_verbose") == "TRUE" else False
   
    print(f"Starting job {job_id}.")

    url = build_url(f"jobs/execute/{job_id}", None, None)

    polling = True
    while polling:
        if verbose:
            resp = requests.post(url, cookies={"XDEBUG_SESSION": "XDEBUG_ECLIPSE"})
        else:
            resp = requests.post(url)

        body = resp.json()

        # Request error
        if resp.status_code != 200:
            polling = False
            error = True
            message = body.get("error", {}).get("message", None)

        # Job error
        elif body.get("job", {}).get("error", False):
            polling = False
            error = True
            message = body.get("job", {}).get("error", None)

        # Continue
        elif "job" in body and "nexturl" in body["job"]:
            polling = True
            error = False
            message = body.get("job", {}).get("message", None)
            progressCurrent = body.get("job", {}).get("progress", None)
            progressMax = body.get("job", {}).get("progressmax", -1)
            if progressMax == -1:
                print(f"Progress {progressCurrent}")
            else:
                print(f"Progress {progressCurrent} / {progressMax}")

        # Finished
        else:
            polling = False
            error = False
            message = body.get("message", None)

        # Output
        if error:
            raise Exception(f"Could not execute job: {message}")

        if message is not None:
            print(message)

    return not (polling or error)


def patch(data, database, table=None, type=None, wide=True):

    """
    Update records in the database using the API.
    Existing records will be updated, missing records will be created.
    The function supports uploading all data related to articles:
    articles, sections, items, links, footnotes, properties, projects, users, types.
    The IRI path in the ID column of the dataframe must contain the specific table name.

    :param data: (pandas.DataFrame) A dataframe with the column id
                 (must be a valid IRI path).
                 Additional columns such as norm_data will be written to the record.
    :param database: (str) The database name
    :param table: (str or None) Check that the data only contains rows for a specific table
    :param type: (str or None) Check that the data only contains rows with a specific type
    :param wide: (bool) Convert wide format to long format
    :return: None
    """
    
    if wide:
        data = base.wide_to_long(data)

    # Process data (remove empty columns and rows with all NA values)
    data = data.loc[:, data.notna().all()]
    data = data.dropna(how="all")

    if data.empty:
        raise Exception("Data is empty or contains NA values.")

    if len(data.columns) == 1 and "id" in data.columns:
        raise Exception("Skipped, the data only contains the ID column.")

    print(f"Uploading {len(data)} rows.")

    job_create("articles/import", None, database, {"data": data.to_dict(orient="records")})

def patch_wide(data, database):
    """
    Patch data and create related properties, items, sections, articles, and projects

    Args:
        data: A dataframe with the column id containing a valid IRI path.
              Additional columns such as norm_data will be written to the record.
              Column names prefixed with "properties", "items", "sections", "articles"
              and "projects" followed by a dot (e.g. "properties.id", "properties.lemma")
              will be extracted and patched as additional records.
        database: The database name

    Returns: None

    """
    rows = base.wide_to_long(data)
    patch(rows, database)


def to_epitable(data: pd.DataFrame, source: dict = None) -> pd.DataFrame:
    """
    Add the epigraf source and type attributes to the DataFrame

    :param data: A pandas DataFrame
    :param source: A dictionary of source parameters, containing endpoint, parameters, and database name
    :return: Modified DataFrame with metadata in the attrs property
    """
    # Set source
    if source is not None:
        data.attrs["epi_source"] = source

    # Reorder columns
    id_cols = [col for col in  ["database", "table", "row", "type", "norm_iri"] if col in data.columns]
    belongsto_idcols = [col for col in data.columns if col.endswith("id")]
    belongsto_namedcols = [col for col in ["project","article","section","item","property","footnote"] if col in data.columns]
    state_cols = [col for col in data.columns if col.startswith(("created", "modified"))]
    content_cols = [col for col in data.columns if col not in id_cols + belongsto_idcols + belongsto_namedcols + state_cols]

    ordered_cols = id_cols + content_cols + belongsto_idcols + state_cols
    data = data[ordered_cols]

    # Add Epigraf type attribute
    data.attrs["epi_type"] = "table"
    return data