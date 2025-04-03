import pandas as pd
from tqdm import tqdm
from epygraf import api, check


def table(table: str, columns=None, params=None, db: str = None, maxpages: int = 1):
    """
    Fetch tables such as articles, projects, or properties.

    :param table: The table name (e.g., "articles")
    :param columns: A list of column names
    :param params: A dictionary of query parameters
    :param db: The database name
    :param maxpages: Maximum number of pages to request. Set to 1 for non-paginated tables.
    :return: Data from the API
    """
    if columns is None:
        columns = []
    if params is None:
        params = {}

    columns = ["id"] + list(set(columns))
    params["columns"] = ",".join(columns)
    params["idents"] = "id"

    return api.table(table, params, db, maxpages)


def entity(ids, params=None, db: str = None, silent: bool = False):
    """
    Fetch entities such as single articles, projects, or properties.

    :param ids: A list of IDs or a DataFrame containing an 'id' column
    :param params: A dictionary of query parameters
    :param db: The database name (extracted if not provided when IDs are from a DataFrame)
    :param silent: Whether to suppress status messages
    :return: Data from the API
    """
    if params is None:
        params = {}

    # Extract database name if IDs come from a DataFrame
    if db is None and isinstance(ids, pd.DataFrame) and isinstance(ids.attrs.get("epi_source"), dict):
        db = ids.attrs["epi_source"].get("db")

    check.is_db(db)

    # Extract ID list if given a DataFrame
    if isinstance(ids, pd.DataFrame):
        ids = ids["id"].tolist()

    if not isinstance(ids, list):
        ids = [ids]

    if len(ids) == 0:
        return api.to_epitable(pd.DataFrame(), {"params": params, "db": db})

    if len(ids) > 1:
        data = pd.DataFrame()
        for idx, id_val in enumerate(tqdm(ids, desc="Fetching entities")):
            data = pd.concat([data, entity(id_val, params, db, silent=True)], ignore_index=True)
        return data

    elif len(ids) == 1:
        id = ids[0]

    check.is_id(id)
    table, row_id = id.split("-", 1)

    data = api.table(f"{table}/view/{row_id}", params, db, 1, silent=silent)

    if "id" in data.columns:
        data[["table", "row"]] = data["id"].str.split("-", n=1, expand=True)

    data = api.to_epitable(data)
    return data