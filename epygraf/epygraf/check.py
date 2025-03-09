import pandas as pd
import re

def is_iripath(iripath, table=None, type=None):

    """
    Check whether the provided vector contains a valid IRI path.

    :param iripath: (str or list) The vector that will be proofed.
    :param table: (str or None) Check whether the path contains the table. Leave empty to allow all tables.
    :param type: (str or None) Check whether the path contains the type. Leave empty to allow all types.
    :return: (bool) True if iripath is a valid IRI path, False otherwise.
    """
    if table is None:
        table = "(projects|articles|sections|items|properties|links|footnotes|types|users)"
    if type is None:
        type = "([a-z0-9_-]+)"
    fragment = "([a-z0-9_~-]+)"
    pattern = f"^{table}/{type}/{fragment}$"
    return iripath.str.match(pattern)


def is_id(ids, table=None):
    """
    Check whether the provided vector contains valid IDs prefixed with table names.
    Example: articles-123

    :param ids: (str or list) The vector that will be checked for valid IDs.
    :param table: (str or None) Check whether the IDs are prefixed with table names. Leave empty to allow all tables.
    :return: (bool or list of bool) True for valid IDs, False otherwise.
    """
    # Handle single string input
    if not isinstance(ids, list):
        ids = [ids]

    if table is None:
        table = "(projects|articles|sections|items|properties|links|footnotes|types|users)"
    else:
        table = f"({table})"

    fragment = "([0-9]+)"
    pattern = f"^{table}.{fragment}$"

    # Match the pattern for each ID
    valid_ids = [bool(re.match(pattern, id_)) for id_ in ids]

    # Return True for a single valid ID, otherwise the list of boolean values
    if len(valid_ids) == 1:
        return valid_ids[0]
    else:
        return valid_ids


def is_irifragment(irifragment):

    """
    Check whether the provided vector contains a valid IRI fragment

    :param irifragment: (str or list) The vector that will be checked for valid IRI fragments.
    :return: (bool or list of bool) True for valid IRI fragments, False otherwise.
    """
    return irifragment.str.match("^[a-z0-9_~-]+$")


def is_db(value: str, msg: str = None) -> bool:
    """
    Check whether a value is a valid database name, and raise an error if not.

    :param value: A string value
    :param msg: A custom error message if the check fails
    :return: True if the value is valid, raises ValueError otherwise
    """
    if not isinstance(value, str):
        msg = msg or f"The value {value} is not a valid Epigraf database name."
        raise ValueError(msg)
    return True