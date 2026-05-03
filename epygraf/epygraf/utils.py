import os
import re


def drop_empty_columns(df):
    return df.dropna(axis=1, how='all')

def get_extension(path):
    filename = os.path.basename(path)

    # Check if filename has a dot and something after it
    if "." in filename:
        ext_match = re.sub(r".*\.(.*)$", r"\1", filename)

        # If the dot is at the start (hidden files like .bashrc), treat as no extension
        if filename.startswith(".") and not re.search(r"\..+\.", filename):
            return ""
        else:
            return ext_match
    else:
        return ""

def confirm_action():
    """
    Ask the user to confirm script execution.

    :return: True if execution should proceed
    :rtype: bool
    :raises Exception: if the user cancels the action
    """
    silent = os.getenv("epi_silent")
    if silent == "TRUE":
        return True

    user_input = input("Are you sure you want to proceed? (y/n)  ")
    if user_input != "y":
        raise Exception("Canceled")


def is_local_server(server):
    """
    Check whether the URL is on a local server.

    :param server: The server URL
    :type server: str
    :return: True if the server is localhost or 127.0.0.1, otherwise False
    :rtype: bool
    """
    return (
        server.startswith("https://127.0.0.1") or
        server.startswith("http://127.0.0.1") or
        server.startswith("https://localhost") or
        server.startswith("http://localhost")
    )