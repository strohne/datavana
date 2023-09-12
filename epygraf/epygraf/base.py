import pandas as pd
import re


def create_iri(table, type, fragment):

     """
    Create a clean IRI.

    :param table: (str) The table name
    :param type: (str or None) If None, the type will be omitted.
    :param fragment: (str) The IRI fragment that will be cleaned
    :return: (str) The clean IRI
    """
    cleaned_fragment = epi_clean_irifragment(fragment)

    if type is None:
        return f"{table}/{cleaned_fragment}"
    else:
        return f"{table}/{type}/{cleaned_fragment}"


def clean_irifragment(fragment):
    
    """
    Create a clean IRI fragment.

    Replaces all non-alphanumeric characters by hyphens
    and converts the input to lowercase.

    :param fragment: (str) The dirty IRI fragment that will be cleaned
    :return: (str) The clean IRI fragment
    """
    cleaned_fragment = fragment.lower()
    cleaned_fragment = re.sub("ä", "ae", cleaned_fragment)
    cleaned_fragment = re.sub("ö", "oe", cleaned_fragment)
    cleaned_fragment = re.sub("ü", "ue", cleaned_fragment)
    cleaned_fragment = re.sub("ß", "ss", cleaned_fragment)
    cleaned_fragment = re.sub("[^a-z0-9_~-]", "-", cleaned_fragment)
    cleaned_fragment = re.sub("-+", "-", cleaned_fragment)
    cleaned_fragment = cleaned_fragment.strip("-")
    return cleaned_fragment


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
    Check whether the provided vector contains valid IDs prefixed with table names. Example: articles-123

    :param ids: (str or list) The vector that will be checked for valid IDs.
    :param table: (str or None) Check whether the IDs are prefixed with table names. Leave empty to allow all tables.
    :return: (bool or list of bool) True for valid IDs, False otherwise.
    """
    if table is None:
        table = "(projects|articles|sections|items|properties|links|footnotes|types|users)"
    fragment = "([0-9]+)"
    pattern = f"^{table}.{fragment}$"
    return ids.str.match(pattern)


def is_irifragment(irifragment):

     """
    Check whether the provided vector contains a valid IRI fragment

    :param irifragment: (str or list) The vector that will be checked for valid IRI fragments.
    :return: (bool or list of bool) True for valid IRI fragments, False otherwise.
    """
    return irifragment.str.match("^[a-z0-9_~-]+$")


def extract_wide(data, cols_prefix, cols_keep=[]):

    """
    Select nested data from prefixed columns.

    :param data: (pandas.DataFrame) A data frame
    :param cols_prefix: (str) All columns with the prefix will be selected,
                        the prefix will be removed from the column name.
    :param cols_keep: (list of str) Convert the provided column names to underscored columns.
    :return: (pandas.DataFrame) A dataframe containing all columns with the prefix without the prefix.
    """
    if len(cols_keep) > 0:
        regex_keep = "|".join([f"{col}.id|{col}_id" for col in cols_keep])
        regex_keep = f"^{regex_keep}$"
    else:
        regex_keep = "^$"

    prefix_columns = [col for col in data.columns if col.startswith(f"{cols_prefix}.")]

    data = data[prefix_columns + cols_keep]

    data = data.rename(columns=lambda x: x.replace(f"{cols_prefix}.", "").replace(".", "_"))

    data = data.dropna(how="all")

    if data.empty:
        data = pd.DataFrame()

    return data


def wide_to_long(data):

    """
    Convert wide to long format.

    :param data: (pandas.DataFrame) A dataframe with the column id containing a valid IRI path
                 and additional columns. The additional columns may contain nested
                 data in the following form:
                 Column names prefixed with "properties", "items", "sections",
                 "articles" and "projects" followed by a dot (e.g. "properties.id",
                 "properties.lemma") will be extracted and stacked to the dataframe.
    :return: (pandas.DataFrame) A dataframe with all input rows and the nested records stacked.
    """
    extracted_data = []

    # Extract data from prefixed columns
    for prefix in ["properties", "projects", "articles", "sections", "items"]:
        cols = [col for col in data.columns if col.startswith(prefix + ".")]
        if cols:
            extracted = data[["id"] + cols].rename(columns={col: col[len(prefix) + 1:] for col in cols})
            extracted["table"] = prefix
            extracted_data.append(extracted)

    # Select columns of object dtype (if any)
    extracted = data.select_dtypes(include=["object"])
    if not extracted.empty:
        extracted_data.append(extracted)

    if extracted_data:
        rows = pd.concat(extracted_data, ignore_index=True)

        # Drop rows with all NaN values
        rows = rows.dropna(how="all")

        return rows

    return pd.DataFrame()  # Return an empty DataFrame if no data is extracted



def create_properties(propertytype, lemmata, names=None, irifragments=None):

    """
    Creates properties table.

    :param propertytype: (str) The property type (character)
    :param lemmata: (list of str) A vector of lemmata (character)
    :param names: (list of str or None) Optional. A vector of names (character)
    :param irifragments: (list of str or None) Optional. A vector of IRI fragments (character)
    :return: (pandas.DataFrame) A dataframe representing the properties table.
    """
    properties = pd.DataFrame({
        "lemma": lemmata,
        "name": names if names is not None else lemmata,
        "irifragment": irifragments
    })

    iri_path = f"properties/{propertytype}/"

    properties["id"] = properties.apply(lambda row: f"{iri_path}{row['irifragment']}" if pd.notna(row['irifragment']) else f"{iri_path}{row.name}", axis=1)

    properties = properties[["id", "lemma", "name"]]

    return properties

# Create sections
def create_sections(data, sectiontype, name=None):

    """
    Creates a section for each row in the input data frame.

    :param data: (pandas.DataFrame) A data frame containing the columns articletype and norm_iri
    :param sectiontype: (str) A string with the sectiontype
    :param name: (str or None) The section name. Leave empty to use the name defined in the Epigraf domain model.
    :return: (pandas.DataFrame) A dataframe representing the created sections.
    """
    sections = pd.DataFrame({
        "articles_id": f"articles/{data['articletype']}/{data['norm_iri']}",
        "id": f"sections/{sectiontype}/{data['norm_iri']}",
        "name": name
    })

    return sections

def create_empty_items(sections, itemtype):

    """
    Create one empty item for each section.

    :param sections: (pandas.DataFrame) A data frame containing the sections
    :param itemtype: (str) A string with the name for the itemtype
    :return: (pandas.DataFrame) A dataframe representing the created empty items.
    """
    items = sections[["id", "articles_id"]].copy()
    items["id"] = items["id"].str.replace(r"sections/", f"items/{itemtype}/")
    return items

def create_property_items(data, col_articletype, col_value, col_prop, sectiontype, itemtype):

    """
    Create filled items and properties from values.

    :param data: (pandas.DataFrame) A data frame containing the columns articletype, norm_iri, value, and propertytype
    :param col_articletype: (str) The column in data specifying the articletype
    :param col_value: (str) The column in data specifying the value
    :param col_prop: (str) The column in data containing the propertytype
    :param sectiontype: (str) A string with the name of the section
    :param itemtype: (str) A string with the name of the itemtype
    :return: (tuple of pandas.DataFrame) A tuple containing dataframes representing the created properties and items.
    """
    properties = epi_create_properties("coding-sample", data[col_prop].unique())

    items = data.copy()
    items["id"] = items["id"].str.replace(r"articles/", f"items/{itemtype}/")
    items["sections_id"] = items["id"].str.replace(f"items/{itemtype}/", f"sections/{sectiontype}/")
    items["articles_id"] = items["id"].str.replace(f"items/{itemtype}/", f"articles/{itemtype}/")
    items = items.rename(columns={col_value: "value", col_prop: "properties_lemma"})

    items = items.merge(properties[["id", "lemma"]], left_on="properties_lemma", right_on="lemma", how="left")
    items = items[["id", "sections_id", "articles_id", "properties_id", "value"]]

    return pd.concat([properties, items])


def text2article(text):

    """
    Convert text to epigraf article.

    :param text: (pandas.DataFrame) Dataframe with the columns id, project, caption and content
    :return: (pandas.DataFrame) Dataframe with article, section and item
    """
    projects = pd.DataFrame({
        "table": "projects",
        "type": "default",
        "id": f"projects-int{text['project'].unique()[0]}",
        "name": text["project"].unique()
    })

    articles = pd.DataFrame({
        "table": "articles",
        "type": "default",
        "id": f"articles-int{text['id']}",
        "projects_id": f"projects-int{text['project'].unique()[0]}",
        "name": text["caption"]
    })

    sections = pd.DataFrame({
        "table": "sections",
        "type": "default",
        "id": f"sections-int{text['id']}",
        "articles_id": f"articles-int{text['id']}",
        "name": "Text"
    })

    items = pd.DataFrame({
        "table": "items",
        "type": "default",
        "id": f"items-int{text['id']}",
        "sections_id": f"sections-int{text['id']}",
        "articles_id": f"articles-int{text['id']}",
        "content": text["content"]
    })

    return pd.concat([projects, articles, sections, items])
