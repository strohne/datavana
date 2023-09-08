import pandas as pd
import re

# Create a clean IRI
def create_iri(table, type, fragment):
    cleaned_fragment = epi_clean_irifragment(fragment)
    if type is None:
        return f"{table}/{cleaned_fragment}"
    else:
        return f"{table}/{type}/{cleaned_fragment}"

# Create a clean IRI fragment
def clean_irifragment(fragment):
    cleaned_fragment = fragment.lower()
    cleaned_fragment = re.sub("ä", "ae", cleaned_fragment)
    cleaned_fragment = re.sub("ö", "oe", cleaned_fragment)
    cleaned_fragment = re.sub("ü", "ue", cleaned_fragment)
    cleaned_fragment = re.sub("ß", "ss", cleaned_fragment)
    cleaned_fragment = re.sub("[^a-z0-9_~-]", "-", cleaned_fragment)
    cleaned_fragment = re.sub("-+", "-", cleaned_fragment)
    cleaned_fragment = cleaned_fragment.strip("-")
    return cleaned_fragment

# Check whether the provided vector contains a valid IRI path
def is_iripath(iripath, table=None, type=None):
    if table is None:
        table = "(projects|articles|sections|items|properties|links|footnotes|types|users)"
    if type is None:
        type = "([a-z0-9_-]+)"
    fragment = "([a-z0-9_~-]+)"
    pattern = f"^{table}/{type}/{fragment}$"
    return iripath.str.match(pattern)

# Check whether the provided vector contains valid IDs prefixed with table names
def is_id(ids, table=None):
    if table is None:
        table = "(projects|articles|sections|items|properties|links|footnotes|types|users)"
    fragment = "([0-9]+)"
    pattern = f"^{table}.{fragment}$"
    return ids.str.match(pattern)

# Check whether the provided vector contains a valid IRI fragment
def is_irifragment(irifragment):
    return irifragment.str.match("^[a-z0-9_~-]+$")

# Select nested data from prefixed columns
def extract_wide(data, cols_prefix, cols_keep=[]):
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


# Create properties
def create_properties(propertytype, lemmata, names=None, irifragments=None):
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
    sections = pd.DataFrame({
        "articles_id": f"articles/{data['articletype']}/{data['norm_iri']}",
        "id": f"sections/{sectiontype}/{data['norm_iri']}",
        "name": name
    })

    return sections

# Create one empty item for each section
def create_empty_items(sections, itemtype):
    items = sections[["id", "articles_id"]].copy()
    items["id"] = items["id"].str.replace(r"sections/", f"items/{itemtype}/")
    return items

# Create filled items and properties from values
def create_property_items(data, col_articletype, col_value, col_prop, sectiontype, itemtype):
    properties = epi_create_properties("coding-sample", data[col_prop].unique())

    items = data.copy()
    items["id"] = items["id"].str.replace(r"articles/", f"items/{itemtype}/")
    items["sections_id"] = items["id"].str.replace(f"items/{itemtype}/", f"sections/{sectiontype}/")
    items["articles_id"] = items["id"].str.replace(f"items/{itemtype}/", f"articles/{itemtype}/")
    items = items.rename(columns={col_value: "value", col_prop: "properties_lemma"})

    items = items.merge(properties[["id", "lemma"]], left_on="properties_lemma", right_on="lemma", how="left")
    items = items[["id", "sections_id", "articles_id", "properties_id", "value"]]

    return pd.concat([properties, items])

# Convert text to epigraf article
def text2article(text):
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
