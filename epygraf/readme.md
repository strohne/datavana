# Epiygraf Python Package

The Epygraf package makes working with [Epigraf](https://epigraf.inschriften.net/) data from Python easier.

## Installation

You can install the package directly from GitHub:

```
pip install "git+https://github.com/strohne/datavana.git#egg=epygraf&subdirectory=epygraf"
```

Alternatively, clone the repository and from inside the epygraf folder call:
```
pip install -e .
```

The e option is useful when developing the package, you can change code without reinstalling the package. 
In Jupyter, after installing or changing the package, don't forget to restart the kernel.

## Usage

There are two ways to access Epigraf data: 

- **API**: Used to access data and create jobs from outside the server. 
- **Database**: Presumes you have a direct connection to the database server, e.g. in a development environment. 

## Api functions 

The endpoints for accessing article data can be found in the [Epigraf help](https://epigraf.inschriften.net/help/epiweb-api). To get an access token for nonpublic data access follow the instructions in the help. After loading the Epygraf package, you configure the connection to the API:

```
import epygraf as epi 

apiserver = "https://epigraf-dev.uni-muenster.de"
apitoken = "MYACCESSTOKEN"

epi.api.setup(apiserver, apitoken)
```

The access token is like a password, don't show it to anyone and make sure it is not printed in any logs or outputs.

Note: If you are working as a developer in a local environment, use the URL https://127.0.0.1/. The function `api.setup()` provides a third parameter for enabling debug output.

If you get an "Error 401" when using the following methods, check your permissions.

To warm up, try to get an article list. The following method fetches articles (first parameter) without any further search filters (second parameter) from the database epi_all (third parameter). Results are paginated, depending on the endpoint you only get the first 50 or 100 results in one requests. The last parameter defines the number of pages that are requested. Please be aware: at the moment the API is under development and not yet fast. Please don't stress the servers.

```
epi.api.table("articles/index", params={}, db="epi_all", maxpages=5)
```

Api access not only provide functions to fetch data, you can also import, write or annotate data. For example, you can create or update properties and articles. Therefore you can use the function `api.patch()`. On the properties page, datasets are imported into the properties table with the selected category set as propertytype. On the articles page, articles are imported with their associated sections and content.

The following command creates one categorie "Hansestädte" with the IRI "properties/topics/hanseatic" in the database epi_all.

```
data = pd.DataFrame({
  "id": ["properties/topics/hanseatic"],
  "lemma": ["Hansestädte"]
})

database = "epi_all"

epi.api.patch(data, database)
```

If a property with the given IRI already exists, it will not be created, but updated. This way you can change the labels.

If you used a new propertytype, "topics" in the example, you need to configure the type in the config menu of EpiWeb. Thereafter, you can see the new properties in EpiWeb by clicking the categories menu button. 

Trouble shooting:
- If you get the error "Error loading data from source" the data could not be uploaded to the server. Ask a developer for help.

## Database functions

Given you have direct access to an Epigraf server, 
you can show all article records from the epi_all-database 
by calling :

```
import epygraf as epi

epi.db.table("articles", "epi_all")
```

If your server runs under different settings than default, 
setup the connection settings first:

```
epi.db.setup(
    host="localhost", 
    port=3306, 
    username="root", 
    password="root" 
)
```

The table method returns a pandas dataframe, thus,
you can directly analyze the data:

```
articles = epi.db.table("articles")
articles.articletype.value_counts()
```

## All functions 

| Function         | Description                                                                                                                                                                                                     |
| --------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| api.setup()   | Save API connection settings to environment variables.                                                                                                                                                                    |
| api.build_url()    | Build base URL. Example: `api.build_url("endpoint", {"param1":"value1"}, "database", "csv")`                                                                                                                                                                   |
| api.table()     | Download tabular data. Example: `api.table("tablename", {"param1":"value1"}, "database", maxpages = 3)`                                                                                                                                                               |
| api.job_create()   | Create a job. Example: `api.job_create("endpoint_name", {"param1": "value1"}, "example_db", payload={"key": "value"})` 
|api.job_execute()  |Execute a job. Example: `api.job_execute("job_id")`. Job functions are integrated in `api.patch()`                                                                                                                                                  |
| api.patch()    | Update records in the database using the API. Existing records will be updated, missing records will be created. The function supports uploading all data related to articles: articles, sections, items, links, footnotes, properties, projects, users, types. The IRI path in the ID column of the dataframe must contain the specific table name. Example: `api.patch(data_frame, "database", "tablename", "type", wide=True)`                                                                              |
| base.create_iri()       | Create a clean IRI. Example: `base.create_iri("tablename", "type", "IRI-fragment")`                                                                                                                                          |
| base.clean_irifragment()   | Replaces all non alphanumeric characters by hyphens and converts the input to lowercase. Example: `base.clean_irifragment("dirtyIRI-Fragment")`        |
| base.is_iripath()   | Check whether the provided vector contains a valid IRI path. The combination of table, type and fragment is called an IRI path in Epigraf. Example for the given vector 'paths': `paths = pd.Series(["articles/epi-article/mv~5627", "items/text/di-103-17", "invalid/path"]) base.is_iripath(paths)` |
| base.is_id()     | Check whether the provided vector contains valid IDs prefixed with table names. Example: `base.is_id("articles-123")`. This will print 'True'. `base.is_id("invalid-id")`. This will print 'False'. |
| base.is_irifragment()    | Check whether the provided vector contains a valid IRI fragment. Example for the given vector 'iri-fragments': `iri_fragments = pd.Series(["di-103-17", "invaliDfragment", "mv~5627"]) base.is_irifragment(iri_fragments)`      |
| base.extract_wide()    | Select nested data from prefixed columns. Example for the given dataframe 'data': `data = pd.DataFrame({"data.id": [1, 2, 3], "data.value": [10, 20, 30],"other.id": [4, 5, 6]}) base.extract_wide(data, "data") ` This command creates a DataFrame containing only columns with the prefix 'data' and the prefix will be removed.                                     |
| base.wide_to_long() |Convert wide to long format. Example for the given dataframe 'data_wide' in wide format: `wide_data = pd.DataFrame({'id': ['item1', 'item2', 'item3'],'properties.lemma': ['Schwert', 'Tafel', 'Grab'],'projects.name': ['project1', 'project2', 'project3'],})    base.wide_to_long(wide_data)`                      This will return a dataframe with all input rows and the nested records stacked.                                                                                                                           |
| base.create_properties()           | Creates properties table. Example: `propertytype = "example_property" lemmata = ["lemma1", "lemma2", "lemma3"] names = ["name1", "name2", "name3"] irifragments = ["fragment1", "fragment2", "fragment3"] base.create_properties(propertytype, lemmata, names, irifragments)` This will create a dataframe with compounded iri fragments.                                                                                                                                     |
| base.create_sections()    | Create sections for each row in the input data frame. A data frame containing the columns articletype and norm_iri is needed. Example for the given dataframe 'data': `data = pd.DataFrame({'articletype': ['article1', 'article2', 'article3'],'norm_iri': ['iri1', 'iri2', 'iri3'],}) sectiontype = "example_section" sections = base.create_sections(data, sectiontype)`. 'Sections' is a dataframe representing the created sections.                                                                               |
| base.create_empty_items()   |  Create one empty item for each section. Example for a given dataframe 'sections': `itemtype = "example_item" base.create_empty_items(sections, itemtype)`                                                                                                  |
| base.create_property_items()         | Create filled items and properties from values.                                                                     |
| base.text2article()   | Convert text to epigraf article. Example for the given dataframe 'text': `text = pd.DataFrame({'id': [1],'project': ['Import'],'caption': ['My first text'],'content': ['Words are letters with glue']}) base.text2article(text)`                                                                                                                                                                    |
| db.setup() | Save database connection settings to environment variables. Environment variables are prefixed with "epi_" and used in db.connect() to establish the connection. Example: `db.setup(host="example.com", port=3306, username="user",    password="password", database="database")`     |
  | db.connect()   | Get a connection to a database. Before you can use this function, call db.setup() once to set the connection parameters. All parameters are stored in the environment. Example: `db.connect()`. This will connect to the database using the environment-based parameters. `connect(db="database")`. This will connect to the specified database using the provided database name. 
| db.name() | Retrieve the database name from the MySQL connection. Example: `connection = db.connect() db.name(connection)`. This will show the name of the connected database. 
| db.condition() | Create a filter condition for a field based on specified values. Example: `condition = db.condition("tablename", "fieldtofilter", "value")`. This will create a filter condition to use in `db.table()`.|
| db.table()    | Retrieve data from a table based on the specified conditions. Example: `db.table("tablename", cond=condition)`. This will retrieve records from the given table based on predefined conditions using `db.condition()`     | 
| db.databases() |  Retrieve a list of databases, optionally filtered by prefix. Example: `db.databases()`. This will print a list of all databases. `db.databases(epi=True)`. This will print a list of databases with 'epi_' prefix.| 
| db.geolocations()| Retrieve geolocations data for a given database. Example: `db.geolocations("database")`| 
|db.get_codes()|  Retrieve codes for a given database.This function retrieves codes from the 'properties' table for the specified database. It includes information about properties, their types, and hierarchical relationships. Example: `db.get_codes("database")`| 
db.get_codings() |Retrieve codes data from the specifie database. Example: `db.get_codings("database")`  |                                                                              |
