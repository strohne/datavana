# Epiygraf Python Package

The Epygraf package makes working with Epigraf data from Python easier.

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

Note: If you are working as a developer in a local environment, use the URL https://127.0.0.1/. The api.setup()-function provides a third parameter for enabling debug output.

If you get an "Error 401" when using the following methods, check your permissions.

To warm up, try to get an article list. The following method fetches articles (first parameter) without any further search filters (second parameter) from the database epi_all (third parameter). Results are paginated, depending on the endpoint you only get the first 50 or 100 results in one requests. The last parameter defines the number of pages that are requested. Please be aware: at the moment the API is under development and not yet fast. Please don't stress the servers.

```
epi.api.table("articles/index", params={}, db="epi_all", maxpages=5)

```

Api access not only provide functions to fetch data, you can also import/write/annotate data. For example, you can create or update properties and articles. Therefore you can use the function api.patch(). On the properties page, datasets are imported into the properties table with the selected category set as propertytype. On the articles page, articles are imported with their associated sections and content.

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
| api.setup()   |Save API connection settings to environment variables                                                                                                                                                                       |
| api.build_url()    | Build base URL                                                                                                                                                                       |
| api.table()     | Download tabular data                                                                                                                                                                  |
| api.job_create(), api.job_execute()     | Create and execute a job                                                                                                                                                          |
| api.patch()    | Update records in the database using the API. Existing records will be updated, missing records will be created. The function supports uploading all data related to articles: articles, sections, items, links, footnotes, properties, projects, users, types. The IRI path in the ID column of the dataframe must contain the specific table name.                                                                               |
| base.create_iri()       | Create a clean IRI                                                                                                                                          |
| base.clean_irifragment()   | Replaces all non alphanumeric characters by hyphens and converts the input to lowercase        |
| base.is_iripath()   | Check whether the provided vector contains a valid IRI path |
| base.is_id()     | Check whether the provided vector contains valid IDs prefixed with table names |
| base.is_irifragment()    | Check whether the provided vector contains a valid IRI fragment                                                                                                                                                       |
| base.extract_wide()    | Select nested data from prefixed columns                                                                                                                                                       |
| base.wide_to_long() |Convert wide to long format                                                                                                                               |
| base.create_properties()           | Creates properties table                                                                                                                                     |
| base.create_sections()    | Create sections                                                                                             |
| base.create_empty_items()   |  Create one empty item for each section                                                                                                   |
| base.create_property_items()         | Create filled items and properties from values                                                                     |
| base.text2article()   | Convert text to epigraf article                                                                                                                                                                     |
| db.setup() | Save database connection settings to environment variables. Environment variables are prefixed with "epi_" and used in db_connect() to establish the connection.                                                                                                                                                                    |
| db.connect()   | Get a connection to a database. Before you can use this function, call db.setup() once to set the connection parameters. All parameters are stored in the environment.                                                                                                                                                   |
| db.unconnect() | Closes a database connection            
| db.table()    | Get data from a database table                                                                                                      |
