# Facepager Package


## Installation
As with all other packages you'll have to install the package and load the library first.

```
library(devtools)
install_github("strohne/datavana/facepager")
library(facepager)
```


## Load data

Now you can access the various functions of the facepager package that allow you to read data. Depending on the format of your data, you can use different functions. In total you can load the following data types:

* csv files
* one Facepager database or multiple Facepager databases at the same time (file extension .db)
* certain columns from a Facepager database


#### Example script to load different files

*Please note that the system.file workaround is only used here to be able to use the example data of the package. Normally it is possible to read the data in the usual way only with the filename.*

```
# Load a CSV file with fp_read_csv2(): works the same way as the
# read_csv2 function from the tidyverse package. So you'll see a
# tibble containing a representation of the data in your csv file.

data <- fp_read_csv2(system.file("extdata", "example.csv", package = "facepager"))


# Load a complete database with fp_loaddatabase(): It is also
# possible to load the data directly from the .db file of the
# Facepager database.

data_db <- fp_load_database(system.file("extdata", "example.db", package = "facepager"))


# The data will be contained in the response column as JSON. Therefore
# you need to convert the JSON data in the response to own columns
# with fp_parse_response_data().

data_db <- fp_parse_response_data(data_db)

```

In case you have performed several data collections with Facepager and accordingly have several .db files, you can load them easily with `fp_loaddatabases()` at the same time.This way you get one big combined dataset instead of loading the Facepager databases individually.


## Extract data

Now you can access the various functions that allow you to go deeper into the different data levels. Even though the explanations of the functions appear here in a certain order, this does not mean that this is a chronological approach. Rather, the functions should be seen on one level. Mostly, you can use them to check the completeness of the dataset.

As you may already know, Facepager always collects data on multiple levels, also called [parent and child levels](https://github.com/strohne/Facepager/wiki/URLs,-Placeholders,-Nodes-and-Keys#nodes). The following functions mostly refer to these terms.

#### Possible functions for data extraction

| Function | Description |
| -------- | ----------- |
|`fp_children_bynode()`: Get the number of childnodes for each node shown in Facepager (including duplicates) | First of all, you can view how many child nodes were collected per object ID/parent node. With `fp_children_bynode()` you get the exact number of children per "parent_objectid". Additionally you get the position of the dataset in Facepager with "parent_no" and a boxplot for the distribution of the number of children. This number includes all children of a parent, including duplicates.|
| `fp_childcount()`: Count the number of different children per Object ID (without duplicates) | If you want to find out how many single or different children an Object ID produces, you can use `fp_childcount()`. Using this function, the duplicates are not included in the count. See fp_duplicates for getting these excluded nodes.|
| `fp_children()`: Get the collected data per Object ID |`fp_children()` gives you not only the number of children per Object ID, but also: (1) the number of offcuts and the last cursor of the pagination per object ID, (2) the first and last date of the children per object ID and (3) if children are missing (if there is a target, how many children should be collected) |
| `fp_getparents()`: Get the Object IDs and number of parent nodes | `fp_getparents()` can be used to get details about the parents. You get an overview of (1) which object IDs were used ("parent_objectid"), (2) which position they have in the facepager request ("parent_no") and (3) which ID they have been assigned to ("parent_id").|
| `fp_extracttags()`: Get the tags included in the nodes (urls, mentions, hashtags) | Passt die Funktion wirklich hier hin? |


#### Example script to extract data

```
# get number of all child nodes (without duplicates)
fp_children_bynode(data)

# get number of different child nodes per Object ID
childcount <- fp_childcount(data)
print(childcount)

# get collected data per Object ID
fp_children(data)
print(children)

# get object IDs & number of parent nodes
fp_getparents(data)

# get urls, mentions & hashtags included in the nodes
fp_extracttags(data)

```

## Statistics 

Another bundle of functions from the Facepager Package concern the query itself. With these functions you get data with which you can evaluate the quality of the Facepager collection.


#### Get status details of the query with `fp_status()`

With `fp_status()` you can get a first overview of your query and its results.

* How many nodes were queried via which Facepager module (e.g. Twitter or Facebook) and which endpoint (e.g. "search/tweets" or "page-id/posts")?

* For how many nodes was the query successful ("fetched(200)")?

* And how many nodes actually contain data ("data")?

```
fp_status(data_db)
```

#### Get requests per minute with `fp_timing()`

Especially if you are conducting surveys over a long period of time, it can be useful to get an overview over time as well. With `fp_timing()` you get details and plots about the time distribution of the requests.

* When did the survey start?

* When was it finished?

* How long did it last (in hours, minutes, seconds)?

* How many queries were sent per day?

```
fp_timing(data_db)
```


#### Get the number of childnodes for each seed for a certain period of time with `fp_nodesperseed()`

If you want to look more closely at a certain period of time, you can use `fp_nodesperseed()`. This function makes it possible to specify an exact daterange (e.g. `daterange = ymd(c("2019-11-01","2020-02-01"),tz="Europe/Berlin"`) and get exactly for it the children per parent (without duplicates).

```
fp_nodesperseed(data,
                daterange = ymd(c("2022-11-01","2022-11-02"), tz="Europe/Berlin"),
                col_created = quo(created_time)
                )

```

#### Evaluate duplicates on different node levels with `fp_duplicates()`

If you want to check how many unique/duplicate nodes were collected, you can use `fp_duplicates()`. This function extracts duplicates at three different levels:

1. At the parent level,

2. on the level of the children with the same parent/seed,

3. on the level of the children independent of the parent.

```
fp_duplicates(data_db)

```

