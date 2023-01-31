# Epigraf Package 

## Why using Epigraf package? 

The Epigraf package aims to make data work with [Epigraf](https://epigraf.inschriften.net/) easier. 
It provides functions for data transfer using the Epigraf APIs: preparing data imports, e.g. from social media datasets, and preparing data analyses.

## Installation 

```
library(devtools)
install_github("strohne/datavana/epigraf")
```

## Access via the API

The endpoints for accessing article data can be found in the [Epigraf help](https://epigraf.inschriften.net/help/epiweb-api). To get an access token for nonpublic data access follow the instructions in the help. After loading the Epigraf package, you configure the connection to the API:

```
library(epigraf)
api_setup("https://epigraf.inschriften.net/", "MYACCESSTOKEN")
```

The access token is like a password, don't show it to anyone and make sure it is not printed in any logs or outputs.

Note: If you are working as a developer in a local environment, use the URL https://127.0.0.1/. The api_setup()-function provides a third parameter for enabling debug output.

If you get an "Error 401" when using the following methods, check your permissions.

To warm up, try to get an article list. The following method fetches articles (first parameter) without any further search filters (second parameter) from the database epi_public (third parameter). Results are paginated, depending on the endpoint you only get the first 50 or 100 results in one requests. The last parameter defines the number of pages that are requested. Please be aware: at the moment the API is under development and not yet fast. Please don't stress the servers.

```
articles <- api_table("articles",c(), "epi_all", 1)
```


## Writing data

Categories used to annotate or tag articles are called properties in the Epigraf data model. You can create or update such properties with api_patch_properties(). The following command creates two categories, "Klösterlandschaften" with the IRI "properties/topics/monasteries" and "Hansestädte" with the IRI "properties/topics/hanseatic" (last three parameters) in the database epi_all (first parameter).

```
api_patch_properties(
  "epi_all",
  "topics",
  c("Klösterlandschaften","Hansestädte"),
  c("monasteries", "hanseatic")
)
```

If a property with the given IRI already exists, it will not be created, but updated. This way you can change the labels.

If you used a new propertytype, "topics" in the example, you need to configure the type in the config menu of EpiWeb. Thereafter, you can see the new properties in EpiWeb by clicking the categories menu button. 

Trouble shooting:
- If you get the error "Error loading data from source" the data could not be uploaded to the server. Ask a developer for help.



## Working with social media datasets 

Get started with loading the example data and diving into the data structure:

```
library(tidyverse)
threads <- read_csv2(system.file("extdata", "threads.csv", package = "epigraf"))
```

The data contains columns usually found in social media datasets. We define the canonical form of a social media dataset as a table where each row is a page, post, comment or reply with the following columns:

| Column          | Description                                                                                                                                                                                                     |
| --------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| sample\_name    | Give your sample a name, e.g. "sample1".                                                                                                                                                                        |
| sample\_no      | The case number in your sample, e.g. 3.                                                                                                                                                                         |
| platform        | The platform of the message, e.g. "Instagram".                                                                                                                                                                  |
| msg\_type       | The message type, one of "page", "post", "comment" or "reply".                                                                                                                                                          |
| tree\_thread    | All messages in a thread share the ID of of the initial post. You can use the platform specific ID, e.g. "B4fjlLNiWjW".                                                                                         |
| tree\_id        | The platform specific ID of the message, e.g. "B4fjlLNiWjW".                                                                                                                                                    |
| tree\_parent    | The platform specific ID of the parent message. The parent of a reply is its comment, the parent of the comment is its post and the parent of the post is the page (or stays empty). E.g. "B4fjlLNiWjW".        |
| tree\_pos       | The number of the message in the thread, e.g. 117. In a sample, you usually don't include all messages of a thread. Based on the number you can see, how many messages come before or after a comment or reply. |
| tree\_level        | The level of the message, beginning with pages on level 0, posts on level 1, comments on level 2 and replies on all deeper levels |
| caption         | Caption of the post, comment or reply.                                                                                                                                                                          |
| text            | Text of the post, comment or reply.                                                                                                                                                                             |
| created         | Date and time the message was created in UTC-format, e.g. "2019-11-05T18:01:41Z".                                                                                                                               |
| link            | A link to the original message, this helps you navigating to the platform.                                                                                                                                      |
| author\_id      | A platform specific ID of the message author (you can anonymize it with the epigraf package).                                                                                                                   |
| author\_name    | The name of the message author (you can anonymize it with the epigraf package).                                                                                                                                 |
| tags            | A semicolon separated list of hastags or other tags you want to attach to the message. The tags can be used to filter messages in Epigraf.                                                                      |
| count\_likes    | The number of likes the message received.                                                                                                                                                                       |
| count\_dislikes | The number of dislikes the message received.                                                                                                                                                                    |
| count\_shares   | The number of times the message was shared.                                                                                                                                                                     |
| count\_comments | The number of answerts to the message.            
| seed\_domain    | Give the pages that are provided by the same user or organization on different platforms a common name. For news outlets, as an example, the internet domain is a reasonable choice.                            |
| seed\_handle    | The handle of the page or user profile where the thread started, e.g. the Twitter handle of a news outlet.                                                                                                      |

On this basis, data can be converted to the Epigraf import format.


```
library(tidyverse)
library(epigraf)


# Replace user names
threads <- sm_pseudonyms(threads)

# Convert to epi format
threads <- sm_canonical2epi(threads)

# Clean HTML content
threads <- sm_cleanhtml(threads, content)
```


In the next step, the threads can be transferred into Epigraf.

*to be continued*
