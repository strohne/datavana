# Datavana
 Helper scripts for working in the Datavana
 
 ## Facepager package

Installation:

```
library(devtools)
install_github("strohne/datavana/facepageR")
```


 ## Epigraf package
 
Installation:

```
library(devtools)
install_github("strohne/datavana/epigraf")
```

Example data:

```
threads <- read_csv2(system.file("extdata", "threads.csv", package = "epigraf"))
```


Example script for converting a social media dataset into an Epigraf import file. 
The input is a social media dataset in its canonical form (see below).                                                                                                                                                               |


```
library(tidyverse)
library(epigraf)


# Load example data
threads <- read_csv2(system.file("extdata", "threads.csv", package = "epigraf"))

# Replace user names
threads <- sm_pseudonyms(threads)

# Convert to epi format
threads <- sm_canonical2epi(threads)

# Clean HTML content
threads <- sm_cleanhtml(threads, content)
```

We define the canonical form of a social media dataset as a table where each row is a post, comment or reply with the following columns:

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

On this basis, the package functions create article, section, item and property records to be imported from a CSV file or via the API into Epigraf.
