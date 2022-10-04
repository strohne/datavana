# Datavana
 Helper scripts for working in the Datavana
 
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


Example script for converting a social media dataset into an Epigraf import file

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


