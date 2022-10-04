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