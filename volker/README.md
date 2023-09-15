# Volker Package


## Installation
As with all other packages you'll have to install the package and load the library first.

```
library(devtools)
install_github("strohne/datavana/volker")
library(volker)
```


Alternative using remotes:
```
if (!require(remotes)) { install.packages("remotes") }
remotes::install_github("strohne/datavana", subdir="epigraf")
p_load("epigraf")
```
