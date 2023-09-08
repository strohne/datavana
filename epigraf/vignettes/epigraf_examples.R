#
# Beispiele zum Anlegen von Artikeln und Kategorien
#


library(tidyverse)
library(epigraf)

# Beispiel um Kategorie (=property) zu erzeugen

properties <- tibble(
  id = c("properties/topics/grabmaeler"),
  lemma = c("Verlorene Grabmäler")
)

epigraf::api_setup("https://epigraf-dev.uni-muenster.de", "ACCESSTOKEN", T)
epigraf::api_patch(properties, "epi_workshop")


# Beispiel um Artikel zu erzeugen


articles <- tibble(
  id = c(
    "articles/epi-article/example~1",
    "sections/text/example~1~text",
    "items/text/example~1~text"
  ),
  name = c(
    "Mein Lieblingsbeispiel",
    "Beispielabschnitt",
    ""
  ),
  content = c(
    "",
    "",
    "Hallo Isabel und Henrieke! :)"
  ),
  projects_id = c(
    "projects/epi/tp",
    "projects/epi/tp",
    "projects/epi/tp"
  ),
  articles_id = c(
    "",
    "articles/epi-article/example~1",
    "articles/epi-article/example~1"
  ),
  sections_id = c(
    "",
    "",
    "sections/text/example~1~text"
  )
)

epigraf::api_patch(articles, "epi_workshop")
