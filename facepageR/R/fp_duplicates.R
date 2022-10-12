#' Evaluate duplicates on different node levels
#' @description With this function duplicates will be extracted on three different levels:
#' 1. at parent level,
#' 2. at children level with same parent/seed
#' 3. at children level independent of parent
#' @param data the loaded data from Facepager
#' @param level the node level
#' @return a list of duplicates at parent level,
#' duplicates within the seeds, duplicates at all childnodes
#' @examples
#' @export
fp_duplicates <- function(data, level=0) {

  # Duplikate auf Elternebene
  parents <- fp_getparents(data,!!level)
  dupl.parents <- parents %>%
    count(parent_objectid,sort=T)

  cat("Auswertung der Duplikate bei den Seeds:\n")
  cat("- ",sum(dupl.parents$n)," Elternknoten insgesamt\n")
  cat("- ",nrow(dupl.parents)," eindeutige Elternknoten\n")

  dupl.parents <- dupl.parents %>%
    dplyr::filter(n > 1)

  cat("- ",nrow(dupl.parents)," mehrfache Elternknoten\n")

  if (nrow(dupl.parents) > 0)
    print(dupl.parents)


  # Bei gleichen Eltern, auf zweiter Ebene
  dupl.children <- data %>%
    dplyr::filter(objecttype %in% c("data","unpacked"),querystatus=="fetched (200)",level==!!level+1) %>%
    count(objectid,parent_id, sort=T) %>%  #level,querytype
    inner_join(parents,by=c("parent_id"))

  dupl.children.missing <- parents %>%
    anti_join(dupl.children,by=c("parent_id"))

  cat("Auswertung der Duplikate innerhalb der Seeds:\n")
  cat("- ",nrow(dupl.children.missing)," Seeds ohne Kindknoten\n")
  cat("- ",sum(dupl.children$n)," Kindknoten insgesamt\n")
  cat("- ",nrow(dupl.children)," eindeutige Kindknoten\n")

  dupl.children <- dupl.children %>%
    dplyr::filter(n > 1)

  cat("- ",nrow(dupl.children)," mehrfache Kindknoten\n")
  if (nrow(dupl.children) > 0)
    print(select(dupl.children,objectid,n,file,parent_objectid))


  # Unabhängig von Eltern, auf gleicher Ebene
  dupl.all <- data %>%
    dplyr::filter(objecttype %in% c("data","unpacked"),querystatus=="fetched (200)",level==!!level+1) %>%
    count(objectid,sort=T)   #level,querytype,


  cat("Auswertung der Duplikate bei allen Kindknoten:\n")
  cat("- ",sum(dupl.all$n)," Kindknoten insgesamt\n")
  cat("- ",nrow(dupl.all)," eindeutige Kindknoten\n")


  dupl.all <- dupl.all %>%
    dplyr::filter(n > 1)

  cat("- ",nrow(dupl.all)," mehrfache Kindknoten\n")

  if (nrow(dupl.all) > 0)
    print(dupl.all)

  result <- list("dupl.parents"=dupl.parents,
                 "dupl.children"=dupl.children,
                 "dupl.all"=dupl.all)


  invisible(result)
}
