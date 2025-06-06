#
# Functions to create and update data
#

#' Craft a project
#'
#' @export
craft_project <- function(ds, col_id, col_name="", col_signature="", type="default", skip=FALSE) {

  if (rlang::quo_is_symbol(rlang::enquo(col_id))) {
    check_has_column(ds, {{ col_id }})
  }

  ds <- .craft_add_id(ds,".project", "projects", type, {{ col_id }})

  rows <- ds %>%
    dplyr::mutate(
      table = "projects",
      id= ds$.project,
      signature = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_signature)), nrow(ds)),
        {{ col_signature }}, col_signature
      ),
      name = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_name)), nrow(ds)),
        {{ col_name }}, col_name
      )
    )

  rows <- rows |>
    dplyr::mutate(`_fields` = "id,table,type,norm_iri,signature,name") |>
    dplyr::distinct(
      dplyr::across(tidyselect::any_of(c(
        "table","type","id","signature","name",
        ".project","_fields"
      )))
    )


  ram_add(ds, rows, skip)
}

#' Craft an article
#'
#' @export
craft_article <- function(ds, col_id, col_name="", col_signature="", col_sortno="",  type="default", skip=FALSE) {

  if (!(".project" %in% colnames(ds))) {
    .craft_stop("Please, craft a project first")
  }

  if (rlang::quo_is_symbol(rlang::enquo(col_id))) {
    check_has_column(ds, {{ col_id }})
  }

  ds <- .craft_add_id(ds,".article", "articles", type, {{ col_id }}, ds$.project)
  # ds <- ds %>%
  #   dplyr::mutate(
  #     .article = epi_create_iri(
  #       "articles",
  #       type,
  #       ifelse(
  #         rep(rlang::quo_is_symbol(rlang::enquo(col_id)), nrow(ds)),
  #         {{ col_id }}, col_id
  #       )
  #     )
  #   )

  rows <- ds %>%
    dplyr::mutate(
      table = "articles",
      id= ds$.article,
      sortno = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_sortno)), nrow(ds)),
        {{ col_sortno }}, col_sortno
      ),
      signature = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_signature)), nrow(ds)),
        {{ col_signature }}, col_signature
      ),
      name = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_name)), nrow(ds)),
        {{ col_name }}, col_name
      ),
      projects_id = ds$.project
    )

  rows <- rows |>
    dplyr::mutate(`_fields` = "id,table,type,norm_iri,sortno,signature,name,projects_id") |>
    dplyr::distinct(
      dplyr::across(tidyselect::any_of(c(
        "table","type","id","sortno","signature","name", "projects_id",
        ".project", ".article","_fields"
      )))
    )

  ram_add(ds, rows, skip)
}


#' Craft a section
#'
#' @export
craft_section <- function(ds, col_id, col_name="", col_alias="", col_sortno="", type="default", skip=FALSE) {

  if (!(".article" %in% colnames(ds))) {
    .craft_stop("Please, craft an article first")
  }

  if (rlang::quo_is_symbol(rlang::enquo(col_id))) {
    check_has_column(ds, {{ col_id }})
  }

  ds <- .craft_add_id(ds,".section", "sections", type, {{ col_id }}, ds$.article)

  rows <- ds %>%
    dplyr::mutate(
      table = "sections",
      id= ds$.section,
      sortno = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_sortno)), nrow(ds)),
        {{ col_sortno }}, col_sortno
      ),
      # TODO: alias the "alias" field with "signature" in PHP
      alias = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_alias)), nrow(ds)),
        {{ col_alias }}, col_alias
      ),
      name = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_name)), nrow(ds)),
        {{ col_name }}, col_name
      ),
      articles_id = ds$.article # TODO: Rename to "article" in PHP
    )

  rows <- rows |>
    dplyr::mutate(`_fields` = "id,table,type,norm_iri,sortno,alias,name,articles_id") |>
    dplyr::distinct(
      dplyr::across(tidyselect::any_of(c(
        "table","type","id","sortno","alias","name","articles_id",
        ".project", ".article", ".section","_fields"
      )))
    )

  ram_add(ds, rows, skip)
}


#' Craft an item
#'
#' @export
craft_item <- function(ds, col_id, col_content=NULL, type_property="default", col_property=NULL, col_sortno=NULL, type="default") {

  if (!(".article" %in% colnames(ds))) {
    .craft_stop("Please, craft an article first")
  }

  if (!(".section" %in% colnames(ds))) {
    .craft_stop("Please, craft a section first")
  }

  if (rlang::quo_is_symbol(rlang::enquo(col_id))) {
    check_has_column(ds, {{ col_id }})
  }

  ds <- .craft_add_id(ds,".item", "items", type, {{ col_id }}, ds$.section)

  fields <- c("id","table","type","norm_iri","articles_id","sections_id")
  if (!missing(col_property)) {
    ds <- .craft_add_id(ds,".property", "properties", type_property, {{ col_property }})
    fields <- c(fields,"properties_id")
  } else {
    ds$.property <- NA
  }

  if (!missing(col_content)) {
    fields <- c(fields,"content")
  } else {
    col_content <- NA
  }

  if (!missing(col_sortno)) {
    fields <- c(fields,"sortno")
  } else {
    col_sortno <- NA
  }

  rows <- ds %>%
    dplyr::mutate(
      table = "items",
      id= ds$.item,

      sortno = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_sortno)), nrow(ds)),
        {{ col_sortno }}, col_sortno
      ),

      content = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_content)), nrow(ds)),
        {{ col_content }}, col_content
      ),
      properties_id = ds$.property,
      sections_id = ds$.section, # TODO: Rename to "section" in PHP
      articles_id = ds$.article # TODO: Rename to "article" in PHP
    )

  rows <- rows |>
    dplyr::mutate(`_fields` = paste0(fields, collapse=",")) |>
    dplyr::distinct(
      dplyr::across(tidyselect::any_of(c(
        "id","table","type","norm_iri","content","properties_id",
        "sortno","articles_id","sections_id",
        ".project", ".article", ".section",".item",
        "_fields"
      )))
    )

  ram_add(ds, rows)
}


#' Craft properties
#'
#' @export
craft_property <- function(ds, col_id, col_lemma="", type="default") {


  if (rlang::quo_is_symbol(rlang::enquo(col_id))) {
    check_has_column(ds, {{ col_id }})
  }

  ds <- .craft_add_id(ds,"id", "properties", type, {{ col_id }})


  rows <- ds %>%
    dplyr::mutate(
      table = "properties",
      lemma =
        ifelse(
          rep(rlang::quo_is_symbol(rlang::enquo(col_lemma)), nrow(ds)),
          as.character({{ col_lemma }}),
          col_lemma

        ),
      name = NULL
    )  |>
    dplyr::mutate(name = .data$lemma)


  rows <- rows |>
    dplyr::mutate(`_fields` = "id,table,type,norm_iri,lemma,name") |>
    dplyr::distinct(
      dplyr::across(tidyselect::any_of(c(
        "table","type","id","lemma","_fields"
      )))
    )

  ram_add(ds, rows)
}



#' Craft a type
#'
#' @export
craft_type <- function(ds, col_id, col_name, col_caption, col_config, mode, type) {

  # if (rlang::quo_is_symbol(rlang::enquo(col_id))) {
  #   check_has_column(ds, {{ col_id }})
  # }

  ds <- .craft_add_id(ds,".type", "types", {{ type }}, {{ col_id }})

  #
  rows <- ds %>%
    dplyr::mutate(
      table = "types",
      id= ds$.type,
      mode = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(mode)), nrow(ds)),
        {{ mode }}, mode
      ),
      name = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_name)), nrow(ds)),
        {{ col_name }}, col_name
      ),
      caption = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_caption)), nrow(ds)),
        {{ col_caption }}, col_caption
      ),
      config = ifelse(
        rep(rlang::quo_is_symbol(rlang::enquo(col_config)), nrow(ds)),
        {{ col_config }}, col_config
      )
    )

  # Default values
  rows <- rows |>
    mutate(
      mode = ifelse(is.na(.data$mode), "default", .data$mode),
      id = ifelse(is.na(.data$mode), .data$id, paste0(.data$id,"~",.data$mode))
    )

  rows <- rows |>
    dplyr::mutate(`_fields` = "id,table,type,norm_iri,name,caption,config,mode") |>
    dplyr::distinct(
      dplyr::across(tidyselect::any_of(c(
        "table","type","id","name","caption","config","mode",
        ".type"
      )))
    )

  ram_add(ds, rows)
}



#' Craft field configuration
#'
#' @export
craft_type_fields <- function(ds, col_name = field_name, cols_fields, col_id, col_mode = "default", type) {

  # Add defaults
  ds <- ds |>
    dplyr::mutate(
    mode = ifelse(
      rep(rlang::quo_is_symbol(rlang::enquo(col_mode)), nrow(ds)),
      {{ col_mode }}, col_mode
    ))

#   if (! ("edit" %in% colnames(ds))) {
#     ds$edit <- NA
#   }

  # Filter out NAs
  ds_config <- dplyr::filter(ds, !is.na({{ col_name }}))

  # Collapse columns into a named list (without NAs)
  fields <- select(ds_config, tidyselect::eval_select(expr = enquo(cols_fields), data = ds_config))
  ds_config$config <- map(pmap(fields, list), na.omit)

  ds_config <- ds_config |>

    # Nest fields
    dplyr::group_by({{ col_id }}, {{ type }}, mode) |>
    dplyr::summarise(
      config = list(config),
      {{col_name}} := list({{ col_name }})
      #edit = any(.data$edit, na.rm=T)
    ) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(config = list(stats::setNames(config, {{ col_name }}))) |>
    dplyr::select(- {{ col_name }}) |>

    dplyr::rowwise() |>
    #dplyr::mutate(config = list(list(fields = .data$config, edit=.data$edit))) |>
    dplyr::mutate(config = list(list(fields = .data$config))) |>
    ungroup()


  # Encode JSON
  ds_config <- ds_config |>
    rowwise() |>
    mutate(config = toJSON(.data$config, auto_unbox = TRUE)) |>
    ungroup()

  # Merge back
  #joinby <- setNames(c({{ type }}, {{ col_id }}), c(quo_name(enquo(type)), quo_name(enquo(col_id))))
  joinby <- c(quo_name(enquo(type)), quo_name(enquo(col_id)), "mode")
  left_join(ds, ds_config, by= joinby)

}

#' Add ID column
#'
#' @param ds A tibble
#' @param newcol New column
#' @param table The table
#' @param type The type as character value or an unquoted column name
#' @param col_id The column where IDs are stored or a single character value
#' @param prefix Will be inserted between type and col_id (e.g. for sections, the article id)
.craft_add_id <- function(ds, newcol, table,  type, col_id, parent=NULL) {

  if (missing(parent)) {
    parent <- ""
  } else {
    parent <- strsplit(parent, "/", fixed=T)
    parent <- sapply(parent, function(x) utils::tail(x, 1))
    parent <- paste0(parent,"~")
  }

  ds %>%
    dplyr::mutate(
      !!rlang::sym(newcol) := epi_create_iri(
        table,
        ifelse(
          rep(rlang::quo_is_symbol(rlang::enquo(type)), nrow(ds)),
          {{ type }}, type
        ),
        paste0(
          parent,

          ifelse(
            rep(rlang::quo_is_symbol(rlang::enquo(col_id)), nrow(ds)),
            as.character({{ col_id }}),
            col_id
          )
        )
      )
    )
}

#' Stop and say how to proceed
#'
#' @param msg The stop message, e.g. "Please, craft an article first"
.craft_stop <- function(msg) {
  stop(msg, call. = F)
}

