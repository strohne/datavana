#' Sample data from BBC Facebook page - posts and comments
#'
#' A small dataset generated as an example,
#' where the unit of analysis is either a post or a comment.
#' It contains information for 20 posts and their comments posted in September 2022
#' to illustrate the use of the package.
#' The dataset is in the state as exported directly from Facepager.
#'
#' @docType data
#'
#' @usage data(exampledata)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{level}{node Level, a value between 0 and 2}
#'  \item{id}{own ID in the data set, chronological numbers starting at 1}
#'  \item{parent_id}{ID of the parent node in the data set, from which the data was fetched}
#'  \item{object_id}{own ID,in the given platform format,
#'  here two 15/16 digits numbers for the page and the post "XXX_XXX"}
#'  \item{object_type}{kind of node, possible types: seed, data, offcut, unpacked}
#'  \item{object_key}{keys for the further extraction in Facepager}
#'  \item{query_status}{how far is the query?
#'  was it able to collect data, was there an error message, etc.?}
#'  \item{query_time}{Time of the query via facepager}
#'  \item{query_type}{which Facepager module and which resource were used for the query}
#'  \item{message}{text of the post/comment}
#'  \item{created_time}{time of publication }
#'  \item{like_count}{like count of the comments}
#'  \item{comment_count}{comment count of the comments (replies)}
#' }
#'
#' @references This data set was created as an example for the facepageR package.
#' @keywords datasets
#'
"exampledata"
