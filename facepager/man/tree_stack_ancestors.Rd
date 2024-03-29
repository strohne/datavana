% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trees.R
\name{tree_stack_ancestors}
\alias{tree_stack_ancestors}
\title{For each node, add each ancestors id.
In the result, nodes will be duplicated for all their ancestors.
As an example: a node on level 2 will be present two times,
\enumerate{
\item the node containing the parent_id in the col_stack column
\item the node containing the parents parent_id in the col_stack column
}}
\usage{
tree_stack_ancestors(data, col_id, col_parent, col_stack)
}
\arguments{
\item{data}{All nodes}

\item{col_id}{The column holding IDs of the nodes}

\item{col_parent}{The column holding IDs of the parent nodes}

\item{col_stack}{The column that will hold the ancestors IDs}
}
\description{
For each node, add each ancestors id.
In the result, nodes will be duplicated for all their ancestors.
As an example: a node on level 2 will be present two times,
\enumerate{
\item the node containing the parent_id in the col_stack column
\item the node containing the parents parent_id in the col_stack column
}
}
