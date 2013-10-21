\name{treeapply}
\alias{treeapply}
\title{Apply to tree structure}
\usage{
  treeapply(dat, values, depth = NULL, fun,
    prepare.dat = FALSE, ...)
}
\arguments{
  \item{dat}{a data.frame or data.table that should contain
  only index variables. Required.}

  \item{values}{vector with values that apply to the root
  node}

  \item{depth}{variable of dat that indicated the node
  depths}

  \item{fun}{function to be applied}

  \item{prepare.dat}{data is by default preprocessed,
  except for interal use}
}
\value{
  list
}
\description{
  Apply certain function to a tree structure.
}

