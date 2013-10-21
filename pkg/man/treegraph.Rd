\name{treegraph}
\alias{treegraph}
\title{Draw a tree graph}
\usage{
  treegraph(dat, palette.HCL.options, show.labels = FALSE,
    rootlabel = "", vertex.size = 3,
    vertex.label.dist = 0.5, vertex.label.cex = 0.8,
    vertex.label.family = "sans",
    vertex.label.color = "black")
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

