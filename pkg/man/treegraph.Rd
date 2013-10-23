\name{treegraph}
\alias{treegraph}
\title{Draw a tree graph}
\usage{
  treegraph(dtf, index = names(dtf), palette.HCL.options,
    show.labels = FALSE, stack.labels = FALSE,
    rootlabel = "", vertex.size = 3,
    vertex.label.dist = 0.5, vertex.label.cex = 0.8,
    vertex.label.family = "sans",
    vertex.label.color = "black")
}
\arguments{
  \item{dtf}{a data.frame or data.table. Required.}

  \item{index}{the index variables of dtf}

  \item{palette.HCL.options}{palette.HCL.options}

  \item{show.labels}{show.labels}

  \item{stack.labels}{stack.labels}

  \item{rootlabel}{rootlabel}

  \item{vertex.size}{vertex.size}

  \item{vertex.label.dist}{vertex.label.dist}

  \item{vertex.label.cex}{vertex.label.cex}

  \item{vertex.label.family}{vertex.label.family}

  \item{vertex.label.color}{vertex.label.color}
}
\value{
  invisible igraph object
}
\description{
  Apply certain function to a tree structure.
}

