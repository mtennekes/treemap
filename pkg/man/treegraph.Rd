\name{treegraph}
\alias{treegraph}
\title{Tree graph}
\usage{
treegraph(dtf, index = names(dtf), directed = FALSE, palette.HCL.options,
  show.labels = FALSE, rootlabel = "", vertex.layout, vertex.layout.params,
  truncate.labels = NULL, vertex.size = 3, vertex.label.dist = 0.3,
  vertex.label.cex = 0.8, vertex.label.family = "sans",
  vertex.label.color = "black", mai = c(0, 0, 0, 0), ...)
}
\arguments{
  \item{dtf}{a data.frame or data.table. Required.}

  \item{index}{the index variables of dtf (see
  \code{\link{treemap}})}

  \item{directed}{logical that determines whether the graph
  is directed (\code{TRUE}) or undirected (\code{FALSE})}

  \item{palette.HCL.options}{palette.HCL.options (see
  \code{\link{treemap}})}

  \item{show.labels}{show the labels}

  \item{rootlabel}{name of the rootlabel}

  \item{vertex.layout}{layout algorithm}

  \item{vertex.layout.params}{list of arguments passed to
  \code{vertex.layout}}

  \item{truncate.labels}{number of characters at which the
  levels are truncated. Either a single value for all index
  variables, or a vector of values for each index variable}

  \item{vertex.size}{vertex.size (see
  \code{\link[igraph:igraph.plotting]{igraph.plotting}})}

  \item{vertex.label.dist}{vertex.label.dist (see
  \code{\link[igraph:igraph.plotting]{igraph.plotting}})}

  \item{vertex.label.cex}{vertex.label.cex (see
  \code{\link[igraph:igraph.plotting]{igraph.plotting}})}

  \item{vertex.label.family}{vertex.label.family (see
  \code{\link[igraph:igraph.plotting]{igraph.plotting}})}

  \item{vertex.label.color}{vertex.label.color (see
  \code{\link[igraph:igraph.plotting]{igraph.plotting}})}

  \item{mai}{margins see \code{\link[graphics:par]{par}}}

  \item{...}{arguments passed to
  \code{\link[igraph:plot.igraph]{plot.igraph}}}
}
\value{
(invisible) igraph object
}
\description{
This function draws a tree graph with a radial layout.
Experimental.
}
\examples{
data(business)
treegraph(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), show.labels=FALSE)
treegraph(business[business$NACE1=="F - Construction",],
    index=c("NACE2", "NACE3", "NACE4"), show.labels=TRUE, truncate=c(2,4,6))
}

