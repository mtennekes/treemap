\docType{package}
\name{treemap-package}
\alias{treemap}
\alias{treemap-package}
\title{Treemap visualization}
\description{
  \tabular{ll}{ Package: \tab treemap\cr Type: \tab
  Package\cr Version: \tab 1.1-1\cr Date: \tab
  2012-07-10\cr License: \tab GPL-3\cr LazyLoad: \tab
  yes\cr }
}
\details{
  A treemap is a space-filling visualization of
  hierarchical structures. With this package different kind
  of treemaps can be generated, such as comparison treemaps
  and density treemaps. Small multiples are also supported.

  The function \code{\link{tmPlot}} is used to plot
  treemaps. The functions \code{\link{tmClick}} and
  \code{\link{tmLocate}} are used to respectively capture a
  mouse click on a treemap and locate the corresponding
  object.
}
\examples{
# load Gross national income data
data(GNI2010)

# create treemap
tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")
}
\author{
  Martijn Tennekes \email{mtennekes@gmail.com}
}
\references{
  \href{http://www.cs.umd.edu/hcil/treemap-history}{Shneiderman,
  B, Plaisant, C (2009) Treemaps for space-constrained
  visualization of hierarchies. Website}

  \href{http://www.von-tijn.nl/tijn/research/publications/Top-down_data_analysis_with_Treemaps.pdf"}{Tennekes,
  M., Jonge, E. de (2011) Top-down data analysis with
  treemaps. Proceedings of the International Conference on
  Information Visualization Theory and Applications, IVAPP
  2011, Algarve, Portugal.}
}
\keyword{treemap}

