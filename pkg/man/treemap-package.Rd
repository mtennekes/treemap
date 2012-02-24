\docType{package}
\name{treemap-package}
\alias{treemap-package}
\title{Treemap visualization}
\description{
  \tabular{ll}{ Package: \tab treemap\cr Type: \tab
  Package\cr Version: \tab 0.11\cr Date: \tab 2012-02-24\cr
  License: \tab GPL-3\cr LazyLoad: \tab yes\cr }
}
\details{
  Different kind of treemaps can be generated, such as
  comparison treemaps, density treemaps, and 'Map of the
  Market' kind of treemaps. Small multiples are also
  supported.

  The function \code{\link{tmPlot}} is used to plot
  treemaps. The functions \code{\link{tmClick}} and
  \code{\link{tmLocate}} are used to respectively capture a
  mouse click on a treemap and locate the corresponding
  object.
}
\examples{
# fictive structural business statistics (sbs) data of 2008 and 2009
data(sbsData)

# comparisson treemap
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   sortID="-size")

# density treemap
tmPlot(sbsData,
	   index=c("section", "subsection"),
	   vSize="turnover09",
	   vColor="employees09/1000*turnover09", 
	   sortID="-size")

# value treemap (aka Map of the Market)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   sortID="-size", 
	   type="value", 
	   palette="RdBu")
}
\author{
  Martijn Tennekes \email{mtennekes@gmail.com}
}
\keyword{treemap}

