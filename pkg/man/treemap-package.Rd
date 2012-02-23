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
  With this package, different kind of treemaps can be
  generated such as comparison treemaps and density
  treemaps. Further, it is possible to show small
  multiples.

  The function \code{\link{tmPlot}} is used to plot
  treemaps. The functions \code{\link{tmClick}} and
  \code{\link{tmLocate}} are used to respectively capture a
  mouse click on a treemap and locate the corresponding
  object. The other functions are only used internally.
}
\examples{
# fictive structural business statistics (sbs) data of 2008 and 2009
data(sbsData)

# comparisson treemap
tmPlot(sbsData, index=c("section", "subsection"), vSize="employees09", vColor="employees08", sortID="-size")

# four comparisson treemaps
tmPlot(sbsData, index="section", vSize="employees09+value added09+turnover09+salaries09", vColor="employees08+value added08+turnover08+salaries08", sortID="-size")

# density treemap
tmPlot(sbsData,index=c("section", "subsection"), vSize="turnover09",vColor="employees09/1000*turnover09", sortID="-size")

tmPlot(sbsData,index=c("section", "subsection"), vSize="employees09",vColor="turnover09/employees09", sortID="-size")

# linked treemaps
tmPlot(sbsData[sbsData$section=="Manufacturing",],index="subsection",
vSize="income09+employees09+expenditures09+salaries09",vColor="", sortID="-size")
}
\author{
  Martijn Tennekes \email{mtennekes@gmail.com}
}
\keyword{treemap}

