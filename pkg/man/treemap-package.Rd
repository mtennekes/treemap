\docType{package}
\name{treemap-package}
\alias{treemap}
\alias{treemap-package}
\title{Treemap visualization}
\description{
  \tabular{ll}{ Package: \tab treemap\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2012-03-20\cr
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

# comparisson treemap: colors indicate change of vSize with respect to vColor
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp")

# density treemap: colors indicate density (like a population density map)
tmPlot(sbsData,
	   index=c("section", "subsection"),
	   vSize="turnover09",
	   vColor="employees09*0.001",
	   type="dens")

# value treemap (aka Map of the Market)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
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

