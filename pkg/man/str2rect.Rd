\name{str2rect}
\alias{str2rect}
\title{Places text in rectangles}
\usage{
  str2rect(grb, fontcol = "black", fill = NA, bold = FALSE,
    enlargable = FALSE)
}
\arguments{
  \item{grb}{rectGrob object defining the rectangle(s),
  where \code{grb$name} is the text to display}

  \item{fontcol}{vector defining the colors of the text}

  \item{fill}{vector defining the backgroud colors of the
  text}

  \item{bold}{logical defining whe the text is bold}

  \item{enlargable}{logical defining whether the textsize
  may exceed \code{cex=1}}
}
\description{
  Places text in rectangles
}

