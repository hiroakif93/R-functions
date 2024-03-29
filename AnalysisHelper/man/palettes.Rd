% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\name{palettes}
\alias{palettes}
\title{Take color platte}
\usage{
palettes(x = 70, pal = "RColorBrewer")
}
\arguments{
\item{x}{x is single integer. Default 70}

\item{pal}{you can select 'RColorBrewer' or 'rainbow'. Default RColorBrewer.}
}
\description{
You can generate maximum 70 colors from RColorBrewer palattes
}
\examples{
palettes(10)
palettes(20)
palettes(20, pal='gradient')

}
\references{
https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
}
