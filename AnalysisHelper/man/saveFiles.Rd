% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SaveFiles.R
\name{saveFiles}
\alias{saveFiles}
\title{Saving an object in various file formats}
\usage{
saveFiles(
  obj,
  filename,
  dir = getwd(),
  row.names = FALSE,
  type = c(1:3, "rds", "csv", "tab")
)
}
\arguments{
\item{type}{: You can save object as RDS, CSV and tab delimited format. default outputs are RDS and CSV files}
}
\description{
Saving an object in various file formats
}
\examples{
data(iris) 

## Save rds and csv format
saveFile(obj=iris, filename='iris')

## Save all format
saveFile(obj=iris, filename='iris', type=1:3)

}
