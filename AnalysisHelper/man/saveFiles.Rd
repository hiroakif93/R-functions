% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SaveFiles.R
\name{saveFiles}
\alias{saveFiles}
\title{Saving object in multiple file format}
\usage{
saveFiles(
  obj = NULL,
  filename = "file",
  row.names = NULL,
  names = "sampleID",
  type = c(1:2, "rds", "csv")
)
}
\arguments{
\item{type}{: You can save object as RDS, CSV and tab delimited format. default outputs are RDS and CSV files}
}
\description{
Saving object in multiple file format
}
\examples{
# If you want infomation of one iris species.
data(iris) 

## Save rds and csv format
saveFile(obj=iris, filename='iris')

## Save all format
saveFile(obj=iris, filename='iris', type=1:3)

}