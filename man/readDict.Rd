% Generated by roxygen2 (4.0.2): do not edit by hand
\name{readDict}
\alias{readDict}
\title{readDict}
\usage{
readDict(dict.path, ...)
}
\arguments{
\item{dict.path}{folder path of dictionary file}
}
\value{
HMD class object
}
\description{
Read a dictionary csv file. Convert data fram to HMD class
}
\examples{
# dic csv file exists working directory
hmd <- readDict(dict.path='./dic.csv')
}

