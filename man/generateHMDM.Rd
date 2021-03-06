% Generated by roxygen2 (4.0.2): do not edit by hand
\name{generateHMDM}
\alias{generateHMDM}
\title{generateHMDM}
\usage{
generateHMDM(corpus, dict, ...)
}
\arguments{
\item{corpus}{object of class Corpus}

\item{dict}{object of class HMD}
}
\value{
data frame (row : Document , column : Dictionary Term)
}
\description{
Generate HMDM(Hierarchical Multi-Dictionary term Matrix) function
}
\examples{
data.freq <- generateHMDM(data.corpus, dict=hmd)
}

