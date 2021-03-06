% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sanity_check_data.R
\name{sanity_check_data}
\alias{sanity_check_data}
\title{Does a few sanity checks on the experimental data}
\usage{
sanity_check_data(df, pilot = TRUE)
}
\arguments{
\item{df}{a data frame}

\item{pilot}{boolean that is \code{TRUE} when the data frame is from a pilot experiment}
}
\value{
Nothing
}
\description{
Designed to make be run at the beginning of an analysis file on the data being used, to 
make sure that the pre-processing has not majorly screwed up anything with the data
files being used.
}

