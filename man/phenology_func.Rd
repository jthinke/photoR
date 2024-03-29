% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenology_func.R
\name{phenology_func}
\alias{phenology_func}
\title{phenology_func}
\usage{
phenology_func(dat, rdat, tabletype = "Raw", degf = 10)
}
\arguments{
\item{dat}{A data frame of adult attendance at each nest of interest. The data should have the following headers (case sensitive):SPLIT_YEAR, ROOKERY, COLONY, CAMERA,    SPP, NEST,    DATE, and    MAXN. Note that DATE can be specified in any way (default is for m/d/y), or as separate columns named DAY, MON, YR.}

\item{rdat}{A data frame of nest content observations. The data frame should have the following headers (case sensitive):SPLIT_YEAR,  ROOKERY,  SPP,  COLONY,  CAMERA,  NEST, DATE,  LAY,  MAXE,  HATCH,  MAXC, and  CRECHE. Note that DATE can be specified in anyway (default is for m/d/y), or as separate columns named DAY, MON, YR.}

\item{tabletype}{A character vector indicating output of interest, either "Raw", "CEMP Protocol A6b", or "CEMP Protocol A9"}

\item{degf}{numeric value for degrees of freedom used in the smooth.spline function}
}
\value{
A data frame with results
}
\description{
This is the main function run in the photoR app. This function processes the data derived from time lapse cameras to estimate the timing of breeding events (i.e., egg lay, hatch, etc) and reproductive success in each nest. Data are summarized and formatted to fit into CCAMLR CEMP e-forms (www.ccamlr.org).The app requests you input two data files in .csv format. To run phenology_func independently, you will need to import your data and format the date field to POSIXct appropriately. See included data sets (test_att and test_repro) for examples.
}
\examples{
phenology_func(test_att, test_repro, tabletype="Raw", degf=10)
}
