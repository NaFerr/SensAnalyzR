% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sens_analysis.R
\name{sens_analysis}
\alias{sens_analysis}
\title{Sensitivity Analysis}
\usage{
sens_analysis(data = data, goods = goods, weight = weight,
  disagg = disagg, precent = percent, func = func)
}
\arguments{
\item{data}{A data set}

\item{weight}{A list of weights that should correspond to the amount AND POSITION to the goods in the market basket}

\item{disagg}{A column name in the data set to disaggregate the data by}

\item{func}{The function that the market basket will be disaggregated with, will be used in the summarise_at funciton.}

\item{good}{A list of column names in the dataset that constitute the basket you want to look at}

\item{percent}{A list of percentages you would like to see columns multiplied by, NOTE: there is code in the script to make all values negative as well so for example 5, 10, 15 would actually be 5,10,15,-5,-10,-15 within the script}
}
\value{
A binded object of modified tabled that had multiplied across percentages that utilizes a weighted dataset
}
\description{
This function creates a weighted data set that will produce an object where columns are disaggregated and multiplied by stated percentages.
}
\examples{
sens_analysis(data, goods, weight, disagg, percent, func)
sens_analysis(df, c(a,b,c), c(1,2,3), "coiuntry_ID", c(.01,.05), median)
}
