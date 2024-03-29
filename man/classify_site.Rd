% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify_site.R
\name{classify_site}
\alias{classify_site}
\title{Classify inventory data based on site index}
\usage{
classify_site(df, site, nc = 3, plot, .groups = NA)
}
\arguments{
\item{df}{A data frame.}

\item{site}{Quoted name for the site variable.}

\item{nc}{number of categories used to classify the data. If \code{3}, a additional column will be created with levels Lower, Middle and Upper, referencing the 3 categories. If not, only numbers will be used to differentiate the categories. Default: \code{3}.}

\item{plot}{Quoted name for the plot variable.}

\item{.groups}{Optional argument. Quoted name(s) of grouping variables used to fit multiple regressions, one for each level of the provided variable(s). Default \code{NA}.}
}
\value{
A data frame classified based on the site index.
}
\description{
Use the site variable to classify a forest management data.
}
\examples{

library(forestmangr)
data("exfm17")
head(exfm17)

# Classify data into 3 classes:
ex_class <- classify_site(exfm17, "S", 3, "plot")
head(ex_class ,15)

}
\seealso{
other sampling functions: 
  \code{\link{fit_clutter}} for  fitting Clutter's Growth and Yield, and
  \code{\link{est_clutter}} for estimating Clutter's Growth and Yield model variables.
}
\author{
Sollano Rabelo Braga \email{sollanorb@gmail.com}
}
