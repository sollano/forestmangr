% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smalianwb.R
\name{smalianwb}
\alias{smalianwb}
\title{Calculate the volume with bark of trees using the Smalian method}
\usage{
smalianwb(
  df,
  di,
  hi,
  tree,
  .groups = NA,
  di_mm_to_cm = FALSE,
  hi_cm_to_m = FALSE
)
}
\arguments{
\item{df}{A data frame.}

\item{di}{Quoted name of the section diameter  variable, in centimeters.}

\item{hi}{Quoted name of the section height  variable, in meters}

\item{tree}{Quoted name of the tree variable. used to differentiate the trees' sections. If this argument is \code{NA}, the defined groups in the data frame will be used}

\item{.groups}{Optional argument. Quoted name(s) of additional grouping variables that can be added to differentiate subdivisions of the data. 
If this argument is not supplied, the defined groups in the data frame will be used. Default: \code{NA}.}

\item{di_mm_to_cm}{Boolean argument that, if \code{TRUE}, converts the di argument from milliliters to centimeters. Default: \code{FALSE}.}

\item{hi_cm_to_m}{Boolean argument that, if \code{TRUE}, converts the hi argument from centimeters to meters. Default: \code{FALSE}.}
}
\value{
Data frame with volume values by section.
}
\description{
Function used to calculate the volume with bark of trees using the Smalian method.
This function has integration with dplyr, so it can be used inside a pipe, along with the
\code{group_by} function.
}
\examples{
library(forestmangr)
data("exfm7")
head(exfm7)

# Calculate the volume with bark using the Smalian method:
smalianwb(exfm7,"di_wb", "hi", "TREE")

# Using pipes:
library(dplyr)

 exfm7 \%>\% 
 group_by(TREE) \%>\% 
 smalianwb("di_wb", "hi")

}
\references{
Campos, J. C. C. and Leite, H. G. (2017) Mensuracao Florestal: Perguntas e Respostas. 5a. Vicosa: UFV.
}
\seealso{
Complementary functions:
  \code{\link{smalianwob}}, For calculation of volume without bark using the Smalian method,
  \code{\link{huberwb}}, for calculation of volume with bark using the Huber method,
  \code{\link{huberwob}}, for calculation of volume without bark the Huber method.
}
\author{
Sollano Rabelo Braga \email{sollanorb@gmail.com}
}
