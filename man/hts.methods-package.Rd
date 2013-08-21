\name{hts.methods-package}
\alias{hts.methods-package}
\alias{hts.methods}
\docType{package}
\title{
helper methods and plot functions for HTS data analysis
}
\description{
This package includes methods for normalizing and plotting HTS data using normalization methods
}
\details{
\tabular{ll}{
Package: \tab hts.methods\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-08-21\cr
License: \tab GPL-3\cr
}

}
\author{
Markus List

Maintainer: Markus List <mlist@health.sdu.dk>
}
\references{
Malo et al. Statistical practice in high-throughput screening data analysis (Nature biotechnology)
}
\keyword{ package }
\seealso{
}
\examples{ 
#load hts.methods library
library(hts.methods)

#load toy data
data(plates)

#applying normalization strategies
hts.normalize(plates)

#scatterplot across all wells, black bars correspond to plate means
hts.scatter(plates)

#boxplot showing control performance
hts.ctrl.boxplot(plates)

#per plate comparison of different normalization methods
hts.norm.comparison(hts.normalize(plates), Plate=1)

#heatmap including hits
hts.heatmap(plates, "CTB", method="SD", margin=3)

#heatmap using normalized data and MAD
hts.heatmap(hts.normalize(plates), "Bscore", method="MAD", margin=3)

#hit list for export
hts.hits(plates, "CTB", method="SD", margin=3)

}
