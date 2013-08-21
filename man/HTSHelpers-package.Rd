\name{HTSHelpers-package}
\alias{HTSHelpers-package}
\alias{HTSHelper}
\docType{package}
\title{
helper methods and plot functions for HTS data analysis
}
\description{
This package includes methods for normalizing and plotting HTS data using normalization methods
}
\details{
\tabular{ll}{
Package: \tab HTSHelpers\cr
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
library(HTSHelpers)

#load toy data
data(plates)

#applying normalization strategies
plates.norm <- hts.normalize(plates)

#scatterplot across all wells, black bars correspond to plate means
hts.scatter(plates.norm)

#boxplot showing control performance
hts.ctrl.boxplot(plates.norm)

#per plate comparison of different normalization methods
hts.norm.comparison(plates.norm, plate=1)

#heatmap including hits
hts.heatmap(plates.norm, "CTB", method="SD", margin=3)

#heatmap using normalized data and MAD
hts.heatmap(plates.norm, "Bscore", method="MAD", margin=3)

#hit list for export
hts.hits(plates.norm, "CTB", method="SD", margin=3)

}
