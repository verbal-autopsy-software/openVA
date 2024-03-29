% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CSMF.r
\name{getTopCOD}
\alias{getTopCOD}
\title{Extract the most likely cause(s) of death}
\usage{
getTopCOD(x, interVA.rule = TRUE, n = 1, include.prob = FALSE)
}
\arguments{
\item{x}{a fitted object from \code{codeVA}.}

\item{interVA.rule}{Logical indicator for \code{interVA} object only. If
TRUE and (the parameter) n <= 3, then the InterVA thresholds are
used to determine the top causes.}

\item{n}{Number of top causes to include (if n > 3, then the parameter interVA.rule is
treated as FALSE).}

\item{include.prob}{Logical indicator for including the probabilities (for \code{insilico})
or indicator of how likely the cause is (for \code{interVA}) in the results}
}
\value{
a data frame of ID, most likely cause assignment(s), and corresponding
probability (for \code{insilico}) or indicator of how likely the cause is (for \code{interVA})
}
\description{
Extract the most likely cause(s) of death
}
\examples{
data(RandomVA1)
# for illustration, only use interVA on 100 deaths
fit <- codeVA(RandomVA1[1:100, ], data.type = "WHO", model = "InterVA", 
                  version = "4.02", HIV = "h", Malaria = "l", write=FALSE)
getTopCOD(fit)
\dontrun{
library(openVA)

# InterVA4 Example
data(SampleInput)
fit_interva <- codeVA(SampleInput, data.type = "WHO2012", model = "InterVA",
                      version = "4.03", HIV = "l", Malaria = "l", write = FALSE)
getTopCOD(fit_interva, n = 1)
getTopCOD(fit_interva, n = 3)
getTopCOD(fit_interva, n = 3, include.prob = TRUE)
getTopCOD(fit_interva, interVA.rule = FALSE, n = 3)
getTopCOD(fit_interva, n = 5)
getTopCOD(fit_interva, n = 5, include.prob = TRUE)

# InterVA5 & Example
data(RandomVA5)
fit_interva5 <- codeVA(RandomVA5[1:50,], data.type = "WHO2016", model = "InterVA",
                       version = "5", HIV = "l", Malaria = "l", write = FALSE)
getTopCOD(fit_interva5, n = 1)
getTopCOD(fit_interva5, n = 3)
getTopCOD(fit_interva5, n = 3, include.prob = TRUE)
getTopCOD(fit_interva5, interVA.rule = FALSE, n = 3)
getTopCOD(fit_interva5, n = 5)
getTopCOD(fit_interva5, n = 5, include.prob = TRUE)

# InSilicoVA Example
data(RandomVA5)
fit_insilico <- codeVA(RandomVA5[1:100,], data.type = "WHO2016", 
                       auto.length = FALSE)
getTopCOD(fit_insilico, n = 1)
getTopCOD(fit_insilico, n = 3)
getTopCOD(fit_insilico, n = 3, include.prob = TRUE)


# Tariff Example (only top cause is returned)
data(RandomVA3)
test <- RandomVA3[1:200, ]
train <- RandomVA3[201:400, ]
allcauses <- unique(train$cause)
fit_tariff <- tariff(causes.train = "cause", symps.train = train, 
                     symps.test = test, causes.table = allcauses)
getTopCOD(fit_tariff, n = 1)

# NBC Example
library(nbc4va)
data(nbc4vaData)
train <- nbc4vaData[1:50, ]
test <- nbc4vaData[51:100, ]
fit_nbc <- nbc(train, test, known=TRUE)
getTopCOD(fit_nbc, n = 1)
getTopCOD(fit_nbc, n = 3)
getTopCOD(fit_nbc, n = 3, include.prob = TRUE)
}

}
\seealso{
Other output extraction: 
\code{\link{getCCC}()},
\code{\link{getCSMF_accuracy}()},
\code{\link{getCSMF}()},
\code{\link{getIndivProb}()}
}
\concept{output extraction}
