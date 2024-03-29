% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CSMF.r
\name{getCCC}
\alias{getCCC}
\title{Calculate Overall chance-corrected concordance (CCC)}
\usage{
getCCC(cod, truth, C = NULL)
}
\arguments{
\item{cod}{a data frame of estimated cause of death. The first column is the ID and the second column is the estimated cause.}

\item{truth}{a data frame of true causes of death. The first column is the ID and the second column is the estimated cause.}

\item{C}{the number of possible causes to assign. If unspecified, the number of unique causes in cod and truth will be used.}
}
\description{
Denote the cause-specific accuracy for the j-th cause to be (# of deaths correctly assigned to cause j) / (# of death due to cause j). For causes 1, 2, ..., C, the cause-specific CCC is computed for the j-th cause is defined to be (j-th cause-specific accuracy - 1 / C) / (1 - 1 / C) and the overall CCC is the average of each cause-specific CCC.
}
\examples{
est <- data.frame(ID = c(1, 2, 3), cod = c("C1", "C2", "C1"))
truth <- data.frame(ID = c(1, 2, 3), cod = c("C1", "C3", "C3"))
# If there are only three causes
getCCC(est, truth)
# If there are 20 causes that can be assigned
getCCC(est, truth, C = 20)

}
\seealso{
Other output extraction: 
\code{\link{getCSMF_accuracy}()},
\code{\link{getCSMF}()},
\code{\link{getIndivProb}()},
\code{\link{getTopCOD}()}
}
\concept{output extraction}
