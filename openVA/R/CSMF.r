#' Obtain CSMF from fitted model
#'
#' @param x a fitted object from \code{codeVA}.
#' @param CI For \code{insilico} object only, specifying the credible interval to return. 
#' Default value to be 0.95.
#' @param interVA.rule Logical indicator for \code{interVA} object only. If TRUE, it means
#' only up to top 3 causes for each death are used to calculate CSMF and the rest are 
#' categorized as "undetermined"
#'
#' @return a vector or matrix of CSMF for all causes.
#' @export getCSMF
#'
#' @examples
#' data(RandomVA1)
#' # for illustration, only use interVA on 100 deaths
#' fit <- codeVA(RandomVA1[1:100, ], data.type = "WHO", model = "InterVA", 
#'                   version = "4.02", HIV = "h", Malaria = "l")
#' getCSMF(fit)
#' 
getCSMF <- function(x, CI = 0.95, interVA.rule = TRUE){

  # For InSilico object
  if(class(x) == "insilico"){
    return(summary(x, CI.csmf = CI)$csmf) 
  }

  if(class(x) == "interVA"){
    return(CSMF(x, interVA = interVA.rule, noplot = TRUE))
  } 
   
  if(class(x) == "tariff"){
    return(x$csmf)
  }
}


#' Extract the most likely cause of death
#'
#' @param x a fitted object from \code{codeVA}.
#' @param interVA.rule Logical indicator for \code{interVA} object only. If TRUE, 
#' only the InterVA reported first cause is extracted.
#'
#' @return a data frame of ID and most likely cause assignment.
#' @export getTopCOD
#'
#' @examples
#' data(RandomVA1)
#' # for illustration, only use interVA on 100 deaths
#' fit <- codeVA(RandomVA1[1:100, ], data.type = "WHO", model = "InterVA", 
#'                   version = "4.02", HIV = "h", Malaria = "l")
#' getTopCOD(fit)
#' 
getTopCOD <- function(x, interVA.rule = TRUE){
  
  if(class(x) == "insilico"){
    probs <- x$indiv.prob
    pick <- colnames(probs)[apply(probs, 1, which.max)]
    id <- x$id
  }else if(class(x) == "interVA"){
      id <- x$ID
      pick <- rep("", length(x$VA))
      for(i in 1:length(x$VA)){
        if(interVA.rule){
          pick[i] <- x$VA[[i]]$CAUSE1
        }else{
            prob <- x$VA[[i]]$wholeprob
            causenames <- names(prob)
            causeindex <- 1:length(causenames)
            if(causenames[1] == "Not pregnant or recently delivered" &&
                causenames[2] == "Pregnancy ended within 6 weeks of death" &&
                causenames[3] == "Pregnant at death"){
                    causeindex <- causeindex[-c(1:3)]
                    causenames <- causenames[-c(1:3)]    
            }
            pick[i] <- causenames[which.max(prob[causeindex])]
          }
      }
      pick[which(pick == " ")] <- "Undetermined"
    }else if(class(x) == "tariff"){
      pick <- x$causes.test[, 2]
      id <- as.character(x$causes.test[, 1])
    }

    return(data.frame(ID = id, cause = pick))
}