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
#' \dontrun{
#' library(InSilicoVA)
#' data(RandomVA1)
#' # for illustration, only use interVA on 100 deaths
#' fit <- codeVA(RandomVA1[1:100, ], data.type = "WHO2012", model = "InterVA", 
#'                   version = "4.03", HIV = "h", Malaria = "l", write=FALSE)
#' getCSMF(fit)
#' library(InterVA5)
#' data(RandomVA5)
#' fit <- codeVA(RandomVA5[1:100, ], data.type = "WHO2016", model = "InterVA", 
#'                   version = "5.0", HIV = "h", Malaria = "l", write=FALSE)
#' getCSMF(fit)
#' }
#' 
getCSMF <- function(x, CI = 0.95, interVA.rule = TRUE){

  # For InSilico object
  if(methods::is(x, "insilico")){
    return(summary(x, CI.csmf = CI)$csmf) 
  }

  if(methods::is(x, "interVA")){
    return(InterVA4::CSMF(x, InterVA.rule = interVA.rule, noplot = TRUE))
  } 
  if(methods::is(x, "interVA5")){
    return(InterVA5::CSMF5(x, InterVA.rule = interVA.rule, noplot = TRUE))
  } 
   
  if(methods::is(x, "tariff")){
    return(x$csmf)
  }

  if(methods::is(x, "nbc")){
   if (!isTRUE(requireNamespace("nbc4va", quietly = TRUE))) {
        stop("You need to install the package 'nbc4va'. Please run in your R terminal:\n install.packages('nbc4va')")
      }
    return(nbc4va::csmf.nbc(x))
  }
  stop("Input object is not a fitted model from openVA. ")
}

#' Calculate CSMF accuracy
#'
#' @param csmf a CSMF vector from \code{getCSMF} or a InSilicoVA fitted object.
#' @param truth a CSMF vector of the true CSMF.
#' @param undet name of the category denoting undetermined causes. Default to be NULL.
#'
#' @return a number (or vector if input is InSilicoVA fitted object) of CSMF accuracy as 1 - sum(abs(CSMF - CSMF_true)) / (2 * (1 - min(CSMF_true))).
#' @export getCSMF_accuracy
#'
#' @examples
#' csmf1 <- c(0.2, 0.3, 0.5)
#' csmf0 <- c(0.3, 0.3, 0.4)
#' acc <- getCSMF_accuracy(csmf1, csmf0)
#' 
#'

getCSMF_accuracy <- function(csmf, truth, undet = NULL){
  ## when input is insilico fit
  if(methods::is(csmf, 'insilico')){
    if(!is.null(names(truth))){
      order <- match(colnames(csmf$csmf), names(truth))
      if(is.na(sum(order))){stop("Names not matching")}
      truth <- truth[order]
    }
    acc <- 1 - apply(abs(truth - t(csmf$csmf)), 2, sum) / 2 / (1 - min(truth))

  }else{
      ## when input is vector
      if(!is.null(undet)){
      if(undet %in% names(csmf)){
        csmf <- csmf[-which(names(csmf)==undet)]
      }else{
        warning("The undetermined category does not exist in input CSMF.")
      }
    }  
    if(!is.null(names(csmf)) & !is.null(names(truth))){
      order <- match(names(csmf), names(truth))
      if(is.na(sum(order))){stop("Names not matching")}
      truth <- truth[order]
    }

    acc <- 1 - sum(abs(truth - csmf)) / 2 / (1 - min(truth))
  }


 return(acc)
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
#'                   version = "4.02", HIV = "h", Malaria = "l", write=FALSE)
#' getTopCOD(fit)
#' 
getTopCOD <- function(x, interVA.rule = TRUE){
  
  if(methods::is(x, "insilico")){
    probs <- x$indiv.prob
    pick <- colnames(probs)[apply(probs, 1, which.max)]
    # add possibility of no possible COD
    if(sum(apply(probs, 1, max) == 0) > 0){
      pick[which(apply(probs, 1, max) == 0)] <- "Undetermined"
    }
    id <- x$id
  }else if(methods::is(x, "interVA" )){
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
    }else if(methods::is(x, "interVA5")){
      id <- x$ID
      pick <- rep("", length(x$VA))
      for(i in 1:length(x$VA)){
        if(interVA.rule){
          pick[i] <- x$VA[[i]]$CAUSE1
        }else{
            prob <- x$VA[[i]]$wholeprob[4:64]
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
    }else if(methods::is(x, "tariff")){
      pick <- x$causes.test[, 2]
      id <- as.character(x$causes.test[, 1])
    }else if(methods::is(x, "nbc")){
      if (!isTRUE(requireNamespace("nbc4va", quietly = TRUE))) {
        stop("You need to install the packages 'nbc4va'. Please run in your R terminal:\n install.packages('nbc4va')")
      }
      pick <-  nbc4va::topCOD.nbc(x)[, 2]
      id <-  nbc4va::topCOD.nbc(x)[, 1]
    }

    return(data.frame(ID = id, cause = pick))
}

#' Extract individual distribution of cause of death
#'
#' @param x a fitted object from \code{codeVA}.
#' @param CI Credible interval for posterior estimates. If CI is set to TRUE, a list is returned instead of a data frame.
#' @param ... additional arguments that can be passed to \code{get.indiv} from InSilicoVA package.
#'
#' @return a data frame of COD distribution for each individual specified by row names.
#' @export getIndivProb
#'
#' @examples
#' data(RandomVA1)
#' # for illustration, only use interVA on 100 deaths
#' fit <- codeVA(RandomVA1[1:100, ], data.type = "WHO", model = "InterVA", 
#'                   version = "4.02", HIV = "h", Malaria = "l", write=FALSE)
#' probs <- getIndivProb(fit)
#' 
getIndivProb <- function(x, CI = NULL, ...){
  
  if(methods::is(x, "insilico")){    
    if(!is.null(CI)){
       indiv  <- InSilicoVA::get.indiv(x, CI = CI, ...)
       probs <- NULL
       probs$indiv.prob <- x$indiv.prob
       probs$indiv.prob.lower <- indiv$lower
       probs$indiv.prob.upper <- indiv$upper
       probs$indiv.prob.median <- indiv$median
       probs$indiv.CI <- CI
    }else{
       probs <- x$indiv.prob
    }

  }else if(methods::is(x, "interVA")){
      id <- x$ID
      probs <- matrix(NA, length(x$VA), length(x$VA[[1]]$wholeprob))
      for(i in 1:length(x$VA)){
          probs[i, ] <- x$VA[[i]]$wholeprob
      }
      rownames(probs) <- id
      colnames(probs) <- names(x$VA[[i]]$wholeprob)

    }else if(methods::is(x, "interVA5")){
      id <- x$ID
      probs <- matrix(NA, length(x$VA), length(x$VA[[1]]$wholeprob[4:64]))
      for(i in 1:length(x$VA)){
          probs[i, ] <- x$VA[[i]]$wholeprob[4:64]
      }
      rownames(probs) <- id
      colnames(probs) <- names(x$VA[[i]]$wholeprob[4:64])

    }else if(methods::is(x, "tariff")){
      warning("Tariff method produces only rankings of causes, not probabilities")
      probs <- x$score
    }else if(methods::is(x, "nbc")){
      probs <- x$prob
    }

    return(probs)
}

