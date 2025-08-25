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
#' @family output extraction
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
#'                   version = "5", HIV = "h", Malaria = "l", write=FALSE)
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
  
  if(methods::is(x, "eava")) {
    return(csmf_eava(x))
  }
  stop("Input object is not a fitted model from openVA. ")
}

#' Calculate CSMF accuracy
#'
#' @param csmf a CSMF vector from \code{getCSMF} or a InSilicoVA fitted object.
#' @param truth a CSMF vector of the true CSMF.
#' @param undet name of the category denoting undetermined causes. Default to be NULL. If undetermined cause is present, it will be removed and the rest of the CSMF will be re-normalized to sum to 1.
#'
#' @return a number (or vector if input is InSilicoVA fitted object) of CSMF accuracy as 1 - sum(abs(CSMF - CSMF_true)) / (2 * (1 - min(CSMF_true))).
#' @export getCSMF_accuracy
#' @family output extraction
#' @examples
#' csmf1 <- c(0.2, 0.3, 0.5)
#' csmf0 <- c(0.3, 0.3, 0.4)
#' names(csmf0) <- names(csmf1) <- c("c1", "c2", "c3")
#' getCSMF_accuracy(csmf1, csmf0)
#' getCSMF_accuracy(csmf1, rev(csmf0))
#' 
#' 
#'

getCSMF_accuracy <- function(csmf, truth, undet = NULL){
  ## when input is insilico fit
  if(methods::is(csmf, 'insilico')){
    if(!is.null(names(truth))){
      order1 <- match(colnames(csmf$csmf), names(truth))
      if(is.na(sum(order1))){stop("Exist names in estimated CSMF but not in the true CSMF")}
      order2 <- match(names(truth), colnames(csmf$csmf))
      if(is.na(sum(order2))){stop("Exist names in true CSMF but not in the estimated CSMF")}
      truth <- truth[order1]

    }else if(dim(csmf$csmf)[2] == length(truth)){
      warning("The CSMF vectors do not have name attributes. Treat the CSMFs as having the same order. Please double check they are correctly ordered.")
    }else{
      stop("The CSMF vector doesn't have the same length and there are no cause labels.")
    }
    truth <- matrix(truth, dim(csmf$csmf)[2], dim(csmf$csmf)[1])
    acc <- 1 - apply(abs(truth - t(csmf$csmf)), 2, sum) / 2 / (1 - min(truth))

  }else{
      ## when input is vector
      if(!is.null(undet)){
      if(undet %in% names(csmf)){
        csmf <- csmf[-which(names(csmf)==undet)]
        csmf <- csmf / sum(csmf)
      }else{
        warning("The undetermined category does not exist in input CSMF.")
      }
    }  
    if(!is.null(names(csmf)) && !is.null(names(truth))){
      order1 <- match(names(csmf), names(truth))
      if(is.na(sum(order1))){stop("Exist names in estimated CSMF but not in the true CSMF")}
      order2 <- match(names(truth), names(csmf))
      if(is.na(sum(order2))){stop("Exist names in true CSMF but not in the estimated CSMF")}
      
      truth <- truth[order1]
    }else if(length(csmf) == length(truth)){
      warning("The CSMF vectors do not have name attributes. Treat the CSMFs as having the same order. Please double check they are correctly ordered.")
    }else{
      stop("The CSMF vectors don't have the same length and there are no cause labels.")
    }

    acc <- 1 - sum(abs(truth - csmf)) / 2 / (1 - min(truth))
  }


 return(acc)
}


#' Extract the most likely cause(s) of death
#'
#' @param x a fitted object from \code{codeVA}.
#' @param interVA.rule Logical indicator for \code{interVA} object only. If
#' TRUE and (the parameter) n <= 3, then the InterVA thresholds are
#' used to determine the top causes.
#' @param n Number of top causes to include (if n > 3, then the parameter interVA.rule is
#' treated as FALSE).
#' @param include.prob Logical indicator for including the probabilities (for \code{insilico})
#' or indicator of how likely the cause is (for \code{interVA}) in the results
#'
#' @return a data frame of ID, most likely cause assignment(s), and corresponding
#' probability (for \code{insilico}) or indicator of how likely the cause is (for \code{interVA})
#' @export getTopCOD
#' @family output extraction
#' @examples
#' data(RandomVA1)
#' # for illustration, only use interVA on 100 deaths
#' fit <- codeVA(RandomVA1[1:100, ], data.type = "WHO", model = "InterVA", 
#'                   version = "4.02", HIV = "h", Malaria = "l", write=FALSE)
#' getTopCOD(fit)
#' \dontrun{
#' library(openVA)
#' 
#' # InterVA4 Example
#' data(SampleInput)
#' fit_interva <- codeVA(SampleInput, data.type = "WHO2012", model = "InterVA",
#'                       version = "4.03", HIV = "l", Malaria = "l", write = FALSE)
#' getTopCOD(fit_interva, n = 1)
#' getTopCOD(fit_interva, n = 3)
#' getTopCOD(fit_interva, n = 3, include.prob = TRUE)
#' getTopCOD(fit_interva, interVA.rule = FALSE, n = 3)
#' getTopCOD(fit_interva, n = 5)
#' getTopCOD(fit_interva, n = 5, include.prob = TRUE)
#'
#' # InterVA5 & Example
#' data(RandomVA5)
#' fit_interva5 <- codeVA(RandomVA5[1:50,], data.type = "WHO2016", model = "InterVA",
#'                        version = "5", HIV = "l", Malaria = "l", write = FALSE)
#' getTopCOD(fit_interva5, n = 1)
#' getTopCOD(fit_interva5, n = 3)
#' getTopCOD(fit_interva5, n = 3, include.prob = TRUE)
#' getTopCOD(fit_interva5, interVA.rule = FALSE, n = 3)
#' getTopCOD(fit_interva5, n = 5)
#' getTopCOD(fit_interva5, n = 5, include.prob = TRUE)
#'
#' # InSilicoVA Example
#' data(RandomVA5)
#' fit_insilico <- codeVA(RandomVA5[1:100,], data.type = "WHO2016", 
#'                        auto.length = FALSE)
#' getTopCOD(fit_insilico, n = 1)
#' getTopCOD(fit_insilico, n = 3)
#' getTopCOD(fit_insilico, n = 3, include.prob = TRUE)
#' 
#'
#' # Tariff Example (only top cause is returned)
#' data(RandomVA3)
#' test <- RandomVA3[1:200, ]
#' train <- RandomVA3[201:400, ]
#' allcauses <- unique(train$cause)
#' fit_tariff <- tariff(causes.train = "cause", symps.train = train, 
#'                      symps.test = test, causes.table = allcauses)
#' getTopCOD(fit_tariff, n = 1)
#'
#' # NBC Example
#' library(nbc4va)
#' data(nbc4vaData)
#' train <- nbc4vaData[1:50, ]
#' test <- nbc4vaData[51:100, ]
#' fit_nbc <- nbc(train, test, known=TRUE)
#' getTopCOD(fit_nbc, n = 1)
#' getTopCOD(fit_nbc, n = 3)
#' getTopCOD(fit_nbc, n = 3, include.prob = TRUE)
#' }
#' 
getTopCOD <- function(x, interVA.rule = TRUE, n = 1, include.prob = FALSE){
  
  if (!is.numeric(n) | n < 1 | is.na(n)) {
    n <- 1
  }
  n_top <- round(n)
  if (methods::is(x, "insilico")) {
    probs <- x$indiv.prob

    output <- data.frame(ID = row.names(probs))
    if (n_top == 1) {
      cause_col_names <- "cause1"
      probs_col_names <- "prob1"
      output[cause_col_names] = colnames(probs)[apply(probs, 1, which.max)]
      output[probs_col_names] = apply(probs, 1, max)
      n_max <- apply(probs, 1, function(x) sum(x == max(x)))
      if (any(n_max > 1)) {
          warning("There are ties (i.e., multiple causes have the same max probability. \n  This can be seen by increasing the argument n.")
      }
    } else {
        n_top <- ifelse(n_top > ncol(probs), ncol(probs), n_top)
        for (i in 1:n_top) {
          output[[paste0("cause", i)]] <- ""
          output[[paste0("prob", i)]] <- NA_real_
        }
        top_probs_index <- apply(probs, 1, order, decreasing = TRUE)[1:n_top,]
        ## n_unique <- apply(probs, 1, function(x) length(unique(x)))
        n_non_zero <- apply(probs, 1, function(x) sum(x > 0))

        for (i in 1:nrow(probs)) {
          ## n_top_i <- ifelse(n_unique[i] - 1 < n_top, n_unique[i] - 1, n_top)
          n_top_i <- ifelse(n_non_zero[i] < n_top, n_non_zero[i], n_top)
          cause_col_names <- paste0("cause", 1:n_top_i)
          prob_col_names <- paste0("prob", 1:n_top_i)
          top_probs_index_i <- top_probs_index[1:n_top_i, i]
          output[i, cause_col_names] <- names(probs[i, top_probs_index_i])
          output[i, prob_col_names] <- probs[i, top_probs_index_i]
        }
    }
    # add possibility of no possible COD
    if (sum(apply(probs, 1, max) == 0) > 0) {
        cause_col_names <- paste0("cause", 1:n_top)
        prob_col_names <- paste0("prob", 1:n_top)
        output[which(apply(probs, 1, max) == 0), cause_col_names] <- "Undetermined"
        output[which(apply(probs, 1, max) == 0), prob_col_names] <- NA_real_
    }
  } else if (methods::is(x, "interVA") | methods::is(x, "interVA5")) {
      preg_comcat_cols <- c("Not pregnant or recently delivered",
                            "Pregnancy ended within 6 weeks of death",
                            "Pregnant at death",
                            "Culture", "Emergency", "Health systems",
                            "Inevitable", "Knowledge", "Resources")
      index_cod <- !(names(x$VA[[1]]$wholeprob) %in% preg_comcat_cols)
      n_top <- ifelse(n_top > sum(index_cod), sum(index_cod), n_top)
      output <- data.frame(ID = x$ID)
      for (i in 1:n_top) {
        output[[paste0("cause", i)]] <- ""
        output[[paste0("lik", i)]] <- NA_real_
      }
      for (i in 1:length(x$VA)) {
        if (interVA.rule & n_top <=3) {
          cause_col_names <- paste0("cause", 1:n_top)
          prob_col_names <- paste0("lik", 1:n_top)
          results_col_names <- paste0("CAUSE", 1:n_top)
          results_prob_names <- paste0("LIK", 1:n_top)
          output[i, cause_col_names] <- x$VA[[i]][results_col_names]
          output[i, prob_col_names] <- as.numeric(x$VA[[i]][results_prob_names])/100
          if (x$VA[[i]]$CAUSE1 == " ") {
              output[i, "cause1"] <- "Undetermined"
              output[i, "lik1"] <- 1.00
          }
        } else {
            probs <- x$VA[[i]]$wholeprob[index_cod]
            top_probs_index <- order(probs, decreasing = TRUE)[1:n_top]
            n_unique <- length(unique(probs))
            n_top_i <- ifelse(n_unique - 1 < n_top, n_unique - 1, n_top)
            cause_col_names <- paste0("cause", 1:n_top_i)
            prob_col_names <- paste0("lik", 1:n_top_i)
            top_probs_index_i <- top_probs_index[1:n_top_i]
            output[i, cause_col_names] <- names(probs[top_probs_index_i])
            output[i, prob_col_names] <- probs[top_probs_index_i]
        }
      }
  } else if (methods::is(x, "tariff")) {
      n_top <- 1
      output <- data.frame(ID = as.character(x$causes.test[, 1]),
                           cause1 = x$causes.test[, 2])
  } else if (methods::is(x, "nbc")) {
      if (!isTRUE(requireNamespace("nbc4va", quietly = TRUE))) {
        stop("You need to install the packages 'nbc4va'. Please run in your R terminal:\n install.packages('nbc4va')")
      }
      output <- data.frame(ID = x$prob$CaseID)
      n_cod <- ncol(x$prob) - 1
      n_top <- ifelse(n_top > n_cod, n_cod, n_top) 
      for (i in 1:n_top) {
        output[[paste0("cause", i)]] <- ""
        output[[paste0("prob", i)]] <- NA_real_
      }
      probs <- x$prob[, names(x$prob) != "CaseID"]
      top_probs_index <- apply(probs, 1, order, decreasing = TRUE)[1:n_top,]
      n_unique <- apply(probs, 1, function(x) length(unique(x)))

      for (i in 1:nrow(probs)) {
        n_top_i <- ifelse(n_unique[i] - 1 < n_top, n_unique[i] - 1, n_top)
        if (n_top_i == 0) {
          cause_col_names <- "cause1"
          output[i, cause_col_names] <- "Undetermined"
        } else {
            cause_col_names <- paste0("cause", 1:n_top_i)
            prob_col_names <- paste0("prob", 1:n_top_i)
            if (is.null(dim(top_probs_index))) {
              top_probs_index_i <- top_probs_index[i]
            } else {
                top_probs_index_i <- top_probs_index[1:n_top_i, i]
            }
            output[i, cause_col_names] <- colnames(probs)[top_probs_index_i]
            output[i, prob_col_names] <- probs[i, top_probs_index_i]
        }
      }
  }
  
  if (!include.prob) {
    cause_col_names <- paste0("cause", 1:n_top)
    output <- output[, c("ID", cause_col_names)]
  }

  return(output)
}


#' Extract individual distribution of cause of death
#'
#' @param x a fitted object from \code{codeVA}.
#' @param CI Credible interval for posterior estimates. If CI is set to TRUE, a list is returned instead of a data frame.
#' @param ... additional arguments that can be passed to \code{get.indiv} from InSilicoVA package.
#'
#' @return a data frame of COD distribution for each individual specified by row names.
#' @export getIndivProb
#' @family output extraction
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

#' Calculate Overall chance-corrected concordance (CCC)
#' 
#' Denote the cause-specific accuracy for the j-th cause to be (# of deaths correctly assigned to cause j) / (# of death due to cause j). For causes 1, 2, ..., C, the cause-specific CCC is computed for the j-th cause is defined to be (j-th cause-specific accuracy - 1 / C) / (1 - 1 / C) and the overall CCC is the average of each cause-specific CCC.
#' 
#' 
#' @param cod a data frame of estimated cause of death. The first column is the ID and the second column is the estimated cause.
#' @param truth a data frame of true causes of death. The first column is the ID and the second column is the estimated cause.
#' @param C the number of possible causes to assign. If unspecified, the number of unique causes in cod and truth will be used.
#' 
#' @export getCCC
#' @family output extraction
#' @examples
#' est <- data.frame(ID = c(1, 2, 3), cod = c("C1", "C2", "C1"))
#' truth <- data.frame(ID = c(1, 2, 3), cod = c("C1", "C3", "C3"))
#' # If there are only three causes
#' getCCC(est, truth)
#' # If there are 20 causes that can be assigned
#' getCCC(est, truth, C = 20)
#' 
getCCC <- function(cod, truth, C = NULL){

  order1 <- match(cod[,1], truth[,1])
  if(is.na(sum(order1))){stop("Exist IDs in estimated COD but not in the true COD")}
  order2 <- match(truth[,1], cod[,1])
  if(is.na(sum(order2))){stop("Exist IDs in true COD but not in the estimated COD")}
  truth <- truth[order1, ]

  cod <- as.character(cod[, 2])
  truth <- as.character(truth[, 2])
  if(is.null(C)) C <- length(unique(c(truth, cod)))
  cccj <- rep(NA, C)
  correct <- cod[which(cod == truth)]
  N <- length(truth)

  for(i in 1:length(unique(truth))){
    c <- sort(unique(truth))[i]
    if(length(which(truth == c)) == 0) next
    cccj[i] <- length(which(correct == c))/length(which(truth == c))
    cccj[i] <- (cccj[i] - 1/C) / (1 - 1/C)
  }
  ccc <- mean(cccj, na.rm = TRUE)
  return(ccc)
}
