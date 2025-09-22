#' Convert the assigned causes to the format expected by
#' vacalibration::vacalibration().
#' 
#' This function prepares the output from codeVA (using the InSilicoVA or
#' InterVA5 algorithms) and/or EAVA for the vacalibration function.  The 
#' output is a named list, with each element consisting of a data frame with
#' columns for the death ID ("ID") and the assigned CoD ("cause").  The name
#' of each element is the algorithm used to assign the CoD -- "insilicova",
#' "interva", or "eava".  If the 
#'
#' @param fit a fitted object from \code{codeVA} using either InSilicoVA,
#' InterVA5, or output from \code{EAVA::codEAVA}.  The EAVA algorithm is assumed
#' if the the argument is a Data Frame (with columns "ID" and "cause").
#' 
#' @param ... more fitted objects from \code{codeVA} using either InSilicoVA or
#' InterVA5, or from \code{EAVA::codEAVA}.
#'
#' @returns a list with each element being a Data Frames that contains variables
#' "ID" and "cause", and the element's name (in the list) is the algorithm name
#' used to assign causes of death (either "insilicova", "interva", or "eava")
#' 
#' @export prepCalibration
#'
#' @examples
#' \dontrun{
#' data(NeonatesVA5)
#' fit_insilico <- codeVA(NeonatesVA5)
#' insilico_prep <- prepCalibration(fit_insilico)
#' calib_insilico = vacalibration::vacalibration(va_data = insilico_prep,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique")
#' summary(calib_insilico)
#' 
#' fit_interva <- codeVA(NeonatesVA5, model = "InterVA", version = "5", write = FALSE)
#' interva_prep <- prepCalibration(fit_interva)
#' calib_interva = vacalibration::vacalibration(va_data = interva_prep,
#'                                              age_group = "neonate",
#'                                              country = "Mozambique")
#' summary(calib_interva)
#' 
#' two_fits <- prepCalibration(fit_insilico, fit_interva)
#' calib_ensemble = vacalibration::vacalibration(va_data = two_fits,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique")
#' summary(calib_ensemble)
#' 
#' } 
prepCalibration <- function(fit, ...) {
  
  args <- list(fit, ...)
  out <- list()
  out_names <- NULL
  index <- 1
  
  for (x in args) {
    
    if (methods::is(x, "insilico")) {
      out[[index]] <- prepCalibrationInsilico(x)[[1]]
      out_names <- c(out_names, "insilicova")
    } else if (methods::is(x, "interVA5")) {
      out[[index]] <- prepCalibrationInterva5(x)[[1]]
      out_names <- c(out_names, "interva")
    } else if (is.data.frame(x) & ncol(x) == 2 & 
               all(tolower(names(x)) %in% c("id", "cause"))) {
      out[[index]] <- x
      out_names <- c(out_names, "eava")
    } else {
      out[[index]] <- NA
      out_names <- c(out_names, "ERROR")
      warning (paste0("Argument ", index, 
                      " is not a fitted object from InSilicoVA or InterVA5",
                      "\n(corresponding output is NA)\n"))
    }
    index <- index + 1
  }
  names(out) <- out_names
  return (out)
}

prepCalibrationInsilico <- function(x) {
  
  cod_labels <- colnames(x$indiv.prob)
  cod_index <- apply(x$indiv.prob, 1, which.max)
  cause <- cod_labels[cod_index]
  id <- row.names(x$indiv.prob)
  cods <- data.frame("ID" = id, "cause" = cause)
  return ( stats::setNames(list(cods), list("insiliocva")) )
}

prepCalibrationInterva5 <- function(x) {
  
  cods <- lapply(x$VA5, function(va) c("ID" = va$ID, "cause" = va$CAUSE1))
  cods <- as.data.frame(do.call(rbind, cods))
  cods$cause[cods$cause == " "] <- "Unspecified"
  return ( stats::setNames(list(cods), list("interva")) )
}
