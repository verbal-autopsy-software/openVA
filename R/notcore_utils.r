#' Print method for "eava" class.
#'
#' @param x \code{eava} object.
#' 
#' \dontrun{
#' data(DataEAVA)
#' eava_results <- codeVA(DataEAVA, data.type = "EAVA", model = "EAVA")
#' eava_results
#' }
#'  
#' @exportS3Method EAVA::print
print.eava <- function(x) {
  cat(paste("EAVA fitted on", length(x$ID), x$age_group,
            "deaths. Here are the first 5 records...\n"))
  cat("\n")
  out <- data.frame("ID" = x$ID, "cause" = x$cause)
  print(head(out, n = 5))
}

#' Calculate CSMF from EAVA::codEAVA output
#'
#' @param x output from EAVA::codEAVA
#'
#' @returns Named vector 
#'
csmf_eava <- function(x) {
  csmf <- table(x$cause) / length(x$cause)
  cod_labels <- names(csmf)
  csmf <- as.vector(csmf)
  names(csmf) <- cod_labels
  return(csmf)
}

#' Summary of results obtained by fitting the EAVA algorithm.
#' 
#' This function prints a summary message of the results along with
#' the top cause-specific mortality fractions (CSMFs).
#'
#' @param x \code{eava} object
#' @param top number of top CSMF to show
#' @param rnd number of decimal places to round the CSMF
#' @return \item{id}{ all IDs of the deaths}
#' \item{cause}{ assigned cause for individual of death }
#' \item{N}{ number of deaths}
#' \item{age_group}{age group that the deaths belong to (either child or neonate)}
#' \item{csmf.ordered}{ cause-specific mortality fractions in decreasing order }
#'
#' \dontrun{
#' data(DataEAVA)
#' eava_results <- codeVA(DataEAVA, data.type = "EAVA", model = "EAVA")
#' eava_summary <- summary(eava_results)
#' eava_summary
#' } 
#' @exportS3Method EAVA::summary
summary.eava <- function(x, top = 5, rnd = 4) {
  
  csmf <- csmf_eava(x)
  csmf <- csmf[order(csmf, decreasing = TRUE)]
  n_top <- min(length(csmf), top)
  csmf.ordered <- data.frame("cause" = names(csmf)[1:n_top],
                             "proportion" = round(csmf[1:n_top], rnd))
  row.names(csmf.ordered) <- NULL
  out <- list("id" = x$ID,
              "cause" = x$cause,
              "N" = length(x$ID),
              "age_group" = x$age_group,
              "csmf.ordered" = csmf.ordered)
  class(out) <- "eava_summary"
  return(out)
}

#' Print method for summarizing results from EAVA algorithm.
#' 
#' This function prints a summary message of the results along with
#' the top cause-specific mortality fractions (CSMFs).
#'
#' @param x \code{eava} object
#'
#' @exportS3Method EAVA::print
print.eava_summary <- function(x) {
  cat(paste("EAVA fitted on ", length(x$id), x$age_group, "deaths.\n"))
  top <- nrow(x$csmf.ordered)
  cat(paste("Top", top, "CSMFs:\n\n"))
  out <- x$csmf.ordered
  out$proportion <- out$proportion
  print(out, right = FALSE, row.names = FALSE)
}

#' Create CSMF plot for EAVA::codEAVA output
#'
#' @param x Output from EAVA::codEAVA
#' @param top The number of top causes to include in plot. This is exceeded if
#' there are ties.
#' @param main Title for CSMF plot
#' @param type An indicator of the type of chart to plot.  "pie" for pie chart;
#' "bar" for bar chart.
#' @param return.barplot a logical indicating if the (barplot) ggplot() object
#' should be returned (instead of printed).  Default value is FALSE.
#'
#' @returns A barplot if return.barplot is TRUE; otherwise, nothing is returned.
plot.eava <- function(x, top = 10, title = "Top CSMF Distribution",
                      type = "bar", return.barplot = FALSE, ...) {
  dist.cod <- csmf_eava(x)
  dist.cod <- sort(dist.cod, decreasing = TRUE)
  min.prob <- 0
  if (length(dist.cod) > top) {
    min.prob <- dist.cod[top]
  }
  if (type == "pie") {
    dist.cod.sort <- sort(dist.cod, decreasing=TRUE)
    pie.color <- grey.colors(length(dist.cod.sort[dist.cod.sort >= min.prob]))
    pie.color.left <- rep(pie.color[length(pie.color)], length(dist.cod.sort[dist.cod.sort < min.prob]))
    pie.color <- c(pie.color, pie.color.left)
    pie(dist.cod.sort, main = title,
        col = pie.color, labels = names(dist.cod.sort)[dist.cod.sort >= min.prob],
        ...)
  }
  if (type == "bar") {
    dist.cod.min <- dist.cod[dist.cod >= min.prob ]
    dist.cod.min <- sort(dist.cod.min, decreasing = FALSE)
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      barplot.df <- data.frame(Probability = dist.cod.min,
                               Causes = names(dist.cod.min))
      g <- ggplot2::ggplot(barplot.df,
                           ggplot2::aes(x = stats::reorder(Causes,
                                                           seq(1:length(Causes))),
                                        y = Probability,
                                        fill = stats::reorder(Causes,
                                                              seq(1:length(Causes))))) +
        ggplot2::geom_bar(stat="identity") +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_grey(start = 0.8, end = 0.2) +
        ggplot2::ggtitle(title) +
        ggplot2::theme(legend.position = "none")
      if (return.barplot) {
        return(g)
      } else {
        par(las = 2)
        par(mar = c(5,15,4,2))
        print(g)
      }
      
    } else {
      bar.color <- grey.colors(length(dist.cod.min))
      bar.color <- rev(bar.color)
      barplot(dist.cod.min, horiz = TRUE, names.arg = names(dist.cod.min),
              main = title, col = bar.color, cex.names=0.8,
              xlab = "Probability", ...)
    }
  }
}

#' Summary of results obtained by vacalibration
#' 
#' This function prints a summary message of the results along with
#' the top cause-specific mortality fractions (CSMFs).
#'
#' @param x \code{vacalibration} object
#' @param top number of top CSMF to show
#' @param rnd number of decimal places to round the CSMF
#' @param algorithm a name or vector of names of algorithm(s) which
#' limits the output to those specific results
#'
#' \dontrun{
#' data(NeonatesVA5)
#' fit_insilico <- codeVA(NeonatesVA5, auto.length = FALSE)
#' insilico_prep <- prepCalibration(fit_insilico)
#' calib_insilico = vacalibration::vacalibration(va_data = insilico_prep,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique",
#'                                               plot_it = FALSE)
#' calib_insilico_summ <- summary(calib_insilico)
#' names(calib_insilico_summ)
#' calib_insilico_summ
#' 
#' fit_interva <- codeVA(NeonatesVA5, model = "InterVA", version = "5", write = FALSE)
#' interva_prep <- prepCalibration(fit_interva)
#' calib_interva = vacalibration::vacalibration(va_data = interva_prep,
#'                                              age_group = "neonate",
#'                                              country = "Mozambique",
#'                                              plot_it = FALSE)
#' summary(calib_interva, top = 3)
#' 
#' two_fits <- prepCalibration(fit_insilico, fit_interva)
#' calib_ensemble = vacalibration::vacalibration(va_data = two_fits,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique",
#'                                               plot_it = FALSE)
#' summary(calib_ensemble)
#' }
#' @exportS3Method vacalibration::summary
summary.vacalibration <- function(x, top = 5, rnd = 4, algorithm = NULL) {
  csmf <- x$pcalib_postsumm
  n <- lapply(x$input$va_unlabeled, sum)
  n <- unlist(n)
  alg_names <- row.names(csmf)
  
  if (!is.null(algorithm) &
      !(all(algorithm %in% alg_names))) {
    not_found <- algorithm[!(algorithm %in% row.names(csmf))]
    not_found <- paste(not_found, collapse = " ")
    warning(paste("`algorithm`", not_found, "not found."),
            immediate.=TRUE)
    if (length(not_found) == length(algorithm)) {
      out <- list("Error" = "No results for requested algorithm(s).")
      class(out) <- "vacalibration_summary"
      return (out)
    }
  }
  
  if (!is.null(algorithm)) alg_names <- algorithm[algorithm %in% alg_names]
  
  out <- NULL
  out$nBurn <- x$input$nBurn
  out$nIterations <- x$input$nBurn + x$input$nMCMC*x$input$nThin
  out$nMCMC <- x$input$nMCMC
  out$nThin <- x$input$nThin
  out$age_group <- x$input$age_group
  out$algorithms <- alg_names
  out$n <- n
  out$show_top = top
  out$ensemble <- x$input$ensemble
  out$ensemble_algorithms <- names(n)
  uncal <- t(x$p_uncalib)
  uncal <- round(uncal, rnd)
  uncalibrated <- data.frame(uncal, row.names = NULL)
  uncalibrated$cause <- row.names(uncal)
  out$uncalibrated <- uncalibrated[, c("cause", alg_names)]
  
  # assuming dimension order is (algorithm, summary statistics, cause)
  postsumm <- aperm(x$pcalib_postsumm, c(3,2,1))
  postsumm <- round(postsumm, rnd)
  pcalib_alg <- dimnames(postsumm)[[3]]
  pcalib <- data.frame(matrix(ncol = 5, nrow = 0))
  names(pcalib) <- c("algorithm", "cause", "mean", "lower", "upper")
  for (alg in pcalib_alg) {
    if (!(alg %in% alg_names)) next
    tmp_df <- data.frame(postsumm[,,alg])
    names(tmp_df) <- c("mean", "lower", "upper")
    cause <- row.names(tmp_df)
    row.names(tmp_df) <- NULL
    tmp_df$cause <- cause
    tmp_df$algorithm <- alg
    pcalib <- rbind(pcalib, tmp_df)
  }
  out$pcalib_postsumm <- pcalib[, c("algorithm", "cause", "mean", "lower", "upper")]
  
  for (alg in alg_names) {
    alg_csmf <- t(csmf[alg,,])
    csmf_order <- order(alg_csmf[, "postmean"], decreasing = TRUE)
    csmf_ordered <- alg_csmf[csmf_order,]
    csmf_ordered <- round( csmf_ordered, rnd)
    nrows_keep <- min(nrow(csmf_ordered), top)
    csmf_ordered <- csmf_ordered[1:nrows_keep,]
    colnames(csmf_ordered) <- c("mean", "lower", "upper")
    cause <- row.names(csmf_ordered)
    csmf_ordered <- data.frame(csmf_ordered, row.names = NULL)
    csmf_ordered$cause <- cause
    out[[alg]] <- csmf_ordered[, c("cause", "mean", "lower", "upper")]
  }
  class(out) <- "vacalibration_summary"
  return(out)
}

#' Print method for summarizing vacalibration results
#' 
#' This function prints a summary message of the results along with
#' the top cause-specific mortality fractions (CSMFs).
#'
#' @param x \code{vacalibration} object
#' @param top number of top CSMF to show
#' @param rnd number of decimal places to round the CSMF
#' @param algorithm a name or vector of names of algorithm(s) which
#' limits the output to those specific results
#'
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
#' summary(calib_interva, top = 3, rnd = 2)
#' 
#' two_fits <- prepCalibration(fit_insilico, fit_interva)
#' calib_ensemble = vacalibration::vacalibration(va_data = two_fits,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique")
#' summary(calib_ensemble, algorithm = "ensemble")
#' summary(calib_ensemble, algorithm = c("ensemble", "insilicova"))
#' 
#' }
#' @exportS3Method vacalibration::print
print.vacalibration_summary <- function(x) {
  
  if ("Error" %in% names(x)) {
    cat("No results for requested algorithm(s).\n")
  } else {
    cat("VA Calibration\n")
    cat(paste(x$nIterations, "iterations performed, with first", x$nBurn,
              "iterations discarded.\n", x$nMCMC,
              "iterations saved after thinning\n\n"))
    
    for (alg in x$algorithms) {
      if (alg == "ensemble") {
        ensemble_algorithms <- paste(x$ensemble_algorithms, collapse = " ")
        cat(paste("Ensemble of:", ensemble_algorithms, "\n"))
      } else {
        cat(paste(alg, "(calibrated)\n"))
        cat(paste(x$n[[alg]], x$age_group, "deaths\n"))
      }
      cat(paste("Top", x$show_top, "CSMFs:\n\n"))
      
      print(x[[alg]], right = FALSE)
      cat("\n\n")
    }
  }
}

# neonate.cod <- c("NNT", "Malformation", "Intrapartum",
#                  "Preterm", "Meningitis", "Diarrhea", "Pneumonia",
#                  "Pneumonia", "Sepsis", "Other", "Unspecified")
# 
# child.cod <- c("Intrapartum", "Malformation", "Preterm", "Injury",
#                "AIDS", "Measles", "Meningitis/Encephalitis",
#                "Diarrhea/Dysentery", "Other infections", "Pneumonia",
#                "Malaria", "Unspecified")
