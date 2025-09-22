#' Print method for "eava" class.
#'
#' @param x \code{eava} object.
#' @param \dots not used
#' 
#' @examples
#' \dontrun{
#' data(DataEAVA)
#' eava_results <- codeVA(DataEAVA, data.type = "EAVA", model = "EAVA")
#' eava_results
#' }
#'  
#' @exportS3Method EAVA::print
print.eava <- function(x, ...) {
  cat(paste("EAVA fitted on", length(x$ID), x$age_group,
            "deaths. Here are the first 5 records...\n"))
  cat("\n")
  out <- data.frame("ID" = x$ID, "cause" = x$cause)
  print(utils::head(out, n = 5))
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
#' @param object \code{eava} object
#' @param top number of top CSMF to show
#' @param rnd number of decimal places to round the CSMF
#' @param \dots not used
#' @return \item{id}{ all IDs of the deaths}
#' \item{cause}{ assigned cause for individual of death }
#' \item{N}{ number of deaths}
#' \item{age_group}{age group that the deaths belong to (either child or neonate)}
#' \item{csmf.ordered}{ cause-specific mortality fractions in decreasing order }
#'
#' @examples
#' \dontrun{
#' data(DataEAVA)
#' eava_results <- codeVA(DataEAVA, data.type = "EAVA", model = "EAVA")
#' eava_summary <- summary(eava_results)
#' eava_summary
#' } 
#' @exportS3Method EAVA::summary
summary.eava <- function(object, top = 5, rnd = 4, ...) {
  
  csmf <- csmf_eava(object)
  csmf <- csmf[order(csmf, decreasing = TRUE)]
  n_top <- min(length(csmf), top)
  csmf.ordered <- data.frame("cause" = names(csmf)[1:n_top],
                             "proportion" = round(csmf[1:n_top], rnd))
  row.names(csmf.ordered) <- NULL
  out <- list("id" = object$ID,
              "cause" = object$cause,
              "N" = length(object$ID),
              "age_group" = object$age_group,
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
#' @param \dots not used
#'
#' @exportS3Method EAVA::print
print.eava_summary <- function(x, ...) {
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
#' @param title Title for CSMF plot
#' @param top The number of top causes to include in plot. This is exceeded if
#' there are ties.
#' @param type An indicator of the type of chart to plot.  "pie" for pie chart;
#' "bar" for bar chart.
#' @param return.barplot a logical indicating if the (barplot) ggplot() object
#' should be returned (instead of printed).  Default value is FALSE.
#' @param \dots Not used.
#'
#' @returns A barplot if return.barplot is TRUE; otherwise, nothing is returned.
#' @importFrom rlang .data
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
    pie.color <- grDevices::grey.colors(length(dist.cod.sort[dist.cod.sort >= min.prob]))
    pie.color.left <- rep(pie.color[length(pie.color)], length(dist.cod.sort[dist.cod.sort < min.prob]))
    pie.color <- c(pie.color, pie.color.left)
    graphics::pie(dist.cod.sort, main = title,
        col = pie.color, labels = names(dist.cod.sort)[dist.cod.sort >= min.prob],
        ...)
  }
  if (type == "bar") {
    dist.cod.min <- dist.cod[dist.cod >= min.prob ]
    dist.cod.min <- sort(dist.cod.min, decreasing = FALSE)
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      barplot.df <- data.frame("Probability" = dist.cod.min,
                               "Causes" = names(dist.cod.min))
      g <- ggplot2::ggplot(barplot.df,
                           ggplot2::aes(x = stats::reorder(.data$Causes,
                                                           seq(1:length(.data$Causes))),
                                        y = .data$Probability,
                                        fill = stats::reorder(.data$Causes,
                                                              seq(1:length(.data$Causes))))) +
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
        graphics::par(las = 2)
        graphics::par(mar = c(5,15,4,2))
        print(g)
      }
      
    } else {
      bar.color <- grDevices::grey.colors(length(dist.cod.min))
      bar.color <- rev(bar.color)
      graphics::barplot(dist.cod.min, horiz = TRUE, names.arg = names(dist.cod.min),
                        main = title, col = bar.color, cex.names=0.8,
                        xlab = "Probability", ...)
    }
  }
}

#' Print method for vacalibration model fits
#' 
#' This function is the print method for class \code{vacalibration}
#'
#' @param x \code{vacalibration} object
#' @param ... not used
#'
#' @exportS3Method vacalibration::print
#'
#' @examples
#' \dontrun{
#' data(NeonatesVA5)
#' fit_insilico <- codeVA(NeonatesVA5, auto.length = FALSE)
#' insilico_prep <- prepCalibration(fit_insilico)
#' calib_insilico = vacalibration::vacalibration(va_data = insilico_prep,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique",
#'                                               plot_it = FALSE)
#' calib_insilico
#' }
print.vacalibration <- function(x, ...) {
  cat("vacalibration fitted object:\n")
  nIterations <- x$input$nBurn + x$input$nMCMC*x$input$nThin
  cat(paste(nIterations, "iterations performed, with first",
            x$input$nBurn,
            "iterations discarded.\n", x$input$nMCMC,
            "iterations saved after thinning\n\n"))
  cat(paste("Results for: "))
  
  algorithms <- row.names(x$pcalib_postsumm)
  ensemble_algorithms <- names(x$input$va_unlabeled)
  n <- lapply(x$input$va_unlabeled, sum)
  age_group <- x$input$age_group
  
  for (alg in algorithms) {
    if (alg == "ensemble") {
      cat(paste("Ensemble of:", ensemble_algorithms, "\n"))
    } else {
      cat(paste(alg, "(calibrated):  "))
      cat(paste(n[[alg]], age_group, "deaths\n"))
    }
    cat("\n\n")
  }
}

#' Summary of results obtained by vacalibration
#' 
#' This function prints a summary message of the results along with
#' the top cause-specific mortality fractions (CSMFs).
#'
#' @param object \code{vacalibration} object
#' @param top number of top CSMF to show
#' @param rnd number of decimal places to round the CSMF
#' @param algorithm a name or vector of names of algorithm(s) which
#' limits the output to those specific results
#' @param \dots not used
#'
#' @examples
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
summary.vacalibration <- function(object, top = 5, rnd = 4, algorithm = NULL, ...) {
  csmf <- object$pcalib_postsumm
  n <- lapply(object$input$va_unlabeled, sum)
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
  out$nBurn <- object$input$nBurn
  out$nIterations <- object$input$nBurn + object$input$nMCMC*object$input$nThin
  out$nMCMC <- object$input$nMCMC
  out$nThin <- object$input$nThin
  out$age_group <- object$input$age_group
  out$algorithms <- alg_names
  out$n <- n
  out$show_top = top
  out$ensemble <- object$input$ensemble
  out$ensemble_algorithms <- names(n)
  uncal <- t(object$p_uncalib)
  uncal <- round(uncal, rnd)
  uncalibrated <- data.frame(uncal, row.names = NULL)
  uncalibrated$cause <- row.names(uncal)
  out$uncalibrated <- uncalibrated[, c("cause", alg_names)]
  
  # assuming dimension order is (algorithm, summary statistics, cause)
  postsumm <- aperm(object$pcalib_postsumm, c(3,2,1))
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
#' @param \dots not used
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
print.vacalibration_summary <- function(x, top, rnd, algorithm, ...) {
  
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

#' Plot CSMF from a vacalibration object
#'
#' @param x Fitted \code{"vacalibration"} objects
#' @param type An indicator of the type of chart to plot. "errorbar" for line
#' plots of only the error bars on single population; "bar" for bar chart with
#' error bars on single population; "compare" for line charts on multiple
#' calibrated algorithms.
#' @param algorithm Name or vector of names of algorithm(s) which
#' limits the output to those specific results
#' @param uncalibrated Logical (TRUE/FALSE) indicator for including the
#' uncalibrated CSMF in the plots (not valid with \code{type} is set to
#' "compare").
#' @param top The number of top causes (in the calibrated CSMF) to plot. If
#' results from multiple algorithms are included in the fitted "vacalibration"
#' object and \code{type} is set to "compare", it will plot the union of the
#' top causes in all algorithms.
#' @param title Title of the plot.
#' @param xlab Labels for the causes.
#' @param ylab Labels for the CSMF values.
#' @param horiz Logical indicator indicating if the bars are plotted
#' horizontally.
#' @param angle Angle of rotation for the texts on x axis when \code{horiz} is
#' set to FALSE
#' @param fill The color to fill the bars when \code{type} is set to "bar".
#' @param fill_uncalibrated The color to fill the bars for the uncalibrated
#' CSMFs when \code{type} is set to "bar".
#' @param border The color to color the borders of bars when \code{type} is set
#' to "bar".
#' @param err_width Size of the error bars.
#' @param err_size Thickness of the error bar lines.
#' @param point_size Size of the points.
#' @param bw Logical indicator for setting the theme of the plots to be black
#' and white.
#' @param plot_it Logical (TRUE/FALSE) indicating if the first plot should
#' be rendered.
#' @param \dots Not used.
#' @return A list of plots for each algorithms included in the fitted
#' "vacalibration" object.
#'
#' @examples
#' \dontrun{
#' data(NeonatesVA5)
#' fit_insilico <- codeVA(NeonatesVA5)
#' insilico_prep <- prepCalibration(fit_insilico)
#' calib_insilico = vacalibration::vacalibration(va_data = insilico_prep,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique")
#' 
#' fit_interva <- codeVA(NeonatesVA5, model = "InterVA", version = "5", write = FALSE)
#' interva_prep <- prepCalibration(fit_interva)
#' calib_interva = vacalibration::vacalibration(va_data = interva_prep,
#'                                              age_group = "neonate",
#'                                              country = "Mozambique")
#'                                              
#' two_fits <- prepCalibration(fit_insilico, fit_interva)
#' calib_ensemble = vacalibration::vacalibration(va_data = two_fits,
#'                                               age_group = "neonate",
#'                                               country = "Mozambique")
#' plot(calib_ensemble)
#' }
#' @importFrom rlang .data
#' @exportS3Method vacalibration::plot
plot.vacalibration <- function(x, type = c("errorbar", "bar", "compare")[1],
                               algorithm = NULL, uncalibrated = FALSE,
                               top = 10, title = "Top CSMF Distribution",
                               xlab = "Causes", ylab = "CSMF", horiz = TRUE,
                               angle = 60, fill = "lightblue", 
                               fill_uncalibrated = "lightpink",
                               err_width = .4, err_size = .6, point_size = 2,
                               border = "black", bw = TRUE, plot_it = TRUE, ...) {
  if (!(type %in% c("errorbar", "bar", "compare"))) {
    warning(paste("Plot type", type, "not recognized."))
    return (NULL)
  }
  if (!is.null(algorithm)) {
    # summary should print message if algorithm parameter not found
    sx <- summary(x, top = top, rnd = 6, algorithm = algorithm)
    if ("Error" %in% names(sx)) {
      warning("No results for requested algorihtm(s).")
      return (NULL)
    }
  } else {
    sx <- summary(x, top = top, rnd = 6)
  }
  algorithm <- sx$algorithms
  if ("ensemble" %in% algorithm) {
    # use ensemble to set the order of the CSMF
    algorithm <- c("ensemble", algorithm[algorithm != "ensemble"])
  }
  
  if (type == "compare" & length(sx$algorithms) == 1) {
    type <- "errorbar"
    warning("Only 1 algorithm detected, switching to errorbar plot")
  }
  
  if (type == "compare" & uncalibrated) {
    warning("The uncalibrated option is not available if type == 'compare'.")
  }
  
  csmf <- sx$pcalib_postsumm
  csmf$"estimate" = "calibrated"
  fill_col = fill
  if (uncalibrated & type != "compare") {
    df_uncal <- stats::reshape(sx$uncalibrated, varying = algorithm,
                               direction = "long", v.names = "mean",
                               times = algorithm, timevar = "algorithm")
    row.names(df_uncal) = NULL
    df_uncal$"estimate" = "uncalibrated"
    df_uncal <- df_uncal[, names(df_uncal) != "id"]
    df_uncal$"lower" <- df_uncal$"upper" <- NA
  }
  out <- list()
  if (type != "compare") {
    for (alg in algorithm) {
      sub_csmf <- csmf[csmf$algorithm == alg,]
      sub_csmf_order <- order(sub_csmf$mean, decreasing = TRUE)
      sub_csmf <- sub_csmf[sub_csmf_order,]
      n_top <- min(dim(sub_csmf), top)
      sub_csmf <- sub_csmf[1:n_top, ]
      cause_order <- sub_csmf$cause
      sub_csmf$"cause" <- factor(sub_csmf$cause, levels = cause_order)
      if (uncalibrated) {
        sub_csmf_uncal <- df_uncal[df_uncal$algorithm == alg,]
        sub_csmf_uncal <- sub_csmf_uncal[sub_csmf_uncal$cause %in% sub_csmf$cause,]
        sub_csmf_uncal$"cause" <- factor(sub_csmf_uncal$cause, levels = cause_order)
        sub_csmf <- rbind(sub_csmf, sub_csmf_uncal)
        fill_col <- c(fill, fill_uncalibrated)
        if (horiz) {
          sub_csmf$estimate <- factor(sub_csmf$estimate,
                                      levels = c("uncalibrated", "calibrated"))
          #fill_col <- c(fill, fill_uncalibrated)
        }
      }
      
      g <- ggplot(sub_csmf, aes(x = .data$cause,
                                y = .data$mean,
                                group = .data$estimate))
      if (horiz) {
        g <- g + ggplot2::scale_x_discrete(limits=rev)
      }
      
      if (type == "bar") {
        g <- g + geom_bar(aes(fill = .data$estimate),
                          stat = "identity", color = border,
                          linewidth = .3, position = position_dodge(.9)) 
        if (horiz) {
          g <- g + 
            scale_fill_manual(values = c("calibrated" = fill,
                                         "uncalibrated" = fill_uncalibrated),
                              guide = ggplot2::guide_legend(reverse = TRUE))
        } else {
          g <- g +
            scale_fill_manual(values = c("calibrated" = fill,
                                         "uncalibrated" = fill_uncalibrated))
        }
      }
      if (type == "errorbar"){
        g <- g + ggplot2::geom_point(stat = "identity", size = point_size,
                                     aes(group = .data$estimate,
                                         shape = .data$estimate),
                                     position = position_dodge(.9))
        if (horiz) {
          g <- g +
            ggplot2::scale_shape_manual(values = c("calibrated" = 16,
                                                   "uncalibrated" = 17),
                                        guide = ggplot2::guide_legend(reverse = TRUE))
        }
      }
      g <- g + geom_errorbar(aes(ymin = .data$lower,
                                 ymax = .data$upper,
                                 group = .data$estimate),
                             linewidth = err_size,
                             width = err_width, position = position_dodge(.9))
      g <- g + xlab(xlab) + ylab(ylab) + ggtitle(title)
      if (horiz) g <- g + coord_flip()
      if (bw) g <- g + theme_bw()
      if (!horiz) {
        g <- g + theme(axis.text.x = element_text(angle = angle, hjust = 1))
      }
      if (!uncalibrated) g <- g + theme(legend.position = "none")
      out[[alg]] <- g
    }
  }
  if (type == "compare") {
    list_csmf <- split(csmf, factor(csmf$algorithm))
    list_orded_cods <- lapply(list_csmf, 
                              function(x) x$cause[order(x$mean, 
                                                        decreasing = TRUE)])
    
    sub_csmf <- csmf[csmf$algorithm == algorithm[1], ]
    n_top <- min(dim(sub_csmf), top)
    list_top_cod <- lapply(list_orded_cods, function(x) x[1:n_top])
    top_causes <- unique(unlist(list_top_cod))
    sub_csmf <- sub_csmf[sub_csmf$cause %in% top_causes, ]
    sub_csmf_order <- order(sub_csmf$mean, decreasing = TRUE)
    cause_order <- sub_csmf$cause[sub_csmf_order]
    csmf$cause <- factor(csmf$cause, levels = cause_order)
    if (horiz) {
      csmf$algorithm <- factor(csmf$algorithm, levels = rev(algorithm))
    } else {
      csmf$algorithm <- factor(csmf$algorithm, levels = algorithm)
    }
    
    g <- ggplot(csmf, aes(x = .data$cause,
                          y = .data$mean,
                          fill = .data$algorithm,
                          ymax = max(.data$upper)*1.05))
    g <- g + ggplot2::geom_point(aes(color = .data$algorithm), 
                                 position = position_dodge(0.5),
                                 size = point_size)
    g <- g + geom_errorbar(aes(ymin = .data$lower,
                               ymax = .data$upper,
                               color = .data$algorithm),
                           linewidth = err_size, width = err_width,
                           position = position_dodge(0.5))
    g <- g + xlab(xlab) + ylab(ylab) + ggtitle(title)
    g <- g + ggplot2::scale_y_continuous()
    if (horiz) {
      g <- g + ggplot2::scale_x_discrete(limits=rev) + coord_flip()
    }
    if (bw) g <- g + theme_bw()
    cbp <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
             "#D55E00", "#CC79A7")
    maxn <- length(unique(csmf$algorithm))
    if(maxn > length(cbp)){
      cbp <- colorRampPalette(cbp)(maxn)
    }
    if (horiz) {
      cbp <- rev(cbp[1:maxn])
      g <- g + scale_color_manual(values = cbp,
                                  guide = ggplot2::guide_legend(reverse = TRUE))
    } else {
      g <- g + scale_color_manual(values = cbp) + 
        theme(axis.text.x = element_text(angle = angle, hjust = 1))
    }
    # if(!horiz) {
    #   g <- g + theme(axis.text.x = element_text(angle = angle, hjust = 1))
    # }
    out[["compare"]] <- g
  }
  
  if (plot_it) {
    out[[1]]
  }
  return(out)
}

# CODs for EAVA
# neonate.cod <- c("NNT", "Malformation", "Intrapartum",
#                  "Preterm", "Meningitis", "Diarrhea", "Pneumonia",
#                  "Pneumonia", "Sepsis", "Other", "Unspecified")
# 
# child.cod <- c("Intrapartum", "Malformation", "Preterm", "Injury",
#                "AIDS", "Measles", "Meningitis/Encephalitis",
#                "Diarrhea/Dysentery", "Other infections", "Pneumonia",
#                "Malaria", "Unspecified")
