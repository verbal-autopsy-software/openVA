#' Print method for "eava" class.
#'
#' @param x \code{eava} object.
#'
#' @exportS3Method EAVA::print
#'
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
#'
#' @exportS3Method EAVA::summary
#'
summary.eava <- function(x, top = 5, rnd = 4) {
  cat(paste("EAVA fitted on ", length(x$ID), x$age_group, "deaths.\n"),
      paste("Top", top, "CSMFs:\n\n"))
  csmf <- csmf_eava(x)
  csmf_ordered <- csmf[order(csmf, decreasing = TRUE)]
  n_top <- min(length(csmf), top)
  out <- data.frame("cause" = names(csmf_ordered)[1:n_top],
                    "proportion" = round(csmf_ordered[1:n_top], rnd))
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
#'
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

# neonate.cod <- c("NNT", "Malformation", "Intrapartum",
#                  "Preterm", "Meningitis", "Diarrhea", "Pneumonia",
#                  "Pneumonia", "Sepsis", "Other", "Unspecified")
# 
# child.cod <- c("Intrapartum", "Malformation", "Preterm", "Injury",
#                "AIDS", "Measles", "Meningitis/Encephalitis",
#                "Diarrhea/Dysentery", "Other infections", "Pneumonia",
#                "Malaria", "Unspecified")
