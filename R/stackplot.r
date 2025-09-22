#' plot grouped CSMF from a "insilico" object
#' 
#' Produce bar plot of the CSMFs for a fitted object in broader groups. This function extends the stackplot() function in the InSilicoVA package to allow for the same visualization for results from InterVA, NBC, and Tariff algorithms.
#' 
#' 
#' @param x one or a list of fitted object from \code{codeVA} function
#' @param grouping C by 2 matrix of grouping rule. If set to NULL, make it default.
#' @param type type of the plot to make
#' @param group_order list of grouped categories. If set to NULL, make it default.
#' @param err indicator of inclusion of error bars
#' @param CI Level of posterior credible intervals.
#' @param sample_size_print Logical indicator for printing also the sample size for each sub-population labels.
#' @param xlab Labels for the causes.
#' @param ylab Labels for the CSMF values.
#' @param ylim Range of y-axis.
#' @param title Title of the plot.
#' @param horiz Logical indicator indicating if the bars are plotted
#' horizontally.
#' @param angle Angle of rotation for the texts on x axis when \code{horiz} is
#' set to FALSE
#' @param border The color for the border of the bars.
#' @param err_width Size of the error bars.
#' @param err_size Thickness of the error bar lines.
#' @param bw Logical indicator for setting the theme of the plots to be black
#' and white.
#' @param filter_legend Logical indicator for including all broad causes in the plot legend (default; FALSE) or filtering to only the broad causes in the data being plotted
#' @param \dots Not used.
#' @author Zehang Li, Tyler McCormick, Sam Clark
#' 
#' Maintainer: Zehang Li <lizehang@@uw.edu>
#' @importFrom ggplot2 ggplot aes geom_bar position_dodge geom_errorbar ggtitle coord_flip theme_bw theme element_text scale_fill_grey scale_color_manual scale_fill_manual
#' @importFrom grDevices colorRampPalette
#' @family visualization
#' @examples 
#' \donttest{
#' data(RandomVA3)
#' test <- RandomVA3[1:200, ]
#' train <- RandomVA3[201:400, ]
#' fit1 <- codeVA(data = test, data.type = "customize", model = "InSilicoVA",
#'                     data.train = train, causes.train = "cause",
#'                     Nsim=1000, auto.length = FALSE)
#'
#' fit2 <- codeVA(data = test, data.type = "customize", model = "InterVA",
#'                data.train = train, causes.train = "cause", write=FALSE,
#'                version = "4.02", HIV = "h", Malaria = "l")
#'
#' fit3 <- codeVA(data = test, data.type = "customize", model = "Tariff",
#'                data.train = train, causes.train = "cause", 
#'                nboot.sig = 100)
#'
#' data(SampleCategory)
#' stackplotVA(fit1, grouping = SampleCategory, type ="dodge", 
#'             ylim = c(0, 1), title = "InSilicoVA")
#' stackplotVA(fit2, grouping = SampleCategory, type = "dodge", 
#'             ylim = c(0, 1), title = "InterVA4.02")
#' stackplotVA(fit3, grouping = SampleCategory, type = "dodge", 
#'             ylim = c(0, 1), title = "Tariff")
#' }
#' @export stackplotVA
stackplotVA <- function(x, grouping = NULL,
                        type = c("stack", "dodge")[1], 
                        group_order = NULL, err = TRUE,
                        CI = 0.95, sample_size_print = FALSE,
                        xlab = "", ylab = "CSMF", ylim = NULL,
                        title = "CSMF by broader cause categories", 
                        horiz = FALSE, angle = 0,  
                        err_width = .4, err_size = .6, 
                        border = "black", bw = FALSE, filter_legend = FALSE, ...) {
  
  # Check that user-provided arguments are usable
  
  # Error if invalid type given
  if(!(type %in% c("stack", "dodge"))) {
    stop("Invalid 'type' specified, must be either 'stack' or 'dodge'")
  }
  
  # Error if invalid numbers for error bars
  if(err_width < 0) stop("err_width must be non-negative and finite")
  if(err_size < 0) stop("err_size must be non-negative and finite")
  
  
  # Default grouping if none specified
  if(is.null(grouping)) {
    data("SampleCategory", envir = environment())
    SampleCategory <- get("SampleCategory", envir  = environment())
    grouping <- SampleCategory
  }
  grouping[] <- lapply(grouping, as.character) # convert the grouping dataframe to all character
  
  # assign grouping names
  if(length(unique(grouping[, 1]))>length(unique(grouping[, 2]))) {
    names(grouping) <- c("cod", "broad_cause")
  } else {
    names(grouping) <- c("broad_cause", "cod")
  }
  
  # If no order specified, order taken from grouping provided
  if(is.null(group_order)) {
    group_order <- unique(grouping$broad_cause)
  } else if (!setequal(group_order, unique(grouping$broad_cause))){
    # make sure that group_order agrees with the grouping; if not, warn and replace
    warning("User-specified group_order does not encompass what is contained in the grouping specification. Replacing with the default.")
    group_order <- unique(grouping$broad_cause)
  }
  
  csmf <- NULL
  
  if(!methods::is(x, "list")) {
    x <- list(x)
  }
  n <- length(x)
  counts <- rep(0, n)
  
  # Collects info from each given model for CSMF calculation
  for(i in 1:n) {
    if(methods::is(x[[i]],  "insilico")) {
      if(is.null(x[[i]]$subpop)) {
        # for InSilicoVA, since the error bar needs to be calculated
        # based on the grouping, need the raw CSMF here
        # instead of summarized version as others
        csmf[[i]] <- x[[i]]$csmf
        counts[i] <- length(x[[i]]$id)
        
        present_cod <- colnames(csmf[[i]])
        
      } else if(n == 1){
        g <- InSilicoVA::stackplot(x[[i]], grouping = grouping,
                        type = type, 
                        order.group = group_order, err = err,
                        CI = CI, sample_size_print = sample_size_print,
                        xlab = xlab, ylab = ylab, ylim = ylim,
                        title = title,  horiz = horiz, angle = angle,  
                        err_width = err_width, err_size = err_size, 
                        border = border, bw = bw, filter_legend = filter_legend, ...)
        return(g)
       }else{ 
        stop("Sub-population specification exists in InSilicoVA fit,
             # please rerun with only the InSilicoVA object\n")
      }
    }
    else {
      csmf[[i]] <- getCSMF(x[[i]], CI = CI)
      if(methods::is(x[[i]], "interVA") || methods::is(x[[i]], "interVA5") ) {
        counts[i] <- length(x[[i]]$VA) 
      } else if(methods::is(x[[i]], "tariff")) {
        counts[i] <- dim(x[[i]]$causes.test)[1]
      } else if(methods::is(x[[i]],  "nbc")) {
        counts[i] <- dim(x[[i]]$test)[1]
      } else if(methods::is(x[[i]], "eava")) {
        counts[i] <- length(x[[i]]$ID)
      }
      
      present_cod <- names(csmf[[i]])[csmf[[i]]>0]
      
    }
    
    # if broad causes are missing, add them in
    if (!identical(setdiff(present_cod, grouping$cod), character(0))){
      warning("Causes exist in the CSMF that are not specified in the grouping. Automatically carrying through missed CODs.")
      
      miss_bc <- present_cod[!present_cod %in% grouping$cod]
      group_order <- unique(c(group_order, miss_bc))
      
      miss_bc_df <- data.frame(miss_bc, miss_bc)
      names(miss_bc_df) <- names(grouping)
      grouping <- rbind(grouping, miss_bc_df)
    }
    
    if(!is.null(names(x[i])[i])) {
      names(csmf)[i] <- names(x)[i]
    } else {
      names(csmf)[i] <- paste("Input", i)
    }
    
    }
  
  if(length(x)>1) {
    if(is.null(names(x))){
      barNames <- paste("model", 1:length(x))
    }else{
      barNames <- names(x)
    }
  } else {
    barNames <- ""
  }
  
  csmf_group <- NULL
  group <- NULL
  label <- NULL
  low <- (1 - CI) / 2
  high <- 1 - low
  
  # Calculates grouped CSMF for each given model
  for(index in 1:length(csmf)) {
    
    if(methods::is(x[[index]], "insilico")) {
      this_csmf <- t(as.data.frame(csmf[[index]]))
      grouped_sums <- merge(this_csmf, grouping, by.x = "row.names",
                            by.y = colnames(grouping)[1], all.x = TRUE)
      grouped_sums <- merge(as.data.frame(group_order), grouped_sums, 
                            by.x = "group_order", 
                            by.y = colnames(grouped_sums)[ncol(grouped_sums)], all.x = TRUE)
      grouped_sums <- apply(grouped_sums[, 3:ncol(grouped_sums)], 2, function(x) 
        tapply(x, grouped_sums[, 1], sum))
      
      # handle the case where there is one broad cause; only need to order if there is more than one
      if (length(unique(grouping$broad_cause))==1){
        grouped_sums <- t(as.matrix(grouped_sums))
        rownames(grouped_sums) <- unique(grouping$broad_cause)
      } else {
        grouped_sums <- grouped_sums[order(match(rownames(grouped_sums), group_order)),]
      }
      
      grouped_sums[is.na(grouped_sums)] <- 0
      csmf_mean <- apply(grouped_sums, 1, mean)
      csmf_lower <- apply(grouped_sums, 1, function(x) {quantile(x, low, na.rm = T)})
      csmf_upper <- apply(grouped_sums, 1, function(x) {quantile(x, high, na.rm = T)})
    } else {
      this_csmf<-as.data.frame(as.table(csmf[[index]]))
      grouped_sums <- merge(this_csmf, grouping, by.x = "Var1",
                            by.y = colnames(grouping)[1], all.x = TRUE)
      grouped_sums <- merge(as.data.frame(group_order), grouped_sums, 
                            by.x = "group_order", by.y = colnames(grouped_sums)[3], all.x = TRUE)
      grouped_sums <- tapply(grouped_sums[, 3], grouped_sums[, 1], sum)
      grouped_sums <- grouped_sums[order(match(names(grouped_sums), group_order))]
      csmf_mean <- csmf_lower <- csmf_upper <- ifelse(is.na(grouped_sums), 0, grouped_sums)
    }
      
    csmf_group <- rbind(csmf_group, cbind(csmf_mean, csmf_lower, csmf_upper))
    group <- c(group, group_order)
    if(sample_size_print) {
      label <- c(label, rep(paste(barNames[index], "\n", "n = ", counts[index], sep = ""),
                            length(group_order)))
    } else {
      label <- c(label, rep(barNames[index], length(group_order)))
    }
  }
  
  rownames(csmf_group) <- NULL
  toplot <- data.frame(csmf_group)
  subpop <- Causes <- NULL
  
  if (filter_legend){
    rm <- rowSums(toplot)==0
    toplot <- toplot[!rm, ]
    group_order <- group_order[!rm]
    group <- group[group %in% group_order]
    label <- label[!rm]
  }
  
  if(type == "stack" ) {
    toplot$Causes <- factor(group, levels = rev(group_order))
  } else {
    toplot$Causes <- factor(group, levels = (group_order))
  }
  toplot$subpop <- label
  

  
  # initialize ggplot, force order of bars
  hjust <- NULL
  if(angle != 0) hjust = 1
  if(horiz) {
    g <- ggplot(toplot, aes(x=reorder(subpop, seq(1:length(subpop))),
                            y=csmf_mean, fill=Causes, order = as.numeric(Causes))) +
      theme(axis.text.y = element_text(angle = angle, hjust = hjust)) +
      coord_flip()
  } else {
    g <- ggplot(toplot, aes(x=reorder(subpop, seq(length(subpop):1)),
                            y=csmf_mean, fill=Causes, order = -as.numeric(Causes))) +
      theme(axis.text.x = element_text(angle = angle, hjust = hjust))	
  }
  
  if(type == "stack") {
    g <- g + geom_bar(stat='identity', color=border)
  } else {
    g <- g + geom_bar(stat='identity', color=border, position=position_dodge(0.9))
    if(err) {
      g <- g + geom_errorbar(aes(ymin = csmf_lower, ymax = csmf_upper),
                             linewidth = err_size, width = err_width,  position = position_dodge(0.9))
    }
  }
  
  g <- g + xlab(xlab) + ylab(ylab) + ggtitle(title)
  if(!is.null(ylim)) {
    g <- g + ylim(ylim)
  }
  if(bw){
    g <- g + theme_bw() + scale_fill_grey(start = 0, end = 0.9)
  }else{
    cbp <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    maxn <- length(unique(toplot$Causes))
    if(maxn > length(cbp)){
      cbp <- colorRampPalette(cbp)(maxn)
    }
    g <- g + scale_fill_manual(values = cbp)
  }
  return(g)
  
}
