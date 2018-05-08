#' Running automated method on VA data
#'
#' @param data Input VA data, see \code{data.type} below for more information about the format.
#' @param data.type There are four data input types currently supported by \code{codeVA} function as below. 
#'\itemize{
#' \item{\code{WHO2012}: }
#'   {InterVA-4 input format using WHO 2012 questionnaire. For example see \code{data(RandomVA1)}. The first column should be death ID.}
#' \item{\code{WHO2016}: }
#'    {InterVA-5 input format using WHO 2016 questionnaire. For example see \code{data(RandomVA5)}. The first column should be death ID.} 
#' \item{\code{PHMRC}: }{PHMRC data format. For example see \code{\link{ConvertData.phmrc}}} 
#' \item{\code{customized}: }{Any dichotomized dataset with ``Y`` denote ``presence'', ``'' denote ``absence'', and ``.'' denote ``missing''. The first column should be death ID.}}
#' @param data.train Training data with the same columns as \code{data}, except for an additional column specifying cause-of-death label. It is not used if \code{data.type} is ``WHO'' and \code{model} is ``InterVA'' or ``InSilicoVA''.  The first column also has to be death ID for ``WHO'' and ``customized'' types.
#' @param causes.train the column name of the cause-of-death assignment label in training data.
#' @param causes.table list of causes to consider in the training data. Default to be NULL, which uses all the causes present in the training data.
#' @param model Currently support four models: ``InSilicoVA'', ``InterVA'', ``Tariff'', and ``NBC''.
#' @param Nchain Parameter specific to ``InSilicoVA'' model. Currently not used.
#' @param Nsim Parameter specific to ``InSilicoVA'' model. Number of iterations to run the sampler.
#' @param version Parameter specific to ``InterVA'' model. Currently supports ``4.02'', ``4.03'', and ``5.0''. For InterVA-4, ``4.03'' is strongly recommended as it fixes several major bugs in ``4.02'' version. ``4.02'' is only included for backward compatibility. ``5.0'' version implements the InterVA-5 model, which requires different data input format.
#' @param HIV Parameter specific to ``InterVA'' model. HIV prevalence level, can take values ``h'' (high), ``l'' (low), and ``v'' (very low).
#' @param Malaria HIV Parameter specific to ``InterVA'' model. Malaria prevalence level, can take values ``h'' (high), ``l'' (low), and ``v'' (very low). 
#' @param phmrc.type Which PHMRC data format is used. Currently supports only ``adult'' and ``child'', ``neonate'' will be supported in the next release.
#' @param convert.type type of data conversion when calculating conditional probability (probability of each symptom given each cause of death) for InterVA and InSilicoVA models. Both ``quantile'' and ``fixed'' usually give similar results empirically. 
#' \itemize{
#' \item{\code{quantile}: }{the rankings of the P(S|C) are obtained by matching the same quantile distributions in the default InterVA P(S|C)}\item{\code{fixed}: }{P(S|C) are matched to the closest values in the default InterVA P(S|C) table.} \item{\code{empirical}: }{no ranking is calculated, but use the empirical conditional probabilities directly, which will force \code{updateCondProb} to be FALSE for InSilicoVA algorithm.}  
#'}
#' @param ... other arguments passed to \code{\link[InSilicoVA]{insilico}}, \code{\link[InterVA4]{InterVA}}, \code{\link{interVA.train}}, \code{\link[Tariff]{tariff}}, and \code{\link[nbc4va]{nbc}}. See respective package documents for details. 
#'
#' @return a fitted object
#' @export codeVA
#' @seealso \code{\link[InSilicoVA]{insilico}}, \code{\link[InterVA4]{InterVA}}, \code{\link{interVA.train}}, \code{\link[Tariff]{tariff}}, and \code{\link[nbc4va]{nbc}}.
#' @import nbc4va 
#' @import InSilicoVA 
#' @import InterVA4  
#' @import InterVA5 
#' @import Tariff
#' @importFrom graphics plot
#' @importFrom stats aggregate median quantile reorder
#' @importFrom utils data
#' 
#' @references Tyler H. McCormick, Zehang R. Li, Clara Calvert, Amelia C.
#' Crampin, Kathleen Kahn and Samuel J. Clark (2016) \emph{Probabilistic
#' cause-of-death assignment using verbal autopsies.}
#' \url{http://arxiv.org/abs/1411.3042}, \emph{To appear, Journal of the American Statistical Association}
#' @references James, S. L., Flaxman, A. D., Murray, C. J., & Population Health Metrics Research Consortium. (2011). \emph{Performance of the Tariff Method: validation of a simple additive algorithm for analysis of verbal autopsies.} \emph{Population Health Metrics, 9(1), 1-16.}
#' @references Zehang R. Li, Tyler H. McCormick, Samuel J. Clark (2014) \emph{InterVA4: An R package to analyze verbal autopsy data.} \emph{Center for Statistics and the Social Sciences Working Paper, No.146}
#' @references http://www.interva.net/
#' @references Miasnikof P, Giannakeas V, Gomes M, Aleksandrowicz L, Shestopaloff AY, Alam D, Tollman S, Samarikhalaj, Jha P. \emph{Naive Bayes classifiers for verbal autopsies: comparison to physician-based classification for 21,000 child and adult deaths.} \emph{BMC Medicine. 2015;13:286.} 
#' @keywords InSilicoVA InterVA4 Tariff NBC4VA

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
#'                data.train = train, causes.train = "cause",
#'                version = "4.02", HIV = "h", Malaria = "l")
#'
#' fit3 <- codeVA(data = test, data.type = "customize", model = "Tariff",
#'                data.train = train, causes.train = "cause", 
#'                nboot.sig = 100)
#'
#' fit4 <- codeVA(data = test, data.type = "customize", model = "NBC",
#'                data.train = train, causes.train = "cause", known.nbc = TRUE)
#'
#' }


codeVA <- function(data, data.type = c("WHO2012", "WHO2016", "PHMRC", "customize")[1], 
                  data.train = NULL, 
                  causes.train = NULL, 
                  causes.table = NULL,
                  model = c("InSilicoVA", "InterVA", "Tariff", "NBC")[1],
                  Nchain = 1, Nsim=10000, 
                  version = c("4.02", "4.03", "5.0")[2], HIV = "h", Malaria = "h", 
                  phmrc.type = c("adult", "child", "neonate")[1], 
                  convert.type = c("quantile", "fixed", "empirical")[1],
                  ...){

  args <- as.list(match.call())
  # --------------------------------------------------------------------#
  # backward compatibility
  # --------------------------------------------------------------------#
  if(data.type == "WHO"){
      data.type <- "WHO2012"
      warning("The argument data.type of 'WHO' is no longer in use. 'WHO2012' or 'WHO2016' needs to be specified. Default change to 'WHO2012' for backward compatibility.\n", immediate.=TRUE)
      args$data.type <- "WHO2012"
  }

  

  # --------------------------------------------------------------------#
  # check data input 
  # --------------------------------------------------------------------#
  if(data.type == "WHO2016" & model == "InterVA" & version != "5.0"){
    stop("WHO2016 type input does not works with InterVA 4.02 or 4.03. Consider switching to 5.0")
  }
  if(data.type == "WHO2012" & model == "InterVA" & version == "5.0"){
    stop("WHO2012 type input does not works with InterVA 5.0. Consider switching to 4.03")
  }
  
  if(data.type %in% c("WHO2012", "WHO2016") && 
     (model == "Tariff" || model == "NBC")) {
    if(is.null(data.train) || is.null(causes.train)){
      stop("Error: need training data for WHO questionnaire input with Tariff or NBC method.")
    }
  }
  if(data.type == "customize"){
    if(is.null(data.train) || is.null(causes.train)){
      stop("Error: need training data for customized input.")
    }
  }

  if(data.type == "PHMRC"){
    if(is.null(data.train) ){
      stop("Error: need training data for PHMRC data, possible training data could be obtained at http://ghdx.healthdata.org/record/population-health-metrics-research-consortium-gold-standard-verbal-autopsy-data-2005-2011")
    }
    if(is.null(causes.train)){
      stop("Error: please specify which column is the cause-of-death in PHMRC input")
    }
    binary <- ConvertData.phmrc(input = data.train, 
                                input.test = data, 
                                cause = causes.train, ...)
    data.train <- binary$output
    data <- binary$output.test
    causes.train <- colnames(data.train)[2]
  }
 
 # --------------------------------------------------------------------#
 #                          InSilicoVA 
 # --------------------------------------------------------------------#
  if(model == "InSilicoVA"){
    if(is.null(Nsim)){
      stop("Please specify Nsim: number of iterations to draw from InSilicoVA sampler")
    }


    if(is.null(args$burnin)){
      args$burnin <- round(Nsim / 2)
    }
    if(is.null(args$thin)){
      args$thin <- 10 * (Nsim < 10000) + 10 *(Nsim >= 10000)
    }


    if(data.type %in% c("WHO2012", "WHO2016")){
      fit <- do.call("insilico", pairlist(args)[[1]][-1])

    }else if(data.type == 'PHMRC'|| data.type == "customize"){
      
      args$data <- as.name("data")
      args$train <- as.name("data.train")
      args$cause <- as.name("causes.train")
      args$type <- convert.type      

      fit <- do.call("insilico.train", pairlist(args)[[1]][-1])
  
    }else{
      stop("Error: unknown data type specified")
    }
  
  # --------------------------------------------------------------------#
  #                          InterVA 
  # --------------------------------------------------------------------#
  }else if(model == "InterVA"){
    

    if(version == "4.02"){
      replicate = TRUE
    }else{
      replicate = FALSE
    }
    
    if(data.type == "WHO2012"){
        # to avoid writing to file every time
        if(is.null(args$write)){
          write <- FALSE
        }
        fit <- InterVA(Input = data, HIV = HIV, Malaria = Malaria, replicate = replicate, 
                       write = write, ...)
    }else if(data.type == "WHO2016"){
        # to avoid writing to file every time
        if(is.null(args$write)){
          write <- FALSE
        }
        # for interVA to recognize the syntax
        for(i in 1:dim(data)[2]){                
          data[, i] <- as.character(data[, i])
          data[, i][data[, i] == ""] <- "n"
        }
        # check the input variables
        tmp <-  tolower(as.character(as.matrix(data[,-1])))
        if(sum(tmp %in% c("y", "n", "-", ".")) < length(tmp)){
          stop("InterVA5 input data contains values other than 'y', 'n', '.', or '-'. Please check your input, especially for extra space characters in the cells, or standardize how missing data is coded.")
        }
        fit <- InterVA5(Input = data, HIV = HIV, Malaria = Malaria, write = write, ...)
    }else if(data.type == 'PHMRC'|| data.type == "customize"){
        fit <- interVA.train(data = data, 
                             train=data.train, 
                             causes.train = causes.train, 
                             causes.table = causes.table, 
                             type = convert.type,
                             ...)
                       
    }else{
      stop("Error: unknown data type specified")
    }
        
    
  # --------------------------------------------------------------------#
  #                          Tariff 
  # --------------------------------------------------------------------#
  }else if(model == "Tariff"){
   if(data.type == "WHO2016"){
      data <- ConvertData(data, yesLabel = c("y", "Y"), noLabel = c("n", "N"), missLabel = c("-"))
    }
    if(data.type %in% c("WHO2012", "WHO2016")){
      fit <- tariff(causes.train = causes.train, 
                    symps.train = data.train, 
                    symps.test = data, 
                    causes.table = NULL,
                    ...)
      
    }else if(data.type == 'PHMRC'|| data.type == "customize"){
      fit <- tariff(causes.train = causes.train, 
                    symps.train = data.train, 
                    symps.test = data, 
                    causes.table = NULL,
                    ...)
    }else{
      stop("Error: unknown data type specified")
    }

  # --------------------------------------------------------------------#
  #                          NBC 
  # --------------------------------------------------------------------#
  }else if(model == "NBC"){
    #  # make sure the second column is cause     
    # if(which(colnames(data.train) == causes.train) != 2){
    #   k <- which(colnames(data.train) == causes.train)
    #   rest <- (1:dim(data.train)[2])[-c(1, k)]
    #   data.train <- data.train[, c(1, k, rest)]
    # }
    
    # # make sure the second column is cause     
    # if(causes.train %in% colnames(data) == FALSE){
    #   # it is handled
    # }else if(which(colnames(data) == causes.train) != 2){
    #   k <- which(colnames(data) == causes.train)
    #   rest <- (1:dim(data)[2])[-c(1, k)]
    #   data <- data[, c(1, k, rest)]
    # }

    # data.train[data.train == "Y"] <- 1
    # data.train[data.train == ""] <- 0    
    # data.train[data.train == "."] <- 0
    # data[data == "Y"] <- 1
    # data[data == ""] <- 0    
    # data[data == "."] <- 0
    # for(i in 3:dim(data.train)[2]){
    #   data.train[, i] <- as.numeric(data.train[, i])
    # }
    # for(i in 2:dim(data)[2]){
    #   if(colnames(data)[i] != causes.train) data[, i] <- as.numeric(data[, i])
    # }
    if(data.type == "WHO2016"){
      data <- ConvertData(data, yesLabel = c("y", "Y"), noLabel = c("n", "N"), missLabel = c("-"))
    }
    data.train[, 1] <- as.character(data.train[, 1])
    data[, 1] <- as.character(data[, 1])
    # fit <- nbc(train = data.train, 
    #            test = data, 
    #            known = known.nbc)

    # update with NBC's official wrapper function
    fit <- ova2nbc(data.train, data, causes.train)

    # fix the caseID bug of NBC
    if(colnames(fit$prob)[1] != "CaseID"){
      temp <- data.frame(CaseID = fit$test.ids)
      fit$prob <- cbind(temp, fit$prob)
    }
    for(i in 1:dim(fit$prob)[1]){
      if(sum(fit$prob[i, -1]) > 0){
        fit$prob[i, -1] <- fit$prob[i, -1] / sum(fit$prob[i, -1])
      }
    }
  }else{
        stop("Error, unknown model specification")
  }

  return(fit)
}


#' Plot top CSMF for a fitted model
#'
#' @param object a fitted object using \code{\link{codeVA}} 
#' @param top number of top causes to plot
#' @param title title of the plot
#' @param ... additional arguments passed to \code{\link[InSilicoVA]{plot.insilico}}, 
#' \code{\link[Tariff]{plot.tariff}}, \code{\link[InterVA4]{CSMF}}, or \code{\link[nbc4va]{plot.nbc}}.
#'
#' @export plotVA
#' @seealso \code{\link[InSilicoVA]{plot.insilico}}, \code{\link[Tariff]{plot.tariff}}, 
#' \code{\link[InterVA4]{CSMF}}
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
#'                data.train = train, causes.train = "cause",
#'                version = "4.02", HIV = "h", Malaria = "l")
#'
#' fit3 <- codeVA(data = test, data.type = "customize", model = "Tariff",
#'                data.train = train, causes.train = "cause", 
#'                nboot.sig = 100)
#'
#' fit4 <- codeVA(data = test, data.type = "customize", model = "NBC",
#'                data.train = train, causes.train = "cause", known.nbc = TRUE)
#'
#' plotVA(fit1)
#' plotVA(fit2)
#' plotVA(fit3)
#' plotVA(fit4)
#' }
plotVA <- function(object, top = 10, title = NULL, ...){
  if(class(object) == "interVA"){
    csmf <- CSMF(object, top.plot = top, main = title, ...)
  }else if(class(object) == "interVA5"){
    csmf <- CSMF5(object, top.plot = top, main = title, ...)
  }else if(class(object) == "tariff"){
    plot(object, top = top, main = title, ...)
  }else if(class(object) == "insilico"){
    plot(object, top = top, title = title, ...)
  }else if(class(object) == "nbc"){
    plot(object, top.plot = top, main = title, ...)
  }else{
    stop("Unknown object to plot")
  }
}