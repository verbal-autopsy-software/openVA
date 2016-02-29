
#' Running automated method on VA data
#'
#' @param data 
#' @param data.type 
#' @param model 
#' @param Nchain 
#' @param Nsim 
#' @param version 
#' @param HIV 
#' @param Malaria 
#' @param physician 
#' @param ... 
#' @param data.train 
#' @param causes.train 
#' @param causes.table 
#' @param phmrc.type 
#'
#' @return fitted object
#' @export codeVA
#'
#' @examples
#' \dontrun{
#'  x <- 1
#' }
#'
#'  
codeVA <- function(data, data.type = c("WHO", "PHMRC", "customize")[1], 
                  data.train = NULL, 
                  causes.train = NULL, 
                  causes.table = NULL,
                  model = c("InSilicoVA", "InterVA", "Tariff")[1],
                  Nchain = 1, Nsim=10000, physician = NULL,
                  version = "4.02", HIV = "h", Malaria = "h", 
                  phmrc.type = c("adult", "child", "neonate")[1], ...){
  
  args <- as.list(match.call())

  # --------------------------------------------------------------------#
  # check data input 
  # --------------------------------------------------------------------#
 
  if(data.type == "WHO" && model == "Tariff"){
    if(is.null(data.train) || is.null(causes.train)){
      stop("Error: need training data for WHO questionnaire input with Tariff method.")
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
                                input.test = test, 
                                cause = causes.train,
                                type = phmrc.type, ...)
    data.train <- data$output
    data <- data$output.test
    causes.train <- colnames(data.train)[2]
  }
 
 # --------------------------------------------------------------------#
 #                          InSilicoVA 
 # --------------------------------------------------------------------#
  if(model == "InSilicoVA"){
    if(is.null(args$burnin)){
      burnin <- round(Nsim / 2)
    }
    if(is.null(args$thin)){
      thin <- 10 * (Nsim <= 10000) + 20 *(Nsim > 10000)
    }  
    
    if(data.type == "WHO"){
      fit <- insilico(data, length.sim = Nsim, burnin = burnin, thin = thin, ...)  
    }else if(data.type == 'PHMRC'|| data.type == "customize"){
      fit <- insilico.train(data,
                          train = data.train, 
                          cause = causes.train,
                          causes.table = causes.table,
                          length.sim = Nsim, 
                          burnin = burnin, 
                          thin = thin, ...)  
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
    
    if(data.type == "WHO"){
        fit <- InterVA(Input = data, HIV = HIV, Malaria = Malaria, replicate = replicate, ...)
    }else if(data.type == 'PHMRC'|| data.type == "customize"){
              
    }else{
      stop("Error: unknown data type specified")
    }
        
    
  # --------------------------------------------------------------------#
  #                          Tariff 
  # --------------------------------------------------------------------#
  }else if(model == "Tariff"){
    if(data.type == "WHO"){
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
  }else{
    stop("Error, unknown model specification")
  }
  
  return(fit)
}


#' Title
#'
#' @param object 
#' @param top 
#' @param title 
#' @param ... 
#'
#' @export plotVA
#'
#' @examples
#' \dontrun{
#'   x <- 1
#' }
plotVA <- function(object, top = 10, title = NULL, ...){
  if(class(object) == "interVA"){
    csmf <- CSMF(object, top.plot = top, main = title, ...)
  }else if(class(object) == "tariff"){
    plot(object, top = top, main = title, ...)
  }else if(class(object) == "insilico"){
    plot(object, top = top, title = title, ...)
  }else{
    stop("Unknown object to plot")
  }
}