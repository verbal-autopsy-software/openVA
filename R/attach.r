core <- c("InSilicoVA", "InterVA4", "InterVA5", "Tariff")
notcore <- c("nbc4va", "vacalibration", "EAVA")

core_loaded <- function() {
  search <- paste0("package:", core)
  core[search %in% search()]
}
core_unloaded <- function() {
  search <- paste0("package:", core)
  core[!search %in% search()]
}
notcore_loaded <- function() {
  search <- paste0("package:", notcore)
  notcore[search %in% search()]
}
notcore_unloaded <- function() {
  search <- paste0("package:", notcore)
  notcore[!search %in% search()]
}
notcore_installed <- function() {
  notcore[notcore %in% installed.packages()[, "Package"]]
}


openVA_attach <- function() {
  exist <- core_loaded()
  if(length(exist) > 0){
    suppressPackageStartupMessages(
      lapply(paste0("package:", exist), detach, character.only = TRUE)
    )
  }

  to_load <- core_unloaded()
  if (length(to_load) == 0)
    return(invisible())

  packageStartupMessage(
    cli::rule(
      center = paste0("Attaching packages for openVA ", package_version("openVA")), line_col="green"
    )  
  )

  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  packageStartupMessage(paste(packages, collapse = "\n"))

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )


  packageStartupMessage(
    cli::rule(
      left = paste0("Optional packages (require manual installation if not attached)"), line_col="green"
    )  
  )

  notcore_to_load <- notcore_installed()
  if(length(notcore_to_load) > 0){
    suppressPackageStartupMessages(
      lapply(notcore_to_load, library, character.only = TRUE, warn.conflicts = FALSE)
    )
  }
  notcore_has <- notcore_loaded()
  notcore_install <- notcore_unloaded()
  if(length(notcore_has) > 0){
    versions <- vapply(notcore_has, package_version, character(1))
    packages <- paste0(
      crayon::green(cli::symbol$tick), " ", crayon::blue(format(notcore_has)), " ",
      crayon::col_align(versions, max(crayon::col_nchar(versions)))
    )
    packageStartupMessage(paste(packages, collapse = "\n"))
  }
  
  if(length(notcore_install) > 0){
    packages <- paste0(
      crayon::red(cli::symbol$times), " ", crayon::blue(format(notcore_install))
    )
    if ((length(packages) %% 2) != 0) packages <- c(packages, " ")
    col1 <- 1:floor(length(packages)/2)
    info <- paste0(packages[col1], "     ", packages[-col1])
    packageStartupMessage(paste(info, collapse = "\n"))
    packageStartupMessage(paste0("If you need to use these methods, you may need to load or install the packages: ", paste0(notcore_install, collapse=", "), "."))
  }

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}
