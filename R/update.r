
#' Update openVA packages
#'
#' This will check to see if all openVA packages (and optionally, their
#' dependencies) are up-to-date, and will install after an interactive
#' confirmation.
#' @family package status 
#' @importFrom utils available.packages
#' @importFrom utils compareVersion
#' @importFrom tools package_dependencies
#' @importFrom utils packageVersion
#' @importFrom cli rule
#' @importFrom cli symbol
#' @importFrom crayon green 
#' @importFrom crayon blue 
#' @importFrom crayon red 
#' @importFrom crayon bold 
#' @importFrom crayon col_nchar 
#' @importFrom crayon col_align 
#'  
#' @export
#' @examples
#' \dontrun{
#' openVA_update()
#' }
openVA_update <- function() {

  pkgs <- utils::available.packages()
  deps <- tools::package_dependencies("openVA", pkgs)
  pkg_deps <- unique(sort(unlist(deps)))
  pkg_deps <- pkg_deps[pkg_deps %in% pkgs[, 1]]
  cran_version <- lapply(pkgs[pkg_deps, "Version"], base::package_version)
  local_version <- lapply(pkg_deps, utils::packageVersion)
  behind <- rep(FALSE, length(local_version))
  for(i in 1:length(local_version)){
    behind[i] <- utils::compareVersion(as.character(local_version[[i]]), as.character(cran_version[[i]])) < 0 
  }
  behind <- behind[behind == TRUE]

  if (sum(behind) == 0) {
    message("All required openVA packages up-to-date. Run openVA_status() for a complete list of suggested packages.")
    return(invisible())
  }

  # cli::cat_line("The following packages are out of date:")
  # cli::cat_line()
  # cli::cat_bullet(format(behind$package), " (", behind$local, " -> ", behind$cran, ")")

  message("Start a clean R session then run:")
  need <- names(cran_version)[behind]
  for(i in 1:length(need)) need[i] <- paste0("'", need[i], "'")
  pkg_str <- paste0(need, collapse = ", ")
  message(paste0("install.packages(c(", pkg_str, "))"))

  invisible()
}



#' Check openVA packages status
#'
#' This will print the current versions of all openVA packages (and optionally, their
#' dependencies) are up-to-date, and will install after an interactive
#' confirmation.
#' @family package status 
#'  
#' @export
#' @examples
#' \dontrun{
#' openVA_status()
#' }
openVA_status <- function() {

  core <- c("InSilicoVA", "InterVA4", "InterVA5", "Tariff", "nbc4va",
            "vacalibration", "EAVA")

  core_loaded <- function() {
    search <- paste0("package:", core)
    core[search %in% search()]
  }
  core_unloaded <- function() {
    search <- paste0("package:", core)
    core[!search %in% search()]
  }
  to_load <- core_unloaded()
  loaded <- core_loaded()
  
  message(
    cli::rule(
      center = paste0("Attached packages for openVA ", packageVersion("openVA")), line_col="green"
    )  
  )

  versions <- unlist(lapply(loaded, function(x){as.character(packageVersion(x))}))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(loaded)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  message(paste(packages, collapse = "\n"))

if(length(to_load) > 0){
  message(
    cli::rule(
      center = paste0("Packages not attached for openVA ", packageVersion("openVA")), line_col="green"
    )  
  )

  versions <- unlist(lapply(to_load, function(x){as.character(packageVersion(x))}))
  packages <- paste0(
    crayon::red(cli::symbol$times), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )
  message(paste(packages, collapse = "\n"))
  }
  invisible()
}