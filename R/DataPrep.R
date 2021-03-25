#' Prepare Data for use
#'
#' This function will take a `data.frame` as an argument, and optionally a data
#' dictionary, and compile everything that the program needs to work, or export
#' a csv to the working directory to be filled in and later input
#'
#' @param df asdf
#' @param dd asdf
#' @param varLabel asdf
#' @param dir asdf
#'
#' @return sdofi
#' @export
#'
#' @examples #asdf
dataPrep <- function(df, dd = NULL, varLabel = NULL, dir = getwd()) {
  browser()
  if (is.null(dd)) {
    infraDD <- data.frame(
      variable = unlist(names(df)),
      variableLable = ifelse(is.null(varLabel), NA, varLabel),
      variableType = NA
    )
    header <-
      data.frame(
        c(strwrap(width = 10000, simplify = TRUE,
        "Please fill in the missing information and import into R and use
          as the dd aregument for the dataPrep function"),
        strwrap(width = 10000, simplify = TRUE, 'Variable type Must be one of
        "", "id", "y", "y2", "t", "t2", "e", "e2", "x", NA_character_')),
        NA, NA)
    names(header) <- names(infraDD)
    infraDD <- rbind(header, infraDD)
    utils::write.csv(infraDD, paste0(dir, "/InfraDD.csv"), row.names = FALSE,
                     na = "")
    message(
      paste0("The Infra Data dictionary was exported to ", dir, "/InfraDD.csv.",
             " Once the Infra data dictionary is filled in, read it into R and",
             " provide as the argument of the `dd` parameter of `prepData`"))
    return(invisible(df))
  }
}
