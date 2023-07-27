#' @title writeLog
#' @description For internal use. Add text to a logger
#' @param logger The logger to write the text to. Can be NULL or a function
#' @param ... Messages to write to the logger
#' @param type One of "default", "error", "warning"
#' @keywords internal
#' @export
writeLog <- function(logger, ..., type = 'default') {
  if (is.null(logger)) {
    if (type == 'error') {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == 'warning') {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else if (is.function(logger)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == 'info') {
      shinyalert::shinyalert(...,
                             type = "info")
      pre <- '> <font color="blue"><b>INFO</b></font> : '
      } else if (type == 'error') {
      shinyalert::shinyalert("Please, check Log window for more information ",
                             type = "error")
      pre <- '> <font color="red"><b>! ERROR</b></font> : '
    } else if (type == 'warning') {
      shinyalert::shinyalert("Please, check Log window for more information ",
                             type = "warning")
      pre <- '> <font color="orange"><b>! WARNING</b></font> : '
    }
    newEntries <- paste0('<br>', pre, ..., collapse = "")
    logger(paste0(logger(), newEntries))
  } else {
    warning("Invalid logger type")
  }
  invisible()
}