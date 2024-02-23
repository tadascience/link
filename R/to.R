link <- function(url, text) {
  glue::glue('<a href="{url}">{text}</a>')
}
#' Create a link to a function or a package
#'
#' @param topic topic
#' @param package package
#'
#' @examples
#' \dontrun{
#'   link::to(dplyr::summarise)
#'   link::to(package = "admiral")
#'   link::to("init", package = "teal")
#' }
#'
#' @importFrom rlang is_string is_call enexpr
#' @importFrom cli cli_abort
#' @export
to <- function(topic, package) {
  if (missing(topic)) {
    if (missing(package) || !is_string(package)) {
      cli_abort("{.arg package} must be a single string")
    }
    link(downlit::href_package(package), package)
  } else {
    if (!missing(package) && is_string(package)) {
      text <- glue::glue("{package}::{topic}()")
      url <- downlit::href_topic(topic, package = package)
      if (is.na(url)) {
        cli_abort("Can't find link for {text}")
      }
      link(url, text)
    } else {
      topic <- enexpr(topic)
      if (!is_call(topic, "::")) {
        cli_abort("{.arg topic} must be of the form <pkg>::<topic>")
      }

      package <- as.character(topic[[2]])
      topic <- as.character(topic[[3]])
      text <- glue::glue("{package}::{topic}()")

      link(downlit::href_topic(topic, package = package), text)
    }
  }
}
