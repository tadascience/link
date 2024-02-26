#' @importFrom htmltools tags
rx_valid_pkg <- "[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]"
rx_valid_fun <- "([a-zA-Z]\\w*|\\.\\w+)"
rx_pkg  <- glue::glue("[{{]({rx_valid_pkg})[}}]")
rx_call <- glue::glue("({rx_valid_pkg})::({rx_valid_fun})\\(\\)")

get_title <- function(url) {
  httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    xml2::xml_find_all("//title") |>
    xml2::xml_text()
}

#' @rdname auto
#' @export
tip_pkg <- function(pkg, keep_braces = TRUE, text = get_title(url), ...) {
  url <- downlit::href_package(pkg)
  link_text <- if (keep_braces) "{{{pkg}}}" else "{pkg}"
  bslib::tooltip(
    tags$a(glue::glue(link_text), href = url, class = "r-link-pkg"),
    text,
    ...
  )
}

#' @rdname auto
#' @export
tip_call <- function(call, keep_pkg_prefix = TRUE, text = get_title(url), ...) {
  url <- downlit::autolink_url(call)

  link_text <- if (keep_pkg_prefix) {
    call
  } else {
    glue::glue("{fun}()", fun = stringr::str_extract(call, rx_call, group = 2))
  }

  bslib::tooltip(
    tags$a(link_text, href = url, class = "r-link-call"),
    text,
    ...
  )

}

autolink_pkg <- function(x, keep_braces = TRUE) {
  stringr::str_replace_all(
    x, rx_pkg,
    function(pkg) {
      pkg <- stringr::str_extract(pkg, rx_pkg, group = 1)
      as.character(tip_pkg(pkg, keep_braces = keep_braces))
    }
  )
}

autolink_call <- function(x, keep_pkg_prefix = TRUE) {
  stringr::str_replace_all(
    x, rx_call, function(call) {
      as.character(tip_call(call, keep_pkg_prefix = keep_pkg_prefix))
    }
  )
}

#' Setup automatic linking
#'
#' @param keep_braces Should the braces be kept ?
#' @param keep_pkg_prefix Should the package prefix be kept ?
#' @param pkg package name
#' @param call Function call of the form `pkg::fun()`
#' @param text Tooltip text, set to the title of the resolved url by default
#' @param ... See [bslib::tooltip()]

#' @examples
#'
#' tip_pkg("tidyverse", text = "The tidyverse")
#' tip_call("dplyr::summarise()", text = "Summarise each group down to one row")
#'
#' \dontrun{
#'   # auto is mostly meant to be called inside rmarkdown or quarto documents
#'   auto()
#'   auto(keep_braces = FALSE, keep_pkg_prefix = FALSE)
#'
#'   # manually generate the tooltips for {pkg} and pkg::fun()
#'   tip_pkg("tidyverse")
#'   tip_call("dplyr::summarise()")
#' }
#'
#' @export
auto <- function(keep_braces = TRUE, keep_pkg_prefix = TRUE) {
  default_text_hook <- knitr::knit_hooks$get("text")
  knitr::knit_hooks$set(text = function(x) {

    x <- autolink_pkg(x, keep_braces)
    x <- autolink_call(x, keep_pkg_prefix)

    default_text_hook(x)
  })

  # This is a hack because I currently don't know how to inject the
  # dependencies otherwise
  bslib::tooltip("", "")
}
