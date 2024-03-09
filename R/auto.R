#' @importFrom htmltools tags
rx_valid_pkg <- "[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]"
rx_valid_fun <- "([a-zA-Z]\\w*|\\.\\w+)"
rx_pkg  <- glue::glue("[{{]({rx_valid_pkg})[}}]")
rx_call <- glue::glue("({rx_valid_pkg})::({rx_valid_fun})\\(\\)")

#' Setup automatic linking
#'
#' @param type "plain" for plain links, "tooltip" for adding a tooltip
#' @param keep_braces Should the braces be kept ?
#' @param keep_pkg_prefix Should the package prefix be kept ?
#' @param pkg package name
#' @param call Function call of the form `pkg::fun()`
#' @param ... See [bslib::tooltip()]
#'
#' @return [to_pkg()] and [to_call()] return text to include in html documents
#'
#' @examples
#'
#' \dontrun{
#'   # auto is mostly meant to be called inside rmarkdown or quarto documents
#'   auto()
#'   auto(keep_braces = FALSE, keep_pkg_prefix = FALSE)
#'
#'   # manually generate the tooltips for {pkg} and pkg::fun()
#'   link::to_pkg("tidyverse")
#'   link::to_pkg("tidyverse")
#'
#'   link::to_call("dplyr::summarise()")
#'   link::to_call("dplyr::summarise()")
#' }
#'
#' link::to_pkg("tidyverse", type = "plain")
#' link::to_call("dplyr::summarise()", type = "plain")
#'
#' @export
auto <- function(type = c("tooltip", "plain"), keep_braces = TRUE, keep_pkg_prefix = TRUE) {
  type <- rlang::arg_match(type)
  default_text_hook <- knitr::knit_hooks$get("text")
  knitr::knit_hooks$set(text = function(x) {

    x <- autolink_pkg(x, type = type, keep_braces = keep_braces)
    x <- autolink_call(x, type = type, keep_pkg_prefix = keep_pkg_prefix)

    default_text_hook(x)
  })

  # This is a hack because I currently don't know how to inject the
  # dependencies otherwise
  switch(
    type,
    tooltip = bslib::tooltip("", ""),

    invisible(NULL)
  )
}

get_title <- function(url) {
  httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    xml2::xml_find_all("//title") |>
    xml2::xml_text()
}


#' @rdname auto
#' @export
to_pkg <- function(pkg, type = c("tooltip", "plain"), keep_braces = TRUE, ...) {
  type <- rlang::arg_match(type)
  link_text <- glue::glue(if (keep_braces) "{{{pkg}}}" else "{pkg}")

  url <- downlit::href_package(pkg)
  if (is.na(url)) {
    return(tags$span(link_text, class = "r-link-pkg-error"))
  }
  link <- tags$a(link_text, href = url, class = "r-link-pkg", target = "_blank")

  switch(
    type,
    plain = link,
    tooltip = bslib::tooltip(link, get_title(url))
  )
}

#' @rdname auto
#' @export
to_call <- function(call, type = c("tooltip", "plain"), keep_pkg_prefix = TRUE, ...) {
  type <- rlang::arg_match(type)

  link_text <- if (keep_pkg_prefix) {
    call
  } else {
    glue::glue("{fun}()", fun = stringr::str_extract(call, rx_call, group = 2))
  }

  url <- downlit::autolink_url(call)
  if (is.na(url)) {
    return(tags$span(link_text, class = "r-link-pkg-error"))
  }
  link <- tags$a(link_text, href = url, class = "r-link-call", target = "_blank")

  switch(
    type,
    plain = link,
    tooltip = bslib::tooltip(link, get_title(url))
  )
}

autolink_pkg <- function(x, type, keep_braces = TRUE) {
  stringr::str_replace_all(
    x, rx_pkg,
    function(pkg) {
      pkg <- stringr::str_extract(pkg, rx_pkg, group = 1)
      as.character(to_pkg(pkg, type = type, keep_braces = keep_braces))
    }
  )
}

autolink_call <- function(x, type, keep_pkg_prefix = TRUE) {
  stringr::str_replace_all(
    x, rx_call, function(call) {
      as.character(to_call(call, type = type, keep_pkg_prefix = keep_pkg_prefix))
    }
  )
}
