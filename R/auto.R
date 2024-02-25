#' @importFrom htmltools tags
rx_valid_pkg <- "[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]"
rx_valid_fun <- "([a-zA-Z]\\w*|\\.\\w+)"

get_title <- function(url) {
  httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    xml2::xml_find_all("//title") |>
    xml2::xml_text()
}

get_description <- function(url) {
  httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_html() |>
    xml2::xml_find_all("//meta[@name='description']") |>
    xml2::xml_attr("content")
}

tip_pkg <- function(pkg, title = get_title(url), ...) {
  url <- downlit::href_package(pkg)
  bslib::tooltip(
    tags$a(glue::glue("{{{pkg}}}"), href = url, class = "r-link-pkg"),
    title,
    ...
  )
}

tip_call <- function(call, title = get_title(url), ...) {
  url <- downlit::autolink_url(call)

  tip <- bslib::tooltip(
    tags$a(call, href = url, class = "r-link-call"),
    title,
    ...
  )
}

autolink_pkg <- function(x) {
  rx_pkg  <- glue::glue("[{{]({rx_valid_pkg})[}}]")
  stringr::str_replace_all(
    x, rx_pkg,
    function(pkg) {
      pkg <- stringr::str_extract(pkg, rx_pkg, group = 1)
      as.character(tip_pkg(pkg))
    }
  )
}

autolink_call <- function(x) {
  rx_call <- glue::glue("({rx_valid_pkg})::({rx_valid_fun})\\(\\)")
  stringr::str_replace_all(
    x, rx_call, function(call) {
      as.character(tip_call(call))
    }
  )

}

#' Setup automatic linking
#'
#' @examples
#' \dontrun{
#'   auto()
#' }
#'
#' @export
auto <- function() {
  default_text_hook <- knitr::knit_hooks$get("text")
  knitr::knit_hooks$set(text = function(x) {

    x <- autolink_pkg(x)
    x <- autolink_call(x)

    default_text_hook(x)
  })

  # This is a hack because I currently don't know how to inject the
  # dependencies otherwise
  bslib::tooltip("", "")
}
