rx_valid_pkg <- "[a-zA-Z][a-zA-Z0-9.]*[a-zA-Z0-9]"
rx_valid_fun <- "([a-zA-Z]\\w*|\\.\\w+)"

autolink_pkg <- function(x) {
  rx_pkg  <- glue::glue("[{{]({rx_valid_pkg})[}}]")
  stringr::str_replace_all(
    x, rx_pkg,
    function(pkg) {
      to(package = stringr::str_extract(pkg, rx_pkg, group = 1))
    }
  )
}

autolink_call <- function(x) {
  rx_call <- glue::glue("[{{]({rx_valid_pkg})::({rx_valid_fun})[(][)][}}]")
  stringr::str_replace_all(
    x, rx_call,
    function(call){
      to(
        package = stringr::str_extract(call, rx_call, group = 1),
        topic   = stringr::str_extract(call, rx_call, group = 2)
      )
    }
  )

}

#' Setup automatic linking
#'
#' @export
auto <- function() {
  default_text_hook <- knitr::knit_hooks$get("text")
  knitr::knit_hooks$set(text = function(x) {

    x <- autolink_pkg(x)
    x <- autolink_call(x)

    default_text_hook(x)
  })

}
