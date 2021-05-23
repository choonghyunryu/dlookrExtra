#' @name dlookrExtra_templ_paged
#' @rdname dlookrExtra_templ_paged
#' @title Generate paged HTML document
#'
#' @param ... arguments to be passed to \code{pagedown::\link[pagedown]{html_paged}}.
#' @references \url{https://pagedown.rbind.io}
#' @return document of markdown format.
#' @import pagedown
#' @export dlookrExtra_templ_paged
#'
dlookrExtra_templ_paged <- function(...) {
  cssfile <- function(...) {
    system.file("resources", "css", paste0(..., ".css"), package = "dlookrExtra")
  }

  svgfile <- function(...) {
    system.file("resources", "svg", paste0(..., ".svg"), package = "dlookrExtra")
  }
  
  pagedown::html_paged(
    css = c(cssfile('custom-fonts'), cssfile('custom-page'), cssfile('custom')), ...)
}
