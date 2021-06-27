#' @export
fixed_text <- function(x, max_width = 30) {
  ifelse(nchar(x) > max_width, paste(strtrim(x, max_width - 4), "..."), x)
}