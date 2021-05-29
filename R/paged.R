#' @name break_page
#' @rdname break_page
#' @title Breaking pages in HTML
#' @details break_page() insert the HTML page break code in the R code chunk. and 
#' break_page_asis() inserts the HTML page break code in the asis statement of 
#' the R code chunk.
#' @return HTML code
#' @importFrom htmltools HTML div
#' @export
break_page <- function() {
  htmltools::HTML(
    paste0(htmltools::div(class = 'page-break-after', ""))
  )
}

#' @name break_page_asis
#' @rdname break_page
#' @importFrom htmltools HTML div
#' @export
break_page_asis <- function() {
  htmltools::HTML(
    cat(
      paste0(
        htmltools::div(class = 'page-break-after', "")
      )
    )
  )
}

#' @name break_line_asis
#' @rdname break_page
#' @importFrom htmltools br
#' @export
break_line_asis <- function(n = 1) {
  for (i in seq(n)) {
    htmltools::br() %>% 
      as.character() %>% 
      cat()
  }
}

#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom htmltools HTML div
#' @import dplyr
#' @export
print_tab <- function(tab, n_rows = 25, add_row = 3, caption = "", full_width = TRUE,
                      font_size = 14) {
  N <- nrow(tab)
  n_pages <- 1
  
  if (N > n_rows) {
    n_pages <- n_pages + ceiling((N - n_rows) / (n_rows + add_row))
  }

  for (i in seq(n_pages)) {
    if (i == 1) {
      idx <- intersect(seq(N), seq(n_rows))
    } else {
      idx <- (max(idx) + 1):(max(idx) + n_rows + add_row) %>% 
        pmin(N) 
    }
    
    knitr::kable(tab[idx, ], digits = 2, format = "html",
                 caption = ifelse(i > 1, paste(caption, "(continued)"), caption), 
                 format.args = list(big.mark = ",")) %>%
      kableExtra::kable_styling(full_width = full_width, font_size = font_size, 
                                position = "left") %>% 
      cat()
    
    if (n_pages == 1 | i < n_pages) {
      break_page_asis()
    }
  }
}
