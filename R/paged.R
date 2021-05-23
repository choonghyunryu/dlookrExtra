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

#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @import dplyr
#' @export
print_tab <- function(tab, n_rows = 25, caption = "", full_width = TRUE,
                      font_size = 14) {
  n_pages <- ceiling(nrow(tab) / n_rows)
  
  for (i in seq(n_pages)) {
    idx <- ((i - 1) * n_rows + 1):(i * n_rows) %>% 
      pmin(nrow(tab)) %>% 
      unique()
    
    knitr::kable(tab[idx, ], digits = 2, caption = caption, format = "html",
                 format.args = list(big.mark = ",")) %>%
      kableExtra::kable_styling(full_width = full_width, font_size = font_size, 
                                position = "left") %>% 
      cat()
    
    break_page_asis()
  }
}
