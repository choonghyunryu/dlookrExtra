#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom purrr map_df
#' @import dplyr
#' @import htmltools
#' @export
html_normality <- function(.data) {
  normality_indicator <- function(value) {
    if (value %in% "Balanced") {
      args <- shiny::icon("balance-scale", 
                          style = "color: rgb(255, 127, 42)")
    } else if (value %in% "Right-Skewed") {
      args <- shiny::icon("balance-scale-left", 
                          style = "color: rgb(255, 127, 42)")
    } else if (value %in% "Left-Skewed") {
      args <- shiny::icon("balance-scale-right", 
                          style = "color: rgb(255, 127, 42)")
    } else if (value %in% "Invalid") {
      args <- shiny::icon("exclamation-triangle", 
                          style = "color: rgb(255, 127, 42)")
    } 
    
    div(args, paste("", value))
  }
  
  N <- nrow(.data)
  
  describe_num <- dlookr::describe(.data) %>% 
    select(variable, n, na, mean, p00, p25, p50,  p75, p100, skewness, kurtosis) %>% 
    rename("Min" = p00,
           "Max" = p100,
           "Median" = p50,
           "Q1" = p25,
           "Q3" = p75,
           "Missing" = na) %>% 
    mutate(balance = case_when(
      abs(skewness) <= 1.2 ~ "Balanced",
      skewness > 1.2 ~ "Right-Skewed",
      skewness < 1.2 ~ "Left-Skewed",
      is.na(skewness) ~ "Invalid"
    )) %>% 
    mutate(mean = round(mean, 2))
  
    describe_num %>% 
    reactable(
      columns = list(
        balance = colDef(
          name = "Balance",
          width = 150,
          cell = function(value) normality_indicator(value)
        ),
        variable = colDef(
          name = "Variables",
          width = 130
        ),
        mean = colDef(
          name = "Mean"
        ),        
        n = colDef(
          show = FALSE
        ), 
        Missing = colDef(
          show = FALSE
        ),         
        skewness = colDef(
          show = FALSE
        ),
        kurtosis = colDef(
          show = FALSE
        )         
      ),
      details = function(index) {
        variable <- describe_num$variable[index]
        
        flag_sample <- FALSE
        
        x <- .data[, variable] %>%
          .[!is.na(.)]
        
        if (length(x) > 5000) {
          x <- sample(x, size = 5000, replace = TRUE)
          flag_sample <- TRUE
        }  
        
        if (length(x) == 0) {
          tab_test <- data.frame(alert = "all values are NA") %>% 
            reactable()
          stats <- data.frame(alert = "all values are NA") %>% 
            reactable()
          p_normality <- data.frame(alert = "all values are NA") %>% 
            reactable()          
        } else if (length(x) < 3L || length(unique(x)) < 3L) {
          tab_test <- data.frame(
            alert = "(unique) sample size must be greater then 3") %>% 
            reactable()
          stats <- data.frame(
            alert = "(unique) sample size must be greater then 3") %>% 
            reactable()
          p_normality <- data.frame(
            alert = "(unique) sample size must be greater then 3") %>% 
            reactable()   
        } else if (diff(range(x, na.rm = TRUE)) == 0) {
          tab_test <- data.frame(alert = "all values are identical") %>% 
            reactable() 
          stats <- data.frame(alert = "all values are identical") %>% 
            reactable() 
          p_normality <- data.frame(alert = "all values are identical") %>% 
            reactable()           
        } else {
          y <- shapiro.test(x)
          statistic <- round(y$statistic, 5)
          p_value <- round(y$p.value, 7)
          
          tab_test <- data.frame(
              Statistic = statistic,
              p_value = p_value,
              remark = ifelse(flag_sample, "5000 samples", "No sample"),
              row.names = NULL
            ) %>% 
            reactable(
              fullWidth = FALSE,
              columns = list(
                Statistic = colDef(
                  name = "Shapiro-Wilk Statistic",
                  width = 200
                ),
                p_value = colDef(
                  name = "P-value of Test",
                  width = 200
                ),
                remark = colDef(
                  name = "Whether Sample",
                  width = 200
                )               
              )  
            )
          
          if (is.factor(x)) x <- as.numeric(x)
          
          if (sum(x < 0, na.rm = TRUE) > 0) {
            type <- c("original", "log+a transformation", "Box-Cox transformation")
            
            skew <- c(skewness(x, na.rm = TRUE),
                      skewness(get_transform(x, "log+a"), na.rm = TRUE),
                      skewness(get_transform(x, "Box-Cox"), na.rm = TRUE))
            
            kurt <- c(kurtosis(x, na.rm = TRUE),
                      kurtosis(get_transform(x, "log+a"), na.rm = TRUE),
                      kurtosis(get_transform(x, "Box-Cox"), na.rm = TRUE))          
          } else {
            if (any(x == 0, na.rm = TRUE)) {
              type <- c("original", "log+1 transformation", "sqrt transformation")
              
              skew <- c(skewness(x, na.rm = TRUE),
                        skewness(get_transform(x, "log+1"), na.rm = TRUE),
                        skewness(sqrt(x), na.rm = TRUE))
              
              kurt <- c(kurtosis(x, na.rm = TRUE),
                        kurtosis(get_transform(x, "log+1"), na.rm = TRUE),
                        kurtosis(sqrt(x), na.rm = TRUE))         
            } else {
              type <- c("original", "log transformation", "sqrt transformation")
              
              skew <- c(skewness(x, na.rm = TRUE),
                        skewness(log(x), na.rm = TRUE),
                        skewness(sqrt(x), na.rm = TRUE))
              
              kurt <- c(kurtosis(x, na.rm = TRUE),
                        kurtosis(log(x), na.rm = TRUE),
                        kurtosis(sqrt(x), na.rm = TRUE)) 
            }  
          }
          
          stats <- data.frame(type = type, 
                              skewness = round(skew, 4), 
                              kurtosis = round(kurt, 4)) %>% 
            reactable(
              fullWidth = FALSE,
              columns = list(
                type = colDef(
                  name = "Data Type",
                  width = 200
                ),
                skewness = colDef(
                  name = "Skewness",
                  width = 200
                ),
                kurtosis = colDef(
                  name = "Kurtosis",
                  width = 200
                )              
              )  
            )
          
          p_normality <- htmltools::plotTag({
            x <- data.frame(x)
            
            if (sum(x < 0, na.rm = TRUE) > 0) {
              plot_normality(x, x, left = "log+a", right = "Box-Cox")
            } else {
              if (any(x == 0, na.rm = TRUE)) 
                plot_normality(x, x, left = "log+1", right = "sqrt")
              else
                plot_normality(x, x)
            }
          }, sprintf("A plot of the %s variable", variable), width = 600,
          height = 400, device = grDevices::png)
        }
        
        shiny::tabsetPanel(
          shiny::tabPanel("Distribution", p_normality,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"), 
          shiny::tabPanel("Normality test", tab_test,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"), 
          shiny::tabPanel("Skewness and Kurtosis", stats,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;")
        )
      }      
    )  
} 
