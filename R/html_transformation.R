agg_imputation <- function(object, ...) {
  success <- attr(object, "success")
  
  if (!success) {
    message("imputation object isn't success.")
    return()
  }
  
  type <- attr(object, "type")
  method <- attr(object, "method")
  var_type <- attr(object, "var_type")
  
  original <- object
  
  if (type == "missing values") {
    na_pos <- attr(object, "na_pos")
    seed <- attr(object, "seed")
    
    original[na_pos] <- NA
  } else if (type == "outliers") {
    outlier_pos <- attr(object, "outlier_pos")
    outliers <- attr(object, "outliers")
    
    original[outlier_pos] <- outliers
  }
  
  if (var_type == "numerical") {
    original <- as.numeric(original)
    object <- as.numeric(object)
  } else if (var_type == "categorical") {
    original <- factor(original)
    object <- factor(object)
  }
  
  dframe <- data.frame(original = original,
                       imputation = object) %>%
    tidyr::gather()
  
  if (var_type == "numerical") {
    smmry <- dframe %>%
      group_by(key) %>%
      describe("value") %>%
      select(which(!names(.) %in% c("variable", "key"))) %>% 
      t
    
    smmry <- smmry[, 2:1]
    colnames(smmry) <- c("Original", "Imputation")
  } else if (var_type == "categorical") {
    tab_freq <- xtabs(~ value + key, dframe, addNA = TRUE)
    tab_relat <- round(prop.table(tab_freq, 2) * 100, 2)
    
    smmry <- cbind(tab_freq, tab_relat)
    smmry <- smmry[, c(2, 1, 4, 3)]
    colnames(smmry) <- c("original", "imputation",
                         "original_percent", "imputation_percent")
  }
  
  invisible(smmry)
}


#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @import dplyr
#' @import reactable
#' @importFrom dlookr find_na get_class
#' @export
html_impute_missing <- function(.data, target = NULL) {
  numerics <- c("mean", "median", "mode", "knn", "rpart", "mice")
  categories <- c("mode", "rpart", "mice")
  
  n_na <- .data %>% 
    purrr::map_dbl(function(x) sum(is.na(x))) %>% 
    .[. > 0]
  
  if (length(n_na) > 0) {
    data_type <- get_class(.data)
    
    tab_missing <- n_na %>% 
      tibble::as_tibble() %>% 
      mutate(variable = names(n_na)) %>% 
      rename("missing" = value) %>% 
      bind_cols(n = NROW(.data)) %>% 
      mutate(rate_missing = missing / n) %>% 
      inner_join(data_type, by = "variable") %>% 
      select(variable, class, n, missing, rate_missing) 
    
    tab_missing %>% 
      reactable(
        columns = list(
          variable = colDef(
            name = "variables"
          ),        
          class = colDef(
            name = "data type"
          ),
          n = colDef(
            name = "observations"
          ),
          rate_missing = colDef(
            name = "missing(%)",
            format = colFormat(
              percent = TRUE,
              digits = 2
            )
          )
        ),
        details = function(index) {
          variable <- tab_missing$variable[index]
          type <- tab_missing$class[index] %>% 
            as.character()
          
          if (type %in% c("integer", "numeric")) {
            method <- numerics
          } else if (type %in% c("factor", "ordered")) {
            method <- categories
          }
          
          if (is.null(target)) {
            method <- setdiff(method, c("knn", "rpart", "mice"))
          } else {
            method <- method
          }
          
          imputes <- method %>% 
            lapply(function(x) {
              if (is.null(target)) {
                imputate_na(.data, all_of(variable), all_of(target), method = x, 
                            print_flag = FALSE)
              } else {
                imputate_na(.data, all_of(variable),  method = x, 
                            print_flag = FALSE)
              }
            })
          
          p_compare <- imputes %>% 
            lapply(function(x) {
              htmltools::plotTag({
                plot(x)
              }, sprintf("A plot of imputation"), 
              width = 600, height = 400, device = grDevices::png)
            })
          
          suppressMessages({
            mat <- imputes %>% 
              purrr::map_dfc(function(x) {
                agg_imputation(x) %>% 
                  as.data.frame()
              })
          })

          if (type %in% c("integer", "numeric")) {
            drops <- seq(ncol(mat))[seq(ncol(mat)) %% 2 == 1][-1]
            mat <- mat[, -c(drops)]
            names(mat) <- c("original", paste("inpute", method, sep = "_"))
            
            tab_compare <- mat %>% 
              t() %>% 
              as.data.frame() %>% 
              select(mean, sd, IQR, p00, p25, p50,  p75, p100, skewness, kurtosis) %>% 
              reactable(
                defaultColDef = colDef(
                  format = colFormat(
                    digits = 2
                  )
                ),              
                columns = list(
                  mean = colDef(
                    name = "Mean"
                  ),
                  sd = colDef(
                    name = "Standard Devation"
                  ),
                  p00 = colDef(
                    name = "Min"
                  ),  
                  p25 = colDef(
                    name = "Q1"
                  ),
                  p50 = colDef(
                    name = "Median"
                  ),
                  p75 = colDef(
                    name = "Q3"
                  ),
                  p100 = colDef(
                    name = "Max"
                  ),
                  skewness = colDef(
                    name = "Skewness"
                  ),
                  kurtosis = colDef(
                    name = "Kurtosis"
                  )                
                )
              )
            
            tabs <- lapply(seq(length(method) + 1), function(x) {
              if (x == 1) {
                shiny::tabPanel("Distribution", tab_compare,
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              } else {
                shiny::tabPanel(title = paste0("Plot_", method[x-1]),
                                p_compare[[x-1]],
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              }
              
            })
            
            do.call(shiny::tabsetPanel, c(tabs, id = "viz"))
          } else if (type %in% c("factor", "ordered")) {
            drops <- seq(ncol(mat))[seq(ncol(mat)) %% 4 != 2][-1]
            mat <- mat[, -c(drops)]
            names(mat) <- c("original", paste("inpute", method, sep = "_"))
            
            tab_compare <- mat %>% 
              t() %>% 
              as.data.frame()
            
            names(tab_compare)[ncol(tab_compare)] <- "<Missing>"
            
            tab_compare_rate <- tab_compare / rowSums(tab_compare)
            
            tab_compare <- tab_compare %>% 
              reactable(
                defaultColDef = colDef(
                  format = colFormat(
                    separators = TRUE
                  )
                )
              )
            
            tab_compare_rate <- tab_compare_rate %>% 
              reactable(
                defaultColDef = colDef(
                  format = colFormat(
                    percent = TRUE,
                    digits = 2
                  )
                )
              )
            
            tabs <- lapply(seq(length(method) + 2), function(x) {
              if (x == 1) {
                shiny::tabPanel("Contingency Table", tab_compare,
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              } else if (x == 2) {
                shiny::tabPanel("Relate Contingency Table", tab_compare_rate,
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              } else {              
                shiny::tabPanel(title = paste0("Plot_", method[x-2]),
                                p_compare[[x-2]],
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              }
              
            })
            
            do.call(shiny::tabsetPanel, c(tabs, id = "viz"))
          }  
        }  
      )
  } else {
    h5("There are no variables in the dataset with missing values.")
  }
}



#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @import dplyr
#' @import reactable
#' @importFrom dlookr diagnose_numeric imputate_na
#' @export
html_impute_outlier <- function(.data) {
  method <- c("mean", "median", "mode", "capping")
  
  n <- nrow(.data)
  
  outlist <- .data %>% 
    dlookr::diagnose_numeric() %>% 
    filter(outlier > 0) %>% 
    mutate(n = n) %>% 
    mutate(rate_outlier = outlier / n)
  
  if (length(outlist) > 0) {
    tab_outlier <- outlist %>% 
      select(variables, n, min, max, outlier, rate_outlier) 
    
    tab_outlier %>% 
      reactable(
        columns = list(
          n = colDef(
            name = "observations"
          ),
          rate_outlier = colDef(
            name = "outlier(%)",
            format = colFormat(
              percent = TRUE,
              digits = 2
            )
          )
        ),
        details = function(index) {
          variable <- tab_outlier$variables[index]
          
          imputes <- method %>% 
            lapply(function(x) {
              dlookr::imputate_outlier(.data, all_of(variable),  method = x)
            })
          
          p_compare <- imputes %>% 
            lapply(function(x) {
              htmltools::plotTag({
                plot(x)
              }, sprintf("A plot of imputation"), 
              width = 600, height = 400, device = grDevices::png)
            })
          
          suppressMessages({
            mat <- imputes %>% 
              purrr::map_dfc(function(x) {
                agg_imputation(x) %>% 
                  as.data.frame()
              })
          }) 
          
          drops <- seq(ncol(mat))[seq(ncol(mat)) %% 2 == 1][-1]
          mat <- mat[, -c(drops)]
          names(mat) <- c("original", paste("inpute", method, sep = "_"))
            
          tab_compare <- mat %>% 
            t() %>% 
            as.data.frame() %>% 
            select(mean, sd, IQR, p00, p25, p50,  p75, p100, skewness, kurtosis) %>% 
            reactable(
              defaultColDef = colDef(
                format = colFormat(
                  digits = 2
                )
              ),              
              columns = list(
                mean = colDef(
                  name = "Mean"
                ),
                sd = colDef(
                  name = "Standard Devation"
                ),
                p00 = colDef(
                  name = "Min"
                ),  
                p25 = colDef(
                  name = "Q1"
                ),
                p50 = colDef(
                  name = "Median"
                ),
                p75 = colDef(
                  name = "Q3"
                ),
                p100 = colDef(
                  name = "Max"
                ),
                skewness = colDef(
                  name = "Skewness"
                ),
                kurtosis = colDef(
                  name = "Kurtosis"
                )                
              )
            )
            
            tabs <- lapply(seq(length(method) + 1), function(x) {
              if (x == 1) {
                shiny::tabPanel("Distribution", tab_compare,
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              } else {
                shiny::tabPanel(title = paste0("Plot_", method[x-1]),
                                p_compare[[x-1]],
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              }
              
            })
            
            do.call(shiny::tabsetPanel, c(tabs, id = "viz"))
        }          
      )
  } else {
    h5("There are no variables in the dataset with outliers.")
  }
}

#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @import dplyr
#' @import reactable
#' @importFrom dlookr diagnose_numeric imputate_na
#' @export
html_resolve_skewness <- function(.data) {
  skewlist <- find_skewness(.data, index = FALSE)
  
  outlist <- .data %>% 
    dlookr::diagnose_numeric() %>% 
    filter(outlier > 0) %>% 
    mutate(n = n) %>% 
    mutate(rate_outlier = outlier / n)
  
  if (length(outlist) > 0) {
    tab_outlier <- outlist %>% 
      select(variables, n, min, max, outlier, rate_outlier) 
    
    tab_outlier %>% 
      reactable(
        columns = list(
          n = colDef(
            name = "observations"
          ),
          rate_outlier = colDef(
            name = "outlier(%)",
            format = colFormat(
              percent = TRUE,
              digits = 2
            )
          )
        )
      )
  } 
}  

