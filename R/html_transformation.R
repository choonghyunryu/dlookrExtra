summary_imputation <- function(object, ...) {
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

summary_transform <- function(object, ...) {
  method <- attr(object, "method")
  origin <- attr(object, "origin")
  
  suppressWarnings({dframe <- data.frame(original = origin,
                                         trans = object) %>%
    tidyr::gather()})
  
  smmry <- dframe %>%
    group_by(key) %>%
    describe("value") %>%
    select(-variable, -key) %>%
    t
  colnames(smmry) <- c("Original", "Transformation")
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
        defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
        columns = list(
          variable = colDef(
            name = "Variables"
          ),        
          class = colDef(
            name = "Data Type"
          ),
          n = colDef(
            name = "Observations"
          ),
          missing = colDef(
            name = "Missing"
          ),          
          rate_missing = colDef(
            name = "Missing(%)",
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
                summary_imputation(x) %>% 
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
                  style = "font-size: 14px;color: hsl(0, 0%, 40%);",
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
                  style = "font-size: 14px;color: hsl(0, 0%, 40%);",
                  format = colFormat(
                    separators = TRUE
                  )
                )
              )
            
            tab_compare_rate <- tab_compare_rate %>% 
              reactable(
                defaultColDef = colDef(
                  style = "font-size: 14px;color: hsl(0, 0%, 40%);",
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
  outlist <- find_outliers(.data, index = FALSE)
  
  if (length(outlist) > 0) {
    tab_outlier <- .data %>% 
      select_at(outlist) %>% 
      dlookr::diagnose_numeric() %>% 
      mutate(n = n) %>% 
      mutate(rate_outlier = outlier / n) %>% 
      select(variables, n, min, max, outlier, rate_outlier) 
    
    tab_outlier %>% 
      reactable(
        defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
        columns = list(
          variables = colDef(
            name = "Variables"
          ),          
          n = colDef(
            name = "Observations"
          ),
          min = colDef(
            name = "Min"
          ), 
          max = colDef(
            name = "Max"
          ), 
          outlier = colDef(
            name = "Outlier"
          ),           
          rate_outlier = colDef(
            name = "Outlier(%)",
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
                summary_imputation(x) %>% 
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
                style = "font-size: 14px;color: hsl(0, 0%, 40%);",
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
#' @importFrom dlookr describe transform
#' @export
html_resolve_skewness <- function(.data) {
  skewlist <- find_skewness(.data, index = FALSE)
  
  if (length(skewlist) > 0) {
    tab_skewness <- .data %>% 
      select_at(skewlist) %>% 
      dlookr::describe(statistics = c("quantiles", "skewness"),
                       quantiles = c(0, 0.25, 0.5, 0.75, 1)) %>% 
      select(variable, p00:p100, skewness)
    
    tab_skewness %>% 
      reactable(
        defaultColDef = colDef(
          style = "font-size: 14px;color: hsl(0, 0%, 40%);",
          minWidth = 80,
          maxWidth = 100
        ),
        columns = list(
          variable = colDef(
            name = "Variables",
            minWidth = 100,
            maxWidth = 150
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
            name = "Skewness",
            format = colFormat(
              digit = 2
            )
          )           
        ),
        details = function(index) {
          variable <- tab_skewness$variable[index]
          skewness <- tab_skewness$skewness[index] 
          
          if (skewness <= 0) {
            method <- c("1/x", "x^2", "x^3", "Box-Cox")
          } else {
            method <- c("log", "log+1", "sqrt", "Box-Cox")
          }
          
          resolve <- method %>% 
            lapply(function(x) {
              dlookr::transform(pull(.data, variable),  method = x)
            })
          
          p_compare <- resolve %>% 
            lapply(function(x) {
              htmltools::plotTag({
                plot(x)
              }, sprintf("A plot of transform"), 
              width = 600, height = 400, device = grDevices::png)
            })
          
          suppressMessages({
            mat <- resolve %>% 
              purrr::map_dfc(function(x) {
                summary_transform(x) %>% 
                  as.data.frame()
              })
          }) 
          
          drops <- seq(ncol(mat))[seq(ncol(mat)) %% 2 == 1][-1]
          mat <- mat[, -c(drops)]
          names(mat) <- c("original", paste("transform", method, sep = "_"))
          
          tab_compare <- mat %>% 
            t() %>% 
            as.data.frame() %>% 
            select(mean, sd, IQR, p00, p25, p50,  p75, p100, skewness, kurtosis) %>% 
            reactable(
              defaultColDef = colDef(
                style = "font-size: 14px;color: hsl(0, 0%, 40%);",
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
    h5("There are no variables including skewed.")
  }
}  



#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @importFrom purrr map_int
#' @import dplyr
#' @import reactable
#' @importFrom dlookr diagnose binning
#' @export
html_binning <- function(.data) {
  method <- c("quantile", "equal", "pretty", "kmeans", "bclust")
  
  numlist <- find_class(.data, "numerical", index = FALSE)
  
  if (length(numlist) > 0) {
    tab_numerical <- .data %>% 
      select_at(numlist) %>% 
      dlookr::diagnose() %>% 
      full_join(data.frame(method = method, stringsAsFactors = FALSE),
                by = character()) %>% 
      select(variables, types, unique_count, unique_rate, method)
    
    options(show.error.messages = FALSE)
    
    bins <- seq(nrow(tab_numerical)) %>% 
      lapply(function(x) {
        binn <- try(dlookr::binning(pull(.data, tab_numerical$variables[x]), 
                                         type = tab_numerical$method[x]),
                    silent = TRUE)
        if (class(binn) == "try-error") {
          return(NULL) 
        } else {
          return(binn)
        }  
      })
      
    options(show.error.messages = TRUE)
    
    tab_numerical$n_bins <- bins %>% 
      purrr::map_int(function(x) attr(x, "levels") %>% length)
    
    tab_numerical %>% 
      reactable(
        defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
        columns = list(
          variables = colDef(
            name = "Variables",
            minWidth = 100,
            maxWidth = 150
          ),
          types = colDef(
            name = "Data Types",
            minWidth = 80,
            maxWidth = 100
          ),
          unique_count = colDef(
            name = "Unique",
            format = colFormat(
              separators = TRUE
            )
          ),
          unique_rate = colDef(
            name = "Unique Rate",
            format = colFormat(
              digits = 3
            )
          ),
          method = colDef(
            name = "Binning Method",
            minWidth = 100,
            maxWidth = 150
          ),
          n_bins = colDef(
            name = "Bins",
            minWidth = 50,
            maxWidth = 80
          )            
        ),
        details = function(index) {
          p_hist <- htmltools::plotTag({
            plot(bins[[index]])
            }, sprintf("A plot of bins"), 
            width = 600, height = 400, device = grDevices::png)
          
          tab_bins <- summary(bins[[index]]) %>% 
            reactable(
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
              columns = list(
                levels = colDef(
                  name = "Bins"
                ),
                freq = colDef(
                  name = "Frequency",
                  format = colFormat(
                    separators = TRUE
                  )
                ),
                rate = colDef(
                  name = "Frequency(%)",
                  format = colFormat(
                    percent = TRUE,
                    digits = 2
                  )
                )
              )
            )
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution", p_hist,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"), 
            shiny::tabPanel("Frequency Table", tab_bins,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")
          )
        }
      )
  } else {
    h5("There are no numerical variables.")
  }
}  


#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @importFrom purrr map_int
#' @import dplyr
#' @import reactable
#' @importFrom dlookr diagnose binning_by
#' @export
html_optimal_binning <- function(.data, target) {
  numlist <- find_class(.data, "numerical", index = FALSE)
  
  if (!target %in% names(.data)) {
    h5(paste0("The target variable ", target, " is not in the data."))
  } else if (length(numlist) == 0) {
    h5("There are no numerical variables.")
  } else {
    n_levles <- length(table(pull(.data, target)))
    
    if (n_levles != 2) {
      h5("The target variable is not a binary class.")
    } else {
      tab_numerical <- .data %>% 
        select_at(numlist) %>% 
        dlookr::diagnose() %>% 
        select(-missing_count, -missing_percent)
      
      # Optimal Binning for Scoring Modeling
      bins <- lapply(numlist, function(x)
        binning_by(.data, y = target, x = all_of(x), p = 0.05))
      
      success <- ifelse(sapply(bins, is.character),
                        "No significant splits", "Success")
      
      tab_numerical <- tab_numerical %>% bind_cols(
        data.frame(success = success))
        
      tab_numerical$n_bins <- bins %>% 
        purrr::map_int(function(x) {
          attr(x, "levels") %>% length
        })  
      
      tab_numerical %>% 
        reactable(
          defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
          columns = list(
            variables = colDef(
              name = "Variables",
              minWidth = 100,
              maxWidth = 150
            ),
            types = colDef(
              name = "Data Types",
              minWidth = 80,
              maxWidth = 100
            ),
            unique_count = colDef(
              name = "Unique",
              format = colFormat(
                separators = TRUE
              )
            ),
            unique_rate = colDef(
              name = "Unique Rate",
              format = colFormat(
                digits = 3
              )
            ),
            success = colDef(
              name = "Success"
            ),
            n_bins = colDef(
              name = "Bins",
              minWidth = 50,
              maxWidth = 80
            )            
          ),
          details = function(index) {
            if (tab_numerical$n_bins[index] > 0) {
              p_dist <- htmltools::plotTag({
                plot(bins[[index]])
              }, sprintf("A plot of bins"), 
              width = 600, height = 400, device = grDevices::png)
              
              sticky_style <- list(position = "sticky", left = 0, 
                                   background = "#fff", zIndex = 1,
                                   borderRight = "1px solid #eee",
                                   fontSize = "15px")
              
              tab_bins <- attr(bins[[index]], "performance") %>% 
                select(-CntCumPos, -CntCumNeg, -RateCumPos, -RateCumNeg) %>% 
                reactable(
                  sortable = FALSE,
                  defaultColDef = colDef(
                    style = "font-size: 14px;color: hsl(0, 0%, 40%);",
                    format = colFormat(
                      digits = 3
                    )
                  ),
                  columns = list(
                    Bin = colDef(
                      style = sticky_style,
                      headerStyle = sticky_style
                    )
                  )
                )
              
              shiny::tabsetPanel(
                shiny::tabPanel("Distribution", p_dist,
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;"), 
                shiny::tabPanel("Frequency Table", tab_bins,
                                hr(style = "border-top: 1px solid black;"),
                                style = "padding-top:5px; padding-bottom:25px;")
              )
            }
          }          
        )  
    }  
  }
}  


