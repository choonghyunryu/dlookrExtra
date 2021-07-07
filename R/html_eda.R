#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @import dplyr
#' @import ggplot2
#' @import reactable
#' @importFrom dlookr diagnose diagnose_numeric plot_bar_category plot_box_numeric
#' @export
html_descriptive <- function(.data) {
  N <- nrow(.data)
  
  raws <- .data %>% 
    dlookr::diagnose() %>% 
    left_join(
      .data %>% 
        dlookr::diagnose_numeric(),
      by = "variables"
    ) %>% 
    left_join(
      .data %>% 
        dlookr::describe(statistics = c("skewness", "kurtosis")) %>% 
        select(-n, -na),
      by = c("variables" = "variable")) %>% 
    mutate(missing_percent = round(missing_percent, 2),
           unique_rate = round(unique_rate, 3),
           skewness = round(skewness, 3),
           kurtosis = round(kurtosis, 3))
  
  raws %>% 
    reactable(
      columns = list(
        variables = colDef(
          name = "Variables",
          cell = JS("function(cellInfo) {
                  const title = '<span class=\"variables\">' + cellInfo.value + '</span>'
                  const type = '<span class=\"box-type\">' + cellInfo.row['types'] + '</span>'
                  const details = '<div class=\"variable-info-details\">' + type + '</div>'
                  const text = '<div class=\"variable-info-text\">' + title + details + '</div>'
                  
                  return '<div class=\"variable-info\">' + text + '</div>'
        }"),
        html = TRUE,    
        minWidth = 150,
        maxWidth = 200
        ),
        types = colDef(
          show = FALSE
        ),
        missing_count = colDef(
          name = "Missing (%)",
          cell = JS("function(cellInfo) {
                  const value = cellInfo.value.toLocaleString('en')
                  const ratio = '(' + cellInfo.row['missing_percent'] + ')'
                  return '<div class=\"value-info-text\">' + value + '<br>' + ratio + '</div>'
        }"),
        html = TRUE,
        align = "right"
        ),
        missing_percent = colDef(
          show = FALSE
        ),
        unique_count = colDef(
          name = "Distincts (Ratio)",
          cell = JS("function(cellInfo) {
                  const value = cellInfo.value.toLocaleString('en')
                  const ratio = '(' + cellInfo.row['unique_rate'] + ')'
                  return '<div class=\"value-info-text\">' + value + '<br>' + ratio + '</div>'
          }"),
          html = TRUE,
          align = "right"
        ),
        unique_rate = colDef(
          show = FALSE
        ),        
        zero = colDef(
          name = "Zeros",
          cell = JS("function(cellInfo) {
                    value = cellInfo.value
                  
                    if (value == null) {
                      value = '-'
                    } else {
                      value = value.toLocaleString('en')
                    }
                    return '<div class=\"value-info-text\">' + value + '</div>'
                  }"),
          html = TRUE,
          align = "right"
        ),
        minus = colDef(
          name = "Negatives",
          cell = JS("function(cellInfo) {
                    value = cellInfo.value
                  
                    if (value == null) {
                      value = '-'
                    } else {
                      value = value.toLocaleString('en')
                    }
                    return '<div class=\"value-info-text\">' + value + '</div>'
                  }"),
          html = TRUE,
          align = "right"
        ),        
        outlier = colDef(
          name = "Outliers",
          cell = JS("function(cellInfo) {
                    value = cellInfo.value
                  
                    if (value == null) {
                      value = '-'
                    } else {
                      value = value.toLocaleString('en')
                    }
                    return '<div class=\"value-info-text\">' + value + '</div>'
                  }"),
          html = TRUE,
          align = "right"
        ),
        min = colDef(
          show = FALSE
        ),
        Q1 = colDef(
          show = FALSE
        ),
        mean = colDef(
          show = FALSE
        ),
        median = colDef(
          show = FALSE
        ),
        Q3 = colDef(
          show = FALSE
        ),
        max = colDef(
          show = FALSE
        ),
        skewness = colDef(
          show = FALSE
        ),
        kurtosis = colDef(
          show = FALSE
        )        
      ),
      bordered = FALSE,
      class = "variables-tbl",
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        cellPadding = "5px 3px"
      ),
      details = function(index) {
        variable <- raws$variables[index]
        type <- raws$types[index]
        
        suppressWarnings(
          unique_missing <- raws %>% 
            select(variables, missing_count, unique_count, zero, minus, 
                   outlier) %>% 
            filter(variables %in% variable) %>% 
            select(-variables) %>% 
            tidyr::gather(key = "metric", value = "count") %>% 
            mutate(metric = sub("_[[:print:]]+", "", metric)) %>%         
            mutate(metric = case_when(metric %in% "unique" ~ "Distincts",
                                      metric %in% "missing" ~ "Missing",
                                      metric %in% "zero" ~ "Zeros",
                                      metric %in% "minus" ~ "Negatives",
                                      metric %in% "outlier" ~ "Outliers")) %>% 
            filter(!is.na(count)) %>% 
            mutate(percent = count / N ) %>% 
            arrange(metric) %>% 
            reactable(
              fullWidth = FALSE,
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
              columns = list(
                metric = colDef(name = "Metrics"),
                count = colDef(name = "Frequency", 
                               format = colFormat(separators = TRUE)),
                percent = colDef(name = "Percent", 
                                 format = colFormat(percent = TRUE, digits = 1))
              ))
        )
        
        if (type %in% c("character")) {
          agg_char <- .data %>%
            rename(value = variable) %>%
            select(value) %>%
            mutate(value = as.character(value)) %>%
            mutate(n_character = nchar(value)) %>%
            mutate(n_word = sapply(gregexpr("[[:print:]]+", value),
                                   function(x) sum(x > 0))) %>%
            summarise(min_word = min(n_word, na.rm = TRUE),
                      mean_word = round(mean(n_word, na.rm = TRUE), 1),
                      median_word = median(n_word, na.rm = TRUE),
                      max_word = max(n_word, na.rm = TRUE),
                      min_character = min(n_character, na.rm = TRUE),
                      mean_character = round(mean(n_character, na.rm = TRUE), 1),
                      median_character = median(n_character, na.rm = TRUE),
                      max_character = max(n_character, na.rm = TRUE)) %>%
            unlist()
          
          stat_char <- data.frame(Class = c("Words Count", "Characters Length"),
                                  rbind(agg_char[1:4], agg_char[5:8]))
          names(stat_char) <- names(stat_char) %>% sub("_[[:print:]]+", "", .)
          
          stat_char <- stat_char %>% 
            reactable(fullWidth = FALSE,
                      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
                      columns = list(Class = colDef(width = 150))
            )          
          
          top_rank <- html_toprank(.data, variable, TRUE)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution", top_rank, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),          
            shiny::tabPanel("Character Stats", stat_char,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Cardinality/Missing", unique_missing,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")          
          )
        } else if (type %in% c("factor", "ordered", "Date", "POSIXct")) {
          top_rank <- html_toprank(.data, variable, TRUE)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution", top_rank, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Cardinality/Missing", unique_missing, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")
          )
        } else if (type %in% c("integer", "numeric")) {
          stats <- raws %>% 
            filter(variables %in% variable) %>%           
            select(min:max, skewness, kurtosis) %>% 
            mutate(mean = round(mean, 3)) %>% 
            reactable(fullWidth = FALSE,
                      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"))
          
          p_hist <- htmltools::plotTag({
            dlookr::plot_hist_numeric(.data, which(names(.data) == variable))
          }, sprintf("A plot of the %s variable", variable), width = 600, 
          height = 400, device = grDevices::png)
          
          diagn_outlier <- .data %>%
            diagnose_outlier(variable)
          
          cols <- c("Outliers count", "Outliers ratio (%)", "Mean of outliers",
                    "Mean with outliers", "Mean without outliers")
          outlier_df <- data.frame(Measures = cols,
                                   Values = as.vector(t(diagn_outlier[, -1]))) 
          
          values <- outlier_df$Values
          
          outlier_df$Values[1] <- round(values[1]) %>% 
            format(big.mark = ",")
          outlier_df$Values[2] <- round(values[2], 2) %>% 
            as.character() %>% 
            paste0("%")
          outlier_df$Values[3] <- values[3] %>% 
            format() %>% 
            as.character()
          outlier_df$Values[4] <- values[4] %>% 
            format() %>% 
            as.character()
          outlier_df$Values[5] <- values[5] %>% 
            format() %>% 
            as.character()
          
          outlier_df <- outlier_df %>% 
            reactable(fullWidth = FALSE,
                      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
                      columns = list(
                        Measures = colDef(width = 200),
                        Values = colDef(align = "right")
                      ))
          
          p_outlier <- htmltools::plotTag({
            dlookr::plot_outlier(.data, variable)
          }, sprintf("A plot of the %s variable", variable), width = 600,
          height = 400, device = grDevices::png)
          
          shiny::tabsetPanel(
            shiny::tabPanel("Distribution", p_hist, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Cardinality/Missing", unique_missing, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Statistics", stats, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Outliers Plot", p_outlier,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Outliers Stats", outlier_df,
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")            
          )
        }
      }      
    )
}


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
      defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
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
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
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
              defaultColDef = colDef(style = "font-size: 14px;color: hsl(0, 0%, 40%);"),
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


#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom dlookr compare_category
#' @importFrom purrr map_int
#' @importFrom hrbrthemes theme_ipsum
#' @import reactable
#' @import dplyr
#' @import htmltools
#' @export
html_compare_category <- function(.data, n_cells = 20, n_levels = 10) {
  plot_compare <- function(x, y, ctab) {
    xvar <- x
    yvar <- y
    
    data <- ctab %>% 
      select(a = xvar, b = yvar, n) 
    
    first <- data[1, 1] %>% pull %>% as.character
    y <- data %>% 
      filter(a %in% first) %>% 
      select(b, n)
    
    y_lab <- y$b %>% rev() %>% as.character()
    y <- y$n %>% rev()
    
    y_cumsum <- cumsum(y)
    y_center <- y / 2
    
    y_pos <- numeric(length(y))
    for (j in seq(y)) {
      if (j == 1) {
        y_pos[j] <- y_center[j]
      } else {
        y_pos[j] <- y_cumsum[j-1] + y_center[j]
      }
      y_pos[j] <- y_pos[j] / sum(y)
    }
    
    suppressWarnings({
      p <- data %>% 
        group_by(a) %>% 
        mutate(x_width = sum(n)) %>% 
        ggplot(aes(x = factor(a), y = n)) +
        geom_col(aes(width = x_width, fill = factor(b)),
                 color = "white", size = 2, 
                 position = position_fill(reverse = FALSE)) +
        facet_grid(~ a, space = "free", scales = "free", switch = "x") +
        scale_x_discrete(name = xvar) +
        scale_y_continuous(name = yvar, breaks = y_pos, labels = y_lab) +
        labs(title = sprintf("Mosaics plot by '%s' vs '%s'", xvar, yvar)) +
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              strip.background = element_blank(),
              panel.spacing = unit(0, "pt")) 
    })
    
      p <- p +
        hrbrthemes::theme_ipsum(base_family = dlookr:::get_font_family()) +
        scale_fill_ipsum(na.value = "grey80") +
        theme(legend.position = "none",
              panel.grid.major.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              panel.spacing = unit(0, "pt"))
      
    suppressWarnings(print(p))
  }
  
  idx <- .data %>% 
    find_class("categorical")
  
  if (length(idx) < 2) {
    return("The number of categorical variables is less than 2.")
  }
  
  idx_target <- idx %>%
    purrr::map_int(
      function(x) levels(.data[, x]) %>% 
        length()
    ) %>% 
    "<="(n_levels) %>% 
    which() %>% 
    idx[.]
  
  if (length(idx_target) < 2) {
    return("The valid categorical variables is less than 2.")
  }
  
  cat_compares <-  compare_category(.data[, idx_target]) 
  tabs <- summary(cat_compares, "all", marginal = TRUE, na.rm = FALSE, 
                  verbose = FALSE)
  tab_compare <- tabs$chisq %>% 
    filter(df <= n_cells) %>% 
    filter(!is.nan(statistic))
  
  tab_compare %>% 
    select(1, 2, 5, 3, 4) %>% 
    reactable(
      columns = list (
        variable_1 = colDef(
          name = "First Variable"
        ),
        variable_2 = colDef(
          name = "Second Variable"
        ),
        df = colDef(
          name = "Degree of Freedom"
        ),
        statistic = colDef(
          name = "Statistic",
          format = colFormat(digits = 2)
        ),
        p.value = colDef(
          name = "P-Value",
          format = colFormat(digits = 5)
        )         
      ),
      details = function(index) {
        ctable <- tabs$table[[index]]
        
        contingency <- function(tab, relate = FALSE) {
          dname <-  tab %>% dimnames()
          dframe <- tab %>% data.frame()
          
          if (relate) {
            dframe <- round(dframe / dframe[nrow(dframe), ncol(dframe)] * 100, 2)
          }
          
          rownames(dframe) <- tab %>% 
            rownames() %>% 
            ifelse(is.na(.), "<NA>", .)
          
          rname <- dname %>% names() %>% "["(1)
          varname <- ifelse(is.na(dname[[2]]), "<NA>", dname[[2]])
          
          colum_list <- seq(ncol(dframe)) %>% 
            lapply(function(x) {
              colDef(
                name = varname[x],
                format = colFormat(
                  separators = TRUE
                ),
                sortable = FALSE
              )
            }) 
          names(colum_list) <- names(dframe)
          
          colum_list[[".rownames"]] <- colDef(
            name = rname, 
            style = "fontWeight: 'bold';",
            sortable = FALSE
          )
          
          cname <- list(
            colGroup(name = dname %>% names() %>% "["(2), 
                     columns = names(dframe))
          )
          
          dframe %>% 
            reactable(
              columns = colum_list,
              columnGroups = cname
            )
        }
        
        x <- ctable %>% dimnames() %>% names() %>% "["(1)
        y <- ctable %>% dimnames() %>% names() %>% "["(2)
        idx_nm <- paste(x, y, sep = " vs ")
        
        p_compare <- htmltools::plotTag({
          plot_compare(x, y, cat_compares[[idx_nm]])
        }, sprintf("A plot of the %s variable", idx_nm), width = 600,
        height = 400, device = grDevices::png)
        
        shiny::tabsetPanel(
          shiny::tabPanel("Mosaics Plot", p_compare,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Contingency Table", contingency(ctable),
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Relative Contingency Table", contingency(ctable, TRUE),
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;")          
        )        
      }  
    )
}


#' @importFrom shiny icon tabsetPanel tabPanel
#' @importFrom dlookr compare_category
#' @importFrom purrr map_int
#' @importFrom hrbrthemes theme_ipsum
#' @import reactable
#' @import dplyr
#' @import htmltools
#' @export
html_compare_numerical <- function(.data) {
  idx <- .data %>% 
    find_class("numerical")
  
  if (length(idx) < 2) {
    return("The number of numerical variables is less than 2.")
  }
  
  num_compares <-  compare_numeric(.data)
  
  num_compares$correlation %>% 
    reactable(
      columns = list (
        var1 = colDef(
          name = "First Variable"
        ),
        var2 = colDef(
          name = "Second Variable"
        ),
        coef_corr = colDef(
          name = "Correlation Coefficient",
          format = colFormat(
            digits = 5
          )
        )       
      ),
      details = function(index) {
        tab_model <- num_compares$linear[index, ] %>% 
          select(-logLik, -AIC, -BIC, -deviance, -df.residual, -nobs) %>% 
          reactable(
            columns = list(
              var1 = colDef(
                name = "First Variable"
              ),
              var2 = colDef(
                name = "Second Variable"
              ),
              r.squared = colDef(
                format = colFormat(
                  digits = 3
                )
              ),
              adj.r.squared = colDef(
                format = colFormat(
                  digits = 3
                )
              ),
              sigma = colDef(
                width = 100,
                format = colFormat(
                  digits = 2
                )
              ),
              statistic = colDef(
                width = 100,
                format = colFormat(
                  digits = 4
                )
              ),
              p.value = colDef(
                width = 100,
                format = colFormat(
                  digits = 4
                )
              ),
              df = colDef(
                width = 50
              )              
            )
          )
        
        num_compare <- num_compares
  
        vars <- attr(num_compare, "combination")[index, ]
        attr(num_compare, "raw") <- attr(num_compare, "raw")[, vars]
        attr(num_compare, "combination") <- vars %>% t()
        
        p_scatter <- htmltools::plotTag({
          plot(num_compare)
        }, sprintf("A plot of the %s variable", vars %>% 
                     paste(collapse = " vs ")), width = 600,
        height = 400, device = grDevices::png)
        
        shiny::tabsetPanel(
          shiny::tabPanel("Scatter Plot", p_scatter,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Linear Model Summaries", tab_model,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;")          
        )        
      }  
    )
}
