#' @importFrom shiny icon
#' @export
alert_indicator <- function(value) {
  args <- list(role = "img")
  
  if (is.na(value)) {
    args <- c(args, list("–", style = "color: #666; font-weight: 700"))
  } else if (value == 1) {
    args <- c(args, list(shiny::icon("exclamation-triangle"), style = "color: rgb(255, 127, 42)"))
  } else if (value == 0) {
    args <- c(args, list(shiny::icon("check"), style = "color: rgb(0, 114, 188)"))
  } 
  
  do.call(span, args)
}

#' @importFrom purrr map_df
#' @import dplyr
#' @import reactable
#' @export
retactable_toprank <- function(.data, variable = NULL, drop_variable = FALSE) {
  N <- nrow(.data)
  
  top10 <- diagnose_category(.data, top = 10, type = "n") %>% 
    filter(!is.na(levels))
  
  nas <- top10$variables %>% 
    unique %>% 
    purrr::map_df(function(x) {
      freq <- .data[, x] %>% 
        is.na %>% 
        sum
      data.frame(variables = x, levels = "Missing", N = N,
                 freq = freq,
                 ratio =  freq / N, rank = 12)
    }) %>% 
    filter(freq > 0)
  
  merge_top <- top10 %>% 
    bind_rows(nas) 
  
  others <- merge_top %>% 
    group_by(variables) %>% 
    summarise(top_freq = sum(freq), .groups = "drop") %>% 
    mutate(other_freq = N - top_freq) %>% 
    filter(other_freq > 0) %>% 
    mutate(levels = "Other levles",
           N = N,
           freq = other_freq,
           ratio = freq / N,
           rank = 11) %>% 
    select(-top_freq, -other_freq)
  
  top_rank <- merge_top %>% 
    bind_rows(others) %>% 
    mutate(ratio = freq / N) %>% 
    arrange(variables, rank) %>% 
    select(-N, -rank) 
  
  if (!is.null(variable)) {
    top_rank <- top_rank %>% 
      filter(variables %in% variable)
  }
  
  if (drop_variable) {
    top_rank <- top_rank %>% 
      select(-variables)
  }  
  # Render a bar chart in the background of the cell
  bar_style <- function(width = 1, fill = "#e6e6e666", levels = NULL) {
    fill <- ifelse(levels %in% "Missing", "#cdcdcd99", fill)
    fill <- ifelse(levels %in% "Other levles", "#0072bc66", fill)
    
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", 
                     fill, position)
    list(
      backgroundImage = image,
      backgroundSize = "100% 75%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    )
  }
  
  reactable(top_rank, defaultPageSize = nrow(top_rank),
            columns = list(
              freq = colDef(
                style = function(value, index) {
                  bar_style(width = value / max(top_rank$freq), fill = "#ff7f2a66",
                            levels = top_rank$levels[index])
                },
                format = colFormat(separators = TRUE),
                align = "left"
              ),
              ratio = colDef(name = "Percent", 
                             format = colFormat(percent = TRUE, digits = 1))  
            )) 
}


#' @importFrom shiny tabsetPanel tabPanel
#' @importFrom htmltools plotTag
#' @import dplyr
#' @import ggplot2
#' @import reactable
#' @importFrom dlookr diagnose diagnose_numeric plot_bar_category plot_box_numeric
#' @export
alert_variable <- function(.data) {
  N <- nrow(.data)
  
  raws <- .data %>% 
    dlookr::diagnose() %>% 
    left_join(
      .data %>% 
        dlookr::diagnose_numeric(),
      by = "variables"
    ) %>% 
    rename("zero_count" = zero,
           "minus_count" = minus,
           "outlier_count" = outlier)
  
  tabs <- raws %>% 
    mutate(missing = ifelse(missing_count > 0, 1, 0)) %>% 
    mutate(constant = ifelse(unique_count == 1, 1, 0)) %>% 
    mutate(identifier = ifelse(unique_rate == 1, 1, 0)) %>% 
    mutate(zero = ifelse(zero_count > 0, 1, 0)) %>% 
    mutate(minus = ifelse(minus_count > 0, 1, 0)) %>% 
    mutate(outlier = ifelse(outlier_count > 0, 1, 0)) %>% 
    select(variables, types, missing:outlier)
  
  reactable(
    tabs,
    defaultPageSize = nrow(tabs),
    columns = list(
      types = colDef(
        width = 90
      ),      
      missing = colDef(
        align = "center",
        width = 90,
        cell = function(value) alert_indicator(value)
      ),
      constant = colDef(
        align = "center",
        width = 90,
        cell = function(value) alert_indicator(value)
      ),
      identifier = colDef(
        align = "center",
        width = 90,
        cell = function(value) alert_indicator(value)
      ),
      zero = colDef(
        align = "center",
        width = 90,
        cell = function(value) alert_indicator(value)
      ),    
      minus = colDef(
        name = "negative",
        align = "center",
        width = 90,
        cell = function(value) alert_indicator(value)
      ),
      outlier = colDef(
        align = "center",
        width = 90,
        cell = function(value) alert_indicator(value)
      )
    ),
    details = function(index) {
      variable <- tabs$variables[index]
      type <- tabs$types[index]
      
      suppressWarnings(
      unique_missing <- raws %>% 
        select(variables, missing_count, unique_count, zero_count, minus_count, 
               outlier_count) %>% 
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
          columns = list(
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
                    columns = list(Class = colDef(width = 150))
          )          
        
        shiny::tabsetPanel(
          shiny::tabPanel("Character Stats", stat_char,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Cardinality/Missing", unique_missing,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;")          
        )
      } else if (type %in% c("factor", "ordered")) {
        top_rank <- retactable_toprank(.data, variable, TRUE)
        
        # p_bar <- htmltools::plotTag({
        #   dlookr::plot_bar_category(.data, which(names(.data) == variable))
        # }, sprintf("A plot of the %s variable", variable), width = 600, 
        # height = 400)
        
        shiny::tabsetPanel(
            shiny::tabPanel("Distribution", top_rank, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;"),
            shiny::tabPanel("Cardinality/Missing", unique_missing, 
                            hr(style = "border-top: 1px solid black;"),
                            style = "padding-top:5px; padding-bottom:25px;")
          )
      } else if (type %in% c("integer", "numeric")) {
        # p_box <- htmltools::plotTag({
        #   dlookr::plot_box_numeric(.data, which(names(.data) == variable))
        # }, sprintf("A plot of the '%s' variable", variable), width = 500, 
        # height = 400)
        
        stats <- raws %>% 
          filter(variables %in% variable) %>%           
          select(min:max) %>% 
          mutate(mean = round(mean, 3)) %>% 
          reactable(fullWidth = FALSE)

        p_hist <- htmltools::plotTag({
          dlookr::plot_hist_numeric(.data, which(names(.data) == variable))
        }, sprintf("A plot of the %s variable", variable), width = 600, 
        height = 400)
          
        shiny::tabsetPanel(
          shiny::tabPanel("Distribution", p_hist, 
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Cardinality/Missing", unique_missing, 
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Statistics", stats, 
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
tab_outlier <- function(.data) {
  outlier_indicator <- function(value) {
    if (value %in% "Both") {
      args <- shiny::icon("arrows-alt-v", 
                          style = "color: rgb(255, 127, 42)")
    } else if (value %in% "Lower") {
      args <- shiny::icon("arrow-alt-circle-down", 
                          style = "color: rgb(255, 127, 42)")
    } else if (value %in% "Upper") {
      args <- shiny::icon("arrow-alt-circle-up", 
                          style = "color: rgb(255, 127, 42)")
    } 
    
    div(args, paste("", value))
  }
  
  N <- nrow(.data)
  
  list_outlier <- diagnose_numeric(.data) %>% 
    select(-mean, -median, -zero, -minus) %>% 
    mutate(rate_outlier = outlier / N) %>% 
    filter(outlier > 0)
  
  out_position <- list_outlier$variables %>%  
    purrr::map_df(function(x){
      outs <- boxplot.stats(.data[, x])$out
      
      flag_lower <- list_outlier %>% 
        filter(variables %in% x) %>% 
        select(Q1) %>% 
        pull() %>% 
        ">"(outs) %>% 
        any()
      
      flag_upper <- list_outlier %>% 
        filter(variables %in% x) %>% 
        select(Q3) %>% 
        pull() %>% 
        "<"(outs) %>% 
        any()
      
      position <- ifelse(flag_lower & flag_upper, "Both", 
                         ifelse(flag_lower, "Lower", "Upper"))
      data.frame(variables = x, pos_outlier = position)
    })
  
  tabs <- list_outlier %>% 
    left_join(out_position, by = "variables")
  
  tabs %>% 
    reactable(
      defaultPageSize = nrow(tabs),
      columns = list(
        rate_outlier = colDef(name = "outliers (%)",
                              format = colFormat(percent = TRUE, digits = 1)),
        outlier = colDef(name = "outliers"),
        pos_outlier = colDef(name = "position",
          cell = function(value) outlier_indicator(value)
        )    
      ),
      details = function(index) {
        variable <- tabs$variables[index]
        
        diagn_outlier <- .data %>%
          diagnose_outlier(variable)
        
        cols <- c("Outliers count", "Outliers ratio (%)", "Mean of outliers",
                  "Mean with outliers", "Mean without outliers")
        outlier_df <- data.frame(Measures = cols,
                                 Values = as.vector(t(diagn_outlier[, -1]))) 
        
        values <- outlier_df$Values
        
        outlier_df$Values[1] <- round(values[1]) %>% 
          as.character()
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
                    columns = list(
                      Measures = colDef(width = 200),
                      Values = colDef(align = "right")
                    ))
        
        p_outlier <- htmltools::plotTag({
          dlookr::plot_outlier(.data, variable)
        }, sprintf("A plot of the %s variable", variable), width = 600,
        height = 400)
        
        shiny::tabsetPanel(
          shiny::tabPanel("Statistics", outlier_df,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;"),
          shiny::tabPanel("Distirubution", p_outlier,
                          hr(style = "border-top: 1px solid black;"),
                          style = "padding-top:5px; padding-bottom:25px;")
        )
      }
    )  
} 

