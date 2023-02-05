#' EDA report of categorical variables
#' @description Generating an excel EDA report of categorical variables
#' @param x data.frame, tbl_df. Dataset to perform EDA on.
#' @param target_variable character. The name of the target variable.
#' @param positive character. The name of the positive class in the target variable
#' @param output_file character. The name of the excel file to be created.
#' @param output_dir character. The directory where the excel file to be created will be stored.
#' @param sample_percent numeric. Percentage of sample when analyzed by sample.
#' @param parallel logical. Whether to perform parallel processing, defaults to FALSE.
#' @param cores integer. The number of cores to use for parallel processing.
#' @param future_globals_maxsize numeric. Maximum amount of global memory to 
#' be used by individual cores in parallel processing.
#' @param base_family character. Font family name to use for visualization.
#' @param verbose logical. Whether to output job progress information, 
#' the default value is TRUE.
#' @details EDA information is output on one worksheet for all categorical 
#' variables except for the overall information of categorical variables.
#' Excel only supports 256 worksheets, so anything with more than 255 categorical 
#' variables produces only 255 pieces of information.
#' @seealso \code{\link{eda_numeric}}
#' @examples
#' \dontrun{
#' testdata <- dlookr::heartfailure
#' target_variable <- "death_event"
#' 
#' # single core processing
#' eda_category(testdata, target_variable, positive = "Yes")
#' 
#' # Generate reports with 30% sampled data
#' eda_category(testdata, target_variable, positive = "Yes", sample_percent = 30)
#' 
#' # parallel processing
#' eda_category(testdata, target_variable, positive = "Yes", parallel = TRUE,
#'              cores = 8)
#' }
#' 
#' @export
#' @import dplyr
#' @import ggplot2
#' @import openxlsx
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom progress progress_bar
#' @importFrom purrr map_df
#' @importFrom rlang sym
#' @importFrom Cairo CairoPNG
#' @importFrom janitor adorn_totals
#' @importFrom here here
#'  
eda_category <- function(x, target_variable, positive = "1", output_file = NULL, 
                         output_dir = getwd(), sample_percent = 100, parallel = FALSE, 
                         cores = parallel::detectCores() - 2,
                         future_globals_maxsize = 500 * 1024^2, base_family = NULL,
                         verbose = TRUE) {
  options(warn=-1)
  on.exit(options(warn=0))
  
  if (is.null(base_family)) {
    if (options("dlookr_offline") %>% unlist()) {
      base_family <- "Liberation Sans Narrow"      
    } else {
      base_family <- "Roboto Condensed"      
    }
  }
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  # Number of observations
  N <- NROW(x)
  # sampling with sample_percent
  if (sample_percent < 100) {
    N_sample <- ceiling(N * sample_percent / 100)
    idx <- sample(seq(N), size = N_sample)
    
    x <- x[idx, ]
  } 
  
  path <- output_dir
  
  if (is.null(output_file))
    output_file <- glue::glue("01_EDA_{target_variable}_category.xlsx")
  output_file <- paste(path, output_file, sep = "/")
  
  
  ## Extract categorical variable list
  var_cat <- x %>% 
    dlookr::find_class(type = "categorical", index = FALSE) %>% 
    setdiff(target_variable)
  
  if (length(var_cat) == 0) {
    stop("There are no categorical variables in the dataset. Change the character you want to analyze to a factor.")
  } 
  
  ## Define target_df class based on target variable
  targeted <- x %>% 
    mutate({{target_variable}} := factor(!! rlang::sym(target_variable))) %>%     
    dlookr::target_by(!!target_variable)
  
  ## Parallel processing parameter setting
  if (parallel) {
    future::plan(future::multisession, workers = cores)
    
    options(future.globals.maxSize = future_globals_maxsize)
    options(future.rng.onMisuse = "ignore")    
  }
  
  
  ## Create a relate class for each categorical variable
  if (verbose) {
    if (parallel) {
      cli::cli_alert_info("Aggregating data with parallel process...")
    } else {
      pb <- progress::progress_bar$new(
        format = "- Aggregating data [:bar] :percent eta: :eta",
        total = length(var_cat), clear = FALSE, width = 80)      
    }
  }  
  
  if (parallel) {
    results_cat <- var_cat %>% 
      furrr::future_map_dfr(
        function(x) {
          tab <- dlookr::relate(targeted, all_of(x))
          
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_levels <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()
          
          
          test <- summary(tab)
          
          statistic <- tibble::tibble(
            statistic = test$statistic,
            df = test$parameter,
            pvalue = test$p.value
          )
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = base_family) +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}")) +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_blank())
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),
            viz = list(p)
          )
        },
        .options = furrr::furrr_options(globals = c("targeted", "target_variable"))
      )    
  } else {
    results_cat <- var_cat %>% 
      purrr::map_df(
        function(x) {
          tab <- dlookr::relate(targeted, all_of(x))
          
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_levels <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()
          
          
          test <- summary(tab)
          
          statistic <- tibble::tibble(
            statistic = test$statistic,
            df = test$parameter,
            pvalue = test$p.value
          )
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = base_family) +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}")) +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_blank())
          
          if (verbose) {
            pb$tick()
          }  
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),
            viz = list(p)
          )
        }
      )      
  }  
  
  ## 독립성 검정 결과
  tab_independence <- results_cat %>% 
    NROW() %>% 
    seq() %>% 
    purrr::map_df(
      function(x) {
        tab <- data.frame(variables = results_cat$variables[x]) %>% 
          bind_cols(results_cat$statistics[x]) %>% 
          bind_cols(results_cat$diag[x])
      }  
    ) %>% 
    arrange(pvalue)
  
  ## Export to excel
  ## Create a new work book 
  wb <- openxlsx::createWorkbook()
  
  ## Create a new worksheet for print table
  sname <- "Categorical Variable_Overall"
  openxlsx::addWorksheet(wb, sheetName = sname, tabColour = "deepskyblue")
  
  ## Write data.frame to a new worksheet
  openxlsx::writeDataTable(wb, sheet = sname, tab_independence)
  
  tab_independence %>% 
    NROW() %>% 
    seq() %>% 
    purrr::walk(
      function(x) {
        if (!is.na(tab_independence$statistic[x])) {
          writeFormula(wb, sheet = sname, startRow = x + 1, startCol = 1,
                       x = makeHyperlinkString(
                         sheet = tab_independence$variables[x], row = 1, col = 1, 
                         text = tab_independence$variables[x]
                       )
          )  
        }
      }
    )
  
  setColWidths(wb, sheet = sname, cols = 1:4, widths = "auto")
  
  ## Create a temporary directory for creating image files
  dir.create(here::here("temp"))
  
  if (verbose) {
    pb <- progress_bar$new(
      format = "- Reporting result [:bar] :percent eta: :eta",
      total = NROW(results_cat), clear = FALSE, width = 80)
  }  
  
  ## Categorical variable separate sheet build loop
  results_cat %>% 
    NROW() %>% 
    seq() %>% 
    # "["(1:2) %>%
    purrr::walk(
      function(x) {
        origin_x <- x
        x <- which(results_cat$variables %in% tab_independence[x, "variables"])
        
        sheet_name <- results_cat$variables[x]
        openxlsx::addWorksheet(wb, sheetName = sheet_name)
        
        predictor_variable <- sheet_name
        
        tab <- results_cat[x, "tabs"] %>% 
          pull() %>% 
          "[["(1)
        
        writeFormula(wb, sheet = sheet_name, startRow = 1, startCol = 9,
                     x = makeHyperlinkString(
                       sheet = "Categorical Variable_Overall", row = origin_x + 1, col = 1, 
                       text = "Go to Categorical Variable_Overall Sheet"
                     )
        )  
        
        ## contingency table ---------------------------------------------------
        tab_contigency <- tab %>% 
          as_tibble() %>% 
          tidyr::pivot_wider(names_from = 2, values_from = "n") %>% 
          janitor::adorn_totals("row") %>% 
          janitor::adorn_totals("col")  
        
        title_str <- "<< Contingency Table >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 2)      
        writeDataTable(wb, sheet = sheet_name, tab_contigency, 
                       startCol = 1, startRow = 3, rowNames = FALSE)
        
        s <- createStyle(numFmt = "#,##0")
        addStyle(wb, sheet = sheet_name, style = s, rows = 4:6, cols = 2:(max(dim(tab)) + 2), 
                 gridExpand = TRUE)
        
        
        ## relative frequency contingency table --------------------------------
        tab_relate <- tab %>% 
          as_tibble() %>% 
          bind_rows(
            tab %>% 
              as_tibble() %>% 
              group_by_at(vars(matches(target_variable))) %>% 
              summarise(n = sum(n)) %>% 
              bind_cols(data.frame(predictor = "Total")) %>% 
              select(1, 3, 2) %>% 
              rename({{predictor_variable}} := predictor)
          ) %>% 
          group_by_at(vars(matches(predictor_variable))) %>% 
          mutate(pct = round(n / sum(n) * 100, 2)) %>% 
          tidyr::pivot_wider(-n, names_from = 2, values_from = "pct") 
        
        title_str <- "<< Contingency Table - Relative Frequencies >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 9)            
        writeDataTable(wb, sheet = sheet_name, tab_relate, 
                       startCol = 1, startRow = 10, rowNames = FALSE)
        
        
        ## 독립성검정  ---------------------------------------------------------    
        test <- summary(tab)
        ctest <- data.frame(
          `Formula` = glue::glue("{target_variable}~{predictor_variable}"),
          `Numberof Case` = test$n.cases,
          `Chisqure Statistic` = test$statistic,
          `df` = test$parameter,
          `p-value` = test$p.value
        )
        
        title_str <- "<< Independence test >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 15)            
        writeDataTable(wb, sheet = sheet_name, ctest, startCol = 1, startRow = 16, 
                       rowNames = FALSE)
        
        
        ## 시각화 --------------------------------------------------------------
        image_name <- here::here("temp", glue::glue("contigency_{runif(1)}.png"))
        
        contigency_plot <- results_cat[x, "viz"] %>%
          pull() %>%
          "[["(1)
        
        Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
        plot(contigency_plot)
        dev.off()
        
        title_str <- "<< Visualization >>"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 20)        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 6, height = 3.5,
                              startCol = 1, startRow = 21)
        
        total_ratio <- tab %>% 
          as_tibble() %>% 
          rename(target := any_of(target_variable)) %>%   
          group_by(target) %>%   
          summarise(n = sum(n)) %>% 
          summarise(positive_ratio = sum(ifelse(target == positive, n, 0)) / sum(n) * 100) %>% 
          pull()
        
        annotation <- data.frame(
          x = NCOL(tab),
          y = total_ratio,
          label = glue::glue("Total percentage({round(total_ratio, 2)})"))
        
        
        p_ratio <- tab %>% 
          as_tibble() %>% 
          rename(target := any_of(target_variable)) %>% 
          group_by_at(vars(matches(predictor_variable))) %>% 
          summarise(positive_ratio = round(sum(ifelse(target == positive, n, 0)) / sum(n) * 100, 2)) %>% 
          ggplot(aes_string(x = predictor_variable, y = "positive_ratio", group = 1)) + 
          geom_point(colour = "blue", size = 3) +
          geom_label(aes_string(label = "positive_ratio"), fill = "lightgray",
                     colour = "blue", size = 3.5, nudge_x = 0) +
          geom_line(colour = "darkgray", linetype = 2) +
          geom_hline(yintercept = total_ratio, color = "red") +
          ggrepel::geom_label_repel(aes(x = x, y = y, label = label), data = annotation) +
          labs(title = glue::glue("{target_variable} ~ {predictor_variable}"),
               y = glue::glue("{target_variable} percentage")) +
          hrbrthemes::theme_ipsum(base_family = base_family)
        
        image_name <- here::here("temp", glue::glue("ratio_{runif(1)}.png"))
        
        Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
        print(p_ratio)
        dev.off()
        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 6, height = 3.5,
                              startCol = 8, startRow = 21)      
        
        if (verbose) {
          pb$tick()
        }  
      }
    )
  
  ## Save excel file
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  
  ## Remove image file
  unlink(here::here("temp"), recursive = TRUE)  
}



#' EDA report of numerical variables
#' @description EDA performance results for continuous variables are created as an excel file.
#' @param x data.frame, tbl_df. EDA를 수행할 데이터셋.
#' @param target_variable character. The name of the target variable.
#' @param positive character. The name of the positive class in the target variable
#' @param output_file character. The name of the excel file to be created.
#' @param output_dir character. The directory where the excel file to be created will be stored.
#' @param sample_percent numeric. Percentage of sample when analyzed by sample.
#' @param trim_quantile numeric. The upper and lower quantiles (%) to trim the 
#' data to remove outliers.
#' The default is NULL to generate a report of individual continuous variables 
#' for all data, i.e. [min, max].
#' In the case of a numeric vector of length 2, the lower and upper bounds are expressed as percentiles.
#' For example, in the case of c(0, 99), data in the range of [max, 99th percentile] is targeted, 
#' and in the case of c(5, 95), data in the range of [5th percentile, 95th percentile] 
#' are individually selected. Generates reports of continuous variables.
#' If this value is NA, reports are made for individual continuous variables for 
#' values in the range (Q1 - 1.5 * IQR, Q3 + 1.5 * IQR). 
#' In other words, a report of individual continuous variables is generated 
#' with the data with outliers removed.
#' @param parallel logical. Whether to perform parallel processing, defaults to FALSE.
#' @param cores integer. The number of cores to use for parallel processing.
#' @param future_globals_maxsize numeric. Maximum amount of global memory to 
#' be used by individual cores in parallel processing.
#' @param base_family character. Font family name to use for visualization.
#' @param verbose logical. Whether to output job progress information, 
#' the default value is TRUE.
#' @details EDA information is output on one worksheet for all numerical 
#' variables except for the overall information of numerical variables.
#' Excel only supports 256 worksheets, so anything with more than 255 numerical 
#' variables produces only 255 pieces of information.
#' @seealso \code{\link{eda_category}}
#' @examples
#' \dontrun{
#' testdata <- dlookr::heartfailure
#' target_variable <- "death_event"
#' 
#' # single core processing
#' eda_numeric(testdata, target_variable, positive = "Yes")
#' 
#' # Generate reports with 30% sampled data
#' eda_numeric(testdata, target_variable, positive = "Yes", sample_percent = 30)
#' 
#' # Create a report of individual variables with data with outliers removed
#' eda_numeric(testdata, target_variable, positive = "Yes", trim_quantile = NA)
#' 
#' # Create a report of individual variables with data with 5% of the upper data limit removed
#' eda_numeric(testdata, target_variable, positive = "Yes", trim_quantile = c(0, 95))
#' 
#' # parallel processing
#' eda_numeric(testdata, target_variable, positive = "Yes", parallel = TRUE,
#'             cores = 8)
#' }
#' 
#' @export
#' @import dplyr
#' @import ggplot2
#' @import openxlsx
#' @import funModeling
#' @importFrom future plan multisession
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom parallel detectCores
#' @importFrom broom tidy
#' @importFrom dlookr find_class target_by binning_by plot_outlier
#' @importFrom hrbrthemes theme_ipsum scale_fill_ipsum
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom Hmisc cut2
#' @importFrom purrr map_df
#' @importFrom Cairo CairoPNG
#' @importFrom ggrepel geom_label_repel
#' @importFrom tidyr pivot_wider
#' @importFrom janitor adorn_totals 
#' @importFrom rlang sym
#' @importFrom gridExtra grid.arrange
#' 
eda_numeric <- function(x, target_variable, positive = "1", output_file = NULL, 
                        output_dir = getwd(), sample_percent = 100, trim_quantile = NULL,
                        parallel = FALSE, cores = parallel::detectCores() - 2,
                        future_globals_maxsize = 500 * 1024^2, base_family = NULL,
                        verbose = TRUE) {
  options(warn=-1)
  on.exit(options(warn=0))
  
  if (is.null(base_family)) {
    if (options("dlookr_offline") %>% unlist()) {
      base_family <- "Liberation Sans Narrow"      
    } else {
      base_family <- "Roboto Condensed"      
    }
  }
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  # Number of observations
  N <- NROW(x)
  # sampling with sample_percent
  if (sample_percent < 100) {
    N_sample <- ceiling(N * sample_percent / 100)
    idx <- sample(seq(N), size = N_sample)
    
    x <- x[idx, ]
  } 
  
  path <- output_dir
  
  if (is.null(output_file))
    output_file <- glue::glue("01_EDA_{target_variable}_numeric.xlsx")
  output_file <- paste(path, output_file, sep = "/")
  
  
  ## Extract list of numeric variables
  var_num <- x %>% 
    dlookr::find_class(type = "numerical", index = FALSE) %>% 
    setdiff(target_variable)
  
  if (length(var_num) == 0) {
    stop("There are no continuous variables in the dataset.")
  } 
  
  ## Define target_df class based on target variable
  targeted <- x %>% 
    mutate({{target_variable}} := factor(!! rlang::sym(target_variable))) %>%    
    dlookr::target_by(!!target_variable)
  
  
  ## Parallel processing parameter setting
  if (parallel) {
    future::plan(future::multisession, workers = cores)
    
    options(future.globals.maxSize = future_globals_maxsize)
    options(future.rng.onMisuse = "ignore")    
  }
  
  ## Create a relate class for each continuous variable
  x_num <- x %>% 
    select(!!var_num, !!target_variable)
  
  if (verbose) {
    if (parallel) {
      cli::cli_alert_info("Aggregating data with parallel process...")
    } else {
      pb <- progress::progress_bar$new(
        format = "- Aggregating data [:bar] :percent eta: :eta",
        total = length(var_num), clear = FALSE, width = 80)      
    }
  }  
  
  if (parallel) {
    results_num <- var_num %>% 
      furrr::future_map_dfr(
        function(x) {
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_minus <- sum(pull(targeted[, x]) < 0, na.rm = TRUE)
          
          tab_diag$n_zero <- sum(pull(targeted[, x]) == 0, na.rm = TRUE)
          
          tab_diag$n_unique <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()   
          
          tab_diag$n_outlier <- dlookr::diagnose_outlier(ungroup(targeted), x) %>% "$"("outliers_cnt")        
          
          if (is.null(trim_quantile)) {
            targeted2 <- targeted
          } else if (is.na(trim_quantile)) {
            coef <- 1.5
            stats <- stats::fivenum(x_num[, x], na.rm = TRUE)
            iqr <- diff(stats[c(2, 4)])
            
            low <- stats[2L] - coef * iqr
            high <- stats[4L] + coef * iqr
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) > low) %>% 
              filter(!!rlang::sym(x) < high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))
          } else if (length(trim_quantile) == 2) {
            quantiles <- quantile(x_num[, x], probs = c(trim_quantile[1], trim_quantile[2]) / 100)
            low <- quantiles[1]
            high <- quantiles[2]
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) >= low) %>% 
              filter(!!rlang::sym(x) <= high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))            
          }
          
          tab <- dlookr::relate(targeted2, all_of(x))
          
          tryCatch(expr = {
            test <- t.test(glue::glue("{x} ~ {target_variable}") %>% 
                             as.formula(), data = x_num)
          },
          error = function(e) e)
          
          if (!exists("test")) {
            statistic <- NA
          } else {
            statistic <- broom::tidy(test)
          }
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = base_family) +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}"))
          
          p_out <- dlookr::plot_outlier(targeted2, x, base_family = base_family)
          
          if (verbose) {
            pb$tick()
          }  
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),        
            viz = list(p),
            viz_out = list(p_out)
          )
        },
        .options = furrr::furrr_options(globals = c("targeted", "x_num", "target_variable"))
      )
  } else {
    results_num <- var_num %>% 
      purrr::map_df(
        function(x) {
          n_missing <- targeted[, x] %>% 
            dplyr::pull() %>% 
            is.na() %>% 
            sum(na.rm = TRUE)
          tab_diag <- data.frame(n_missing = n_missing)
          
          tab_diag$n_minus <- sum(pull(targeted[, x]) < 0, na.rm = TRUE)
          
          tab_diag$n_zero <- sum(pull(targeted[, x]) == 0, na.rm = TRUE)
          
          tab_diag$n_unique <- targeted[, x] %>% 
            dplyr::pull() %>% 
            unique() %>% 
            length()   
          
          tab_diag$n_outlier <- dlookr::diagnose_outlier(ungroup(targeted), x) %>% "$"("outliers_cnt") 
          
          if (is.null(trim_quantile)) {
            targeted2 <- targeted
          } else if (is.na(trim_quantile)) {
            coef <- 1.5
            stats <- stats::fivenum(x_num[, x], na.rm = TRUE)
            iqr <- diff(stats[c(2, 4)])
            
            low <- stats[2L] - coef * iqr
            high <- stats[4L] + coef * iqr
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) > low) %>% 
              filter(!!rlang::sym(x) < high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))
          } else if (length(trim_quantile) == 2) {
            quantiles <- quantile(x_num[, x], probs = c(trim_quantile[1], trim_quantile[2]) / 100)
            low <- quantiles[1]
            high <- quantiles[2]
            
            targeted2 <- targeted %>% 
              filter(!!rlang::sym(x) >= low) %>% 
              filter(!!rlang::sym(x) <= high) 
            
            attr(targeted2, "type_y") <- attr(targeted, "type_y")
            class(targeted2) <- append("target_df", class(targeted2))              
          }
          
          tab <- dlookr::relate(targeted2, all_of(x))
          
          tryCatch(expr = {
            test <- t.test(glue::glue("{x} ~ {target_variable}") %>% 
                             as.formula(), data = x_num)
          },
          error = function(e) e)
          
          if (!exists("test")) {
            statistic <- NA
          } else {
            statistic <- broom::tidy(test)
          }
          
          p <- plot(tab) +
            hrbrthemes::theme_ipsum(base_family = base_family) +
            ggplot2::labs(title = glue::glue("{target_variable} ~ {x}"))
          
          p_out <- dlookr::plot_outlier(targeted2, x)
          
          if (verbose) {
            pb$tick()
          }  
          
          tibble::tibble(
            variables = x,
            diag = list(tab_diag),
            tabs = list(tab),
            statistics = list(statistic),        
            viz = list(p),
            viz_out = list(p_out)
          )
        }
      )
  }
  
  
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "- Binning data     [:bar] :percent eta: :eta",
      total = length(var_num), clear = FALSE, width = 80)      
  }   
  
  ## Binning
  results_num_bin <- var_num %>% 
    purrr::map_df(
      function(x) {
        discretize_rgr <- function (input, target, min_perc_bins = 0.1, max_n_bins = 5) {
          fpoints <- c()
          max_depth <- 20
          target <- as.character(target)
          min_n <- round(min_perc_bins * length(input))
          all_cuts <- funModeling:::recursive_gr_cuts_aux(input, target, fpoints, max_depth, min_n)
          max_n_bins <- max_n_bins - 1
          
          if (is.null(all_cuts)) {
            fpoints_top <- 1:max_n_bins
          } else {
            fpoints_top <- all_cuts[1:min(max_n_bins, length(all_cuts))]
          }
          
          fpoints_top_ord <- fpoints_top[order(fpoints_top)]
          input_bin <- Hmisc::cut2(input, cuts = c(fpoints_top_ord,
                                                   max(input)))
          return(input_bin)
        }
        
        binning_rgr <- function (.data, y, x, min_perc_bins = 0.1, max_n_bins = 5, ordered = TRUE) {
          y <- tidyselect::vars_select(names(.data), !!rlang::enquo(y))
          x <- tidyselect::vars_select(names(.data), !!rlang::enquo(x))
          
          if (tibble::is_tibble(.data)) {
            .data <- as.data.frame(.data)
          }
          
          uniq_y <- length(unique(.data[, y]))
          type_y <- class(.data[, y])[1]
          type_x <- class(.data[, x])[1]
          
          if (!is.data.frame(.data))
            stop("Data is not a data.frame.")
          
          if (!type_x %in% c("integer", "numeric"))
            stop("x is not numeric value.")
          
          if (uniq_y != 2) {
            stop("The number of levels of the y variable is not 2.")
          }
          
          if (!type_y %in% c("character", "factor", "ordered")) {
            stop("y is not character or (ordered)factor.")
          }
          
          if (any(is.na(.data[, x])))
            stop("x with a NA. This fuction not support missing.")
          
          if (any(is.na(.data[, y])))
            stop("y with a NA. This fuction not support missing.")
          
          if (length(unique(.data[, x])) < 5)
            stop("x must be number of unique values greater then 4.")
          
          if (requireNamespace("funModeling", quietly = TRUE)) {
            bins <- discretize_rgr(.data[, x], .data[, y],
                                   min_perc_bins = min_perc_bins, max_n_bins = max_n_bins)
          }
          else {
            stop("Package 'funModeling' needed for this function to work. Please install it.",
                 call. = FALSE)
          }
          
          bin_levels <- levels(bins)
          breaks <- gsub(pattern = "[]\\[)]", "", bin_levels)
          breaks_max <- gsub(pattern = "^[[:print:]]*,", "", breaks) %>%
            as.numeric() %>% max()
          breaks <- gsub(pattern = ",[[:print:]]*$", "", breaks) %>%
            as.numeric() %>% c(breaks_max)
          
          if (ordered == TRUE)
            bins <- ordered(bins)
          results <- bins
          attr(results, "type") <- "infogain"
          attr(results, "breaks") <- breaks
          attr(results, "levels") <- bin_levels
          attr(results, "raw") <- .data[, x]
          attr(results, "x_var") <- x
          attr(results, "y_var") <- y
          class(results) <- append("bins", class(results))
          attr(results, "target") <- .data[, y]
          class(results) <- append("infogain_bins", class(results))
          results
        }
        
        if (is.null(trim_quantile)) {
          targeted2 <- targeted
          x_num2 <- x_num
        } else if (is.na(trim_quantile)) {
          coef <- 1.5
          stats <- stats::fivenum(x_num[, x], na.rm = TRUE)
          iqr <- diff(stats[c(2, 4)])
          
          low <- stats[2L] - coef * iqr
          high <- stats[4L] + coef * iqr
          
          targeted2 <- targeted %>% 
            filter(!!rlang::sym(x) > low) %>% 
            filter(!!rlang::sym(x) < high) 
          
          attr(targeted2, "type_y") <- attr(targeted, "type_y")
          class(targeted2) <- append("target_df", class(targeted2))
          
          x_num2 <- x_num %>% 
            filter(!!rlang::sym(x) > low) %>% 
            filter(!!rlang::sym(x) < high) 
        } else if (length(trim_quantile) == 2) {
          quantiles <- quantile(x_num[, x], probs = c(trim_quantile[1], trim_quantile[2]) / 100)
          low <- quantiles[1]
          high <- quantiles[2]
          
          targeted2 <- targeted %>% 
            filter(!!rlang::sym(x) >= low) %>% 
            filter(!!rlang::sym(x) <= high) 
          
          attr(targeted2, "type_y") <- attr(targeted, "type_y")
          class(targeted2) <- append("target_df", class(targeted2))   
          
          x_num2 <- x_num %>% 
            filter(!!rlang::sym(x) >= low) %>% 
            filter(!!rlang::sym(x) <= high)           
        }
        
        tryCatch(expr = {
          bin_rgr <- x_num2 %>%
            dplyr::select(all_of(x), all_of(target_variable)) %>%
            dplyr::filter(complete.cases(.)) %>%
            binning_rgr(all_of(target_variable), all_of(x))
        },
        error = function(e) e)
        
        if (!exists("bin_rgr")) {
          bin_rgr <- NA
        }
        
        tryCatch(expr = {
          suppressWarnings(
            bin_opt <- x_num2 %>%
              dplyr::select(all_of(x), all_of(target_variable)) %>%
              dplyr::filter(complete.cases(.)) %>%
              dlookr::binning_by(all_of(target_variable), all_of(x))            
          )
        },
        error = function(e) e)
        
        if (!exists("bin_opt")) {
          bin_opt <- NA
        }
        
        if (verbose) {
          pb$tick()
        }  
        
        tibble::tibble(
          variables = x,
          bin_rgr = list(bin_rgr),
          bin_opt = list(bin_opt)
        )
      }
    )
  
  
  ## Merge statistics and bins
  results_num <- results_num %>% 
    inner_join(
      results_num_bin,
      by = "variables"
    )
  
  ## mean test results
  tab_mean_test <- results_num %>% 
    NROW() %>% 
    seq() %>% 
    purrr::map_df(
      function(x) data.frame(variables = results_num$variables[x],
                             stringsAsFactors = FALSE) %>% 
        bind_cols(results_num$statistics[x]) %>% 
        bind_cols(results_num$diag[x]) 
    ) %>% 
    arrange(p.value) %>% 
    select(variables:conf.high, n_missing:n_outlier) %>% 
    rename(`mean_difference_0_and_1` = estimate,
           `estimate_mean_0` = estimate1,
           `estimate_mean_1` = estimate2,
           `df`= parameter,
           `p_value`= p.value,
           `confidence_interval_low`= conf.low,
           `confidence_interval_high`= conf.high)
  
  
  ## Export to excel
  ## Create a new work book 
  wb <- openxlsx::createWorkbook()
  
  ## Create a new worksheet for print table
  sname <- "Numerical Variable_Overall"
  openxlsx::addWorksheet(wb, sheetName = sname, tabColour = "deepskyblue")
  
  ## Write data.frame to a new worksheet
  openxlsx::writeDataTable(wb, sheet = sname, tab_mean_test)
  
  tab_mean_test %>% 
    NROW() %>% 
    seq() %>% 
    purrr::walk(
      function(x) {
        if (!is.na(tab_mean_test$df[x])) {
          openxlsx::writeFormula(wb, sheet = sname, startRow = x + 1, startCol = 1,
                                 x = openxlsx::makeHyperlinkString(
                                   sheet = tab_mean_test$variables[x], row = 1, col = 1, 
                                   text = tab_mean_test$variables[x]
                                   )
          )  
        }
      }
    )
  
  setColWidths(wb, sheet = sname, cols = 1:4, widths = "auto")
  
  ## Create a temporary directory for creating image files
  dir.create(here::here("temp"))
  
  if (verbose) {
    pb <- progress_bar$new(
      format = "- Reporting result [:bar] :percent eta: :eta",
      total = NROW(results_num), clear = FALSE, width = 80)
  }  
  
  ## Numerical variable individual sheet generation loop
  if (is.null(trim_quantile)) {
    header_str <- " "
  } else if (is.na(trim_quantile)) {
    header_str <- "The distribution with outliers removed from the original data."
  } else if (length(trim_quantile) == 2) {
    low <- trim_quantile[1]
    high <- trim_quantile[2]
    
    header_str <- glue::glue("Distribution of data in the raw data [{low}, {high}] percentile range.") %>% 
      as.character()
  }
  
  results_num %>% 
    NROW() %>% 
    seq() %>% 
    purrr::walk(
      function(x) {
        origin_x <- x
        x <- which(results_num$variables %in% tab_mean_test[x, "variables"])
        
        sheet_name <- results_num$variables[x]
        openxlsx::addWorksheet(wb, sheetName = sheet_name)
        
        predictor_variable <- sheet_name
        
        tab <- results_num[x, "tabs"] %>% 
          pull() %>% 
          "[["(1)
        
        writeFormula(wb, sheet = sheet_name, startRow = 1, startCol = 9,
                     x = makeHyperlinkString(
                       sheet = "Numerical Variable_Overall", row = origin_x + 1, col = 1, 
                       text = "Go to Numerical Variable_Overall Sheet"
                     )
        )  
        
        ## Header for trimed information
        writeData(wb, sheet = sheet_name, header_str, startCol = 1, startRow = 1)   
        
        s <- createStyle(fontColour = "#0000FF", halign = "left", textDecoration = "bold")
        addStyle(wb, sheet = sheet_name, style = s, rows = 1, cols = 1, 
                 gridExpand = TRUE)
        
        ## Distributions
        title_str <- "1. Distributions"      
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = 2)   
        
        ## Statistic -----------------------------------------------------------
        subtitle_str <- "<< Statistic >>"
        writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, startRow = 3)      
        writeDataTable(wb, sheet = sheet_name, tab, 
                       startCol = 1, startRow = 4, rowNames = FALSE)
        
        s <- createStyle(numFmt = "#,##0")
        addStyle(wb, sheet = sheet_name, style = s, rows = 5:7, cols = 3, 
                 gridExpand = TRUE)
        
        ## visualization -------------------------------------------------------
        image_name <- here::here("temp", glue::glue("density_{runif(1)}.png")) 
        
        density_plot <- results_num[x, "viz"] %>%
          pull() %>%
          "[["(1)
        
        Cairo::CairoPNG(filename = image_name, width = 800, height = 500)
        plot(density_plot)
        dev.off()
        
        subtitle_str <- "<< Visualization of distribution >>"
        writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, startRow = 10)        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 8, height = 5,
                              startCol = 1, startRow = 11)
        
        ## Binning by recursive information gain ratio maximization
        row_bir <- 37
        title_str <- "2. Binning by recursive information gain ratio maximization"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = row_bir)
        
        ## Frequency table with RIR --------------------------------------------
        bin_rgr <- results_num[x, "bin_rgr"] %>%
          pull() %>%
          "[["(1)
        
        if (length(bin_rgr) == 1) {
          subtitle_str <- "A distribution that is not binned."
          
          s <- createStyle(fontColour = "#FF0000", halign = "left", textDecoration = "bold")
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_bir + 1)    
          addStyle(wb, sheet = sheet_name, style = s, rows = row_bir + 1, cols = 1, 
                   gridExpand = TRUE)        
          
          row_bir_viz <- row_bir + 3
        } else {
          tab_rgr <- summary(bin_rgr) %>% 
            mutate(rate = round(rate * 100, 2)) %>% 
            rename(Frequency = freq,
                   `Rate(%)` = rate)
          
          subtitle_str <- "<< Frequency Table >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, startRow = row_bir + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_rgr,
                         startCol = 1, startRow = row_bir + 2, rowNames = FALSE)
          
          conditionalFormatting(wb, sheet = sheet_name, cols = 2, 
                                rows = (row_bir + 3):(row_bir + 4 + NROW(tab_rgr)), 
                                type = "databar") 
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, 
                   rows = (row_bir + 3):(row_bir + 4 + NROW(tab_rgr)), cols = 2, 
                   gridExpand = TRUE)
          
          ## 시각화 ------------------------------------------------------------
          row_bir_viz <- row_bir + 5 + NROW(tab_rgr)
          
          image_name <- here::here("temp", glue::glue("binn_rir_{runif(1)}.png")) 
          
          Cairo::CairoPNG(filename = image_name, width = 1000, height = 700)
          plot(bin_rgr)
          dev.off()
          
          subtitle_str <- "<< Visualization of distribution >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_bir_viz)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 7.85, height = 5.5,
                                startCol = 1, startRow = row_bir_viz + 1)
          
          ## Binning Information -----------------------------------------------
          tab <- tibble::tibble(x_var = attr(bin_rgr, "raw"), 
                                y_var = attr(bin_rgr, "target")) %>% 
            mutate(x_var = cut(x_var, breaks = unique(attr(bin_rgr, "breaks")), right = FALSE)) %>% 
            rename({{predictor_variable}} := x_var) %>% 
            rename({{target_variable}} := y_var) %>% 
            table() %>% 
            t()
          
          tab_contigency <- tab %>% 
            as_tibble() %>% 
            tidyr::pivot_wider(names_from = 2, values_from = "n") %>% 
            janitor::adorn_totals("row") %>% 
            janitor::adorn_totals("col")  
          
          subtitle_str <- "<< Contingency table >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, startRow = row_bir + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_contigency,
                         startCol = 11, startRow = row_bir + 2, rowNames = FALSE)
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, rows = (row_bir + 3):(row_bir + 5), 
                   cols = 12:(12 + NCOL(tab) + 2), 
                   gridExpand = TRUE)
          
          tab_relate <- tab %>% 
            as_tibble() %>% 
            bind_rows(
              tab %>% 
                as_tibble() %>% 
                group_by_at(vars(matches(target_variable))) %>% 
                summarise(n = sum(n)) %>% 
                bind_cols(data.frame(predictor = "Total")) %>% 
                select(1, 3, 2) %>% 
                rename({{predictor_variable}} := predictor)
            ) %>% 
            group_by_at(vars(matches(predictor_variable))) %>% 
            mutate(pct = round(n / sum(n) * 100, 2)) %>% 
            tidyr::pivot_wider(-n, names_from = 2, values_from = "pct") 
          
          subtitle_str <- "<< Contingency Table - Relative Frequencies >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, startRow = row_bir + 8)    
          writeDataTable(wb, sheet = sheet_name, tab_relate,
                         startCol = 11, startRow = row_bir + 9, rowNames = FALSE)
          
          test <- summary(tab)
          ctest <- data.frame(
            `Formula` = glue::glue("{target_variable}~{predictor_variable}"),
            `Numberof Case` = test$n.cases,
            `Chisqure Statistic` = test$statistic,
            `df` = test$parameter,
            `p-value` = test$p.value
          )
          
          subtitle_str <- "<< Independence test >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, startRow = row_bir + 14)    
          writeDataTable(wb, sheet = sheet_name, ctest,
                         startCol = 11, startRow = row_bir + 15, rowNames = FALSE)        
          
          data <- as.data.frame(tab) %>% 
            select(a = 2, b = 1, n = Freq) 
          
          first <- data[1, 1] %>% as.character
          
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
          
          suppressWarnings(
            contigency_plot <- data %>% 
              group_by(a) %>% 
              mutate(x_width = sum(n)) %>% 
              ggplot(aes(x = a, y = n)) +
              geom_col(aes(width = x_width, fill = b),
                       color = "white", size = 2, 
                       position = position_fill(reverse = FALSE)) +
              facet_grid(~ a, space = "free", scales = "free", switch = "x") +
              scale_x_discrete(name = predictor_variable) +
              scale_y_continuous(name = target_variable, breaks = y_pos, labels = y_lab) +
              labs(title = sprintf("%s ~ %s", target_variable, predictor_variable)) +
              theme(legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    strip.background = element_blank(),
                    panel.spacing = unit(0, "pt")) +
              hrbrthemes::theme_ipsum(base_family = base_family) +
              hrbrthemes::scale_fill_ipsum(na.value = "grey80") +
              theme(legend.position = "none",
                    panel.grid.major.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 12),
                    axis.title.x = element_text(size = 12),
                    axis.title.y = element_text(size = 12),
                    panel.spacing = unit(0, "pt"))            
          )
          
          row_bir_viz <- row_bir + 5 + NROW(tab_rgr)
          
          image_name <- here::here("temp", glue::glue("contigency_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          plot(contigency_plot)
          dev.off()
          
          subtitle_str <- "<< Visualization of distribution >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 11, 
                    startRow = row_bir + 19)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 11, startRow = row_bir + 20)
          
          total_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>%   
            group_by(target) %>%   
            summarise(n = sum(n)) %>% 
            summarise(positive_ratio = sum(ifelse(target == positive, n, 0)) / sum(n) * 100) %>% 
            pull()
          
          annotation <- data.frame(
            x = NCOL(tab),
            y = total_ratio,
            label = glue::glue("Total percentage({round(total_ratio, 2)})"))
          
          
          p_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>% 
            rename(predictor := any_of(predictor_variable)) %>% 
            mutate(predictor = factor(predictor, levels = dimnames(tab)[[2]])) %>% 
            group_by(predictor) %>% 
            summarise(positive_ratio = round(sum(ifelse(target == positive, n, 0)) / sum(n) * 100, 2)) %>% 
            ggplot(aes(x = predictor, y = positive_ratio, group = 1)) + 
            geom_point(colour = "blue", size = 3) +
            ggrepel::geom_label_repel(aes(label = positive_ratio), colour = "blue", size = 3.5) +
            geom_line(colour = "darkgray", linetype = 2) +
            geom_hline(yintercept = total_ratio, color = "red") +
            ggrepel::geom_label_repel(aes(x = x, y = y, label = label), data = annotation) +
            labs(title = glue::glue("{target_variable} ~ {predictor_variable}"),
                 x = predictor_variable,
                 y = glue::glue("{target_variable} percentage")) +
            hrbrthemes::theme_ipsum(base_family = base_family)
          
          image_name <- here::here("temp", glue::glue("bin_ratio_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          print(p_ratio)
          dev.off()
          
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 18, startRow = row_bir + 20)   
          
        }
        
        
        ## Optimal Binning for Scoring Modeling
        row_opt <- row_bir_viz + ifelse(length(bin_rgr) == 1, 0, 30)
        
        title_str <- "3. Optimal Binning for Scoring Modeling"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = row_opt)
        
        ## Frequency table with RIR --------------------------------------------
        bin_opt <- results_num[x, "bin_opt"] %>%
          pull() %>%
          "[["(1)
        
        if (length(bin_opt) == 1) {
          subtitle_str <- "A distribution that is not binned."
          
          s <- createStyle(fontColour = "#FF0000", halign = "left", textDecoration = "bold")
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_opt + 1)    
          addStyle(wb, sheet = sheet_name, style = s, rows = row_opt + 1, cols = 1, 
                   gridExpand = TRUE) 
          
          row_out <- row_opt + 1 + 3
        } else {
          tab_opt <- attr(bin_opt, "performance") %>% 
            as.data.frame()
          
          subtitle_str <- "<< Frequency Table >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = row_opt + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_opt %>% as.data.frame(),
                         startCol = 1, startRow = row_opt + 2, rowNames = FALSE)
          
          # conditionalFormatting(wb, sheet = sheet_name, cols = 2, 
          #                       rows = (row_opt + 3):(row_opt + 1 + NROW(tab_opt)), 
          #                       type = "databar") 
          # conditionalFormatting(wb, sheet = sheet_name, cols = 7, 
          #                       rows = (row_opt + 3):(row_opt + 1 + NROW(tab_opt)), 
          #                       type = "databar")       
          # conditionalFormatting(wb, sheet = sheet_name, cols = 11, 
          #                       rows = (row_opt + 3):(row_opt + 1 + NROW(tab_opt)), 
          #                       type = "databar", style = "#FFA500")       
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, 
                   rows = (row_opt + 3):(row_opt + 2 + NROW(tab_opt)), cols = 2:6, 
                   gridExpand = TRUE)
          
          ## 시각화 ------------------------------------------------------------
          image_name <- here::here("temp", glue::glue("binn_opt_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 1200, height = 700)
          plot(bin_opt)
          dev.off()
          
          subtitle_str <- "<< Visualization of distribution >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                    startRow = (row_opt + 4 + NROW(tab_opt)) + 1)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 9.43, height = 5.5, startCol = 1, 
                                row_opt + 4 + NROW(tab_opt) + 2)        
          
          row_out <- row_opt + 4 + NROW(tab_opt) + 2 + 29
          
          ## Binning Information -----------------------------------------------
          tab <- tibble::tibble(x_var = attr(bin_opt, "raw"), 
                                y_var = attr(bin_opt, "target")) %>% 
            mutate(x_var = cut(x_var, breaks = unique(attr(bin_opt, "breaks")))) %>% 
            rename({{predictor_variable}} := x_var) %>% 
            rename({{target_variable}} := y_var) %>% 
            table() %>% 
            t()
          
          
          tab_contigency <- tab %>% 
            as_tibble() %>% 
            tidyr::pivot_wider(names_from = 2, values_from = "n") %>% 
            janitor::adorn_totals("row") %>% 
            janitor::adorn_totals("col")  
          
          subtitle_str <- "<< Contingency table >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, 
                    startRow = row_opt + 1)    
          writeDataTable(wb, sheet = sheet_name, tab_contigency,
                         startCol = 18, startRow = row_opt + 2, rowNames = FALSE)
          
          s <- createStyle(numFmt = "#,##0")
          addStyle(wb, sheet = sheet_name, style = s, rows = (row_opt + 3):(row_opt + 5), 
                   cols = 19:(19 + NCOL(tab) + 2), 
                   gridExpand = TRUE)
          
          tab_relate <- tab %>% 
            as_tibble() %>% 
            bind_rows(
              tab %>% 
                as_tibble() %>% 
                group_by_at(vars(matches(target_variable))) %>% 
                summarise(n = sum(n)) %>% 
                bind_cols(data.frame(predictor = "Total")) %>% 
                select(1, 3, 2) %>% 
                rename({{predictor_variable}} := predictor)
            ) %>% 
            group_by_at(vars(matches(predictor_variable))) %>% 
            mutate(pct = round(n / sum(n) * 100, 2)) %>% 
            tidyr::pivot_wider(-n, names_from = 2, values_from = "pct") 
          
          subtitle_str <- "<< Contingency Table - Relative Frequencies >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, startRow = row_opt + 8)    
          writeDataTable(wb, sheet = sheet_name, tab_relate,
                         startCol = 18, startRow = row_opt + 9, rowNames = FALSE)
          
          test <- summary(tab)
          ctest <- data.frame(
            `Formula` = glue::glue("{target_variable}~{predictor_variable}"),
            `Numberof Case` = test$n.cases,
            `Chisqure Statistic` = test$statistic,
            `df` = test$parameter,
            `p-value` = test$p.value
          )
          
          subtitle_str <- "<< Independence test >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, startRow = row_opt + 14)    
          writeDataTable(wb, sheet = sheet_name, ctest,
                         startCol = 18, startRow = row_opt + 15, rowNames = FALSE)        
          
          data <- as.data.frame(tab) %>% 
            select(a = 2, b = 1, n = Freq) 
          
          first <- data[1, 1] %>% as.character
          
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
          
          suppressWarnings(
            contigency_plot <- data %>% 
              group_by(a) %>% 
              mutate(x_width = sum(n)) %>% 
              ggplot(aes(x = a, y = n)) +
              geom_col(aes(width = x_width, fill = b),
                       color = "white", size = 2, 
                       position = position_fill(reverse = FALSE)) +
              facet_grid(~ a, space = "free", scales = "free", switch = "x") +
              scale_x_discrete(name = predictor_variable) +
              scale_y_continuous(name = target_variable, breaks = y_pos, labels = y_lab) +
              labs(title = sprintf("%s ~ %s", target_variable, predictor_variable)) +
              theme(legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    strip.background = element_blank(),
                    panel.spacing = unit(0, "pt")) +
              hrbrthemes::theme_ipsum(base_family) +
              hrbrthemes::scale_fill_ipsum(na.value = "grey80") +
              theme(legend.position = "none",
                    panel.grid.major.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_text(size = 12),
                    axis.title.x = element_text(size = 12),
                    axis.title.y = element_text(size = 12),
                    panel.spacing = unit(0, "pt"))            
          )
          
          row_bir_viz <- row_opt + 5 + NROW(tab_opt)
          
          image_name <- here::here("temp", glue::glue("contigency_{runif(1)}.png"))  
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          plot(contigency_plot)
          dev.off()
          
          subtitle_str <- "<< Visualization of distribution >>"
          writeData(wb, sheet = sheet_name, subtitle_str, startCol = 18, 
                    startRow = row_opt + 19)        
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 18, startRow = row_opt + 20)
          
          total_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>%   
            group_by(target) %>%   
            summarise(n = sum(n)) %>% 
            summarise(positive_ratio = sum(ifelse(target == 1, n, 0)) / sum(n) * 100) %>% 
            pull()
          
          annotation <- data.frame(
            x = NCOL(tab),
            y = total_ratio,
            label = glue::glue("Total percentage({round(total_ratio, 2)})"))
          
          
          p_ratio <- tab %>% 
            as_tibble() %>% 
            rename(target := any_of(target_variable)) %>% 
            group_by_at(vars(matches(predictor_variable))) %>% 
            summarise(positive_ratio = round(sum(ifelse(target == 1, n, 0)) / sum(n) * 100, 2)) %>% 
            ggplot(aes_string(x = predictor_variable, y = "positive_ratio", group = 1)) + 
            geom_point(colour = "blue", size = 3) +
            ggrepel::geom_label_repel(aes_string(label = "positive_ratio"), colour = "blue", size = 3.5) +
            geom_line(colour = "darkgray", linetype = 2) +
            geom_hline(yintercept = total_ratio, color = "red") +
            ggrepel::geom_label_repel(aes(x = x, y = y, label = label), data = annotation) +
            labs(title = glue::glue("{target_variable} ~ {predictor_variable}"),
                 y = glue::glue("{target_variable} percentage")) +
            hrbrthemes::theme_ipsum(base_family = base_family)
          
          image_name <- here::here("temp", glue::glue("bin_ratio_{runif(1)}.png"))          
          
          Cairo::CairoPNG(filename = image_name, width = 700, height = 400)
          print(p_ratio)
          dev.off()
          
          openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                                width = 6, height = 3.5,
                                startCol = 25, startRow = row_opt + 20)   
        }
        
        title_str <- "4. Distribution with Information of Outlires"
        writeData(wb, sheet = sheet_name, title_str, startCol = 1, startRow = row_out)
        
        subtitle_str <- "<< Visualization of distribution >>"
        writeData(wb, sheet = sheet_name, subtitle_str, startCol = 1, 
                  startRow = row_out + 1)        
        
        image_name <- here::here("temp", glue::glue("plot_outlier_{runif(1)}.png"))          
        
        outlier_plot <- results_num[x, "viz_out"] %>%
          pull() %>%
          "[["(1)
        
        Cairo::CairoPNG(filename = image_name, width = 700, height = 400,
                        pointsize = 9)
        gridExtra::grid.arrange(outlier_plot[[1]])
        dev.off()
        
        openxlsx::insertImage(wb, sheet = sheet_name, image_name,
                              width = 9.43, height = 5.5, startCol = 1, 
                              row_out + 2)            
        
        if (verbose) {
          pb$tick()
        }        
      }
    )
  
  
  ## Save excel file
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  
  ## Remove image file
  unlink(here::here("temp"), recursive = TRUE)
}

