#' Reporting the information of data diagnosis with html
#'
#' @description The diagnose_report() report the information for diagnosing
#' the quality of the data.
#'
#' @details Generate generalized data diagnostic reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for diagnosing a data frame with a large number of variables
#' than data with a small number of variables.
#' 
#' The title and subtitle colors should be designated by the color name used in 
#' CSS of html, not the color defined in R. 
#' 
#' @section Reported information:
#' Reported from the data diagnosis is as follows.
#'
#' \itemize{
#'   \item Data Diagnosis
#'   \itemize{
#'     \item Overview 
#'     \itemize{
#'       \item Missing/Unique Values
#'     }
#'     \item Missing Values
#'     \itemize{
#'       \item List of Missing Values
#'       \item Visualization
#'     }
#'     \item Unique Values
#'     \itemize{
#'       \item Categorical Variables
#'       \item Numerical Variables
#'     }
#'   }
#'   \item Categorical Variable Diagnosis
#'   \itemize{
#'     \item Top Ranks
#'   }   
#'   \item Numerical Variable Diagnosis
#'   \itemize{
#'     \item Distribution
#'     \itemize{
#'       \item Zero Values
#'       \item Minus Values
#'     }
#'     \item Outliers
#'     \itemize{
#'       \item List of Outliers
#'       \item Individual Outliers
#'     }
#'   }
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "Data Diagnosis Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param title_color character. color of title. default is "white".
#' @param thres_uniq_cat numeric. threshold to use for "Unique Values - 
#' Categorical Variables". default is 0.5.
#' @param thres_uniq_num numeric. threshold to use for "Unique Values - 
#' Numerical Variables". default is 5.
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param logo_img character. name of logo image on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5% of sample data.
#' This is useful for data with a large number of observations.
#' @param ... arguments to be passed to methods.
#'
#' @examples
#' \dontrun{
#' # create pdf file. file name is Diagnosis_Paged_Report.pdf
#' diagnose_report(heartfailure)
#' 
#' # file name is Diagn.html. and change logo image
#' # logo <- file.path(system.file(package = "dlookrExtra"), "report", "R_logo_html.svg")
#' # diagnose_report(heartfailure, logo_img = logo, title_color = "black",
#'     output_file = "Diagn.html")
#'
#' # file name is ./Diagn_heartfailure.html, "blue" theme and not browse
#' # diagnose_report(heartfailure, output_dir = ".", author = "Choonghyun Ryu",
#' #    output_file = "Diagn_heartfailure.html", theme = "blue", browse = FALSE)
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom knitr image_uri
#' @export
diagnose_report <- function(.data, output_file = NULL, output_dir = tempdir(),   
                            browse = TRUE, title = "Data Diagnosis",
                            subtitle = deparse(substitute(.data)), author = "dlookr",
                            title_color = "gray", thres_uniq_cat = 0.5, 
                            thres_uniq_num = 5, logo_img = NULL, 
                            create_date = Sys.time(),
                            theme = c("orange", "blue")[1], 
                            sample_percent = 100, ...) {
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  assign("thres_uniq_cat", thres_uniq_cat, .dlookrExtraEnv)  
  assign("thres_uniq_num", thres_uniq_num, .dlookrExtraEnv) 
  assign("sample_percent", sample_percent, .dlookrExtraEnv)  
  assign("author", author, .dlookrExtraEnv)  
  
  path <- output_dir
  
  rmd   <- "diagnosis_temp.Rmd"
  header <- "header_temp.html"
  logo  <- "dlookr_html.svg"  
  
  if (is.null(output_file))
    output_file <- "Diagnosis_Report.html"
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookrExtra"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  # copy header  
  header_file <- file.path(system.file(package = "dlookrExtra"), "report", header)
  flag <- file.copy(from = header_file, to = path, recursive = TRUE)  
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- gsub("\\$theme\\$", theme, 
                      readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  if (theme == "orange") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightorange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-orange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  } else if (theme == "blue") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightblue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-blue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")        
  }   

  # store title
  header_content <- sub("\\$title\\$", title, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store subtitle
  header_content <- sub("\\$subtitle\\$", subtitle, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store the menus
  menus <- "<li><a href=\"#ID-h1-overview\">Overview</a></li>
            <li><a href=\"#ID-h1-missing\">Missing Values</a></li>	
            <li><a href=\"#ID-h1-uniq-value\">Unique Values</a></li>	
            <li><a href=\"#ID-h1-outlier\">Outliers</a></li>
            <li><a href=\"#ID-h1-sample\">Samples</a></li>"
  header_content <- sub("\\$menu\\$", menus, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", deparse(substitute(.data)), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookrExtra"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  header_content <- sub("\\$logo\\$", base64_logo,
                     readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # rendering
  rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)

  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, header, sep = "/"))  
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}


#' Reporting the information of data diagnosis
#'
#' @description The diagnose_paged_report() paged report the information 
#' for diagnosing the quality of the data.
#'
#' @details Generate generalized data diagnostic reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for diagnosing a data frame with a large number of variables
#' than data with a small number of variables.
#' 
#' The title and subtitle colors should be designated by the color name used in 
#' CSS of html, not the color defined in R. 
#' 
#' @section Reported information:
#' Reported from the data diagnosis is as follows.
#'
#' \itemize{
#'   \item Overview
#'   \itemize{
#'     \item Data Structures 
#'     \item Warnings
#'     \item Variables
#'   } 
#'   \item Missing Values
#'   \itemize{
#'     \item List of Missing Values
#'     \item Visualization
#'   } 
#'   \item Unique Values
#'   \itemize{
#'     \item Categorical Variables
#'     \item Numerical Variables
#'   } 
#'   \item Categorical Variable Diagnosis
#'   \itemize{
#'      \item Top Ranks
#'   }
#'   \item Numerical Variable Diagnosis
#'   \itemize{
#'     \item Distribution
#'     \itemize{
#'       \item Zero Values
#'       \item Minus Values
#'     }
#'     \item Outliers
#'     \itemize{
#'       \item List of Outliers
#'       \item Individual Outliers
#'     }
#'   }
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by rmarkdown::render() and pagedown::chrome_print(). so, 
#' you needed Chrome web browser on computer.  
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "Data Diagnosis Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param abstract_title character. abstract title of report. default is 
#' "Report Overview".
#' @param abstract character. abstract of report. 
#' @param title_color character. color of title. default is "white".
#' @param subtitle_color character. color of title. default is "gold".
#' @param thres_uniq_cat numeric. threshold to use for "Unique Values - 
#' Categorical Variables". default is 0.5.
#' @param thres_uniq_num numeric. threshold to use for "Unique Values - 
#' Numerical Variables". default is 5.
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param flag_content_missing logical. whether to output "Missing Value" information. 
#' the default value is TRUE, and the information is displayed.
#' @param flag_content_zero logical. whether to output "Zero Values" information. 
#' the default value is TRUE, and the information is displayed.
#' @param flag_content_minus logical. whether to output "Minus Values" information. 
#' the default value is TRUE, and the information is displayed.
#' @param cover_img character. name of cover image. 
#' @param logo_img character. name of logo image on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing Diagnosis. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5% of sample data.
#' This is useful for data with a large number of observations.
#' @param ... arguments to be passed to methods.
#'
#' @examples
#' \dontrun{
#' # create pdf file. file name is Diagnosis_Paged_Report.pdf
#' diagnose_paged_report(heartfailure)
#' 
#' # create pdf file. file name is Diagn.pdf. and change cover image
#' # cover <- file.path(system.file(package = "dlookrExtra"), "report", "cover2.jpg")
#' # diagnose_paged_report(heartfailure, cover_img = cover, title_color = "gray",
#'     output_file = "Diagn.pdf")
#'
#' # create pdf file. file name is ./Diagn.pdf and not browse
#' # cover <- file.path(system.file(package = "dlookrExtra"), "report", "cover3.jpg")
#' # diagnose_paged_report(heartfailure, output_dir = ".", cover_img = cover, 
#' #    flag_content_missing = FALSE, output_file = "Diagn.pdf", browse = FALSE)
#' 
#' # create pdf file. file name is Diagnosis_Paged_Report.html
#' # diagnose_paged_report(heartfailure, output_format = "html")
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print
#' @importFrom knitr image_uri
#' @export
diagnose_paged_report <- function(.data, output_format = c("pdf", "html"),
                           output_file = NULL, output_dir = tempdir(),   
                           browse = TRUE, title = "Data Diagnosis Report",
                           subtitle = deparse(substitute(.data)), author = "dlookr",
                           abstract_title = "Report Overview", abstract = NULL,
                           title_color = "white", subtitle_color = "gold",
                           thres_uniq_cat = 0.5, thres_uniq_num = 5,
                           flag_content_zero = TRUE, flag_content_minus = TRUE,
                           flag_content_missing = TRUE, cover_img = NULL, 
                           create_date = Sys.time(),
                           logo_img = NULL, theme = c("orange", "blue")[1],
                           sample_percent = 100, ...) {
  output_format <- match.arg(output_format)
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  assign("thres_uniq_cat", thres_uniq_cat, .dlookrExtraEnv)  
  assign("thres_uniq_num", thres_uniq_num, .dlookrExtraEnv) 
  assign("sample_percent", sample_percent, .dlookrExtraEnv)  
  assign("author", author, .dlookrExtraEnv)  
  
  path <- output_dir
  
  rmd   <- "diagnosis_paged_temp.Rmd"
  html  <- "diagnosis_paged_temp.html"
  cover <- "cover1.jpg"
  logo  <- "dlookr.svg"  
  
  if (is.null(output_file))
    output_file <- paste("Diagnosis_Paged_Report", output_format, sep = ".")
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookrExtra"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- sub("\\$theme\\$", theme, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store title
  rmd_content <- sub("\\$title\\$", title, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle
  rmd_content <- sub("\\$subtitle\\$", subtitle, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store abstract-title
  rmd_content <- sub("\\$abstract_title\\$", abstract_title, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store abstract
  if (is.null(abstract)) {
    abstract <- sprintf("This report was created for an overview quality 
                        diagnosis of ***%s*** data. It was created for **the 
                        purpose of judging the validity of variables** before 
                        conducting EDA.", deparse(substitute(.data)))
  }
  rmd_content <- sub("\\$abstract\\$", abstract, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle color
  subtitle_color <- col2hex(subtitle_color)
  rmd_content <- sub("\\$subtitle_color\\$", subtitle_color, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", deparse(substitute(.data)), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookrExtra"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  rmd_content <- sub("\\$logo\\$", base64_logo, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store cover image
  if (is.null(cover_img)) {
    cover_file <- file.path(system.file(package = "dlookrExtra"), "report", cover)
    base64_cover <- knitr::image_uri(cover_file)
  } else {
    base64_cover <- knitr::image_uri(cover_img)
  }
  rmd_content <- sub("\\$cover\\$", base64_cover, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  
  #--Set contents  -------------------------------------------------------------  
  # missing value contents
  if (flag_content_missing) {
    rmd_content <- gsub("\\$content_missing\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$content_missing\\$[[:print:][:space:]]+\\$content_missing\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }
  
  # zero contents
  if (flag_content_zero) {
    rmd_content <- gsub("\\$content_zero\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$content_zero\\$[[:print:][:space:]]+\\$content_zero\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }
  
  # minus contents
  if (flag_content_minus) {
    rmd_content <- gsub("\\$content_minus\\$", "", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  } else {
    txt <- readLines(paste(path, rmd, sep = "/")) %>% 
      paste(collapse = "\n") 
    
    sub("\\$content_minus\\$[[:print:][:space:]]+\\$content_minus\\$", "", txt) %>% 
      cat(file = paste(path, rmd, sep = "/"))   
  }
  
  if (output_format == "pdf") {
    html_out <- rmarkdown::render(paste(path, rmd, sep = "/"))
    pagedown::chrome_print(html_out, output = output_file)
    
    file.remove(paste(path, html, sep = "/"))
  } else {
    rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  }
  
  file.remove(paste(path, rmd, sep = "/"))
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}



#' Reporting the information of EDA with html
#'
#' @description The eda_report() report the information for diagnosing
#' the quality of the data.
#'
#' @details Generate generalized data diagnostic reports automatically.
#' You can choose to output to pdf and html files.
#' This is useful for diagnosing a data frame with a large number of variables
#' than data with a small number of variables.
#' 
#' The title and subtitle colors should be designated by the color name used in 
#' CSS of html, not the color defined in R. 
#' 
#' @section Reported information:
#' Reported from the data diagnosis is as follows.
#'
#' \itemize{
#'   \item Data Diagnosis
#'   \itemize{
#'     \item Overview 
#'     \item Missing Values
#'     \itemize{
#'       \item List of Missing Values
#'       \item Visualization
#'     }
#'     \item Unique Values
#'     \itemize{
#'       \item Categorical Variables
#'       \item Numerical Variables
#'     }
#'   }
#'   \item Categorical Variable Diagnosis
#'   \itemize{
#'     \item Top Ranks
#'   }   
#'   \item Numerical Variable Diagnosis
#'   \itemize{
#'     \item Distribution
#'     \itemize{
#'       \item Zero Values
#'       \item Minus Values
#'     }
#'     \item Outliers
#'     \itemize{
#'       \item List of Outliers
#'       \item Individual Outliers
#'     }
#'   }
#' }
#'
#' @param .data a data.frame or a \code{\link{tbl_df}}.
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "Data Diagnosis Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param author character. author of report. default is "dlookr".
#' @param title_color character. color of title. default is "white".
#' @param create_date Date or POSIXct, character. The date on which the report is generated. 
#' The default value is the result of Sys.time().
#' @param logo_img character. name of logo image on top right.
#' @param theme character. name of theme for report. support "orange" and "blue". 
#' default is "orange".
#' @param sample_percent numeric. Sample percent of data for performing EDA. 
#' It has a value between (0, 100]. 100 means all data, and 5 means 5% of sample data.
#' This is useful for data with a large number of observations.
#' @param ... arguments to be passed to methods.
#'
#' @examples
#' \dontrun{
#' # create pdf file. file name is Diagnosis_Paged_Report.pdf
#' eda_report(heartfailure)
#' 
#' # file name is EDA.html. and change logo image
#' # logo <- file.path(system.file(package = "dlookrExtra"), "report", "R_logo_html.svg")
#' # eda_report(heartfailure, logo_img = logo, title_color = "black",
#'     output_file = "EDA.html")
#'
#' # file name is ./EDA_heartfailure.html, "blue" theme and not browse
#' # eda_report(heartfailure, output_dir = ".", author = "Choonghyun Ryu",
#' #    output_file = "EDA_heartfailure.html", theme = "blue", browse = FALSE)
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom knitr image_uri
#' @export
eda_report <- function(.data, target = NULL, output_file = NULL, 
                       output_dir = tempdir(), browse = TRUE, 
                       title = "EDA", subtitle = deparse(substitute(.data)), 
                       author = "dlookr", title_color = "gray", logo_img = NULL, 
                       create_date = Sys.time(), theme = c("orange", "blue")[1], 
                       sample_percent = 100, ...) {
  tryCatch(vars <- tidyselect::vars_select(names(.data),
                                           !! rlang::enquo(target)),
           error = function(e) {
             pram <- as.character(substitute(target))
             stop(sprintf("Column %s is unknown", pram))
           },
           finally = NULL)

  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  assign("targetVariable", vars, .dlookrExtraEnv)
  assign("sample_percent", sample_percent, .dlookrExtraEnv)  
  assign("author", author, .dlookrExtraEnv)  
  
  path <- output_dir
  
  rmd   <- "eda_temp.Rmd"
  header <- "header_temp.html"
  logo  <- "dlookr_html.svg"  
  
  if (is.null(output_file))
    output_file <- "EDA_Report.html"
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookrExtra"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  # copy header  
  header_file <- file.path(system.file(package = "dlookrExtra"), "report", header)
  flag <- file.copy(from = header_file, to = path, recursive = TRUE)  
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- gsub("\\$theme\\$", theme, 
                      readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  if (theme == "orange") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightorange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-orange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  } else if (theme == "blue") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightblue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-blue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")        
  }  
  
  # store title
  header_content <- sub("\\$title\\$", title, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store subtitle
  header_content <- sub("\\$subtitle\\$", subtitle, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store the menus
  disabled <- ifelse(is.null(target), 
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-univariate\">Univariate Analysis</a></li>	
                      <li><a href=\"#ID-h1-bivariate\">Bivariate Analysis</a></li>	
                      <li><a href=\"#ID-h1-multivariate\">Multivariate Analysis</a></li>
                      <li><a href=\"#\" class=\"disable-links\">Target based Analysis</a></li>",
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-univariate\">Univariate Analysis</a></li>	
                      <li><a href=\"#ID-h1-bivariate\">Bivariate Analysis</a></li>	
                      <li><a href=\"#ID-h1-multivariate\">Multivariate Analysis</a></li>
                      <li><a href=\"#ID-h1-target-based\">Target based Analysis</a></li>")
  header_content <- sub("\\$menu\\$", disabled, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")

  # store dataset
  rmd_content <- sub("\\$dataset\\$", deparse(substitute(.data)), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                        readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookrExtra"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  header_content <- sub("\\$logo\\$", base64_logo,
                        readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  #--Rendering------------------------------------------------------------------    
  rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  
  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, header, sep = "/"))  
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}


#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print
#' @importFrom knitr image_uri
#' @export
eda_paged_report <- function(.data, target = NULL, output_format = c("pdf", "html"),
                             output_file = NULL, output_dir = tempdir(),   
                             browse = TRUE, title = "EDA Report",
                             subtitle = deparse(substitute(.data)), author = "dlookr",
                             abstract_title = "Report Overview", abstract = NULL,
                             title_color = "white", subtitle_color = "gold",
                             cover_img = NULL, create_date = Sys.time(),
                             logo_img = NULL, theme = c("orange", "blue")[1],
                             sample_percent = 100, ...) {
  output_format <- match.arg(output_format)
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  
  tryCatch(vars <- tidyselect::vars_select(names(.data),
                                           !! rlang::enquo(target)),
           error = function(e) {
             pram <- as.character(substitute(target))
             stop(sprintf("Column %s is unknown", pram))
           },
           finally = NULL)
  
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  assign("targetVariable", vars, .dlookrExtraEnv)  
  assign("sample_percent", sample_percent, .dlookrExtraEnv)  
  assign("author", author, .dlookrExtraEnv)  
  
  path <- output_dir
  
  rmd   <- "eda_paged_temp.Rmd"
  html  <- "eda_paged_temp.html"
  cover <- "cover2.jpg"
  logo  <- "dlookr.svg"  
  
  if (is.null(output_file))
    output_file <- paste("EDA_Paged_Report", output_format, sep = ".")
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookrExtra"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- sub("\\$theme\\$", theme, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store title
  rmd_content <- sub("\\$title\\$", title, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle
  rmd_content <- sub("\\$subtitle\\$", subtitle, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store abstract-title
  rmd_content <- sub("\\$abstract_title\\$", abstract_title, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store abstract
  if (is.null(abstract)) {
    abstract <- sprintf("This report was created for the EDA of ***%s*** data. 
                        It helps explore data to **understand the data 
                        and find scenarios for performing the analysis.**", 
                        deparse(substitute(.data)))
  }
  rmd_content <- sub("\\$abstract\\$", abstract, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")  
  
  # store title color
  title_color <- col2hex(title_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle color
  subtitle_color <- col2hex(subtitle_color)
  rmd_content <- sub("\\$subtitle_color\\$", subtitle_color, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", deparse(substitute(.data)), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookrExtra"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  rmd_content <- sub("\\$logo\\$", base64_logo, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store cover image
  if (is.null(cover_img)) {
    cover_file <- file.path(system.file(package = "dlookrExtra"), "report", cover)
    base64_cover <- knitr::image_uri(cover_file)
  } else {
    base64_cover <- knitr::image_uri(cover_img)
  }
  rmd_content <- sub("\\$cover\\$", base64_cover, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")     
  
  if (output_format == "pdf") {
    html_out <- rmarkdown::render(paste(path, rmd, sep = "/"))
    pagedown::chrome_print(html_out, output = output_file)
    
    file.remove(paste(path, html, sep = "/"))
  } else {
    rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  }
  
  file.remove(paste(path, rmd, sep = "/"))
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}





#' @importFrom rmarkdown render
#' @importFrom knitr image_uri
#' @export
transformation_report <- function(.data, target = NULL, output_file = NULL, 
                       output_dir = tempdir(), browse = TRUE, 
                       title = "Transformation", subtitle = deparse(substitute(.data)), 
                       author = "dlookr", title_color = "gray", logo_img = NULL, 
                       create_date = Sys.time(), theme = c("orange", "blue")[1], 
                       sample_percent = 100, ...) {
  tryCatch(vars <- tidyselect::vars_select(names(.data),
                                           !! rlang::enquo(target)),
           error = function(e) {
             pram <- as.character(substitute(target))
             stop(sprintf("Column %s is unknown", pram))
           },
           finally = NULL)
  
  if (sample_percent > 100 | sample_percent <= 0) {
    stop("sample_percent must be a value between (0, 100].")
  }
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  assign("targetVariable", vars, .dlookrExtraEnv)
  assign("sample_percent", sample_percent, .dlookrExtraEnv)  
  assign("author", author, .dlookrExtraEnv)  
  
  path <- output_dir
  
  rmd   <- "transformation_temp.Rmd"
  header <- "header_temp.html"
  logo  <- "dlookr_html.svg"  
  
  if (is.null(output_file))
    output_file <- "Transformation_Report.html"
  output_file <- paste(path, output_file, sep = "/")
  
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookrExtra"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  # copy header  
  header_file <- file.path(system.file(package = "dlookrExtra"), "report", header)
  flag <- file.copy(from = header_file, to = path, recursive = TRUE)  
  
  #--Store parameters ----------------------------------------------------------  
  # store theme
  rmd_content <- gsub("\\$theme\\$", theme, 
                      readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  if (theme == "orange") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightorange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-orange)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")    
  } else if (theme == "blue") {
    rmd_content <- gsub("\\$customLightColor\\$", "var(--custom-lightblue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
    
    rmd_content <- gsub("\\$customColor\\$", "var(--custom-blue)", 
                        readLines(paste(path, rmd, sep = "/")))
    cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")        
  }  
  
  # store title
  header_content <- sub("\\$title\\$", title, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store subtitle
  header_content <- sub("\\$subtitle\\$", subtitle, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store title color
  subtitle_color <- col2hex(subtitle_color)
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store the menus
  disabled <- ifelse(is.null(target), 
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-imputation\">Imputation</a></li>	
                      <li><a href=\"#ID-h1-skewness\">Resolving Skewness</a></li>	
                      <li><a href=\"#ID-h1-binning\">Binning</a></li>	
                      <li><a href=\"#\" class=\"disable-links\">Optimal Binning</a></li>",
                     "<li><a href=\"#ID-h1-overview\">Overview</a></li>
                      <li><a href=\"#ID-h1-imputation\">Imputation</a></li>	
                      <li><a href=\"#ID-h1-skewness\">Resolving Skewness</a></li>	
                      <li><a href=\"#ID-h1-binning\">Binning</a></li>	
                      <li><a href=\"#ID-h1-optimal-binning\">Optimal Binning</a></li>")
  header_content <- sub("\\$menu\\$", disabled, readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  # store dataset
  rmd_content <- sub("\\$dataset\\$", deparse(substitute(.data)), 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store created date
  rmd_content <- sub("\\$date\\$", create_date, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   
  
  # store logo image
  if (is.null(logo_img)) {
    logo_file <- file.path(system.file(package = "dlookrExtra"), "report", logo)
    base64_logo <- knitr::image_uri(logo_file)
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  header_content <- gsub("\\$logo\\$", base64_logo,
                        readLines(paste(path, header, sep = "/")))
  cat(header_content, file = paste(path, header, sep = "/"), sep = "\n")
  
  #--Rendering------------------------------------------------------------------    
  rmarkdown::render(paste(path, rmd, sep = "/"), output_file = output_file)
  
  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, header, sep = "/"))  
  
  if (browse & file.exists(output_file)) {
    browseURL(output_file)
  }
}


