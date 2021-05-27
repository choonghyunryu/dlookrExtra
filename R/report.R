#' Reporting the information of data diagnosis
#'
#' @description The diagnose_paged() report the information for diagnosing
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
#' @param output_format report output type. Choose either "pdf" and "html".
#' "pdf" create pdf file by rmarkdown::render() and pagedown::chrome_print(). so, 
#' you needed Chrome web browser on computer.  
#' "html" create html file by rmarkdown::render().
#' @param output_file name of generated file. default is NULL.
#' @param output_dir name of directory to generate report file. default is tempdir().
#' @param browse logical. choose whether to output the report results to the browser.
#' @param title character. title of report. default is "Data Diagnosis Report".
#' @param subtitle character. subtitle of report. default is name of data.
#' @param subtitle character. subtitle of report. default is name of data.
#' @param abstract_title character. abstract title of report. default is 
#' "Report Overview".
#' @param abstract character. abstract of report. 
#' @param title_color character. color of title. default is "white".
#' @param subtitle_color character. color of title. default is "gold".
#' @param thres_uniq_cat numeric. threshold to use for "Unique Values - 
#' Categorical Variables". default is 0.5.
#' @param thres_uniq_num numeric. threshold to use for "Unique Values - 
#' Numerical Variables". default is 0.1.
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
#' @param ... arguments to be passed to methods.
#'
#' @examples
#' \dontrun{
#' # create pdf file. file name is DataDiagnosis_Report.pdf
#' diagnose_paged(heartfailure)
#' 
#' # create pdf file. file name is Diagn.pdf. and change cover image
#' # cover <- file.path(system.file(package = "dlookrExtra"), "report", "cover2.jpg")
#' # diagnose_paged(heartfailure, cover_img = cover, title_color = "gray",
#'     output_file = "Diagn.pdf")
#'
#' # create pdf file. file name is ./Diagn.pdf and not browse
#' # cover <- file.path(system.file(package = "dlookrExtra"), "report", "cover3.jpg")
#' # diagnose_paged(heartfailure, output_dir = ".", cover_img = cover, 
#' #    flag_content_missing = FALSE, output_file = "Diagn.pdf", browse = FALSE)
#' }
#' 
#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print
#' @importFrom knitr image_uri
#' @export
diagnose_paged <- function(.data, output_format = c("html", "pdf"),
                           output_file = NULL, output_dir = tempdir(),   
                           browse = TRUE, title = "Data Diagnosis Report",
                           subtitle = deparse(substitute(.data)),
                           abstract_title = "Report Overview", abstract = NULL,
                           title_color = "white", subtitle_color = "gold",
                           thres_uniq_cat = 0.5, thres_uniq_num = 0.1,
                           flag_content_zero = TRUE, flag_content_minus = TRUE,
                           flag_content_missing = TRUE, cover_img = NULL, 
                           logo_img = NULL, theme = "orange", ...) {
  output_format <- match.arg(output_format)
  
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  
  path <- output_dir

  rmd   <- "diagnosis_report_temp.Rmd"
  html  <- "diagnosis_report_temp.html"  
  cover <- "cover1.jpg"
  logo  <- "dlookr.svg"  
    
  if (is.null(output_file))
    output_file <- "Diagnosis_Report.pdf"
    
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
  rmd_content <- sub("\\$title_color\\$", title_color, readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store subtitle color
  rmd_content <- sub("\\$subtitle_color\\$", subtitle_color, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")
  
  # store threshold that is unique ratio for categorical
  rmd_content <- sub("\\$thres_uniq_cat\\$", thres_uniq_cat, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n") 
    
  # store threshold that is unique ratio for numerical
  rmd_content <- sub("\\$thres_uniq_num\\$", thres_uniq_num, 
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
  
  html_out <- rmarkdown::render(paste(path, rmd, sep = "/"))
  pagedown::chrome_print(html_out, output = paste(path, output_file, sep = "/"))

  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, html, sep = "/"))  
  
  if (browse & file.exists(paste(path, output_file, sep = "/"))) {
    browseURL(paste(path, output_file, sep = "/"))
  }
}
