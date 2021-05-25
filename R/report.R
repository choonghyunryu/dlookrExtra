#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print
#' @importFrom knitr image_uri
#' @export
diagnose_paged <- function(.data, output_format = c("html", "pdf"),
                           theme = "orange", title = "Data Diagnosis Report",
                           subtitle = deparse(substitute(.data)),
                           abstract_title = "Report Overview", abstract = NULL,
                           thres_uniq_cat = 0.5, thres_uniq_num = 0.1,
                           flag_content_zero = TRUE, flag_content_minus = TRUE,
                           cover_img = NULL, logo_img = NULL, output_file = NULL, 
                           output_dir = tempdir(), browse = TRUE, ...) {
  output_format <- match.arg(output_format)
  
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  
  path <- output_dir

  rmd <- "diagnosis_report_temp.Rmd"
  html <- "diagnosis_report_temp.html"  
  cover <- "cover.jpg"
  logo <- "dlookr.svg"  
    
  if (is.null(output_file))
    output_file <- "Diagnosis_Report.pdf"
    
  #--Copy files ----------------------------------------------------------------
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookrExtra"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE)
  
  # copy cover image
  cover_file <- file.path(system.file(package = "dlookrExtra"), "report", cover)
  flag <- file.copy(from = cover_file, to = path, recursive = TRUE)
  
  # copy logo image
  logo_file <- file.path(system.file(package = "dlookrExtra"), "report", logo)
  flag <- file.copy(from = logo_file, to = path, recursive = TRUE)  
  
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
    base64_logo <- knitr::image_uri(paste(path, logo, sep = "/"))
  } else {
    base64_logo <- knitr::image_uri(logo_img)
  }
  rmd_content <- sub("\\$logo\\$", base64_logo, 
                     readLines(paste(path, rmd, sep = "/")))
  cat(rmd_content, file = paste(path, rmd, sep = "/"), sep = "\n")   

  #--Set contents  -------------------------------------------------------------  
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
  file.remove(paste(path, cover, sep = "/"))
  file.remove(paste(path, html, sep = "/"))  
  file.remove(paste(path, logo, sep = "/"))  
  
  if (browse & file.exists(paste(path, output_file, sep = "/"))) {
    browseURL(paste(path, output_file, sep = "/"))
  }
}
