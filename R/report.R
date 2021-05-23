#' @importFrom rmarkdown render
#' @importFrom pagedown chrome_print
#' @export
diagnose_paged <- function(.data, output_format = c("html", "pdf"),
                           title = "Data Diagnosis Report",
                           subtitle = deparse(substitute(.data)),
                           abstract_title = "Report Overview",
                           abstract = NULL,
                           output_file = NULL, output_dir = tempdir(), browse = TRUE, ...) {
  output_format <- match.arg(output_format)
  
  assign("reportData", as.data.frame(.data), .dlookrExtraEnv)
  
  path <- output_dir

  rmd <- "diagnosis_report_temp.Rmd"
  html <- "diagnosis_report_temp.html"  
  img <- "cover.jpg"
    
  if (is.null(output_file))
    output_file <- "Diagnosis_Report.pdf"
    
  # copy markdown
  rmd_file <- file.path(system.file(package = "dlookrExtra"), "report", rmd)
  flag <- file.copy(from = rmd_file, to = path, recursive = TRUE, )
  
  # copy cover image
  img_file <- file.path(system.file(package = "dlookrExtra"), "report", img)
  flag <- file.copy(from = img_file, to = path, recursive = TRUE, )
  
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
  
  html_out <- rmarkdown::render(paste(path, rmd, sep = "/"))
  pagedown::chrome_print(html_out, output = paste(path, output_file, sep = "/"))

  file.remove(paste(path, rmd, sep = "/"))
  file.remove(paste(path, img, sep = "/"))
  file.remove(paste(path, html, sep = "/"))  
  
  if (browse & file.exists(paste(path, output_file, sep = "/"))) {
    browseURL(paste(path, output_file, sep = "/"))
  }
}
