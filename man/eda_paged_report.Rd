% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{eda_paged_report}
\alias{eda_paged_report}
\title{Reporting the information of EDA}
\usage{
eda_paged_report(
  .data,
  target = NULL,
  output_format = c("pdf", "html")[1],
  output_file = NULL,
  output_dir = tempdir(),
  browse = TRUE,
  title = "EDA Report",
  subtitle = deparse(substitute(.data)),
  author = "dlookr",
  abstract_title = "Report Overview",
  abstract = NULL,
  title_color = "black",
  subtitle_color = "blue",
  cover_img = NULL,
  create_date = Sys.time(),
  logo_img = NULL,
  theme = c("orange", "blue")[1],
  sample_percent = 100,
  ...
)
}
\arguments{
\item{.data}{a data.frame or a \code{\link{tbl_df}}.}

\item{target}{character. target variable.}

\item{output_format}{report output type. Choose either "pdf" and "html".
"pdf" create pdf file by rmarkdown::render() and pagedown::chrome_print(). so, 
you needed Chrome web browser on computer.  
"html" create html file by rmarkdown::render().}

\item{output_file}{name of generated file. default is NULL.}

\item{output_dir}{name of directory to generate report file. default is tempdir().}

\item{browse}{logical. choose whether to output the report results to the browser.}

\item{title}{character. title of report. default is "Data Diagnosis Report".}

\item{subtitle}{character. subtitle of report. default is name of data.}

\item{author}{character. author of report. default is "dlookr".}

\item{abstract_title}{character. abstract title of report. default is 
"Report Overview".}

\item{abstract}{character. abstract of report.}

\item{title_color}{character. color of title. default is "black".}

\item{subtitle_color}{character. color of title. default is "blue".}

\item{cover_img}{character. name of cover image.}

\item{create_date}{Date or POSIXct, character. The date on which the report is generated. 
The default value is the result of Sys.time().}

\item{logo_img}{character. name of logo image on top right.}

\item{theme}{character. name of theme for report. support "orange" and "blue". 
default is "orange".}

\item{sample_percent}{numeric. Sample percent of data for performing Diagnosis. 
It has a value between (0, 100]. 100 means all data, and 5 means 5% of sample data.
This is useful for data with a large number of observations.}

\item{...}{arguments to be passed to methods.}
}
\description{
The eda_paged_report() paged report the information for EDA.
}
\details{
Generate generalized EDA report automatically. 
You can choose to output to pdf and html files.
This feature is useful for EDA of data with many variables, 
rather than data with fewer variables.

Create an  PDF through the Chrome DevTools Protocol. If you want to create PDF, 
Google Chrome or Microsoft Edge (or Chromium on Linux) must be installed prior to using this function.
If not installed, you must use output_format = "html".
}
\section{Reported information}{

The EDA process will report the following information:

\itemize{
  \item Overview
  \itemize{
    \item Data Structures 
    \item Job Informations
  } 
  \item Univariate Analysis
  \itemize{
    \item Descriptive Statistics
    \itemize{
      \item Numerical Variables
      \item Categorical Variables
    }
    \item Normality Test
  } 
  \item Bivariate Analysis
  \itemize{
    \item Compare Numerical Variables
    \item Compare Categorical Variables
  } 
  \item Multivariate Analysis
  \itemize{
    \item Correlation Analysis
    \itemize{
      \item Correlation Coefficient Matrix
      \item Correlation Plot
    }
  }
  \item Target based Analysis
  \itemize{
    \item Grouped Numerical Variables
    \item Grouped Categorical Variables
    \item Grouped Correlation
  }
}
}

\examples{
\dontrun{
# create the dataset
heartfailure2 <- dlookr::heartfailure
heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA

# create pdf file. file name is EDA_Paged_Report.pdf
# eda_paged_report(heartfailure2, sample_percent = 80)

# create pdf file. file name is EDA.pdf. and change cover image
# cover <- file.path(system.file(package = "dlookrExtra"), "report", "cover1.jpg")
# eda_paged_report(heartfailure2, cover_img = cover, title_color = "gray",
#   output_file = "EDA.pdf")

# create pdf file. file name is ./EDA.pdf and not browse
# cover <- file.path(system.file(package = "dlookrExtra"), "report", "cover3.jpg")
# eda_paged_report(heartfailure2, output_dir = ".", cover_img = cover, 
#   flag_content_missing = FALSE, output_file = "EDA.pdf", browse = FALSE)

# create pdf file. file name is EDA_Paged_Report.html
# eda_paged_report(heartfailure2, target = "death_event", output_format = "html")
}

}
