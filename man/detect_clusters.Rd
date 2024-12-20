% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect_clusters.R
\name{detect_clusters}
\alias{detect_clusters}
\title{Detect Columns and Text Boxes in PDF Document}
\usage{
detect_clusters(pdf_data, algoritme = "dbscan", ...)
}
\arguments{
\item{pdf_data}{list-object resulting from the
\code{\link[pdftools:pdftools]{pdftools::pdf_data()}}-function}

\item{...}{algorithm-specific arguments}

\item{algorithm}{the algorithm to be used to detect text columns
or text boxes}
}
\value{
A list-object, where each page contains a tibble and each
word is assigned to a cluster.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} This function detects columns
and text boxes in a PDF file. To do this, you first need to read the file
using the \code{\link[pdftools:pdftools]{pdftools::pdf_data()}}-function from the \link{pdftools} package.
}
\details{
In the background, the function
\code{\link[=detect_clusters_page]{detect_clusters_page()}} is used
}
\examples{
npo |>
   detect_clusters()
}
