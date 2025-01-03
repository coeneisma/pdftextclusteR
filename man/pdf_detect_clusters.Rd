% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_detect_clusters.R
\name{pdf_detect_clusters}
\alias{pdf_detect_clusters}
\title{Detect Columns and Text Boxes in PDF Document}
\usage{
pdf_detect_clusters(pdf_data, algorithm = "dbscan", ...)
}
\arguments{
\item{pdf_data}{result of the \code{\link[pdftools:pdftools]{pdftools::pdf_data()}}-function or a page of
this result.}

\item{algorithm}{the algorithm to be used to detect text columns or text
boxes}

\item{...}{algorithm-specific arguments. See \code{\link[dbscan:dbscan]{dbscan::dbscan()}}, \code{\link[dbscan:jpclust]{dbscan::jpclust()}}, \code{\link[dbscan:sNNclust]{dbscan::sNNclust()}} and \code{\link[dbscan:hdbscan]{dbscan::hdbscan()}} for more information}
}
\value{
If the input is a list of pages, a list-object is returned, where
each page contains a tibble and each word is assigned to a cluster. If the
input is a single page, a tibble is returned directly, with each word
assigned to a cluster.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function detects columns and text boxes in a PDF file. To do this, you
first need to read the file using the \code{\link[pdftools:pdftools]{pdftools::pdf_data()}}-function from
the \link{pdftools} package.

The function works on both a list of pages, as returned by the
\code{\link[pdftools:pdftools]{pdftools::pdf_data()}} function, and individual pages extracted from that
list. This makes it flexible for use on either the entire document or
specific pages within it.

This package directly utilizes the clustering algorithms implemented in the
\link{dbscan} package. For this a \code{\link[stats:dist]{stats::dist()}} object is created.
}
\examples{
# First 3 pages
head(npo, 3) |>
   pdf_detect_clusters()

# 3th page with sNNclust algorithm with minPts = 5
npo[[3]] |>
   pdf_detect_clusters(algorithm = "sNNclust", minPts = 5)
}
