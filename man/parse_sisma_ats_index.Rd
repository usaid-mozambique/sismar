% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_sisma_ats_index.R
\name{parse_sisma_ats_index}
\alias{parse_sisma_ats_index}
\title{Create tidy dataframes for HIV Index-Case testing services}
\usage{
parse_sisma_ats_index(df)
}
\arguments{
\item{df}{A dataframe object output from \code{clean_sisma_csv}}
}
\value{
\code{parse_sisma_ats_index} returns a tidy object with 16 columns of
site metadata, indicator features and results
}
\description{
\code{parse_sisma_ats_index} produces a tidy dataframe from an object passed in by
\code{sisma_clean_csv}. It engineers useful data features such as sex, age,
indicator disaggregation, sub-group type, etc.
}
\examples{
 \dontrun{

 df <- parse_sisma_ats_index()}
}
