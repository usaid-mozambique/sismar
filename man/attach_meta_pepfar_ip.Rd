% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attach_meta.R
\name{attach_meta_pepfar_ip}
\alias{attach_meta_pepfar_ip}
\title{Anexar: Metadados do apoio dos parceiros PEPFAR}
\usage{
attach_meta_pepfar_ip(df)
}
\arguments{
\item{df}{Quadro de dados limpo através de \code{process_sisma_export}}
}
\value{
Quadro de dados contendo coluna que indica o apoio dos parceiros PEPFAR
}
\description{
\code{attach_meta_pepfar_ip} Une informações sobre o apoio dos parceiros PEPFAR gravadas no pacote \code{sismar} ao objeto de dados introduzido pelo utilizador
}
\examples{
 \dontrun{

 df <- attach_meta_pepfar_ip(df)}
}
