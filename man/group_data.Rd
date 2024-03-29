% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_aggregation.R
\name{group_data}
\alias{group_data}
\title{Group data}
\usage{
group_data(
  raw_data,
  grouping_paras = c("lci_method", "model", "process", "unit"),
  grouping_function = "sum",
  summarise_col = "quantity"
)
}
\arguments{
\item{raw_data}{as retrieved by function import_rawdata()}

\item{grouping_paras}{cleaned column names used for grouping. 
(default: c("lci_method", "model", "process", "unit"))}

\item{grouping_function}{R function used for grouping (default: "sum")}

\item{summarise_col}{column name used for summarising (default: "quantity"), 
i.e. for which the "grouping_function" should be applied}
}
\value{
aggregated data according
}
\description{
Group data
}
\examples{
zipfile <- system.file("extdata/umberto-nxt_v7.1.0.13.503/Beispiel_Auswertung.zip", 
package = "kwb.umberto")
umberto7_csv_dir <- file.path(tempdir(), "Beispiel_Auswertung")
unzip(zipfile, exdir = umberto7_csv_dir)
umberto7_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto7_csv_dir)
umberto7_data_grouped <- kwb.umberto::group_data(umberto7_rawdata)
head(umberto7_data_grouped )

umberto10_csv_dir <- system.file("extdata/umberto-lca+_v10.1.0.3.146", 
package = "kwb.umberto")
umberto10_rawdata <- kwb.umberto::import_rawdata(csv_dir = umberto10_csv_dir)
umberto10_data_grouped <- kwb.umberto::group_data(umberto10_rawdata)
head(umberto10_data_grouped)
}
