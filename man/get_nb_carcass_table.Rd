% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_data_exploration.R
\name{get_nb_carcass_table}
\alias{get_nb_carcass_table}
\title{Get carcass number summary table}
\usage{
get_nb_carcass_table(tab, ...)
}
\arguments{
\item{tab}{the table with fat data}

\item{...}{variable/column we want to group by i.e. get the number of carcasses (sample size) per (e.g.) per cluster_month, season or species}
}
\value{
return a table with the number of carcasses for each variable of choice
}
\description{
Get carcass number summary table
}
