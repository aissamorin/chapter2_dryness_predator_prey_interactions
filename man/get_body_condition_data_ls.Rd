% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_data_exploration.R
\name{get_body_condition_data_ls}
\alias{get_body_condition_data_ls}
\title{Get 'larger sample' body condition data, i.e. keep subadult, adult & old individuals, with bones collected before 6 days or between 6 & 8 days}
\usage{
get_body_condition_data_ls(tab = clean_fat_data, save = FALSE)
}
\arguments{
\item{tab}{table containing cleaned fat data}

\item{save}{whether we want the outcome to be saved or not}
}
\value{
a table with mean fat rate
}
\description{
Get 'larger sample' body condition data, i.e. keep subadult, adult & old individuals, with bones collected before 6 days or between 6 & 8 days
}
