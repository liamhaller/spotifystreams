% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_sheets.R
\name{update_sheets}
\alias{update_sheets}
\title{Update Sheets}
\usage{
update_sheets(list, updated_coulmn, sheet_id, auth_token)
}
\arguments{
\item{list}{Enter a list of dataframes that contain updated streaming metrics}

\item{updated_coulmn}{which column contains the updated metrics}

\item{sheet_id}{code that identifys google sheet}

\item{auth_token}{saved auth token to connnect to googlesheets}
}
\value{
nothing is returned, but external sheet is updated
}
\description{
Update Sheets
}
