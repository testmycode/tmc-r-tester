% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetAvailablePoints.R
\name{run_available_points}
\alias{run_available_points}
\title{Checks the available point for all tests}
\usage{
run_available_points(project_path = getwd())
}
\arguments{
\item{project_path}{The absolute path to the root of the project being tested.
Default value is current work directory}
}
\value{
The function does not return values
}
\description{
Checks the available points for all test in the project
without running test. Creates file .available_points.json in the
project root.
}
