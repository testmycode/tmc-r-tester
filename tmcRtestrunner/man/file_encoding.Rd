% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TestEnvironment.R
\name{file_encoding}
\alias{file_encoding}
\title{File encoding of exercise file}
\usage{
file_encoding(filename)
}
\arguments{
\item{filename}{a string which is the name of the file tested for
encoding.}
}
\value{
a string which is either "ISO-8859", "ASCII", "UTF-8" or ""
depending on the encoding of the filename. The empty string means
either other file encoding or it can mean that the 'file' executable
was not found from the operating system PATH of executable files.
}
\description{
This function tries to determine the file encoding of
the exercise file. It is a wrapper around 'file' executable and if
that is missing, it will return unrecognized value.
}
