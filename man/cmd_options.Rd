% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmd_options.R
\name{cmd_options}
\alias{cmd_options}
\title{Format command line options}
\usage{
cmd_options(..., use_double_hyphens = FALSE)
}
\arguments{
\item{...}{Key-value pairs of command line options.}

\item{use_double_hyphens}{If \code{TRUE}, uses double hyphens to designate
non-abbreviated command line options and single-hyphens to designate
abbreviated ones. If \code{FALSE}, always uses single hyphens. Defaults
to \code{FALSE} as both Java and photon use single hyphens.}
}
\value{
A character vector of formatted command line options that can
be used as input to \code{\link{system2}} or \code{\link[processx]{run}}.
}
\description{
Helper function to format options for command line calls. The function
accepts key-value pairs where the parameter name is the name of the option
and the parameter value is the value of the option.
Arguments are formatted according to the following rules:

\itemize{
\item{If a value is \code{TRUE}, add parameter name as flag.}
\item{If a value is \code{FALSE}, do not add parameter name as flag.}
\item{If a value has \code{length(x) > 1}, collapse it as a CSV.}
\item{If a parameter name is missing, take the value as the flag name.}
\item{If a parameter name is given, replace underscores with hyphens.}
}
}
\examples{
# converts R parameters to CMD options
# parameters for the ping command
cmd_options(n = 1, w = 5, "127.0.0.1")

# sometimes, it is necessary to use double hyphens
# options for the docker ps command
cmd_options("ps", all = TRUE, format = "json", use_double_hyphens = TRUE)

# particularly useful together with photon
# the following options can be used for the `photon_opts` argument
# of photon$start()
cmd_options(cors_any = TRUE, data_dir = "path/to/dir")
}
