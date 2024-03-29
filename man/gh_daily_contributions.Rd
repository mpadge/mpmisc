% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github-contributions.R
\name{gh_daily_contributions}
\alias{gh_daily_contributions}
\title{Details of GitHub contributions for particular day}
\usage{
gh_daily_contributions(day = 0L, quiet = FALSE)
}
\arguments{
\item{day}{0 for today; 1 for yesterday, and so on.}

\item{quiet}{If \code{FALSE}, print notifications to screen}
}
\value{
\code{data.frame} of repositories and contribution counts.
}
\description{
Details of GitHub contributions for particular day
}
