% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{catjitter}
\alias{catjitter}
\title{Add a small amount of (rbokeh-compatible) noise to a character vector}
\usage{
catjitter(x, factor = 0.5)
}
\arguments{
\item{x}{numeric vector to which jitter should be added}

\item{factor}{a factor between 0 and 1 that}
}
\description{
Add a small amount of (rbokeh-compatible) noise to a character vector
}
\examples{
figure(data = lattice::singer) \%>\%
  ly_points(catjitter(voice.part), jitter(height), color = "black") \%>\%
  ly_boxplot(voice.part, height, with_outliers = FALSE)
}

