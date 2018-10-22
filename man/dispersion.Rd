\name{dispersion()}
\alias{dispersion}
\title{Calculate cross-sectional measures of forecast dispersion}
\usage{
dispersion(surveyObj, method = 1)
}
\arguments{
  \itemize{
    \item{surveyObj: an object of class survey}
    \item{method: The dispersion measure:}
    \itemize{
      \item{1 = Interquartile Range}
      \item{2 = Standard deviation}
      \item{3 = Interquartile Range on Q/Q-growth forecasts}
    }
  }
}


\value{an object of class ts}
\description{Calculates cross-sectional measures of forecast dispersion}
\examples{
tmp <- individualSPF(survey = "CPI", variable = "CPI3")
dsp <- dispersion(tmp,method = 2)
}
