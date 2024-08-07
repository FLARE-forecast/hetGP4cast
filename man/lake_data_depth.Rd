% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{lake_data_depth}
\alias{lake_data_depth}
\title{Lake temperature data at various reservoirs in Virginia, USA at multiple depths}
\format{
## `lake_data_depth`
A data frame 7950 rows and 5 columns:
\describe{
  \item{datetime}{date formatted as YYYY-MM-DD}
  \item{site_id}{Name of reservoir}
  \item{depth}{depth at which variable was recorded (m)}
  \item{variable}{name of variable}
  \item{observation}{recorded data}
}
}
\source{
<https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-expanded-observations.csv.gz>
}
\usage{
lake_data_depth
}
\description{
A subset of sensor data from various reservoirs in, Virginia, USA
}
\keyword{datasets}
