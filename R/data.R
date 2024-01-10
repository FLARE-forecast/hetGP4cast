#' Lake temperature data from Falling Creek Reservoir at 1m depth
#'
#' A subset of sensor data from Falling Creek Reservoir, Virginia, USA
#'
#' @format ## `sample_lake_data_1mdepth`
#' A data frame with 1795 rows and 4 columns:
#' \describe{
#'   \item{datetime}{date formatted as YYYY-MM-DD}
#'   \item{site_id}{Name of reservoir}
#'   \item{variable}{name of variable}
#'   \item{observation}{recorded data}
#' }
#' @source <https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-insitu.csv>
"sample_lake_data_1mdepth"

#' Lake temperature data at various reservoirs in Virginia, USA at multiple depths
#'
#' A subset of sensor data from various reservoirs in, Virginia, USA
#'
#' @format ## `lake_data_depth`
#' A data frame 7950 rows and 5 columns:
#' \describe{
#'   \item{datetime}{date formatted as YYYY-MM-DD}
#'   \item{site_id}{Name of reservoir}
#'   \item{depth}{depth at which variable was recorded (m)}
#'   \item{variable}{name of variable}
#'   \item{observation}{recorded data}
#' }
#' @source <https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-expanded-observations.csv.gz>
"lake_data_depth"
