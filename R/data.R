#' A simple fake dataset
#'
#' A random catch and effort dataset just for testing functions.
#'
#' @format A data frame with 18 rows and 4 variables:
#' \describe{
#'   \item{Year}{Year of catch}
#'   \item{Catch}{Catches in t}
#'   \item{Eff}{Effort in fisherman/day}
#'   \item{CPUE}{CPUE in t/(fisherman/day)}
#' }
"simple"

#' Albacore data
#'
#' South Atlantic albacore data from Millar & Meyer (2001).
#'
#' @format A data frame with 23 rows and 4 variables:
#' \describe{
#'   \item{Year}{Year of catch}
#'   \item{C}{Catches (1000 t)}
#'   \item{E}{Effort (10^8 hooks)}
#'   \item{I}{CPUE (kg/100 hooks)}
#'   \item{P}{P = B/K}
#' }
"albacore"

#' Namibian hake data
#'
#' Northern Namibian hake data.
#'
#' @format A data frame with 24 rows and 4 variables:
#' \describe{
#'   \item{year}{Year of catch}
#'   \item{catch}{Catches (1000 t)}
#'   \item{effort}{Effort}
#'   \item{cpue}{CPUE}
#' }
"hake"
