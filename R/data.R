#' Example observed data from vpc package.
#'
#' An observed dataset from a hypothetical PK model. Altered to include NTIME, GROUP, GENDER.
#'
#' @format A data frame with 600 rows and 7 variables:
#' \describe{
#'   \item{ID}{Subect identifier}
#'   \item{TIME}{Time}
#'   \item{DV}{Concentration of drug}
#'   \item{AMT}{Amount of dosage initially administered at DV = 0, TIME = 0}
#'   \item{DOSE}{Dosage amount}
#'   \item{MDV}{Dummy indiciating missing dependent variable value}
#'   \item{NTIME}{Nominal Time}
#'   \item{GENDER}{Character variable indicating subject's gender ("M", "F")}
#'   \item{STUDY}{Character variable indicating study type ("Study A", "Study B")}
#' }
#' @source \code{\link[vpc]{simple_data}} 
"obs_data"

#' Example simulated data from vpc package.
#'
#' A simulated dataset from a hypothetical PK model with 100 replicates.
#'
#' @format A data frame with 60000 rows and 10 variables:
#' \describe{
#'   \item{ID}{Subect identifier}
#'   \item{REP}{Replicate num for simulation}
#'   \item{TIME}{Time}
#'   \item{DV}{Concentration of drug}
#'   \item{IPRED}{Individual prediction variable}
#'   \item{PRED}{Population prediction variable}
#'   \item{AMT}{Amount of dosage initially administered at DV = 0, TIME = 0}
#'   \item{DOSE}{Dosage amount}
#'   \item{MDV}{Dummy indiciating missing dependent variable value}
#'   \item{NTIME}{Nominal Time}
#' }
#' @source \code{\link[vpc]{simple_data}} 
"sim_data"

#' Example observed data from with categorical DV.
#'
#' An observed dataset with 3 levels of categorical DV.
#'
#' @format A data frame with 4014 rows and 4 variables:
#' \describe{
#'   \item{PID_code}{Subect identifier}
#'   \item{agemonths}{Time}
#'   \item{zlencat}{Categorical DV with the 3 levels}
#'   \item{Country_ID_code}{Country code for stratification}
#' }
#' @source Certara University
"obs_cat_data"

#' Example simulated data with categorical DV.
#'
#' A simulated dataset with the 3 levels of categorical DV across 100 replicates.
#'
#' @format A data frame with 401400 rows and 4 variables:
#' \describe{
#'   \item{PID_code}{Subect identifier}
#'   \item{IVAR}{Time}
#'   \item{DV}{Categorical DV with 3 levels}
#'   \item{Replicate}{Replicate num for simulation}
#' }
#' @source Certara University
"sim_cat_data"