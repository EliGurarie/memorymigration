#' Basic World object
#'
#' memorymigration set up for running models
#'
#' @usage
#' data(world)
#'
#' @format List of 5:
#' \describe{
#'   \item{pop}{100 x 50 matrix of initial population ... (sinusoid output of )}
#'   \item{X}{a vector with midpoint X-values}
#'   \item{time}{the time points for the population as integers 1:tau}
#'   \item{dx}{time step}
#'   \item{tau}{maximum value of time}
#' }
#' @examples
#' # code to create world
#' world <- getSinePop(tau = 100, X.min = 0, X.max = 100, dx=2, peak.max = 80, peak.min = 20, sd = 10)
#' # visualize
#' image(world$pop) 
#' @keywords data
#' @export
NULL

#' Resource objects
#' 
#' @usage 
#' data(resources)
#' data(resources_drifting)
#' data(resources_island)
#' 
#' @example examples/WorldsandResources.R
#' 
#' @keywords data
#' @aliases resources_drifting, resources_island
#' @export
NULL


