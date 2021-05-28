#' Basic World object
#'
#' memorymigration set up for running models
#'
#' @usage
#' data(world)
#'
#' @format List of 5:
#' \describe{
#'   \item{pop}{100 x 100 matrix of initial population ... (sinusoid output of )}
#'   \item{X}{a vector with midpoint X-values}
#'   \item{time}{the time points for the population as integers 1:tau}
#'   \item{dx}{time step}
#'   \item{tau}{maximum value of time}
#' }
#' @examples
#' # code to create world
#' world_nonmigratory <- getSinePop(tau = 100, peak.max = 0, peak.min = 0, sd = 10)
#' world_sinusoidal <- getSinePop(tau = 100, peak.max = 40, peak.min = -40, sd = 10)
#' world_optimal <- getOptimalPop(tau = 100, t.peak = 25, x1 = 40, x2 = -40, x.sd = 5, t.sd = 12)
#' # visualize
#' image(world_nonmigratory$pop) 
#' image(world_sinusoidal$pop) 
#' image(world_optimal$pop) 
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
#' #code to create drifting resource
#' resource_drifting <- getResource_drifting(world_sinusoidal, par = c(t.peak = 25, t.sd = 12, x.peak = 40, x.sd = 5))
#' with(world_sinusoidal, image.plot(time, X, resource_drifting))
#' 
#' #code to create island resource
#' resource_island <- getResource_island(world_sinusoidal, par = c(t.peak = 25, t.sd = 12, x.peak = 80, x.sd = 5))
#' with(world_sinusoidal, image.plot(time, X, resource_island))
#' 
#' @keywords data
#' @aliases resources_drifting, resources_island
#' @export
NULL


