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
#' 
#' @examples 
#' # code to create resource
#' resource_R1 <- getPulsedResource(1:world$tau, world$X, 
#'     c(t.peak = 25, t.sd = 9, x.peak = 80, x.sd = 9))
#' # visualize
#' data(resources)
#' par(mfrow = c(2,2))
#' image(resource_R1) 
#' image(resource_R2) 
#' image(resource_R3) 
#' image(resource_R4) 
#' image(fixed_Resource) 
#' @keywords data
#' @export
NULL
