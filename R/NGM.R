#' Defines the 3 state next generation matrix
#'
#' @param pi Proportion of cases detected or “re-captured” by community surveillance
#' @param alpha Reduction in transmission intensity due to active surveillance
#' @param gamma Proportion of contacts actively under surveillance
#' @param phi Proportion of contacts recalled
#' @return The three state next generation matrix
#' @export
#' @examples
#' define_NGM(0.5, 0.8, 0.7, 0.5)
define_NGM <- function(pi, alpha, gamma, phi){

  NGM <- matrix(0, nrow = 3, ncol = 3)

  NGM[1,1] <- 1-pi
  NGM[1,2] <- (1-pi)*(1-gamma*phi)
  NGM[1,3] <- alpha*(1-pi)*(1-gamma*phi)
  NGM[2,1] <- pi
  NGM[2,2] <- pi*(1-gamma*phi)
  NGM[2,3] <- alpha*pi*(1-gamma*phi)
  NGM[3,1] <- 0
  NGM[3,2] <- gamma*phi
  NGM[3,3] <- alpha*gamma*phi

  return (NGM)
}


#' Calculates proportion of missing cases
#'
#' @param pi Proportion of cases detected or “re-captured” by community surveillance
#' @param alpha Reduction in transmission intensity due to active surveillance
#' @param gamma Proportion of contacts actively under surveillance
#' @param phi Proportion of contacts recalled
#' @return Analytic proportion of missing cases
#' @export
#' @examples
#' prop_missing_cases(0.5, 0.8, 0.7, 0.5)

prop_missing_cases <- function(pi, alpha, gamma, phi){
  mu1 = (-1 + pi) * (1 + alpha*(-2 + gamma*phi) - pi*gamma*phi +
                      sqrt(-2*pi*(1 + alpha* (-2 + gamma*phi)) *gamma*phi +
                             pi^2 *(gamma*phi)^2 + (-1 + alpha*gamma*phi)^2))/(2 *(-1 + alpha))

  return (mu1)
}
