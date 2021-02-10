#' Calculates optimum alpha and mu for given other parameters
#'
#' @param pi Proportion of cases detected or “re-captured” by community surveillance
#' @param phi Proportion of contacts recalled
#' @param r1 Ratio of cases not under surveillance (but contact traced) versus the cases under surveillance
#' @param r2 Ratio of de novo cases verses detected cases under surveillance
#'
#' @return Optimum values of alpha and mu1
#' @export
#'
#' @examples
#' compute_alpha_mu1(0.3, 0.7, 0.59, 1.5)
compute_alpha_mu1 <- function(pi = 0.3, phi = 0.7, r1 = 0.59, r2 = 1.5)
{
  # Checks if there is a solution in the bound alpha = [0, 1]
  if(diff_funct_three_state(pi = pi, phi = phi, r1 = r1, r2 = r2, alpha = 0)*
     diff_funct_three_state(pi = pi, phi = phi, r1 = r1, r2 = r2, alpha = 1) > 0) {
    return(NA)
  # Only computes alpha and mu if there is a solution
  } else{
    diff_sq_three_state <- function(x){
      return(diff_funct_three_state(pi = pi, phi = phi, r1 = r1, r2 = r2, alpha = x)^2)
    }
    # Finds optimal alpha
    alpha_opt <- stats::optimise(diff_sq_three_state, lower = 0, upper = 1)$minimum
    # Computes gamma
    gamma <- pi/(r1+pi)

    # Compute eigenvalues
    NGM3 <- define_NGM(pi = pi, alpha = alpha_opt, gamma = gamma, phi = phi)
    EVNGM3 <- eigen(NGM3)
    # Computes proportion of different types of cases
    mu_two <- EVNGM3$vectors[2,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])
    mu_three <- EVNGM3$vectors[3,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])

    # Computes optimal number of missing cases
    nu <- r2*phi*gamma/pi - (1-phi)
    mu_opt <- nu*(mu_two + alpha_opt*mu_three)
    return(list(alpha=alpha_opt, mu1=mu_opt))
  }
}


#' Difference between two functional relationships of proportion of missing cases
#'
#' @param pi Proportion of cases detected or “re-captured” by community surveillance
#' @param phi Proportion of contacts recalled
#' @param alpha Reduction in transmission intensity due to active surveillance
#' @param r1 Ratio of cases not under surveillance (but contact traced) versus the cases under surveillance
#' @param r2 Ratio of de novo cases verses detected cases under surveillance
#' @return Difference between two functional relationships of proportion of missing cases
#' @export
#' @examples
#' diff_funct_three_state(0.3, 0.7, 0.47, 0.59, 1.5)
diff_funct_three_state <- function(pi = 0.3, phi = 0.7, alpha = 0.47, r1=0.59, r2=1.5){
  # Compute gamma
  gamma <- pi/(r1+pi)

  # Compute eigenvectors of solution
  NGM3 <- define_NGM(pi = pi, alpha = alpha, gamma = gamma, phi = phi)
  EVNGM3 <- eigen(NGM3)

  # Computes proportion of each type of cases
  mm_NGM3 <- EVNGM3$vectors[1,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])
  mu_two <- EVNGM3$vectors[2,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])
  mu_three <- EVNGM3$vectors[3,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])

  # Computes proportion of missing cases using alternative relationship
  nu <- r2*phi*gamma/pi - (1-phi)
  mm_reap <- nu*(mu_two + alpha*mu_three)

  # Returns difference between the two formulaic relationships
  return(mm_NGM3 - mm_reap)
}
