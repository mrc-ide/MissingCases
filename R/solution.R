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
compute_alpha_mu1 <- function(pi, phi, r1, r2)
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

#' Calculates optimum phi and mu for given other parameters
#'
#' @param pi Proportion of cases detected in the community
#' @param alpha Scaling of reproduction number for traced cases
#' @param r1 Ratio of cases not under surveillance (but contact traced) versus the cases under surveillance
#' @param r2 Ratio of de novo cases verses detected cases under surveillance
#'
#' @return Optimum values of pi and mu1
#' @export
#'
#' @examples
#' compute_phi_mu1(0.3, 0.7, 0.59, 1.5)
compute_phi_mu1 <- function(pi, alpha, r1, r2){

  # Checks if there is a solution in the bound phi = [0, 1]
  if(diff_funct_three_state(pi = pi, phi = 0, r1 = r1, r2 = r2, alpha = alpha)*
     diff_funct_three_state(pi = pi, phi = 1, r1 = r1, r2 = r2, alpha = alpha) > 0) {
    return(NA)
    # Only computes phi and mu if there is a solution
  } else{
    diff_sq_three_state <- function(x){
      return(diff_funct_three_state(pi = pi, phi = x, r1 = r1, r2 = r2, alpha = alpha)^2)
    }
    # Finds optimal phi
    phi_opt <- stats::optimise(diff_sq_three_state, lower = 0, upper = 1)$minimum
    # Computes gamma
    gamma <- pi/(r1+pi)

    # Compute eigenvalues
    NGM3 <- define_NGM(pi = pi, alpha = alpha, gamma = gamma, phi = phi_opt)
    EVNGM3 <- eigen(NGM3)
    # Computes proportion of different types of cases
    mu_two <- EVNGM3$vectors[2,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])
    mu_three <- EVNGM3$vectors[3,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])

    # Computes optimal number of missing cases
    nu <- r2*phi_opt*gamma/pi - (1-phi_opt)
    mu_opt <- nu*(mu_two + alpha*mu_three)
    return(list(phi=phi_opt, mu1=mu_opt))
  }
}

#' Calculates optimum pi and mu for given other parameters
#'
#' @param phi Proportion of infected cases recalled as contacts
#' @param alpha Scaling of reproduction number for traced cases
#' @param r1 Ratio of cases not under surveillance (but contact traced) versus the cases under surveillance
#' @param r2 Ratio of de novo cases verses detected cases under surveillance
#'
#' @return Optimum values of pi and mu1
#' @export
#'
#' @examples
#' compute_pi_mu1(0.3, 0.7, 0.59, 1.5)
compute_pi_mu1 <- function(phi, alpha, r1, r2){

  # Checks if there is a solution in the bound phi = [0, 1]
  if(diff_funct_three_state(pi = 0.0000000001, phi = phi, r1 = r1, r2 = r2, alpha = alpha)*
     diff_funct_three_state(pi = 1, phi = phi, r1 = r1, r2 = r2, alpha = alpha) > 0) {
    return(NA)
    # Only computes phi and mu if there is a solution
  } else{
    diff_sq_three_state <- function(x){
      return(diff_funct_three_state(pi = x, phi = phi, r1 = r1, r2 = r2, alpha = alpha)^2)
    }
    # Finds optimal phi
    pi_opt <- stats::optimise(diff_sq_three_state, lower = 0, upper = 1)$minimum
    # Computes gamma
    gamma <- pi_opt/(r1+pi_opt)

    # Compute eigenvalues
    NGM3 <- define_NGM(pi = pi_opt, alpha = alpha, gamma = gamma, phi = phi)
    EVNGM3 <- eigen(NGM3)
    # Computes proportion of different types of cases
    mu_two <- EVNGM3$vectors[2,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])
    mu_three <- EVNGM3$vectors[3,1]/(EVNGM3$vectors[1,1] + EVNGM3$vectors[2,1] + EVNGM3$vectors[3,1])

    # Computes optimal number of missing cases
    nu <- r2*phi*gamma/pi_opt - (1-phi)
    mu_opt <- nu*(mu_two + alpha*mu_three)
    return(list(pi=pi_opt, mu1=mu_opt))
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
diff_funct_three_state <- function(pi, phi, alpha, r1, r2){
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

#' Difference between two functional relationships of proportion of missing cases
#' for four state NGM
#'
#' @param pi Proportion of cases detected or “re-captured” by community surveillance
#' @param phi Proportion of contacts recalled
#' @param alpha Reduction in transmission intensity due to active surveillance
#' @param r1 Ratio of cases not under surveillance (but contact traced) versus the cases under surveillance
#' @param r2 Ratio of de novo cases verses detected cases under surveillance
#' @return Difference between two functional relationships of proportion of missing cases
#' @export
#' @examples
#' diff_funct_four_state(0.3, 0.7, 0.47, 0.59, 1.5)
diff_funct_four_state <- function(pi, phi, alpha, r1, r2){
  # Compute gamma
  gamma <- pi/(r1+pi)

  # Compute eigenvectors of solution
  NGM4 <- define_NGM_four(pi = pi, alpha = alpha, gamma = gamma, phi = phi)
  EVNGM4 <- eigen(NGM4)

  # Computes proportion of each type of cases
  mu_nd <- EVNGM4$vectors[1,1]/(EVNGM4$vectors[1,1] + EVNGM4$vectors[2,1] + EVNGM4$vectors[3,1] + EVNGM4$vectors[4,1])
  mu_nas_nc <- EVNGM4$vectors[2,1]/(EVNGM4$vectors[1,1] + EVNGM4$vectors[2,1] + EVNGM4$vectors[3,1] + EVNGM4$vectors[4,1])
  mu_nas_c <- EVNGM4$vectors[3,1]/(EVNGM4$vectors[1,1] + EVNGM4$vectors[2,1] + EVNGM4$vectors[3,1] + EVNGM4$vectors[4,1])
  mu_as <- EVNGM4$vectors[4,1]/(EVNGM4$vectors[1,1] + EVNGM4$vectors[2,1] + EVNGM4$vectors[3,1] + EVNGM4$vectors[4,1])

  # Computes proportion of missing cases using alternative relationship
  nu <- r2*phi*gamma/pi - (1-phi)
  mm_reap <- nu*(mu_nas_nc + mu_nas_c + alpha*mu_as)

  # Returns difference between the two formulaic relationships
  return(mu_nd - mm_reap)
}
