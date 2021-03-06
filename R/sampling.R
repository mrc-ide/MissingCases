#' Sample chains to produce valid parameter solution space
#'
#' @param n_iter Number of iterations
#' @param r1 Ratio of cases not under surveillance (but contact traced) versus the cases under surveillance
#' @param r2 Ratio of de novo cases verses detected cases under surveillance
#'
#' @return List of parameter chains
#' @export
#'
#' @examples
#' sample_chains(10, 0.5, 3)
sample_chains <- function(n_iter, r1, r2){
  # Initialise chains
  mu_chain <- NULL
  pi_chain <- NULL
  phi_chain <- NULL
  alpha_chain <- NULL

  # Compute mu1 and alpha by sampling pi and phi uniformly
  for(i in 1:n_iter)
  {
    # Sample  pi and phi
    pi <- stats::runif(n = 1)
    phi <- stats::runif(n = 1)
    # Compute alpha and mu
    sol <- compute_alpha_mu1(pi = pi, phi = phi, r1 = r1, r2 = r2)

    # If solution isn't na - add point to chain
    if(sum(is.na(sol)) == 0){
      mu_chain <- c(mu_chain, sol$mu1)
      pi_chain <- c(pi_chain, pi)
      phi_chain <- c(phi_chain, phi)
      alpha_chain <- c(alpha_chain, sol$alpha)
    }
  }
  # Compute gamma chain
  gamma_chain <- pi_chain/(r1+pi_chain)

  return(list(mu1 = mu_chain,
              pi = pi_chain,
              phi = phi_chain,
              alpha = alpha_chain,
              gamma = gamma_chain))
}


