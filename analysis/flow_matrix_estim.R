lower_tri_idx <- function(nrow) {
  s <- seq.int(nrow - 1)
  x <- rev(abs(sequence(s) - nrow) + 1)
  y <- rep.int(s, rev(s))
  idx <- cbind(x, y)
  idx
}

##' Probability of moving from location i to j
##'
##' the probability of moving from location i to location j is given by
##' (1 - p_stay_at_i) * (flow_from_i_to_j/(total outward flow from i))
##' @title
##' @param relative_risk n * n matrix where n = n.locations
##' @param p_stay a vector of length n where the ith entry specifies
##' the probability of staying at location i. If length of p_stay is
##' less than n, elements will be recycled.
##' @return a n * n matrix specifying the population flow between n
##' locations
##' @author Sangeeta Bhatia
probability_movement <- function(relative_risk, p_stay) {

  if (nrow(relative_risk) != ncol(relative_risk)) {
    stop("relative_risk should be a square matrix.")
  }
  n_countries      <- nrow(relative_risk)
  p_mat            <- matrix(
    rep(p_stay, each = n_countries,
        length.out = n_countries ^ 2),
    nrow = n_countries,
    byrow = TRUE
  )
  p_mat            <- 1 - p_mat
  p_movement       <- relative_risk * p_mat
  diag(p_movement) <-
    rep(p_stay, each = 1, length.out = n_countries)
  p_movement
}


##' .. Given the populations at two places and the distances between
##' them, returns the flow vector under the specified model ..
##' The models are : gravity and radiation.
##'
##' @title
##' @param K
##' @param alpha
##' @param beta
##' @param gamma
##' @param model
##' @return
##' @author Sangeeta Bhatia
flow_vector <- function(N_from,
                        N_to,
                        distance,
                        model,
                        params) {
  if (model == "gravity") {
    K          <- params$K
    pow_N_from <- params$pow_N_from
    pow_N_to   <- params$pow_N_to
    pow_dist   <- params$pow_dist
    gravity_model_flow(N_from, N_to, distance, K,
                       pow_N_from, pow_N_to, pow_dist)
  } else if (model == "poisson_gravity") {
    K          <- params$K
    pow_N_from <- params$pow_N_from
    pow_N_to   <- params$pow_N_to
    pow_dist   <- params$pow_dist
    poisson_gravity(N_from, N_to, distance, K,
                    pow_N_from, pow_N_to, pow_dist)
  } else if (model == "gravity_alt") {
    tau <- params$tau
    rho <- params$rho
    alpha <- params$alpha
    gravity_alt(N_to, distance, tau, rho, alpha)
  } else
    stop("Model not yet implemented")
}

gravity_model_flow <- function(N_from,
                               N_to,
                               distance,
                               K,
                               pow_N_from,
                               pow_N_to,
                               pow_dist) {
  K * (N_from ^ pow_N_from) * (N_to ^ pow_N_to) /
    (distance ^ pow_dist)

}

## Common params
common_params <- function(metadatafile = "data/processed/all_african_centroids.csv",
                          dist_obj = "data/processed/allafrica_distances.rds",
                          countries_col = "ISO3",
                          pop_col = "pop") {
    mobility <- list(
        K = 1,
        pow_N_from = 1,
        pow_N_to = 1)

  centroids <- readr::read_csv(metadatafile)
  distances <- readRDS(dist_obj)
  distvec <- distances[lower.tri(distances)]

  ## ----pops----------------------------------------------------------------
  idx <- lower_tri_idx(nrow(distances))
  n_from <- pull(centroids, pop_col)[idx[, 1]]
  n_to <- pull(centroids, pop_col)[idx[, 2]]
  params <- list(
    mobility = mobility,
    countries = dplyr::pull(centroids,countries_col),
    distvec = distvec,
    n_from = n_from,
    n_to = n_to
  )
  params
}


flow_mat <- function(gamma,
                     pstay,
                     params
                     ) {

  mobility <- params$mobility
  mobility$pow_dist <- gamma
  flowmat  <-
    matrix(NA,
           length(params$countries),
           length(params$countries))
  rownames(flowmat) <- params$countries
  colnames(flowmat) <- params$countries
  ## fill in the matrix from the vectors
  flow_from_to <- flow_vector(params$n_from,
                              params$n_to,
                              params$distvec,
                              model = "gravity",
                              params = mobility)

  flowmat[lower.tri(flowmat)] <- flow_from_to
  ## fill out the upper triangle
  flowmat <- t(flowmat)

  flow_to_from <- flow_vector(params$n_to,
                              params$n_from,
                              params$distvec,
                              model = "gravity",
                              mobility)

  ## fill out the lower triangle
  flowmat[lower.tri(flowmat)] <-
    flow_to_from
  ## Relative risk
  rel_risk <- flowmat / rowSums(flowmat,
                                na.rm = TRUE)

  ## matrix characterising the population
  ## movement between geographical units

  pij <- probability_movement(
      rel_risk,
      p_stay = pstay
  )

  pij

}

write_output <- function(flow_matrices, outdir, prefix) {

  outfiles <- paste0(outdir,
                     "flow_",
                     seq_along(flow_matrices),
                     "_",
                     prefix,
                     ".rds")

  purrr::walk2(flow_matrices,
               outfiles,
               ~ readr::write_rds(x = .x,
                                  path = .y))

}

flow_mat2 <- function(gamma,
                      pstay,
                      metadatafile = "data/processed/all_african_centroids.csv",
                      dist_obj = "data/processed/allafrica_distances.rds",
                      countries_col = "ISO3",
                      pop_col = "pop") {
  params <- common_params(metadatafile, dist_obj, countries_col, pop_col)
  mobility <- params$mobility
  mobility$pow_dist <- gamma
  flowmat  <-
    matrix(NA,
           length(params$countries),
           length(params$countries))
  rownames(flowmat) <- params$countries
  colnames(flowmat) <- params$countries
  ## fill in the matrix from the vectors
  flow_from_to <- flow_vector(params$n_from,
                              params$n_to,
                              params$distvec,
                              model = "gravity",
                              params = mobility)

  flowmat[lower.tri(flowmat)] <- flow_from_to
  ## fill out the upper triangle
  flowmat <- t(flowmat)

  flow_to_from <- flow_vector(params$n_to,
                              params$n_from,
                              params$distvec,
                              model = "gravity",
                              mobility)

  ## fill out the lower triangle
  flowmat[lower.tri(flowmat)] <-
    flow_to_from
  ## Relative risk
  rel_risk <- flowmat / rowSums(flowmat,
                                na.rm = TRUE)


  rel_risk

}
