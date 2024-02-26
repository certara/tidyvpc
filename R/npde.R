#' Normalized Prediction Distribution Errors
#'
#' @param o A \code{tidyvpcobj}.
#' @param id A vector of IDs. Used to associate observations (\code{y}) that
#' originate from the same individual. Evaluated in the \code{data.frame}
#' \code{data}.
#' @param data A \code{data.frame}.
#' @param smooth Should a uniform random perturbation be used to smooth the pd/pde values?
#' @param ... Additional arguments.
#'
#' @references
#'
#' Brendel, K., Comets, E., Laffont, C., Laveille, C. & Mentrée, F. Metrics
#' for external model evaluation with an application to the population
#' pharmacokinetics of gliclazide.  Pharm. Res. (2006) 23(9), 2036–2049.
#'
#' Nguyen, T.H.T., et al. Model evaluation of continuous data pharmacometric
#' models: metrics and graphics.  CPT Pharmacometrics Syst. Pharmacol. (2017)
#' 6(2), 87–109; doi:10.1002/psp4.12161.
#'
#' @examples
#' \donttest{
#' require(magrittr)
#' require(ggplot2)
#'
#' obs <- obs_data[MDV==0]
#' sim <- sim_data[MDV==0]
#'
#' npde <- observed(obs, x=NULL, y=DV) %>%
#'     simulated(sim, y=DV) %>%
#'     npde(id=ID)
#'
#' vpc <- observed(npde$npdeobs, x=epred, y=npde) %>%
#'     simulated(npde$npdesim, y=npde) %>%
#'     binning("eqcut", nbins=10) %>%
#'     vpcstats()
#'
#' plot(vpc) +
#' labs(x="Simulation-based Population Prediction", y="Normalized Prediction Distribution Error")
#' }
#'
#' @export
npde <- function(o, ...)
  UseMethod("npde")

#' @rdname npde
#' @export
npde.tidyvpcobj <- function(o,
                            id,
                            data = o$data,
                            smooth = FALSE,
                            ...) {
  obs <- rn <- y <- iter <- NULL
  
  if (missing(id)) {
    id <- o$id
  } else {
    id <- rlang::eval_tidy(rlang::enquo(id), data)
  }
  if (is.null(id)) {
    stop("No id specified")
  }
  
  niter <- nrow(o$sim) / nrow(o$obs)
  
  calc.npde <- function(dv, iter, smooth = FALSE) {
    yobs <- dv[iter == 0]
    nobs <- length(yobs)
    niter <- max(iter) # = length(dv)/nobs - 1
    ysim <- matrix(dv[iter != 0], nrow = nobs, ncol = niter)
    
    # -- Non-decorrelated --
    
    pd.obs <- apply(ysim < yobs, 1, mean)
    
    # Handle extremes
    if (isTRUE(smooth)) {
      u <- runif(nobs, 0, 1 / niter)
      pd.obs[pd.obs == 1] <-
        rep(1 - (1 / niter), len = sum(pd.obs == 1))
      pd.obs <- pd.obs + u
    } else {
      pd.obs[pd.obs == 0] <- 1 / (2 * niter)
      pd.obs[pd.obs == 1] <- 1 - 1 / (2 * niter)
    }
    
    # Normalize
    npd.obs <- qnorm(pd.obs)
    
    # Compute for simulated data
    r <- apply(ysim, 1, rank)
    pd.sim <- r / (niter + 1)   # +1 prevents Inf after normalization
    npd.sim <- qnorm(pd.sim)
    
    # -- Decorrelated --
    
    epred <- apply(ysim, 1, mean)
    ec <- cov(t(ysim))
    ec.inv.chol <-
      solve(chol(ec))  # In future, should we consider different decorrelation methods?
    
    eres <- yobs - epred
    esres <- ysim - epred
    
    ewres <- drop(t(ec.inv.chol) %*% eres)
    eswres <- t(ec.inv.chol) %*% esres
    
    pde.obs <- apply(eswres < ewres, 1, mean)
    
    # Handle extremes
    if (isTRUE(smooth)) {
      # Can use the same uniform as for pd, no need to call runif again
      pde.obs[pde.obs == 1] <-
        rep(1 - (1 / niter), len = sum(pde.obs == 1))
      pde.obs <- pde.obs + u
    } else {
      pde.obs[pde.obs == 0] <- 1 / (2 * niter)
      pde.obs[pde.obs == 1] <- 1 - 1 / (2 * niter)
    }
    
    # Normalize
    npde.obs <- qnorm(pde.obs)
    
    # Compute for simulated data
    r <- apply(eswres, 1, rank)
    pde.sim <- r / (niter + 1)   # +1 prevents Inf after normalization
    npde.sim <- qnorm(pde.sim)
    
    data.table(
      iter = iter,
      epred = epred,
      eres = c(eres, esres),
      ewres = c(ewres, eswres),
      npd = c(npd.obs, npd.sim),
      npde = c(npde.obs, npde.sim)
    )
  }
  
  obssim <- data.table(
    y    = c(o$obs$y, o$sim$y),
    id   = rep(id, length.out = (niter + 1) * nrow(o$obs)),
    iter = rep(0:niter, each = nrow(o$obs))
  )
  
  obssim <-
    obssim[, rn := (1:.N)][, cbind(rn, calc.npde(y, iter, smooth = smooth)), by =
                             id][order(rn)][, rn := NULL]
  
  npdeobs <- obssim[iter == 0]
  npdesim <- obssim[iter != 0]
  
  update(o, npdeobs = npdeobs, npdesim = npdesim)
}

# vim: ts=2 sw=2 et
