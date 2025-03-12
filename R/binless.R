#' Perform binless Visual Predictive Check (VPC)
#'
#' Use this function in place of traditional binning methods to derive VPC. For continuous
#' VPC, this is obtained using additive quantile regression (\code{quantreg::rqss()}) and LOESS for pcVPC. While for categorical
#' VPC, this is obtained using a generalized additive model (\code{gam(family = "binomial")}).
#'
#' @param o A \code{tidyvpcobj}.
#' @param optimize Logical indicating whether smoothing parameters should be optimized using AIC.
#' @param optimization.interval Numeric vector of length 2 specifying the min/max range of smoothing parameter for optimization. Only applicable if \code{optimize = TRUE}.
#' @param loess.ypc (Deprecated) Argument is ignored. For a LOESS pcVPC using the `binless` method, usage of \code{\link{predcorrect}} will automatically perform LOESS prediction correction.
#' @param lambda Numeric vector of length 3 specifying lambda values for each quantile. If stratified, specify a \code{data.frame} with given strata represented the column name, and value specified as a numeric vector of length 3.
#' See below examples. Only applicable to continuous VPC with \code{optimize = FALSE}.
#' @param span Numeric between 0,1 specifying smoothing parameter for LOESS prediction correction. Only applicable for continuous VPC with \code{optimize = FALSE} and usage of \code{\link{predcorrect}}.
#' @param sp List of smoothing parameters applied to \code{mgcv::gam()}. Elements of list must be in the same order as unique values of DV. If one or more stratification variables present, the order of sp
#' should be specified as unique combination of strata + DV, in ascending order. See below examples. Only applicable for categorical VPC, if \code{optimize = FALSE}.
#' @param ... Other arguments to include will be ignored.
#' @return For continuous VPC, updates \code{tidyvpcobj} with additive quantile regression fits for observed and simulated data for quantiles specified in the \code{qpred} argument of \code{vpcstats()}.
#'   If the \code{optimize = TRUE} argument is specified, the resulting \code{tidyvpcobj} will contain optimized lambda values according to AIC.  For prediction
#'   corrected VPC (pcVPC), specifying \code{loess.ypc = TRUE} will return optimized span value for LOESS smoothing. For categorical VPC,
#'   updates \code{tidyvpcobj} with fits obtained by \code{gam(family="binomial")} for observed and simulated data for each category of DV (in each stratum if \code{stratify} defined).
#'   If \code{optimize = TRUE} argument is specified, the resulting \code{tidyvpcobj} wil contain optimized \code{sp} values according to AIC.

#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{censoring}} \code{\link{predcorrect}} \code{\link{stratify}} \code{\link{binning}} \code{\link{vpcstats}}
#' @examples
#' \donttest{
#'
#' require(magrittr)
#' require(data.table)
#'
#' obs_data <- obs_data[MDV == 0]
#' sim_data <- sim_data[MDV == 0]
#'
#'
#'  vpc <- observed(obs_data, y = DV, x = TIME) %>%
#'       simulated(sim_data, y = DV) %>%
#'       binless() %>%
#'       vpcstats()
#'
#'  # Binless example with LOESS prediction correction
#'
#'  obs_data$PRED <- sim_data[REP == 1, PRED]
#'
#'  vpc <- observed(obs_data, y = DV, x = TIME) %>%
#'       simulated(sim_data, y = DV) %>%
#'       binless(optimize = TRUE) %>%
#'       predcorrect(pred = PRED) %>% 
#'       vpcstats()
#'
#' # Binless example with user specified lambda values stratified on
#' # "GENDER" with 2 levels ("M", "F"), 10%, 50%, 90% quantiles.
#'
#'  lambda_strat <- data.table(
#'  GENDER_M = c(3,5,2),
#'  GENDER_F = c(1,3,4)
#'  )
#'
#'  vpc <- observed(obs_data, y = DV, x = TIME) %>%
#'       simulated(sim_data, y = DV) %>%
#'       stratify(~ GENDER) %>%
#'       binless(optimize = FALSE, lambda = lambda_strat) %>%
#'       vpcstats(qpred = c(0.1, 0.5, 0.9))
#'
#'  # Binless example for categorical DV with optimized smoothing
#'  vpc <- observed(obs_cat_data, x = agemonths, yobs = zlencat) %>%
#'        simulated(sim_cat_data, ysim = DV) %>%
#'        stratify(~ Country_ID_code) %>%
#'        binless() %>%
#'        vpcstats(vpc.type = "cat", quantile.type = 6)
#'
#'  # Binless example for categorical DV with user specified sp values
#'  user_sp <- list(
#'  Country1_prob0 = 100,
#'  Country1_prob1 = 3,
#'  Country1_prob2 = 4,
#'  Country2_prob0 = 90,
#'  Country2_prob1 = 3,
#'  Country2_prob2 = 4,
#'  Country3_prob0 = 55,
#'  Country3_prob1 = 3,
#'  Country3_prob2 = 200)
#'
#'  vpc <- observed(obs_cat_data, x = agemonths, yobs = zlencat) %>%
#'         simulated(sim_cat_data, ysim = DV) %>%
#'         stratify(~ Country_ID_code) %>%
#'         binless(optimize = FALSE, sp = user_sp) %>%
#'         vpcstats(vpc.type = "categorical", conf.level = 0.9, quantile.type = 6)
#' }
#'
#' @export
binless <- function(o, ...) UseMethod("binless")

#' @rdname binless
#' @export
binless.tidyvpcobj <- function(o, optimize = TRUE, optimization.interval = c(0,7), loess.ypc = NULL, lambda = NULL, span = NULL, sp = NULL, ...) {

  if(!inherits(o, "tidyvpcobj")) {
    stop("No tidyvpcobj found, observed(...) %>% simulated(...) must be called prior to binless()")
  }

  if(!optimize){
    if(is.null(lambda) && is.null(sp)) {
      stop("Set optimize = TRUE if no lambda or sp arguments specified.")
    }
    # if(!is.null(sp) && length(sp) != length(unique(o$obs$y))){
    #   stop("Argument `sp` must be a vector of length equal to the number of unique values of DV. \n
    #        Note, `sp` argument is only applicable for categorical vpc.")
    # }
  }

  if(!is.null(sp)){
    if(optimize){
      optimize <- FALSE
    }
    sp <- lapply(sp, function(x)
      x <- c(sp = x))
  }

  if (!is.null(loess.ypc)) {
    warning('The loess.ypc argument is deprecated and will be ignored. Usage of `binless()` with `predcorrect()` will now perform LOESS prediction corrected VPC by default.')
  }
  loess.ypc <- o$predcor
  if (is.null(loess.ypc)) {
    loess.ypc <- FALSE
  }

  #if binless categorical, check that sp length = number of unique categories of y

  method <- list(method = "binless",
                 optimize = optimize,
                 optimization.interval = optimization.interval,
                 loess.ypc = loess.ypc,
                 lambda = lambda,
                 span = span,
                 sp = sp)

  update(o, vpc.method = method)

}

binlessfit <- function(o, conf.level = .95, llam.quant = NULL, span = NULL, ...){
  y <- l.ypc <- repl <- NULL
  . <- list
  qpred <- o$qpred
  qnames <- paste0("q", as.character(qpred))
  qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2

  obs <- o$obs
  sim <- o$sim

  if (isTRUE(o$loess.ypc)) {
    if (isTRUE(o$varcorr)) {
      warning("Variability correction is not supported by binless VPC and won't be applied")
      o$varcorr <- FALSE
    }
    
    pred <- o$pred
    obs <- cbind(obs, pred)
    sim[, pred := rep(pred, len=.N), by = .(repl)]
    if(is.null(span)) {
      span <- o$span
    }
  }
  getllam <- function(qnames, userllam, stratlev) {
    userllam <- as.data.frame(userllam)
    userllam <- userllam[, order(names(userllam))]
    llam.list <- vector("list", length(qnames))
    names(llam.list) <- qnames
    if(stratlev == 2) {
      for (i in seq_along(llam.list)) {
        llam.list[[i]] <- list(lambda = userllam[i, 1], lambda = userllam[i,2])
      }
    }
    if(stratlev == 3) {
      for (i in seq_along(llam.list)) {
        llam.list[[i]] <- list(lambda = userllam[i, 1], lambda = userllam[i,2], lambda = userllam[i,3])
      }
    }
    if(stratlev == 4) {
      for (i in seq_along(llam.list)) {
        llam.list[[i]] <- list(lambda = userllam[i, 1], lambda = userllam[i,2], lambda = userllam[i,3], lambda = userllam[i,4])
      }
    }
    if(stratlev == 5) {
      for (i in seq_along(llam.list)) {
        llam.list[[i]] <- list(lambda = userllam[i, 1], lambda = userllam[i,2], lambda = userllam[i,3], lambda = userllam[i,4], lambda = userllam[i,5])
      }
    }
    names(llam.list[[1]]) <- names(o$strat.split)
    names(llam.list[[2]]) <- names(o$strat.split)
    names(llam.list[[3]]) <- names(o$strat.split)
    return(llam.list)
  }

  if(is.null(llam.quant)) {
    if(is.null(o$llam.qpred)) {
      stop("Must specify llambda for binlessfit. Include binlessaugment() before running binlessfit() for optimized llambda values using AIC.")
    } else {
      llam.qpred <- o$llam.qpred
    }
  } else if(!is.null(llam.quant) && !is.null(o$strat)) {
    stratlev <- lapply(o$strat, unique)
    stratlev <- length(stratlev[[1]])
    llam.qpred <- getllam(qnames, llam.quant, stratlev)
  } else {
    llam.qpred <- llam.quant
  }

  if(is.null(span)) {
    if(!is.null(o$span) && isTRUE(o$loess.ypc)) {
      span <- o$span
    } else {
      span <- o$span
    }
  }

  if(!is.null(o$strat)) {
    strat <- o$strat
    strat.split <- split(obs, strat)
    strat.split <- strat.split[lapply(strat.split,NROW)>0]
    x.strat <- c("x", names(strat))
    if (o$replicate) {
      sim.strat <- sim[, c(names(strat)) := rep(strat, len = .N), by = .(repl)]
      strat.split.sim <- split(sim, strat)
    } else {
      strat.split.sim <- split(sim, o$strat.sim)
    }
    strat.split.sim <- strat.split.sim[lapply(strat.split.sim,NROW)>0]
  }

  if (isTRUE(o$loess.ypc) && !is.null(o$strat)) {
    if (isTRUE(o$predcor.log)) {
      for (i in seq_along(strat.split)) {
        strat.split[[i]][, l.ypc := y +  (fitted(loess(pred ~ x, span = span[[i]], na.action = na.exclude, .SD)) - pred)]
      }
    } else {
      for (i in seq_along(strat.split)) {
        strat.split[[i]][, l.ypc := y * (fitted(loess(pred ~ x, span = span[[i]], na.action = na.exclude, .SD)) / pred)]
      }
    }
    obs <- rbindlist(strat.split)
    o <- update(o, obs = obs)
  }

  if(isTRUE(o$loess.ypc) && is.null(o$strat)) {
    if(isTRUE(o$predcor.log)) {
      obs[, l.ypc := y + (fitted(loess(pred ~ x, span = span, na.action = na.exclude, .SD)) - pred)]
    } else {
      obs[, l.ypc := y * (fitted(loess(pred ~ x, span = span, na.action = na.exclude, .SD)) / pred)]
    }
    o <- update(o, obs = obs)
  }

  if (is.null(o$strat)) {
    if (isTRUE(o$loess.ypc)) {
      rqss.obs.fits <- .fitobs(obs, llam.qpred, qpred, l.ypc = TRUE)
      if(isTRUE(o$predcor.log)) {
        rqss.sim.fits <- .fitsim(sim, llam.qpred, span, qpred, qconf, l.ypc = TRUE, log = TRUE)
      } else {
        rqss.sim.fits <- .fitsim(sim, llam.qpred, span, qpred, qconf, l.ypc = TRUE)
      }
    } else {
      rqss.obs.fits <- .fitobs(obs, llam.qpred, qpred)
      rqss.sim.fits <- .fitsim(sim, llam.qpred, qpred = qpred, qconf= qconf)
    }
  }


  if(!is.null(o$strat)){
    if(isTRUE(o$loess.ypc)){
      if(isTRUE(o$predcor.log)) {
        rqss.obs.fits <- .fitobs.strat(strat.split = strat.split, x.strat = x.strat, llam.qpred = llam.qpred, qpred = qpred, l.ypc = TRUE)
        rqss.sim.fits <- .fitsim.strat(strat.split.sim = strat.split.sim, x.strat = x.strat, llam.qpred = llam.qpred, span = span, qpred = qpred, qconf = qconf, l.ypc = TRUE, log = TRUE)
      } else {
        rqss.obs.fits <- .fitobs.strat(strat.split = strat.split, x.strat = x.strat, llam.qpred = llam.qpred, qpred = qpred, l.ypc = TRUE)
        rqss.sim.fits <- .fitsim.strat(strat.split.sim = strat.split.sim, x.strat = x.strat, llam.qpred = llam.qpred, span = span, qpred = qpred, qconf = qconf, l.ypc = TRUE)
      }
    } else {
      rqss.obs.fits <- .fitobs.strat(strat.split = strat.split, x.strat = x.strat, llam.qpred = llam.qpred, qpred = qpred)
      rqss.sim.fits <- .fitsim.strat(strat.split.sim = strat.split.sim, x.strat = x.strat, llam.qpred = llam.qpred, qpred = qpred, qconf = qconf)
    }
  }

  update(o, rqss.obs.fits = rqss.obs.fits, rqss.sim.fits = rqss.sim.fits, llam.qpred = llam.qpred, span = span, conf.level = conf.level)

}

.binlessvpcstats <-  function(o, qpred=c(0.05, 0.5, 0.95), conf.level=0.95, quantile.type=7, vpc.type, ...){
  y <- x <- blq <- fit <- . <- repl <- cprop <- rqssmed <- llam.med <- c.rqssmed <- lloq <- uloq <- alq <- NULL

  obs.fits <- o$rqss.obs.fits
  sim.fits <- o$rqss.sim.fits
  obs      <- o$obs
  sim      <- o$sim
  predcor  <- o$predcor
  xbinless <- o$obs$x

  if(!is.null(o$strat)) {
    stratx <- obs.fits[, list(x, o$strat)]
    x.binless <-  c("x", "qname", names(o$strat))
  } else {
    x.binless <- c("x", "qname")
  }

  qpred <- o$qpred
  conf.level <- o$conf.level
  qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2
  
  censor_vars <- c()
  if (!is.null(obs$blq) && any(obs$blq)) {
    censor_vars <- c(censor_vars, lloq = "blq")
  }
  if (!is.null(obs$alq) && any(obs$alq)) {
    censor_vars <- c(censor_vars, uloq = "alq")
  }
  
  
  for (i in seq_along(censor_vars)) {
    censor_var <- censor_vars[[i]]
    censor_var_name <- names(censor_vars)[[i]]
    if (!is.null(obs[[censor_var]]) && any(obs[[censor_var]])) {
      if (!is.null(o$strat)) {
        stratloq <- c(names(o$strat), censor_var_name)
        loq <- obs[, stratloq, with = FALSE]
        loq <- unique(loq)
        obs.fits <- obs.fits[loq, on = names(o$strat)]
      } else {
        obs.fits[, (censor_var_name) := rep(obs[[censor_var_name]], len = .N)]
      }
      if(censor_var == "blq") {
        obs.fits[, (censor_var) := ifelse(fit < lloq, TRUE, FALSE)]
      } else {
        obs.fits[, (censor_var) := ifelse(fit > uloq, TRUE, FALSE)]
      }
    }
  }

  obs.fits <- setnames(obs.fits[, lapply(.SD, median), by = x.binless], "fit", "y")
  sim.fits <- setnames(sim.fits, c("fit", "fit.lb", "fit.ub"), c("md", "lo", "hi"))

  if(!is.null(obs$blq) && any(obs$blq)) {
    obs.fits[, blq := ifelse(y < lloq, TRUE, FALSE)]
    obs.fits[, y := ifelse(blq == TRUE, NA, y)]
  }
  if(!is.null(obs$alq) && any(obs$alq)) {
    obs.fits[, alq := ifelse(y > uloq, TRUE, FALSE)]
    obs.fits[, y := ifelse(alq == TRUE, NA, y)]
  }

  if (!is.null(o$strat)) {
    stats <- obs.fits[sim.fits, on = c("x", "qname", names(o$strat))]
  } else {
    stats <- obs.fits[sim.fits, on = c("x", "qname")]
  }

  pctblq <- pctalq <- NULL
  
  if (length(censor_vars) > 0) {
    for (i in seq_along(censor_vars)) {
      censor_var <- censor_vars[[i]]
      censor_var_name <- names(censor_vars)[[i]]
      if(is.null(o$strat)) {
      sim[, (censor_var_name) := rep(obs[[censor_var_name]], len=.N)]
      
      if (censor_var == "blq") {
        sim[, (censor_var) := (y < lloq)]
      } else {
        sim[, (censor_var) := (y > uloq)]
      }
      setorder(obs, cols = x)
      cprop.obs <- obs[, cprop := cumsum(get(censor_var)) / 1:length(get(censor_var))]

      sic.cprop <- function(llam) {
        a <- AIC(
          rqss(
            cprop ~
              qss(x, lambda=exp(llam)),
            tau=0.5, na.action=na.exclude, data = cprop.obs
          ),
          k=-1
        )
      }
      llam.med.cprop <- optimize(sic.cprop, interval=c(0, 7))$min

      med.obs.cprop <- rqss(
        cprop ~ qss(x, lambda=exp(llam.med.cprop)),
        tau=0.50, data = cprop.obs
      )
      cprop.obs$med <- fitted(med.obs.cprop)

      setorder(sim, repl, x)[, cprop := cumsum(get(censor_var)) / 1:length(get(censor_var)), by=list(repl)]

      suppressWarnings(sim[, rqssmed := fitted(rqss(cprop ~ qss(x, lambda = exp(llam.med.cprop)),
                                                    tau = 0.5, na.action = na.exclude, .SD)), by = .(repl)])

      u.x <- unique(cprop.obs$x)
      med.obs.cprop <- tapply(cprop.obs$med, cprop.obs$x, median)
      med.sims.blq    <- tapply(sim$rqssmed, sim$x, median)
      med.sims.blq.lb <- tapply(sim$rqssmed, sim$x, quantile, probs=c(qconf[[1]]))
      med.sims.blq.ub <- tapply(sim$rqssmed, sim$x, quantile, probs=c(qconf[[3]]))
      
      if (censor_var == "blq") {
        pctblq <- data.table(cbind(u.x,med.obs.cprop, med.sims.blq.lb, med.sims.blq, med.sims.blq.ub))
        setnames(pctblq, c("x", "y", "lo", "md", "hi"))
        pctblq[, (setdiff(names(pctblq), "x")) := lapply(.SD, function(col) col * 100), .SDcols = -"x"]
      } 
      
      if (censor_var == "alq") {
        pctalq <- data.table(cbind(u.x,med.obs.cprop, med.sims.blq.lb, med.sims.blq, med.sims.blq.ub))
        setnames(pctalq, c("x", "y", "lo", "md", "hi"))
        pctalq[, (setdiff(names(pctalq), "x")) := lapply(.SD, function(col) col * 100), .SDcols = -"x"]
      }

    } else {
      strat <- o$strat
      strat.split <- split(obs, strat)
      strat.split <- strat.split[lapply(strat.split,NROW)>0]
      x.strat <- c("x", names(strat))
      
      sim[, (censor_var_name ) := rep(obs[[censor_var_name]], len=.N), by = names(strat)]
      if (censor_var == "blq") {
        sim[, blq := (y < lloq)]
      } else {
        sim[, alq := (y > uloq)]
      }
      stratx.binless <- obs[, list(x, o$strat)]
      stratxrepl <- data.table(stratx.binless, sim[, .(repl)])
      strat.split.sim <- split(sim, strat)
      strat.split.sim <- strat.split.sim[lapply(strat.split.sim,NROW)>0]
      sic.strat.cprop <- function(llam){
        a <- AIC(
          rqss(
            cprop ~
              qss(x, lambda=exp(llam)),
            tau=.5, na.action=na.exclude, data = strat.split[[j]]
          ),
          k=-1
        )
      }
      llam.strat.med.cprop <- vector("list", length(strat.split))
      for (j in seq_along(strat.split)) {
        setorder(strat.split[[j]], cols = x)
        strat.split[[j]] <- strat.split[[j]][, cprop := cumsum(get(censor_var)) / 1:length(get(censor_var))]
        llam.strat.med.cprop[[j]]   <- strat.split[[j]][, .(llam.med = optimize(sic.strat.cprop,  interval=c(0, 7))$min)][,.(med = unlist(llam.med))]
        strat.split[[j]][, c.rqssmed := fitted(rqss(cprop ~ qss(x, lambda = exp(llam.strat.med.cprop[[j]][[1]])),tau= .5, na.action = na.exclude, data = strat.split[[j]]))]
      }

      obs.cprop <- rbindlist(strat.split)
      obs.cprop <- setnames(obs.cprop[, lapply(.SD, median, na.rm = TRUE), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "y")

      for (j in seq_along(strat.split.sim)) {
        setorder(strat.split.sim[[j]], cols = repl, x)
        strat.split.sim[[j]] <- strat.split.sim[[j]][, cprop := cumsum(get(censor_var)) / 1:length(get(censor_var)), by = .(repl)]
        strat.split.sim[[j]][, c.rqssmed := fitted(rqss(cprop ~ qss(x, lambda = exp(llam.strat.med.cprop[[j]][[1]])),tau= .5, na.action = na.exclude, .SD)), by = .(repl)]
      }

      sim.cprop <- rbindlist(strat.split.sim)
      sim.med <- setnames(sim.cprop[, lapply(.SD, median, na.rm = TRUE), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "md")
      sim.lb <- setnames(sim.cprop[, lapply(.SD, quantile, probs = qconf[[1]]), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "lo")
      sim.ub <- setnames(sim.cprop[, lapply(.SD, quantile, probs = qconf[[3]]), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "hi")

      pctlq <- obs.cprop[sim.med, on = x.strat]
      pctlq <- pctlq[sim.lb, on = x.strat]
      pctlq <- pctlq[sim.ub, on = x.strat]
      pctlq[, (setdiff(names(pctlq), x.strat)) := lapply(.SD, function(col) col * 100), .SDcols = -x.strat]
      
      if (censor_var == "blq") {
        pctblq <- pctlq
      }
      if (censor_var == "alq") {
        pctalq <- pctlq
      }
    }
  }
  }

  update(o, stats = stats, pctblq = pctblq, pctalq = pctalq, vpc.type = vpc.type)
}

binlessaugment <- function(o, qpred = c(0.05, 0.50, 0.95), interval = c(0,7), loess.ypc = FALSE, ...) {

  l.ypc <- strat.split <- y <- NULL

  qpred <- sort(qpred)
  obs <- o$obs
  log <- o$predcor.log

  if(!is.null(o$strat.split)){
    strat.split <- o$strat.split
  }

  environment(.autoloess) <- environment()

  if (loess.ypc) {  #Split data on strata to optimize loess
    if (is.null(o$predcor)) {
      stop("Must use predcorrect() if binless(loess.ypc=TRUE)")
    }
    if (!is.null(o$strat)) {
      pred <- o$pred
      obs <- cbind(obs, pred)
      strat <- o$strat
      strat.split <- split(obs, strat) #JC
      strat.split <- strat.split[lapply(strat.split,NROW)>0] #added
      loess.mod.strat <- vector("list", length(strat.split))
      names(loess.mod.strat) <- names(strat.split)
      if(isTRUE(o$predcor.log)) {
        for (i in seq_along(strat.split)) {
          loess.mod.strat[[i]] <-  .autoloess(loess(pred ~ x, span = .5, data = strat.split[[i]]))
          strat.split[[i]][, lpred := fitted(loess(pred ~ x, span = loess.mod.strat[[i]]$span, na.action = na.exclude))]
          strat.split[[i]][, l.ypc := (lpred - pred) + y]
        }
      } else {
        for (i in seq_along(strat.split)) {
          loess.mod.strat[[i]] <-  .autoloess(loess(pred ~ x, span = .5, data = strat.split[[i]]))
          strat.split[[i]][, lpred := fitted(loess(pred ~ x, span = loess.mod.strat[[i]]$span, na.action = na.exclude))]
          strat.split[[i]][, l.ypc := (lpred/pred) * y]
        }
      }
      span <- .getspan(loess.mod.strat)
    } else {
      pred <- o$pred
      obs <- cbind(obs, pred)
      loess.mod <-  .autoloess(loess(pred ~ x, span = .5, data = obs))
      lpred <- fitted(loess.mod$fit)
      span <- loess.mod$span
      if (isTRUE(o$predcor.log)) {
        obs[, l.ypc := (lpred - pred) + y]
      } else {
        obs[, l.ypc := (lpred/pred) * y]
      }
    }
  }

  if(!loess.ypc && !is.null(o$strat)) {
    strat <- o$strat
    strat.split <- split(obs, strat)
    strat.split <- strat.split[lapply(strat.split,NROW)>0]
  }

  if(loess.ypc) {
    if(!is.null(o$strat)){
      llamoptimize <- .sic.strat.ypc
    } else {
      llamoptimize  <- .sic.ypc
    }
  }

  if(!loess.ypc) {
    span <- NULL
    if(!is.null(o$strat)) {
      llamoptimize <- .sic.strat
    } else {
      llamoptimize <- .sic
    }
  }

  . <- list
  if(!is.null(o$strat.split)) {
    llam.strat.lo  <- vector("list", length(strat.split))
    llam.strat.med <- vector("list", length(strat.split))
    llam.strat.hi  <- vector("list", length(strat.split))

    for (i in seq_along(strat.split)) {
      data_fit <- strat.split[[i]]
      llam.strat.lo[[i]]    <- strat.split[[i]][, .(llam.lo  = optimize(llamoptimize, quant = qpred[1], data = data_fit, interval = interval)$min)][,.(lo = unlist(llam.lo))]
      names(llam.strat.lo)  <- names(strat.split)
      setnames(llam.strat.lo[[i]], paste0("q", qpred[1]))
      llam.strat.med[[i]]   <- strat.split[[i]][, .(llam.med = optimize(llamoptimize, quant = qpred[2], data = data_fit, interval = interval)$min)][,.(med = unlist(llam.med))]
      names(llam.strat.med) <- names(strat.split)
      setnames(llam.strat.med[[i]], paste0("q", qpred[2]))
      llam.strat.hi[[i]]    <- strat.split[[i]][, .(llam.hi  = optimize(llamoptimize, quant = qpred[3], data = data_fit, interval = interval)$min)][,.(hi = unlist(llam.hi))]
      names(llam.strat.hi)  <- names(strat.split)
      setnames(llam.strat.hi[[i]], paste0("q", qpred[3]))
    }

    llam.qpred <- cbind(list(llam.strat.lo, llam.strat.med, llam.strat.hi))
    names(llam.qpred) <- paste0("q", qpred)
  } else {
    llam.lo  <- obs[, .(llam.lo = optimize(llamoptimize, quant = qpred[1], data = obs, interval = interval)$min)]
    llam.med <- obs[, .(llam.med = optimize(llamoptimize, quant = qpred[2], data = obs, interval = interval)$min)]
    llam.hi  <- obs[, .(llam.hi = optimize(llamoptimize, quant = qpred[3], data = obs, interval = interval)$min)]

    llam.qpred <- c(llam.lo, llam.med, llam.hi)
    names(llam.qpred) <- paste0("q", qpred)
    llam.qpred <- unlist(llam.qpred)
  }

  update(o, llam.qpred = llam.qpred, span = span, qpred = qpred, loess.ypc = loess.ypc)
}

# Internal Functions for binlessaugment ####
.sic.strat.ypc <- function(llam, quant, data) {
  a <- AIC(
    rqss(
      l.ypc ~
        qss(x, lambda=exp(llam)),
      tau=quant, na.action=na.exclude, data = data
    ),
    k=-1
  )
}
.sic.strat <- function(llam, quant, data){
  a <- AIC(
    rqss(
      y ~
        qss(x, lambda=exp(llam)),
      tau=quant, na.action=na.exclude, data = data
    ),
    k=-1
  )
}

.sic.ypc <- function(llam, quant, data){
  a <- AIC(
    rqss(
      l.ypc ~
        qss(x, lambda=exp(llam)),
      tau=quant, na.action=na.exclude, data = data
    ),
    k=-1
  )
}

.sic <- function(llam, quant, data){
  a <- AIC(
    rqss(
      y ~
        qss(x, lambda=exp(llam)),
      tau=quant, na.action=na.exclude, data = data
    ),
    k=-1
  )
}
