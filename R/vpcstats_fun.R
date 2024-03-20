#' Compute VPC statistics
#'
#' Compute prediction interval statistics for VPC.
#'
#' @param o A \code{tidyvpcobj}.
#' @param vpc.type Character specifying type of VPC (e.g., \code{"continuous"} (Default) or \code{"categorical"}).
#' @param qpred Numeric vector of length 3 specifying quantile prediction interval. Only applicable for \code{vpc.type = "continuous"}.
#' @param ... Other arguments to include.
#' @param conf.level Numeric specifying confidence level.
#' @param quantile.type Numeric indicating quantile type. See \code{\link[stats]{quantile}}.
#' @return Updates \code{tidyvpcobj} with \code{stats} \code{data.table} object, which contains the following columns:
#' \itemize{
#'   \item \code{bin}: Resulting bin value as specified in \code{binning()} function
#'   \item \code{xbin}: Midpoint x-value of the observed data points in the bin as specified in \code{xbin} argument of \code{binning()} function
#'   \item \code{qname}: Quantiles specified in \code{qpred}.  Only returned if \code{vpc.type = "continuous"}
#'   \item \code{pname}: Categorical probability names. Only returned if \code{vpc.type = "categorical"}
#'   \item \code{y}: Observed y value for the specified quantile
#'   \item \code{lo}: Lower bound of specified confidence interval for y value in simulated data
#'   \item \code{md}: Median y value in simulated data
#'   \item \code{hi}: Upper bound of specified confidence interval for y value in simulated data
#' }
#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{censoring}} \code{\link{stratify}} \code{\link{binning}} \code{\link{binless}} \code{\link{predcorrect}}
#' @export
vpcstats <- function(o, ...) UseMethod("vpcstats")

#' @rdname vpcstats
#' @export
vpcstats.tidyvpcobj <- function(o, vpc.type =c("continuous", "categorical"), qpred=c(0.05, 0.5, 0.95), ..., conf.level=0.95, quantile.type=7) {
  type <- match.arg(vpc.type)
  method <- o$vpc.method

  if (!isTRUE(method$method %in% c("binless", "binning"))) {
    stop(
      "A binning method should be specified through binning() or binless() execution before vpcstats() run."
    )
  }
  
  stopifnot(length(qpred) == 3)

  repl <- ypc <- ypcvc <- y <- x <- blq <- lloq <- alq <- uloq <- NULL
  . <- list
  qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2

  obs      <- o$obs
  sim      <- o$sim
  predcor  <- o$predcor
  varcorr  <- o$varcorr
  stratbin <- o$.stratbin
  xbin     <- o$xbin
  strat    <- o$strat

  if(method$method == "binless" && type == "continuous"){
    o <- binlessaugment(o, qpred = qpred, interval =  method$optimization.interval, loess.ypc = method$loess.ypc)
    o <- binlessfit(o, conf.level = conf.level, llam.quant = method$lambda, span = method$span)
  }

  if (type == "categorical") {
    if(.isCensored(obs)){
      stop("Censoring not supported for categorical vpc")
    }

    ylvls <-  sort(unique(obs$y))

    # categorical binless vpcstats() ----
    if(method$method == "binless"){
      xobs     <- obs$x
      xsim <- sim$x
      sp <- method$sp

      .stratrepl <- data.table(strat, sim[, .(repl)])

      # obs ----
      obs <- obs[, fastDummies::dummy_columns(obs, select_columns = "y")]

      setnames(obs, paste0("y_", ylvls), paste0("prob", ylvls))
      pobs <- melt(obs, id.vars = c(names(strat), "x"),
                   measure.vars = paste0("prob", ylvls),
                   variable.name = "pname", value.name = "y")
      pobs.split <- split(pobs, by = c(names(strat),"pname"), sorted = TRUE)
      pobs.split <- pobs.split[lapply(pobs.split, nrow)>1]

      # Get optimized sp values, only if optimize = TRUE, else, skip this step and directly fit gam with user sp values
      if(method$optimize){
        sp_opt <- list()

        for (i in seq_along(pobs.split)) {
          sp_opt[[i]] <- pobs.split[[i]][, .(sp = optimize(.gam_optimize,
                                                           y = y,
                                                           x = x,
                                                           data = pobs.split[[i]],
                                                           interval = method$optimization.interval)$minimum)]
        }

        names(sp_opt) <- names(pobs.split)
        method$sp <- sp_opt

        for(i in seq_along(pobs.split)){
          pobs.split[[i]][, y := .fitcatgam(y, x, sp = sp_opt[[i]]$sp)]
        }
      } else {
        sp_opt <- method$sp
        if (length(sp_opt) != length(pobs.split)) {
          stop(paste0("`incorrect number of elements specified to `sp` argument in `binless()` function, specify a list of length ", length(pobs.split), " in the following order: \n",
                      paste0(names(pobs.split), collapse = "\n"), "\n",
                      "Note: Do not specify a value for strata where not data is available."))
        }
        for (i in seq_along(pobs.split)) {
          pobs.split[[i]][, y := .fitcatgam(y, x, sp = sp_opt[[i]])]
        }
      }
      pobs <- rbindlist(pobs.split)

      # sim ----
      if(!is.null(strat)){
        sim <- sim[, c(names(strat)) := rep(strat, len = .N), by = .(repl)]
      }
      sim <- sim[, fastDummies::dummy_columns(sim, select_columns = "y")]

      setnames(sim, paste0("y_", ylvls), paste0("prob", ylvls))

      psim <- melt(sim, id.vars = c(names(.stratrepl), "x"),
                   measure.vars = paste0("prob", ylvls),
                   variable.name = "pname", value.name = "y")

      #Coerce y to double, or else data.table will attempt to coerce to integer and precision is lost
      psim$y <- as.double(psim$y)
      psim.split <- split(psim, by = c(names(strat),"pname"), sorted = TRUE)
      psim.split <- psim.split[lapply(psim.split, nrow)>1]

      if(method$optimize){
        for(i in seq_along(psim.split)){
          psim.split[[i]][, y := .fitcatgam(y, x, sp = sp_opt[[i]]$sp), by = list(repl)]
        }
      } else {
        for(i in seq_along(psim.split)){
          psim.split[[i]][, y := .fitcatgam(y, x, sp = sp_opt[[i]]), by = repl]
        }
      }

      psim <- rbindlist(psim.split)

      .stratbinquant <- psim[, !c("repl", "y")]

      ppsim <- psim[, .(lo=quantile(y,probs=qconf[[1]], type=quantile.type),
                        md=median(y),
                        hi=quantile(y,probs=qconf[[3]], type=quantile.type)), by = .stratbinquant]

      stats <- unique(pobs[ppsim, on=names(.stratbinquant)])
      setkeyv(stats, c(names(strat), "x"))
    } else {
      # categorical binning vpcstats() ----
      if (is.null(stratbin)) {
        stop("Need to specify binning before calling vpcstats.")
      }
      if (any(is.na(stratbin$bin))) {
        warning("There are bins missing. Has binning been specified for all strata?", call.=F)
      }

      .stratbinrepl <- data.table(stratbin, sim[, .(repl)])

      obs <- obs[, fastDummies::dummy_columns(obs, select_columns = "y")]
      pobs <- obs[, lapply(.SD, mean, na.rm=TRUE), by=stratbin, .SDcols=paste0("y_", ylvls)]
      setnames(pobs, paste0("y_", ylvls), paste0("prob", ylvls))
      pobs <- melt(pobs, id.vars = names(stratbin),
                   measure.vars = paste0("prob", ylvls),
                   variable.name = "pname", value.name = "y")

      sim <- sim[, fastDummies::dummy_columns(sim, select_columns = "y")]

      psim <- sim[, lapply(.SD, mean, na.rm=TRUE), by=.stratbinrepl, .SDcols=paste0("y_", ylvls)]

      setnames(psim, paste0("y_", ylvls), paste0("prob", ylvls))
      psim <- melt(psim, id.vars = names(.stratbinrepl),
                   measure.vars = paste0("prob", ylvls),
                   variable.name = "pname", value.name = "y")

      .stratbinquant <- psim[, !c("repl", "y")]
      qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2

      ppsim <- psim[, .(lo=quantile(y,probs=qconf[[1]], type=quantile.type),
                        md=median(y),
                        hi=quantile(y,probs=qconf[[3]], type=quantile.type)), by = .stratbinquant]

      stats <- pobs[ppsim, on=names(.stratbinquant)]
      stats <- xbin[stats, on=names(stratbin)]
      setkeyv(stats, c(names(o$strat), "xbin"))
    }
    # update vpc
    update(o, stats=stats, conf.level=conf.level, vpc.type = type, vpc.method = method)
    # continuous vpcstats ----
  } else {
    if(method$method == "binless") {
      .binlessvpcstats(o, qpred=qpred, conf.level=conf.level, quantile.type=quantile.type, vpc.type = type, vpc.method = method)
    } else {

      if (is.null(stratbin)) {
        stop("Need to specify binning before calling vpcstats.")
      }
      if (any(is.na(stratbin$bin))) {
        warning("There are bins missing. Has binning been specified for all strata?", call.=FALSE)
      }

      .stratbinrepl <- data.table(stratbin, sim[, .(repl)])

      if (isTRUE(predcor)) {
        if (isTRUE(varcorr)) {
          qobs <- obs[, quant_loq(ypcvc, probs=qpred, blq=blq,   alq=alq,   type = quantile.type),   by=stratbin]
          qsim <- sim[, quant_loq(ypcvc, probs=qpred, blq=FALSE, alq=FALSE, type = quantile.type), by=.stratbinrepl]
        } else {
          qobs <- obs[, quant_loq(ypc, probs=qpred, blq=blq,   alq=alq,   type = quantile.type),   by=stratbin]
          qsim <- sim[, quant_loq(ypc, probs=qpred, blq=FALSE, alq=FALSE, type = quantile.type), by=.stratbinrepl]
        }
      } else {
        qobs <- obs[, quant_loq(y, probs=qpred, blq=blq,   alq=alq  , type = quantile.type),   by=stratbin]
        qsim <- sim[, quant_loq(y, probs=qpred, blq=FALSE, alq=FALSE, type = quantile.type), by=.stratbinrepl]
      }

      .stratbinquant <- qsim[, !c("repl", "y")]
      qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2
      qqsim <- qsim[, quant_noloq(y, probs=qconf, qname=c("lo", "md", "hi"), type = quantile.type), by=.stratbinquant]
      stats <- qobs[qqsim, on=names(.stratbinquant)]
      stats <- xbin[stats, on=names(stratbin)]
      setkeyv(stats, c(names(o$strat), "xbin"))

      if (!is.null(obs$blq) && any(obs$blq)) {
        sim[, lloq := rep(obs$lloq, len=.N)]
        sim[, blq := (y < lloq)]
        pctblqobs <- obs[, .(y=100*mean(blq)), by=stratbin]
        pctblqsim <- sim[, .(y=100*mean(blq)), by=.stratbinrepl]
        .stratbinpctblq <- pctblqsim[, !c("repl", "y")]
        qpctblqsim <- pctblqsim[, quant_noloq(y, probs=qconf, qname=c("lo", "md", "hi"), type = quantile.type), by=.stratbinpctblq]
        pctblq <- pctblqobs[qpctblqsim, on=names(.stratbinpctblq)]
        pctblq <- xbin[pctblq, on=names(stratbin)]
        setkeyv(pctblq, c(names(o$strat), "xbin"))
      } else {
        pctblq <- NULL
      }

      if (!is.null(obs$alq) && any(obs$alq)) {
        sim[, uloq := rep(obs$uloq, len=.N)]
        sim[, alq := (y > uloq)]
        pctalqobs <- obs[, .(y=100*mean(alq)), by=stratbin]
        pctalqsim <- sim[, .(y=100*mean(alq)), by=.stratbinrepl]
        .stratbinpctalq <- pctalqsim[, !c("repl", "y")]
        qpctalqsim <- pctalqsim[, quant_noloq(y, probs=qconf, qname=c("lo", "md", "hi"), type = quantile.type), by=.stratbinpctalq]
        pctalq <- pctalqobs[qpctalqsim, on=names(.stratbinpctalq)]
        pctalq <- xbin[pctalq, on=names(stratbin)]
        setkeyv(pctalq, c(names(o$strat), "xbin"))
      } else {
        pctalq <- NULL
      }

      update(o, stats=stats, pctblq=pctblq, pctalq=pctalq, conf.level=conf.level, vpc.type = type)
    }
  }
}

quant_loq <- function(y, probs, qname=paste0("q", probs), type, blq=FALSE, alq=FALSE) {
  if (is.null(blq)) blq <- FALSE
  if (is.null(alq)) alq <- FALSE
  blq <- rep(blq, len=length(y))
  alq <- rep(alq, len=length(y))
  y <- ifelse(blq, -Inf, ifelse(alq, Inf, y))
  y <- quantile(y, probs=probs, type=type, names=FALSE, na.rm=TRUE)
  y[y == -Inf] <- NA
  y[y == Inf] <- NA
  qname <- factor(qname, levels=unique(qname))
  data.frame(qname, y)
}

quant_noloq <- function(y, probs, qname=paste0("q", probs), type) {
  y <- quantile(y, probs=probs, type=type, names=FALSE, na.rm=TRUE)
  setNames(as.list(y), qname)
}
