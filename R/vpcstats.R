#' Perform a Visual Predictive Check (VPC) computation
#'
#' These functions work together to calculate the statistics that are plotted
#' in a VPC. They would typically be chained together using the "pipe" operator
#' (see Examples).
#'
#' @param o A \code{tidyvpcobj}.
#' @param ... Additional arguments.
#'
#' @import data.table
#' @import ggplot2
#' @importFrom magrittr %>%
#' @import quantreg
#' @import classInt
#' @importFrom mgcv gam
#' @importFrom stats median as.formula model.frame quantile setNames update AIC fitted loess na.exclude optimize resid time qnorm cov runif
#' @importFrom utils install.packages installed.packages
#' @name generics
NULL

#' Specify observed dataset and variables for VPC
#' 
#' The observed function is the first function in the vpc piping chain and is used for specifying observed data and variables for VPC. Note: Observed
#' data must not contain missing DV and may require filtering \code{MDV == 0} before generating VPC.
#' 
#' @param o A \code{data.frame} of observation data.
#' @param x Numeric x-variable, typically named TIME.
#' @param yobs Numeric y-variable, typically named DV.
#' @param pred Population prediction variable, typically named PRED.
#' @param blq Logical variable indicating below limit of quantification.
#' @param lloq Number or numeric variable in data indicating the lower limit of quantification.
#' @param alq Logical variable indicating above limit of quantification .
#' @param uloq Number or numeric variable in data indicating the upper limit of quantification.
#' @param ... Other arguments.
#' @return A \code{tidyvpcobj} containing both original data and observed data formatted with \code{x} and \code{y} variables as specified in function.
#'   Resulting data is of class \code{data.frame} and \code{data.table}.
#' @examples
#' 
#' obs_data <- obs_data[MDV == 0]
#' sim_data <- sim_data[MDV == 0]
#' 
#' vpc <- observed(obs_data, x=TIME, y=DV) 
#'
#' @seealso \code{\link{simulated}} \code{\link{censoring}} \code{\link{stratify}} \code{\link{predcorrect}} \code{\link{binning}} \code{\link{binless}} \code{\link{vpcstats}}
#' @export
observed <- function(o, ...) UseMethod("observed")

#' @rdname observed
#' @export
observed.data.frame <- function(o, x, yobs, pred=NULL, blq=NULL, lloq=-Inf, alq=NULL, uloq=Inf, ...) {
  data <- o
  x    <- rlang::eval_tidy(rlang::enquo(x),    data)
  yobs <- rlang::eval_tidy(rlang::enquo(yobs), data)
  pred <- rlang::eval_tidy(rlang::enquo(pred), data)
  lloq <- rlang::eval_tidy(rlang::enquo(lloq), data)
  uloq <- rlang::eval_tidy(rlang::enquo(uloq), data)
  blq  <- rlang::eval_tidy(rlang::enquo(blq),  data)
  alq  <- rlang::eval_tidy(rlang::enquo(alq),  data)

  obs <- data.table(x, y=yobs, blq, lloq, alq, uloq)
  
  o <- structure(list(data=data), class="tidyvpcobj")
  o <- update(o, obs=obs, pred=pred)
  censoring(o)
}

#' Specify simulated dataset and variables for VPC
#' 
#' The simulated function is used for specifying simulated input data and variables for VPC. Note: Simulated data must not 
#' contain missing DV and may require filtering \code{MDV == 0} before generating VPC. The ordering of observed and simulated 
#' data must also be consistent, with replicates in simulated data stacked on top of each other.
#' 
#' @param o A \code{tidyvpcobj}.
#' @param data A \code{data.frame} of simulated data.
#' @param ysim Numeric y-variable, typically named DV.
#' @param ... Other arguments.
#' @return A \code{tidyvpcobj} containing simulated dataset \code{sim} formatted with columns \code{x}, \code{y}, and \code{repl}, which indicates the replicate number.
#'  The column \code{x} is used from the \code{observed()} function. Resulting dataset is of class \code{data.frame} and \code{data.table}.
#' @examples
#' require(magrittr)
#' 
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) 
#'     
#' @seealso \code{\link{observed}} \code{\link{censoring}} \code{\link{stratify}} \code{\link{predcorrect}} \code{\link{binning}} \code{\link{binless}} \code{\link{vpcstats}}
#' @name simulated
#' @export
simulated <- function(o, ...) UseMethod("simulated")

#' @rdname simulated
#' @export
simulated.tidyvpcobj <- function(o, data, ysim, ...) {
  ysim <- rlang::eval_tidy(rlang::enquo(ysim), data)
  obs  <- o$obs
  x    <- obs$x
  nrep <- length(ysim)/nrow(obs)
  repl <- rep(1:nrep, each=nrow(obs))
  
  sim <- data.table(x, y=ysim, repl)
  update(o, sim=sim)
}

#' Censoring observed data for Visual Predictive Check (VPC)
#' 
#' Specify censoring variable or censoring value for VPC.
#' 
#' @param o A \code{tidyvpcobj}.
#' @param blq blq variable if present in observed data.
#' @param lloq Numeric value or numeric variable in data indicating the upper limit of quantification.
#' @param alq Logical variable indicating above limit of quantification.
#' @param uloq Numeric value or numeric variable in data indicating the upper limit of quantification.
#' @param data Observed data supplied in \code{observed()} function.
#' @param ... Other arguments to include.
#' @return Updates \code{obs} \code{data.frame} in \code{tidypcobj} with censored values for observed data which includes \code{lloq} and \code{uloq} specified 
#'  values for lower/upper limit of quantification. Logicals for \code{blq} and \code{alq} are returned that indicate whether the DV value lies below/above limit 
#'  of quantification. 
#' @examples
#' require(magrittr)
#' 
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) %>%
#'     censoring(blq=(DV < 50), lloq=50) %>%
#'     binning(bin = "pam", nbins = 5) %>%
#'     vpcstats()
#' 
#' #Using LLOQ variable in data with different values of LLOQ by Study:
#' 
#' obs_data$LLOQ <- obs_data[, ifelse(STUDY == "Study A", 50, 25)]
#' 
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) %>%
#'     censoring(blq=(DV < LLOQ), lloq=LLOQ) %>%
#'     stratify(~ STUDY) %>%
#'     binning(bin = "kmeans", nbins = 4) %>%
#'     vpcstats()
#'
#'  
#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{stratify}} \code{\link{predcorrect}} \code{\link{binning}} \code{\link{binless}} \code{\link{vpcstats}}

#' @export 
censoring <- function(o, ...) UseMethod("censoring")

#' @rdname censoring
#' @export
censoring.tidyvpcobj <- function(o, blq, lloq, alq, uloq, data=o$data, ...) {
  if (missing(blq)) {
    blq <- o$obs$blq
  } else {
    blq <- rlang::eval_tidy(rlang::enquo(blq), data)
  }
  if (missing(lloq)) {
    lloq <- o$obs$lloq
  } else {
    lloq <- rlang::eval_tidy(rlang::enquo(lloq), data)
  }
  if (missing(alq)) {
    alq <- o$obs$alq
  } else {
    alq <- rlang::eval_tidy(rlang::enquo(alq), data)
  }
  if (missing(uloq)) {
    uloq <- o$obs$uloq
  } else {
    uloq <- rlang::eval_tidy(rlang::enquo(uloq), data)
  }

  if (is.null(blq)) {
    blq <- FALSE
  } 
  if (!is.logical(blq)) {
    stop("blq must be of type logical")
  }
  if (any(is.na(blq))) {
    stop("blq cannot contain missing values")
  }
  if (is.null(lloq)) {
    if (any(blq)) {
      stop("No lloq specified for blq")
    } 
    lloq <- -Inf
  }
  if (!is.numeric(lloq)) {
    stop("lloq must be of type numeric")
  }
  if (any(blq & (is.na(lloq) | lloq == -Inf))) {
    stop("No lloq specified for blq")
  }

  if (is.null(alq)) {
    alq <- FALSE
  } 
  if (!is.logical(alq)) {
    stop("alq must be of type logical")
  }
  if (any(is.na(alq))) {
    stop("alq cannot contain missing values")
  }
  if (is.null(uloq)) {
    if (any(alq)) {
      stop("No uloq specified for alq")
    }
    uloq <- Inf
  } 
  if (!is.numeric(uloq)) {
    stop("uloq must be of type numeric")
  }
  if (any(alq & (is.na(uloq) | uloq == Inf))) {
    stop("No uloq specified for alq")
  }
  
  .blq <- blq
  .lloq <- lloq
  .alq <- alq
  .uloq <- uloq
  o$obs[, blq := rep(.blq, len=.N)]
  o$obs[, lloq := rep(.lloq, len=.N)]
  o$obs[, alq := rep(.alq, len=.N)]
  o$obs[, uloq := rep(.uloq, len=.N)]
  update(o, censoring=TRUE)
}

#' Stratification for Visual Predictive Check (VPC)
#' 
#' Use to specify stratification variables for VPC.
#' 
#' @param o A \code{tidyvpcobj}.
#' @param formula Formula for stratification.
#' @param data Observed data supplied in \code{observed()} function.
#' @param ... Other arguments to include.
#' @return Returns updated \code{tidyvpcobj} with stratification formula, stratification column(s), and strat.split datasets, which
#'   is \code{obs} split by unique levels of stratification variable(s). Resulting datasets are of class object \code{data.frame}
#'   and \code{data.table}.
#' @examples 
#' require(magrittr)
#' 
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) %>%
#'     stratify(~ GENDER) %>%
#'     binning(NTIME) %>%
#'     vpcstats()
#'
#' # Example with 2-way stratification by GENDER and STUDY.
#' 
#' vpc <- vpc %>%
#'     stratify(~ GENDER + STUDY) %>%
#'     binning(bin = "centers", centers = c(1,3,5,7,10)) %>%
#'     vpcstats()
#'
#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{censoring}} \code{\link{predcorrect}} \code{\link{binning}} \code{\link{binless}} \code{\link{vpcstats}}

#' @export 
stratify <- function(o, ...) UseMethod("stratify")

#' @method stratify tidyvpcobj
#' @rdname stratify
#' @export
stratify.tidyvpcobj <- function(o, formula, data=o$data, ...) {
  if (!inherits(formula, "formula")) {
    stop("Expecting a formula")
  }
  flist <- as.list(formula)
  if (length(flist) == 3) {
    lhs <- as.call(c(flist[[1]], flist[[2]]))
    rhs <- as.call(c(flist[[1]], flist[[3]]))
    if (flist[[2]] == as.symbol(".")) {
      lhsmf <- NULL
    } else {
      lhsmf <- as.data.table(model.frame(lhs, data))
    }
    if (flist[[3]] == as.symbol(".")) {
      rhsmf <- NULL
    } else {
      rhsmf <- as.data.table(model.frame(rhs, data))
    }
    if (is.null(lhsmf) && is.null(rhsmf)) {
      stop("Invalid stratification formula: no variables specified")
    }
    strat <- cbind(lhsmf, rhsmf)
  } else {
    strat <- as.data.table(model.frame(formula, data))
  }
  
  reserved.names <- c("x", "y", "ypc", "pred", "blq", "lloq", "repl", "bin", "xbin", "qname", "lo", "md", "hi",
                      "nobs", "xmedian", "xmean", "xmin", "xmax", "xmid", "xleft", "xright", "xcenter")
  if (any(names(strat) %in% reserved.names)) {
    stop(paste0("The names of used for stratification must not include: ",
                paste0(reserved.names, collapse=", ")))
  }
  
  o$obs[, names(strat) := strat]
  
  strat.split <- split(o$obs, strat)
  
  strat.split <- strat.split[lapply(strat.split,NROW)>0]
  
  update(o, strat=strat, strat.split = strat.split, strat.formula=formula)
}

#' Binning methods for Visual Predictive Check (VPC)
#' 
#' This function executes binning methods available in classInt i.e. "jenks", "kmeans", "sd", "pretty", "pam", "kmeans", "hclust", "bclust", "fisher", and "dpih".
#' You may also bin directly on x-variable or alternatively specify "centers" or "breaks". For explanation of binning methods see \code{\link[classInt]{classIntervals}}.
#' 
#' @param o A \code{tidyvpcobj}.
#' @param bin Character string indicating binning method or unquoted variable name if binning on x-variable. 
#' @param data Observed data supplied in \code{observed()} function.
#' @param xbin Character string indicating midpoint type for binning.
#' @param centers Numeric vector of centers for binning. Use \code{bin = "centers"}, if supplying centers.
#' @param breaks Numeric vector of breaks for binning. Use \code{bin = "breaks"}, if supplying breaks.
#' @param nbins Numeric number indicating the number of bins to use.
#' @param altx  Unquoted variable name in observed data for alternative x-variable binning.
#' @param stratum List indicating the name of stratification variable and level, if using different binning methods by strata.
#' @param by.strata Logical indicating whether binning should be performed by strata.
#' @param ... Other arguments to include.
#' @return Updates \code{tidyvpcobj} with \code{data.frame} containing bin information including left/right boundaries and midpoint, as specified in \code{xbin} argument.
#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{censoring}} \code{\link{predcorrect}} \code{\link{stratify}} \code{\link{binless}} \code{\link{vpcstats}}
#' @examples 
#' require(magrittr)
#' 
#'  # Binning on x-variable NTIME
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) %>%
#'     binning(bin = NTIME) %>%
#'     vpcstats()
#'     
#'  # Binning using ntile and xmean for midpoint
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) %>%
#'     binning(bin = "ntile", nbins = 8, xbin = "xmean") %>%
#'     vpcstats()
#'     
#'  # Binning using centers
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) %>%
#'     binning(bin = "centers", centers = c(1,3,5,7)) %>%
#'     vpcstats()
#'         
#'  # Different Binning for each level of Strata
#' vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'     simulated(sim_data, y=DV) %>%
#'     stratify(~ GENDER) %>%
#'     binning(stratum = list(GENDER = "M"), bin = "jenks", nbins = 5, by.strata = TRUE) %>%
#'     binning(stratum = list(GENDER = "F"), bin = "pam", nbins = 4, by.strata = TRUE) %>%
#'     vpcstats()
#'     
#'  # Binning Categorical DV using rounded time variable
#'   
#'   vpc <- observed(obs_cat_data, x = agemonths, y = zlencat ) %>%
#'       simulated(sim_cat_data, y = DV) %>%
#'       binning(bin = round(agemonths, 0)) %>%
#'       vpcstats(vpc.type = "categorical")
#' 
#' @export
binning <- function(o, ...) UseMethod("binning")

#' @rdname binning
#' @export
binning.tidyvpcobj <- function(o, bin, data=o$data, xbin="xmedian", centers, breaks, nbins, altx, stratum=NULL, by.strata=TRUE,  ...) {
  keep <- i <- NULL
  . <- list
  
  # If xbin is numeric, then that is the bin
  xbin <- rlang::eval_tidy(rlang::enquo(xbin), data)
  if (is.numeric(xbin)) {
    if (length(xbin) != nrow(o$obs)) {
      stop("A numeric xbin be of length equal to the number of observations")
    }
    bin <- xbin
  } else {
    if (missing(bin) && !missing(centers)) {
      bin <- "centers"
    } else if (missing(bin) && !missing(breaks)) {
      bin <- "breaks"
    } else {
      bin <- rlang::eval_tidy(rlang::enquo(bin), data)
    }
  }
  
  # If you don't want to bin on the observed x, you can specify an alternate x for binning
  if (missing(altx)) {
    x <- o$obs$x
  } else {
    x <- rlang::eval_tidy(rlang::enquo(altx), data)
  }
  
  if (!missing(nbins)) {
    nbins <- rlang::eval_tidy(rlang::enquo(nbins), data)
    if (is.numeric(nbins) && !is.null(o$strat) && (length(nbins) == nrow(o$strat))) {
      nbins <- data.table(nbins)[, .(nbins=unique(nbins)), by=o$strat]
    }
  }
  
  args <- lapply(rlang::enquos(...), rlang::eval_tidy, data=data)
  
  by.strata <- isTRUE(by.strata)
  
  # Check if specific stratum selected (can be more than 1), setup filter
  if (!is.null(stratum)) {
    if (!is.list(stratum)) {
      stop("stratum must be a list, data.frame or data.table")
    }
    if (is.null(o$strat)) {
      stop("No stratification has been specified")
    }
    if (!by.strata) {
      stop("by.strata must be TRUE when stratum is specified")
    }
    filter <- copy(o$strat)[, keep := F]
    filter[as.data.table(stratum), keep := T, on=names(stratum)]
    filter <- filter$keep
  } else {
    filter <- rep(T, nrow(o$obs))  # Keep all
  }
  
  # If xbin is numeric, then that is the bin
  if (is.numeric(xbin)) {
    if (length(xbin) != nrow(o$obs)) {
      stop("A numeric xbin be of length equal to the number of observations")
    }
    bin <- xbin
  } else if (is.character(bin) && length(bin) == 1) {
    
    known.classInt.styles <- c("fixed", "sd", "equal", "pretty", "quantile",
                               "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih")
    
    if (bin == "centers") {
      if (missing(centers)) {
        stop("centers must be specified to use this binning method")
      }
      if (!by.strata && is.data.frame(centers)) {
        stop("by.strata must be TRUE when centers is a data.frame")
      }
      bin <- nearest(centers)
    } else if (bin == "breaks") {
      if (missing(breaks)) {
        stop("breaks must be specified to use this binning method")
      }
      if (!by.strata && is.data.frame(breaks)) {
        stop("by.strata must be TRUE when breaks is a data.frame")
      }
      bin <- cut_at(breaks)
    } else if (bin == "ntile") {
      if (missing(nbins)) {
        stop("nbins must be specified to use this binning method")
      }
      bin <- bin_by_ntile(nbins)
    } else if (bin == "eqcut") {
      if (missing(nbins)) {
        stop("nbins must be specified to use this binning method")
      }
      bin <- bin_by_eqcut(nbins)
    } else if (bin == "pam") {
      if (missing(nbins)) {
        stop("nbins must be specified to use this binning method")
      }
      bin <- bin_by_pam(nbins)
    } else if (bin %in% known.classInt.styles) {
      if (missing(nbins)) {
        nbins <- NULL
      }
      bin <- bin_by_classInt(bin, nbins)
    } else {
      stop(sprintf("Unknown binning method: %s", bin))
    }
  }
  
  if (is.function(bin)) {
    xdat <- data.table(i=1:nrow(o$obs), x=x)
    if (any(is.na(xdat[filter]$x))) {
      warning("x contains missing values, which could affect binning")
    }
    if (any(is.infinite(xdat[filter]$x))) {
      warning("x contains non-finite values, which could affect binning")
    }
    if (by.strata && !is.null(o$strat)) {
      sdat <- copy(o$strat)
      temp <- xdat[filter, .(i=i, j=do.call(bin, c(list(x), args, .BY))), by=sdat[filter]]
      j <- temp[order(i), j]
    } else {
      j <- xdat[filter, do.call(bin, c(list(x), args))]
    }
    if (length(j) != sum(filter)) {
      stop("The binning function did not return the right number of elements")
    }
  } else if (length(bin) == nrow(o$obs)) {
    j <- bin[filter]
  } else {
    stop("Incorrect binning specification")
  }
  o$obs[filter, bin := as.factor(j)]
  bin <- o$obs$bin
  
  if (!is.null(o$strat)) {
    stratbin <- data.table(o$strat, bin)
  } else {
    stratbin <- data.table(bin)
  }
  o <- update(o, .stratbin=stratbin, bin.by.strata=by.strata)
  
  # Assign an x value to each bin
  if (is.numeric(xbin)) {
    xbin <- data.table(xbin=xbin)[, .(xbin = unique(xbin)), by=stratbin]
  } else if (is.character(xbin) && length(xbin) == 1) {
    bi <- bininfo(o)
    xbin <- data.table(bi[, names(stratbin), with=FALSE], xbin=bi[[xbin]])
  } else if (is.function(xbin)) {
    xbin <- data.table(x=x)[, .(xbin = xbin(x)), by=stratbin]
  } else {
    stop("Invalid xbin")
  }
  vpc.method <- list(method = "binning")
  update(o, xbin=xbin, vpc.method = vpc.method)
}

#' Perform binless Visual Predictive Check (VPC)
#' 
#' Use this function in place of traditional binning methods to derive VPC. For continuous
#' VPC, this is obtained using additive quantile regression (\code{quantreg::rqss()}) and LOESS for pcVPC. While for categorical
#' VPC, this is obtained using a generalized additive model (\code{gam(family = "binomial")}).
#' 
#' @param o A \code{tidyvpcobj}.
#' @param optimize Logical indicating whether smoothing parameters should be optimized using AIC. 
#' @param optimization.interval Numeric vector of length 2 specifying the min/max range of smoothing parameter for optimization. Only applicable if \code{optimize = TRUE}.
#' @param loess.ypc Logical indicating LOESS prediction corrected VPC. Must first use \code{\link{predcorrect}}, if specifying \code{loess.ypc = TRUE}. Only applicable to continuous VPC.
#' @param lambda Numeric vector of length 3 specifying lambda values for each quantile. If stratified, specify a \code{data.frame} with given strata represented the column name, and value specified as a numeric vector of length 3.
#' See below examples. Only applicable to continuous VPC with \code{optimize = FALSE}.
#' @param span Numeric between 0,1 specifying smoothing parameter for LOESS prediction correction. Only applicable for continuous VPC with \code{loess.ypc = TRUE} and \code{optimize = FALSE}.
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
#'       predcorrect(pred = PRED) %>%
#'       binless(optimize = TRUE, loess.ypc = TRUE) %>%
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
binless.tidyvpcobj <- function(o, optimize = TRUE, optimization.interval = c(0,7), loess.ypc = FALSE,  lambda = NULL, span = NULL, sp = NULL, ...) {
  
  if(class(o) != "tidyvpcobj") {
    stop("No tidyvpcobj found, observed(...) %>% simulated(...) must be called prior to binless()")
  }
  
  if(!optimize){
    if(is.null(lambda) && is.null(sp)) {
    stop("Set optimize = TRUE if no lambda or sp arguments specified")
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
  
  
 
  if(loess.ypc && is.null(o$predcor)) {
    stop("Use predcorrect() before binless() in order to use LOESS prediction corrected")
  }
  
  if(!is.null(span) && !loess.ypc) {
    stop("Set loess.ypc = TRUE and optimize = FALSE if setting span smoothing parameter for LOESS prediction corrected")
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

#' Prediction corrected Visual Predictive Check (pcVPC)
#' 
#' Specify prediction variable for pcVPC.
#' 
#' @param o A \code{tidyvpcobj}.
#' @param pred Prediction variable in observed data.
#' @param data Observed data supplied in \code{observed()} function.
#' @param ... Other arguments to include.
#' @param log Logical indicating whether DV was modeled in logarithmic scale.
#' @return Updates \code{tidyvpcobj} with required information to performing prediction correction, which includes the \code{predcor} logical indicating whether
#'   prediction corrected VPC is to be performed, the \code{predcor.log} logical indicating whether the DV is on a log-scale, and the \code{pred} prediction
#'   column from the original data.
#' @examples 
#' require(magrittr)
#' 
#' obs_data <- obs_data[MDV == 0]
#' sim_data <- sim_data[MDV == 0]
#' 
#'  # Add PRED variable to observed data from first replicate of 
#'  # simulated data
#'  
#' obs_data$PRED <- sim_data[REP == 1, PRED]
#' 
#'   vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'        simulated(sim_data, y=DV) %>%
#'        binning(bin = NTIME) %>%
#'        predcorrect(pred=PRED) %>%
#'        vpcstats()
#'        
#'  \donttest{    
#'  # For binless loess prediction corrected, use predcorrect() before 
#'  # binless() and set loess.ypc = TRUE
#'  
#'   vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'        simulated(sim_data, y=DV) %>%
#'        predcorrect(pred=PRED) %>%
#'        binless(loess.ypc = TRUE) %>%
#'        vpcstats()
#'        }
#'        
#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{censoring}} \code{\link{stratify}} \code{\link{binning}} \code{\link{binless}} \code{\link{vpcstats}}

#' @export
predcorrect <- function(o, ...) UseMethod("predcorrect")

#' @rdname predcorrect
#' @export
predcorrect.tidyvpcobj <- function(o, pred, data=o$data, ..., log=FALSE) { 
  
  ypc <- y <- NULL
  
  if (missing(pred)) {
    pred <- o$pred
  } else {
    pred <- rlang::eval_tidy(rlang::enquo(pred), data)
  }
  if (is.null(pred)) {
    stop("No pred specified")
  }
  
  stratbin <- o$.stratbin #create loess predcorrect argument in function if want to use below stop because binless comes after predcorrect
  # if (is.null(stratbin)) {
  #     stop("Need to specify binning before pred correction. For binless method set argument loess.ypc = TRUE.")
  # }
  
  mpred <- data.table(stratbin, pred)
  mpred <- mpred[, mpred := median(pred), by=stratbin]
  mpred <- mpred$mpred
  
  if (log) {
    o$obs[, ypc := (mpred - pred) + y]
    o$sim[, ypc := (mpred - pred) + y]
  } else {
    o$obs[, ypc := (mpred/pred)*y]
    o$sim[, ypc := (mpred/pred)*y]
  }
  
  
  update(o, predcor=TRUE, predcor.log=log, pred=pred )
}

#' Remove prediction correction for Visual Predictive Check (VPC)
#' 
#' Optional function to use indicating no pred correction for VPC. 
#' 
#' @param o A \code{tidyvpcobj}.
#' @param ... Other arguments to include.
#' @export
nopredcorrect <- function(o, ...) UseMethod("nopredcorrect")

#' @rdname nopredcorrect
#' @export
nopredcorrect.tidyvpcobj <- function(o, ...) {
  update(o, predcor=FALSE)
}

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
  
  stopifnot(method$method %in% c("binless", "binning"))
  stopifnot(length(qpred) == 3)
  
  repl <- ypc <- y <- x <- blq <- lloq <- alq <- uloq <- NULL
  . <- list
  qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2
  

  obs      <- o$obs
  sim      <- o$sim
  predcor  <- o$predcor
  stratbin <- o$.stratbin
  xbin     <- o$xbin
  strat    <- o$strat
  
  
  
  if(method$method == "binless" && type == "continuous"){  
    o <- binlessaugment(o, qpred = qpred, interval =  method$optimization.interval, loess.ypc = method$loess.ypc)
    o <- binlessfit(o, conf.level = conf.level, llam.quant = method$lambda, span = method$span)
    }
  
  if(type == "categorical"){
    if(.isCensored(obs)){
      stop("Censoring not supported for categorical vpc")
    }
    
    ylvls <-  sort(unique(obs$y))

    #categorical binless vpcstats() ----
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
      
      for(i in seq_along(pobs.split)){
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
      if(length(sp_opt) != length(pobs.split)){
        stop(paste0("`incorrect number of elements specified to `sp` argument in `binless()` function, specify a list of length ", length(pobs.split), " in the following order: \n",
                    paste0(names(pobs.split), collapse = "\n"), "\n",
                    "Note: Do not specify a value for strata where not data is available."))
      }
      for(i in seq_along(pobs.split)){
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
    #categorical binning vpcstats() ----
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
    #update vpc
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
      warning("There are bins missing. Has binning been specified for all strata?", call.=F)
    }
    
    .stratbinrepl <- data.table(stratbin, sim[, .(repl)])
    
    myquant1 <- function(y, probs, qname=paste0("q", probs), type=quantile.type, blq=FALSE, alq=FALSE) {
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
    
    myquant2 <- function(y, probs, qname=paste0("q", probs), type=quantile.type) {
      y <- quantile(y, probs=probs, type=type, names=FALSE, na.rm=TRUE)
      setNames(as.list(y), qname)
    }
    
    if (isTRUE(predcor)) {
      qobs <- obs[, myquant1(ypc, probs=qpred, blq=blq,   alq=alq),   by=stratbin]
      qsim <- sim[, myquant1(ypc, probs=qpred, blq=FALSE, alq=FALSE), by=.stratbinrepl]
    } else {
      qobs <- obs[, myquant1(y, probs=qpred, blq=blq,   alq=alq),   by=stratbin]
      qsim <- sim[, myquant1(y, probs=qpred, blq=FALSE, alq=FALSE), by=.stratbinrepl]
    }
    
    .stratbinquant <- qsim[, !c("repl", "y")]
    qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2
    qqsim <- qsim[, myquant2(y, probs=qconf, qname=c("lo", "md", "hi")), by=.stratbinquant]
    stats <- qobs[qqsim, on=names(.stratbinquant)]
    stats <- xbin[stats, on=names(stratbin)]
    setkeyv(stats, c(names(o$strat), "xbin"))
    
    if (!is.null(obs$blq) && any(obs$blq)) {
      sim[, lloq := rep(obs$lloq, len=.N)]
      sim[, blq := (y < lloq)]
      pctblqobs <- obs[, .(y=100*mean(blq)), by=stratbin]
      pctblqsim <- sim[, .(y=100*mean(blq)), by=.stratbinrepl]
      .stratbinpctblq <- pctblqsim[, !c("repl", "y")]
      qpctblqsim <- pctblqsim[, myquant2(y, probs=qconf, qname=c("lo", "md", "hi")), by=.stratbinpctblq]
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
      qpctalqsim <- pctalqsim[, myquant2(y, probs=qconf, qname=c("lo", "md", "hi")), by=.stratbinpctalq]
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

#' @export
update.tidyvpcobj <- function(object, ...) {
  args <- list(...)
  for (i in names(args)) {
    object[[i]] <- args[[i, exact=TRUE]]
  }
  object
}

#' Print a \code{tidyvpcobj}
#' 
#' Print generic used to return information about VPC.
#' 
#' @param x An \code{tidyvpcobj}.
#' @param ... Further arguments can be specified but are ignored.
#' @return Returns \code{x} invisibly.
#' @export
print.tidyvpcobj <- function(x, ...) {
  if (!is.null(x$sim)) {
    nrep <- nrow(x$sim)/nrow(x$obs)
    if (isTRUE(x$predcor)) {
      cat("Prediction corrected ")
    }
    cat(sprintf("VPC with %d replicates", nrep), "\n")
  }
  cat(sprintf("Stratified by: %s", paste0(names(x$strat), collapse=", ")), "\n")
  if (!is.null(x$stats)) {
    print(x$stats)
  }
  invisible(x)
}

.binlessvpcstats <-  function(o, qpred=c(0.05, 0.5, 0.95), ..., conf.level=0.95, quantile.type=7, vpc.type){
  y <- x <- blq <- fit <- . <- repl <- cprop <- rqssmed <- llam.med <- c.rqssmed <-  NULL
  
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
  
  if(!is.null(obs$blq) && any(obs$blq)) {
    if(!is.null(o$strat)) {
      stratlloq <- c(names(o$strat), "lloq")
      lloq <- obs[, stratlloq, with = FALSE] 
      lloq <- unique(lloq)
      obs.fits <- obs.fits[lloq, on = names(o$strat)]
    } else {
      obs.fits[, lloq := rep(obs$lloq, len=.N)]    
    }
    obs.fits[, blq := ifelse(fit < lloq, TRUE, FALSE)]
  }
  
  obs.fits <- setnames(obs.fits[, lapply(.SD, median), by = x.binless], "fit", "y")
  sim.fits <- setnames(sim.fits, c("fit", "fit.lb", "fit.ub"), c("md", "lo", "hi"))
  
  if(!is.null(obs$blq) && any(obs$blq)) {
    obs.fits[, blq := ifelse(y < lloq, TRUE, FALSE)]
    obs.fits[, y := ifelse(blq == TRUE, NA, y)]
  }
  
  if (!is.null(o$strat)) {
    stats <- obs.fits[sim.fits, on = c("x", "qname", names(o$strat))]
  } else {
    stats <- obs.fits[sim.fits, on = c("x", "qname")]
  }
  
  if (!is.null(obs$blq) && any(obs$blq)) {
    if(is.null(o$strat)) {
    sim[, lloq := rep(obs$lloq, len=.N)]
    sim[, blq := (y < lloq)]
    setorder(obs, cols = x)
    cprop.obs <- obs[, cprop := cumsum(blq) / 1:length(blq)]
    
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
    
    setorder(sim, repl, x)[, cprop := cumsum(blq) / 1:length(blq), by=list(repl)]
    
    suppressWarnings(sim[, rqssmed := fitted(rqss(cprop ~ qss(x, lambda = exp(llam.med.cprop)),
                                                      tau = 0.5, na.action = na.exclude, .SD)), by = .(repl)])  
    
    u.x <- unique(cprop.obs$x) #%#
    med.obs.cprop <- tapply(cprop.obs$med, cprop.obs$x, median)
    med.sims.blq    <- tapply(sim$rqssmed, sim$x, median)
    med.sims.blq.lb <- tapply(sim$rqssmed, sim$x, quantile, probs=c(qconf[[1]]))
    med.sims.blq.ub <- tapply(sim$rqssmed, sim$x, quantile, probs=c(qconf[[3]]))
    pctblq <- data.table(cbind(u.x,med.obs.cprop, med.sims.blq.lb, med.sims.blq, med.sims.blq.ub))
    
    setnames(pctblq, c("x", "y", "lo", "md", "hi"))
    } else {
        strat <- o$strat
        strat.split <- split(obs, strat)
        strat.split <- strat.split[lapply(strat.split,NROW)>0]
        x.strat <- c("x", names(strat))
        sim[, lloq := rep(obs$lloq, len=.N), by = names(strat)]
        sim[, blq := (y < lloq)]
        stratx.binless <- obs[, list(x, o$strat)]
        stratxrepl <- data.table(stratx.binless, sim[, .(repl)])
        strat.split.sim <- split(sim, strat) 
        strat.split.sim <- strat.split.sim[lapply(strat.split.sim,NROW)>0] 
        sic.strat.cprop <- function(llam){
          a <- AIC(
            rqss(
              cprop ~
                qss(x, lambda=exp(llam)),
              tau=.5, na.action=na.exclude, data = strat.split[[i]]
            ),
            k=-1
          )
        }
        llam.strat.med.cprop <- vector("list", length(strat.split))
        for (i in seq_along(strat.split)) {
          setorder(strat.split[[i]], cols = x)
          strat.split[[i]] <- strat.split[[i]][, cprop := cumsum(blq) / 1:length(blq)]
          llam.strat.med.cprop[[i]]   <- strat.split[[i]][, .(llam.med = optimize(sic.strat.cprop,  interval=c(0, 7))$min)][,.(med = unlist(llam.med))]
          strat.split[[i]][, c.rqssmed := fitted(rqss(cprop ~ qss(x, lambda = exp(llam.strat.med.cprop[[i]][[1]])),tau= .5, na.action = na.exclude, data = strat.split[[i]]))]
        }
        
        obs.cprop <- rbindlist(strat.split)
        obs.cprop <- setnames(obs.cprop[, lapply(.SD, median, na.rm = TRUE), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "y")
       
        for (i in seq_along(strat.split.sim)) {
          setorder(strat.split.sim[[i]], cols = repl, x)
          strat.split.sim[[i]] <- strat.split.sim[[i]][, cprop := cumsum(blq) / 1:length(blq), by = .(repl)]
          strat.split.sim[[i]][, c.rqssmed := fitted(rqss(cprop ~ qss(x, lambda = exp(llam.strat.med.cprop[[i]][[1]])),tau= .5, na.action = na.exclude, .SD)), by = .(repl)]
        }
        
        sim.cprop <- rbindlist(strat.split.sim)
        sim.med <- setnames(sim.cprop[, lapply(.SD, median, na.rm = TRUE), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "md")
        sim.lb <- setnames(sim.cprop[, lapply(.SD, quantile, probs = qconf[[1]]), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "lo")
        sim.ub <- setnames(sim.cprop[, lapply(.SD, quantile, probs = qconf[[3]]), by = x.strat, .SDcols = "c.rqssmed"], "c.rqssmed", "hi")
        
        pctblq <- obs.cprop[sim.med, on = x.strat]
        pctblq <- pctblq[sim.lb, on = x.strat]
        pctblq <- pctblq[sim.ub, on = x.strat]
    }
  } else {
    pctblq <- NULL
  }
  
  update(o, stats = stats, pctblq = pctblq, vpc.type = vpc.type)
}


#' Obtain information about the bins from a \code{tidyvpcobj}
#' @param o An object.
#' @param ... Additional arguments.
#' @return A `data.table` containing the following columns:
#' \itemize{
#'   \item \code{nobs}: Number of observed data points in the bin
#'   \item \code{xmedian}: Median x-value of the observed data points in the bin
#'   \item \code{xmean}: Mean x-value of the observed data points in the bin
#'   \item \code{xmax}: Maximum x-value of the observed data points in the bin
#'   \item \code{xmin}: Minimum x-value of the observed data points in the bin
#'   \item \code{xmid}: Value halfway between `xmin` and `xmax`.
#'   x-value of the observed data points in the bin
#'   \item \code{xleft}: Value halfway between the minimum x-value of the
#'   current bin and the maximum x-value of the previous bin to the left (for
#'   the left-most bin, it is the minimum x-value).
#'   \item \code{xright}: Value halfway between the maximum x-value of the
#'   current bin and the minimum x-value of the next bin to the right (for the
#'   right-most bin, it is the maximum x-value).
#'   \item \code{xcenter}: Value halfway between `xleft` and `xright`.
#' }
#' In addition, if stratification was performed, the stratification columns will
#' be included as well.
#' @export
bininfo <- function(o, ...) UseMethod("bininfo")

#' @describeIn bininfo Method for \code{tidyvpcobj}.
#' @param by.strata Should the calculations be done by strata? Defaults to what
#'  was specified when the binning was done.
#' @export
bininfo.tidyvpcobj <- function(o, by.strata=o$bin.by.strata, ...) {
  
  x <- xmin <- xmax <- bin <- NULL
  
  f1 <- function(x) {
    nobs    <- sum(!is.na(x))
    xmedian <- median(x, na.rm=TRUE)
    xmean   <- mean(x, na.rm=TRUE)
    xmin    <- min(x, na.rm=TRUE)
    xmax    <- max(x, na.rm=TRUE)
    xmid    <- 0.5*(xmin + xmax)
    data.table(nobs, xmedian, xmean, xmin, xmax, xmid)
  }
  
  # Compute xleft and xright
  f2 <- function(xmin, xmax) {
    xmin    <- c(xmin, xmax[length(xmax)])
    xmax    <- c(xmin[1], xmax)
    breaks  <- 0.5*(xmin + xmax)
    xleft   <- breaks[-length(breaks)]
    xright  <- breaks[-1]
    xcenter <- 0.5*(xleft + xright)
    data.table(xleft, xright, xcenter)
  }
  if (by.strata && !is.null(o$strat)) {
    bi <- o$obs[, f1(x), by=o$.stratbin]
    setkeyv(bi, c(names(o$strat), "xmin"))
    bi[, c(.SD, f2(xmin, xmax)), by=names(o$strat)]
  } else {
    bi <- o$obs[, f1(x), by=bin]
    setkeyv(bi, "xmin")
    bi <- cbind(bi, bi[, f2(xmin, xmax)])
    bi <- bi[unique(o$.stratbin), on="bin"]
    setkeyv(bi, "xmin")
    bi[, c(names(o$.stratbin), setdiff(names(bi), names(o$.stratbin))), with=FALSE]
  }
}

#' Different functions that perform binning.
#'
#' @param breaks A numeric vector of values that designate cut points between bins.
#' @param centers A numeric vector of values that designate the center of each bin.
#' @param nbins The number of bins to split the data into.
#' @param style a binning style (see \link[classInt]{classIntervals} for details).
#' @return Each of these functions returns a function of a single numeric
#' vector `x` that assigns each value of `x` to a bin.
#' @examples
#'
#' x <- c(rnorm(10, 1, 1), rnorm(10, 3, 2), rnorm(20, 5, 3))
#' centers <- c(1, 3, 5)
#' nearest(centers)(x)
#'
#' breaks <- c(2, 4)
#' cut_at(breaks)(x)
#'
#' bin_by_eqcut(nbins=4)(x)
#' bin_by_ntile(nbins=4)(x)
#'
#' \donttest{
#' bin_by_pam(nbins=4)(x)
#' bin_by_classInt("pretty", nbins=4)(x)
#' }
#'
#' @name binningfunctions
NULL

#' @rdname binningfunctions
#' @export
cut_at <- function(breaks) {
  breaks <- .check_breaks(breaks)
  function(x, ..., right=FALSE) {
    breaks <- .resolve_breaks(breaks, ...)
    breaks <- sort(unique(breaks))
    if (min(x) < min(breaks)) {
      breaks <- c(min(x), breaks)
    }
    if (max(x) > max(breaks)) {
      breaks <- c(breaks, max(x))
    }
    as.character(cut(x, breaks, include.lowest=TRUE, right=right))
  }
}

#' @rdname binningfunctions
#' @export
nearest <- function(centers) {
  centers <- .check_centers(centers)
  function(x, ...) {
    centers <- .resolve_centers(centers, ...)
    centers <- sort(unique(centers))
    dist <- function(a, b) abs(a - b)
    d <- outer(x, centers, dist)
    m <- apply(d, 1, which.min)
    centers[m]
  }
}

#' @rdname binningfunctions
#' @export
bin_by_ntile <- function(nbins) {
  nbins <- .check_nbins(nbins)
  function(x, ...) {
    nbins <- .resolve_nbins(nbins, ...)
    
    # Mimic the function from dplyr
    len <- sum(!is.na(x))
    r <- rank(x, ties.method="first", na.last="keep")
    as.integer(floor(nbins*(r - 1)/len + 1))
  }
}

#' @rdname binningfunctions
#' @export
bin_by_eqcut <- function(nbins) {
  nbins <- .check_nbins(nbins)
  function(x, ..., quantile.type=7) {
    nbins <- .resolve_nbins(nbins, ...)
    
    # Mimic the function from table1
    breaks <- quantile(x, probs=seq.int(nbins - 1)/nbins, na.rm=TRUE, type=quantile.type)
    cut_at(breaks)(x)
  }
}

#' @rdname binningfunctions
#' @export
bin_by_pam <- function(nbins) {
  has_cluster <- requireNamespace("cluster", quietly=TRUE)
  if (!has_cluster) {
    stop("Package 'cluster' is required to use the binning method. Please install it.")
  }
  nbins <- .check_nbins(nbins)
  function(x, ...) {
    nbins <- .resolve_nbins(nbins, ...)
    
    centers <- sort(cluster::pam(x, nbins)$medoids)
    nearest(centers)(x)
  }
}

#' @rdname binningfunctions
#' @export
bin_by_classInt <- function(style, nbins=NULL) {
  has_classInt <- requireNamespace("classInt", quietly=TRUE)
  if (!has_classInt) {
    stop("Package 'classInt' is required to use the binning method. Please install it.")
  }
  style <- style
  if (!is.null(nbins)) {
    nbins <- .check_nbins(nbins)
  }
  function(x, ...) {
    args <- list(var=x, style=style)
    if (!is.null(nbins)) {
      nbins <- .resolve_nbins(nbins, ...)
      args$n <- nbins
    }
    args <- c(args, list(...))
    if (style %in% c("kmeans", "hclust", "dpih")) {
      # These don't accept '...' arguments
      args1 <- args[intersect(names(args), methods::formalArgs(classInt::classIntervals))]
      args2 <- if (style == "kmeans") {
        args[intersect(names(args), methods::formalArgs(stats::kmeans))]
      } else if (style == "hclust") {
        args[intersect(names(args), methods::formalArgs(stats::hclust))]
      } else if (style == "dpih") {
        has_KernSmooth <- requireNamespace("KernSmooth", quietly=TRUE)
        if (!has_KernSmooth) {
          stop("Package 'KernSmooth' is required to use the binning method. Please install it.")
        }
        args[intersect(names(args), methods::formalArgs(KernSmooth::dpih))]
      } else {
        list()
      }
      args <- c(args1, args2)
    }
    args <- args[!duplicated(args)]
    breaks <- do.call(classInt::classIntervals, args)$brks
    cut_at(breaks)(x)
  }
}

#' Perform a consistency check on observed and simulated data
#' 
#' This function performs a simple consistency check on an observed and
#' simulated dataset to make sure they are consistent with respect to ordering
#' as required by the other functions used in the VPC calculation.
#'
#' The consistency check is performed by comparing a combination of unique
#' subject identifier (ID) and time. Both \code{data.frame} objects must be given with
#' those in positions 1 and 2, respectively.
#'
#' @param obs,sim A `data.frame` with 2 columns (see Details).
#' @param tol A tolerance for comparing time values.
#' @return The number of replicates contained in `sim`.
#' @seealso \code{\link{observed}}, \code{\link{simulated}}.
#' @examples
#'
#' \donttest{
#' 
#'  require(data.table)
#'  
#' check_order(obs_data[, .(ID, TIME)], sim_data[, .(ID, TIME)])
#' }
#' @export
check_order <- function(obs, sim, tol=1e-5) {
  if (nrow(sim) %% nrow(obs) != 0) {
    stop("Rows in sim is not a multiple of rows in obs")
  }
  if (is.numeric(obs[[1]])) obs[[1]] <- as.numeric(obs[[1]])
  if (is.numeric(sim[[1]])) sim[[1]] <- as.numeric(sim[[1]])
  if (is.factor(obs[[1]])) obs[[1]] <- as.character(obs[[1]])
  if (is.factor(sim[[1]])) sim[[1]] <- as.character(sim[[1]])
  if (!identical(rep(obs[[1]], len=nrow(sim)), sim[[1]])) {
    stop("ID columns are not identical")
  } 
  if (!all(abs(rep(obs[[2]], len=nrow(sim)) - sim[[2]]) < tol)) {
    stop("Time columns are not equal")
  } 
  nrow(sim) / nrow(obs)
}


# Internal function
.check_centers <- function(centers) {
  if (is.data.frame(centers)) {
    centers <- as.data.table(centers)
    if (is.null(centers$centers)) {
      stop("centers data.frame must contain column centers")
    }
    if (any(is.na(centers$centers))) {
      stop("centers cannot contain missing values")
    }
    keycols <- setdiff(names(centers), "centers")
    setkeyv(centers, keycols)
  } else if (is.numeric(centers)) {
    if (any(is.na(centers))) {
      stop("centers cannot contain missing values")
    }
  } else {
    stop("centers must be a numeric vector or data.frame")
  }
  centers
}

# Internal function
.resolve_centers <- function(centers, ...) {
  if (is.data.table(centers)) {
    keycols <- key(centers)
    key <- as.data.table(list(...)[keycols])
    centers <- unique(centers[key]$centers)
  }
  if (is.null(centers) || !is.numeric(centers) || any(is.na(centers))) {
    stop("invalid centers")
  }
  centers
}

# Internal function
.check_breaks <- function(breaks) {
  if (is.data.frame(breaks)) {
    breaks <- as.data.table(breaks)
    if (is.null(breaks$breaks)) {
      stop("breaks data.frame must contain column breaks")
    }
    if (any(is.na(breaks$breaks))) {
      stop("breaks cannot contain missing values")
    }
    keycols <- setdiff(names(breaks), "breaks")
    setkeyv(breaks, keycols)
  } else if (is.numeric(breaks)) {
    if (any(is.na(breaks))) {
      stop("breaks cannot contain missing values")
    }
  } else {
    stop("breaks must be a numeric vector or data.frame")
  }
  breaks
}

# Internal function
.resolve_breaks <- function(breaks, ...) {
  if (is.data.table(breaks)) {
    keycols <- key(breaks)
    key <- as.data.table(list(...)[keycols])
    breaks <- breaks[key]$breaks
  }
  if (is.null(breaks) || !is.numeric(breaks) || any(is.na(breaks))) {
    stop("invalid breaks")
  }
  breaks
}

# Internal function
.check_nbins <- function(nbins) {
  if (is.data.frame(nbins)) {
    nbins <- as.data.table(nbins)
    if (is.null(nbins$nbins)) {
      stop("nbins data.frame must contain column nbins")
    }
    if (any(is.na(nbins$nbins))) {
      stop("nbins cannot contain missing values")
    }
    keycols <- setdiff(names(nbins), "nbins")
    setkeyv(nbins, keycols)
  } else if (is.numeric(nbins) && length(nbins) == 1) {
    if (any(is.na(nbins))) {
      stop("nbins cannot contain missing values")
    }
  } else {
    stop("nbins must be a numeric vector of length 1 or data.frame")
  }
  nbins
}

# Internal function
.resolve_nbins <- function(nbins, ...) {
  if (is.data.table(nbins)) {
    keycols <- key(nbins)
    key <- as.data.table(list(...)[keycols])
    nbins <- unique(nbins[key]$nbins)
  }
  if (is.null(nbins) || !(is.numeric(nbins) && length(nbins) == 1 && !is.na(nbins))) {
    stop("nbins must be uniquely determined")
  }
  nbins
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
  
  # Internal Function
  .sic.strat.ypc <- function(llam, quant) {
    a <- AIC(
      rqss(
        l.ypc ~
          qss(x, lambda=exp(llam)),
        tau=quant, na.action=na.exclude, data = strat.split[[i]]
      ),
      k=-1
    )
  }
  .sic.strat <- function(llam, quant){
    a <- AIC(
      rqss(
        y ~
          qss(x, lambda=exp(llam)),
        tau=quant, na.action=na.exclude, data = strat.split[[i]]
      ),
      k=-1
    )
  }
  
  .sic.ypc <- function(llam, quant){
    a <- AIC(
      rqss(
        l.ypc ~
          qss(x, lambda=exp(llam)),
        tau=quant, na.action=na.exclude, data = obs
      ),
      k=-1
    )
  }
  
  .sic <- function(llam, quant){
    a <- AIC(
      rqss(
        y ~
          qss(x, lambda=exp(llam)),
        tau=quant, na.action=na.exclude, data = obs
      ),
      k=-1
    )
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
  
  environment(llamoptimize) <- environment()
  . <- list
  if(!is.null(o$strat.split)) {
    llam.strat.lo  <- vector("list", length(strat.split))
    llam.strat.med <- vector("list", length(strat.split))
    llam.strat.hi  <- vector("list", length(strat.split))
    
    for (i in seq_along(strat.split)) {
      llam.strat.lo[[i]]    <- strat.split[[i]][, .(llam.lo  = optimize(llamoptimize, quant = qpred[1], interval = interval)$min)][,.(lo = unlist(llam.lo))]
      names(llam.strat.lo)  <- names(strat.split)
      setnames(llam.strat.lo[[i]], paste0("q", qpred[1]))
      llam.strat.med[[i]]   <- strat.split[[i]][, .(llam.med = optimize(llamoptimize, quant = qpred[2], interval = interval)$min)][,.(med = unlist(llam.med))]
      names(llam.strat.med) <- names(strat.split)
      setnames(llam.strat.med[[i]], paste0("q", qpred[2]))
      llam.strat.hi[[i]]    <- strat.split[[i]][, .(llam.hi  = optimize(llamoptimize, quant = qpred[3], interval = interval)$min)][,.(hi = unlist(llam.hi))]
      names(llam.strat.hi)  <- names(strat.split)
      setnames(llam.strat.hi[[i]], paste0("q", qpred[3]))
    }
    
    llam.qpred <- cbind(list(llam.strat.lo, llam.strat.med, llam.strat.hi))
    names(llam.qpred) <- paste0("q", qpred)
  } else {
    llam.lo  <- obs[, .(llam.lo = optimize(llamoptimize, quant = qpred[1], interval = interval)$min)]
    llam.med <- obs[, .(llam.med = optimize(llamoptimize, quant = qpred[2], interval = interval)$min)]
    llam.hi  <- obs[, .(llam.hi = optimize(llamoptimize, quant = qpred[3], interval = interval)$min)]
    
    llam.qpred <- c(llam.lo, llam.med, llam.hi)
    names(llam.qpred) <- paste0("q", qpred)
    llam.qpred <- unlist(llam.qpred)
  }
  
  update(o, llam.qpred = llam.qpred, span = span, qpred = qpred, loess.ypc = loess.ypc)
}


binlessfit <- function(o, conf.level = .95, llam.quant = NULL, span = NULL, ...){
  y <- l.ypc <- repl <- NULL  
  . <- list
  qpred <- o$qpred
  qnames <- paste0("q", as.character(qpred))
  qconf <- c(0, 0.5, 1) + c(1, 0, -1)*(1 - conf.level)/2
  
  obs <- o$obs
  sim <- o$sim
  
  if(isTRUE(o$loess.ypc)) {
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
    sim.strat <- sim[, c(names(strat)) := rep(strat, len = .N), by = .(repl)]
    strat.split.sim <- split(sim, strat)
    strat.split.sim <- strat.split.sim[lapply(strat.split.sim,NROW)>0] 
  }
  
  if(isTRUE(o$loess.ypc) && !is.null(o$strat)) {
    if(isTRUE(o$predcor.log)) {
      for(i in seq_along(strat.split)) {
        strat.split[[i]][, l.ypc := y +  (fitted(loess(pred ~ x, span = span[[i]], na.action = na.exclude, .SD)) - pred)]
      }
    } else {
      for(i in seq_along(strat.split)) {
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

# Internal Functions ----

# Dispatches AIC.gam method
.gam_optimize <- function(sp, y, x, data){
  suppressWarnings(
  AIC(gam(y ~ s(x, k = 1), sp = sp, family = "binomial", data = data))
  )
}

.fitcatgam <- function(y, x, sp){
  suppressWarnings(
  fitted(gam(y ~ s(x, k = 1), family = "binomial", sp = c(sp)))
  )
}

#Below function used for fitting rqss
.fitobs <- function(obs, llam.qpred, qpred, l.ypc = FALSE) {
  rqsslo <- rqssmed <- rqsshi <- NULL
  qnames <- paste0("q", as.character(qpred))
  
  if(l.ypc) {
    obs[, rqsslo := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[1]])), tau = qpred[1], na.action = na.exclude, data = obs))]
    obs[, rqssmed := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[2]])), tau = qpred[2], na.action = na.exclude, data = obs))]
    obs[, rqsshi := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[3]])), tau = qpred[3], na.action = na.exclude, data = obs))]
    setnames(obs, c("rqsslo", "rqssmed", "rqsshi"), qnames)
    obs.fits <- melt(obs, id.vars = "x", measure.vars = qnames)
    obs.fits <- setnames(obs.fits, c("variable", "value"), c("qname", "fit"))
  } else {
    obs[, rqsslo := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[1]])), tau = qpred[1], na.action = na.exclude, data = obs))]
    obs[, rqssmed := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[2]])), tau = qpred[2], na.action = na.exclude, data = obs))]
    obs[, rqsshi := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[3]])), tau = qpred[3], na.action = na.exclude, data = obs))]
    setnames(obs, c("rqsslo", "rqssmed", "rqsshi"), qnames)
    obs.fits <- melt(obs, id.vars = "x", measure.vars = qnames)
    obs.fits <- setnames(obs.fits, c("variable", "value"), c("qname", "fit"))
  }
  return(obs.fits)
}

# Internal Function
.fitobs.strat <- function(strat.split, x.strat, llam.qpred, qpred, l.ypc = FALSE) {
  rqsslo <- rqssmed <- rqsshi <- NULL
  qnames <- paste0("q", as.character(qpred))
  
  if(l.ypc) {
    for (i in seq_along(strat.split)) {
      strat.split[[i]][, rqsslo  := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[1]][[i]][[1]])),tau= qpred[1], na.action = na.exclude, data = strat.split[[i]]))]
      strat.split[[i]][, rqssmed := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[2]][[i]][[1]])),tau= qpred[2], na.action = na.exclude, data = strat.split[[i]]))]
      strat.split[[i]][, rqsshi  := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[3]][[i]][[1]])),tau= qpred[3], na.action = na.exclude, data = strat.split[[i]]))]
    }
  } else {
    for (i in seq_along(strat.split)) {
      strat.split[[i]][, rqsslo  := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[1]][[i]][[1]])),tau= qpred[1], na.action = na.exclude, data = strat.split[[i]]))]
      strat.split[[i]][, rqssmed := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[2]][[i]][[1]])),tau= qpred[2], na.action = na.exclude, data = strat.split[[i]]))]
      strat.split[[i]][, rqsshi  := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[3]][[i]][[1]])),tau= qpred[3], na.action = na.exclude, data = strat.split[[i]]))]
    }
  }
  strat.obs.fits <- rbindlist(strat.split)
  strat.obs.fits <- setnames(strat.obs.fits, c("rqsslo", "rqssmed", "rqsshi"), qnames)
  strat.obs.fits <- melt(strat.obs.fits, id.vars = x.strat, measure.vars = qnames)
  strat.obs.fits <- setnames(strat.obs.fits, c("variable", "value"), c("qname", "fit"))
}

# Internal Function
.fitsim <- function(sim, llam.qpred, span = NULL, qpred, qconf, l.ypc = FALSE, log = FALSE) {
  . <- list
  rqsslo <- rqssmed <- rqsshi <- y <- pred <- repl <- NULL
  qnames <- paste0("q", as.character(qpred))
  
  if(l.ypc) {
    if(log) {
      sim[, l.ypc := y + (fitted(loess(pred ~ x, span = span, na.action = na.exclude, .SD)) - pred), by = .(repl)]
    } else {
      sim[, l.ypc := y * (fitted(loess(pred ~ x, span = span, na.action = na.exclude, .SD)) / pred), by = .(repl)]
    }
    suppressWarnings(sim[, rqsslo := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[1]])),
                                                 tau  = qpred[1], na.action = na.exclude, .SD)), by = .(repl)]) 
    suppressWarnings(sim[, rqssmed := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[2]])),
                                                  tau = qpred[2], na.action = na.exclude, .SD)), by = .(repl)]) 
    suppressWarnings(sim[, rqsshi := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[3]])),
                                                 tau  = qpred[3], na.action = na.exclude, .SD)), by = .(repl)])
    setnames(sim, c("rqsslo", "rqssmed", "rqsshi"), qnames)
  } else {
    suppressWarnings(sim[, rqsslo := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[1]])),
                                                 tau  = qpred[1], na.action = na.exclude, .SD)), by = .(repl)]) 
    suppressWarnings(sim[, rqssmed := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[2]])),
                                                  tau = qpred[2], na.action = na.exclude, .SD)), by = .(repl)]) 
    suppressWarnings(sim[, rqsshi := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[3]])),
                                                 tau  = qpred[3], na.action = na.exclude, .SD)), by = .(repl)])
    setnames(sim, c("rqsslo", "rqssmed", "rqsshi"), qnames)
  }
  
  sim.lb <- sim[, lapply(.SD, quantile, probs = qconf[[1]]), by = "x"] #CI lb
  sim.lb <- setnames(melt(sim.lb, id.vars = "x", measure.vars = qnames), c("x", "qname", "fit.lb"))  #Wide to long
  
  sim.ub <- sim[, lapply(.SD, quantile, probs = qconf[[3]]), by = "x"] #CI ub
  sim.ub <- setnames(melt(sim.ub, id.vars = "x", measure.vars = qnames), c("x", "qname", "fit.ub"))   #Wide to long
  
  sim <-  sim[, lapply(.SD, median, na.rm = TRUE), by = "x"] #Med fits
  sim <- setnames(melt(sim, id.vars = "x", measure.vars = qnames), c("x", "qname", "fit")) #wide to long
  
  sim <- sim[sim.lb, on = c("x", "qname")]
  sim <- sim[sim.ub, on = c("x", "qname")]
}

# Internal Function
.fitsim.strat <- function(strat.split.sim, x.strat, llam.qpred, span = NULL, qpred, qconf, l.ypc = FALSE, log = FALSE) {
  . <- list
  rqsslo <- rqssmed <- rqsshi <- y <- pred <- repl <- NULL
  qnames <- paste0("q", as.character(qpred))
  
  if(l.ypc) {
    if(log) {
      for (i in seq_along(strat.split.sim)) {
        strat.split.sim[[i]][, l.ypc := y + (fitted(loess(pred ~ x, span = span[[i]], na.action = na.exclude, .SD)) - pred), by = .(repl)]
        
        suppressWarnings(strat.split.sim[[i]][, rqsslo := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[1]][[i]][[1]])),
                                                                      tau  = qpred[1], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.lo = unlist(rqsslo))])
        
        suppressWarnings(strat.split.sim[[i]][, rqssmed := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[2]][[i]][[1]])),
                                                                       tau = qpred[2], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.med = unlist(rqssmed))])
        
        suppressWarnings(strat.split.sim[[i]][, rqsshi := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[3]][[i]][[1]])),
                                                                      tau  = qpred[3], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.hi = unlist(rqsshi))])
      }
    } else {
      for (i in seq_along(strat.split.sim)) {
        strat.split.sim[[i]][, l.ypc := y * (fitted(loess(pred ~ x, span = span[[i]], na.action = na.exclude, .SD)) / pred), by = .(repl)]
        
        suppressWarnings(strat.split.sim[[i]][, rqsslo := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[1]][[i]][[1]])),
                                                                      tau  = qpred[1], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.lo = unlist(rqsslo))])
        
        suppressWarnings(strat.split.sim[[i]][, rqssmed := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[2]][[i]][[1]])),
                                                                       tau = qpred[2], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.med = unlist(rqssmed))])
        
        suppressWarnings(strat.split.sim[[i]][, rqsshi := fitted(rqss(l.ypc ~ qss(x, lambda = exp(llam.qpred[[3]][[i]][[1]])),
                                                                      tau  = qpred[3], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.hi = unlist(rqsshi))])
      }
    }
  } else {  
    for (i in seq_along(strat.split.sim)) {
      suppressWarnings(strat.split.sim[[i]][, rqsslo := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[1]][[i]][[1]])),
                                                                    tau  = qpred[1], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.lo = unlist(rqsslo))])
      
      suppressWarnings(strat.split.sim[[i]][, rqssmed := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[2]][[i]][[1]])),
                                                                     tau = qpred[2], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.med = unlist(rqssmed))])
      
      suppressWarnings(strat.split.sim[[i]][, rqsshi := fitted(rqss(y ~ qss(x, lambda = exp(llam.qpred[[3]][[i]][[1]])),
                                                                    tau  = qpred[3], na.action = na.exclude, .SD)), by = .(repl)][,.(fit.hi = unlist(rqsshi))])
    }
  }
  
  sim <- rbindlist(strat.split.sim)
  setnames(sim, c("rqsslo", "rqssmed", "rqsshi"), qnames)
  
  sim.lb <- sim[, lapply(.SD, quantile, probs = qconf[[1]]), by = x.strat, .SDcols = qnames]
  sim.lb <- setnames(melt(sim.lb, id.vars = x.strat, measure.vars = qnames), c("variable", "value"), c("qname", "fit.lb"))
  
  sim.ub <- sim[, lapply(.SD, quantile, probs = qconf[[3]]), by = x.strat, .SDcols = qnames]
  sim.ub <- setnames(melt(sim.ub, id.vars = x.strat, measure.vars = qnames), c("variable", "value"), c("qname", "fit.ub"))
  
  sim <- sim[, lapply(.SD, median, na.rm = TRUE), by = x.strat, .SDcols = qnames]
  sim <- setnames(melt(sim, id.vars = x.strat, measure.vars = qnames), c("variable", "value"), c("qname", "fit"))
  
  sim <- sim[sim.lb, on = c(x.strat, "qname")]
  sim <- sim[sim.ub, on = c(x.strat, "qname")]
}



# Internal function for optimizing loess fit
.aicc.loess <- function(fit){
  #compute AIC_C for a LOESS fit, from:
  #Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing
  #parameter selection in nonparametric regression using an improved
  #Akaike Information Criterion. Journal of the Royal Statistical
  #Society B 60: 271–293.
  
  stopifnot(inherits(fit, 'loess'))
  n <- fit$n
  trace <- fit$trace.hat
  sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
  return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
}

# Internal function for optimizing loess fit
.autoloess <- function(fit, span=c(.1, .9), ...){
  #compute loess fit which has span minimizes AIC_C
  #fit = loess fit; span parameter value doesn't matter
  #span = a two-value vector representing the minimum and maximum span values
  #Returns LOESS fit with span minimizing the AIC_C function
  
  stopifnot(inherits(fit, 'loess')) #, length(span) == 2)
  
  #loss function in form to be used by optimize
  f <- function(span) .aicc.loess(update(fit, span=span))
  
  #find best loess according to loss function
  opt.fit  <- update(fit, span=optimize(f, span)$minimum)
  opt.span <- optimize(f, span)$minimum
  return(list(fit=opt.fit, span=opt.span))
  
}

# Internal function for returning optimized span by strata
.getspan <- function(x) {
  span <- vector("list", length(x))
  for (i in seq_along(x)) {
    span[[i]] <- x[[i]]$span
  }
  names(span) <- names(x)
  return(span)
}

# Internal function for returning l.ypc fits by strata
.getfitted <- function(x) {
  fits <- vector("list", length(x))
  for (i in seq_along(x)) {
    fits[[i]] <- as.data.table(x[[i]]$loess[[1]]$fit$fitted)
    fits[[i]] <- setnames(fits[[i]], "fitted")
  }
  names(fits) <- names(x)
  return(fits)
}


.getfit <- function(x) {
  fit <- vector("list", length(x))
  for (i in seq_along(x)) {
    fit[[i]] <- x[[i]]$loess[[1]]$fit
  }
  names(fit) <- names(x)
  return(fit)
}

.isCensored <- function(obs) {
  if(!is.null(obs$blq) && any(obs$blq)){
    ret <- TRUE
  } else if (!is.null(obs$alq) && any(obs$alq)){
    ret <- TRUE
  } else {
    ret <- FALSE
  }
  
  ret
  
}



