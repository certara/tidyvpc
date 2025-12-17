#' reference corrected Visual Predictive Check (rcVPC)
#' @details This function creates a reference corrected Visual Predictive Check (rcVPC)
#' @title refcorrect
#' @param o tidyvpc object
#' @param yref dependent variable
#' @param xref independent variable
#' @param ypred population prediction variable
#' @param refdata simulated reference data
#' @param PRED_simdata the simulated data with the prediction variable of the independent variable, with the same variable name as xref
#' @param PRED_refdata the simulated reference data with the prediction variable of the independent variable, with the same variable name as xref
#' @param IDV_refcorr flag if the Independent variable should be reference corrected as well
#' @param ... Other arguments to include
#' @return Updates \code{tidyvpcobj} with required information to performing Reference correction which include \code{cor} logical indicating whether
#'   corrected VPC is to be performed, \code{cor.log} logical indicating whether the DV is on a log-scale.
#' @examples 
#' require(magrittr)
#' 
#' obs_data <- obs_data[MDV == 0]
#' sim_data <- sim_data[MDV == 0]
#' ref_data <- ref_data[MDV == 0]
#' 
#'   vpc <- observed(obs_data, x=TIME, y=DV) %>%
#'        simulated(sim_data, y=DV) %>%
#'        refcorrect(refdata = ref_data, yref=DV, xref=TIME, ypred=PRED) %>% 
#'        binning(bin = NTIME) %>%
#'        vpcstats()
#'        
#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{censoring}} \code{\link{binning}} \code{\link{vpcstats}}
#' @export
# Function to calculate rpvcDV and rpvcDVobs for DV:
refcorrect <- function(o, ...) UseMethod("refcorrect")

#' @rdname refcorrect
#' @export
refcorrect.tidyvpcobj <- function(o,
                                  yref,
                                  xref,
                                  ypred,
                                  refdata,
                                  obsdata=o$data,
                                  simdata=o$simdata,
                                  PRED_simdata=NULL,
                                  PRED_refdata=NULL,
                                  IDV_refcorr=FALSE,
                                  log=o$lnDV,
                                   ...) {
  
  if (!is.null(o$vpc.method)) {
    stop("refcorrect() should be used before binning()")
  }

if (IDV_refcorr) {
  
  n_row<-nrow(simdata)
  DV <-rlang::enquo(yref)
  IDV<-rlang::enquo(xref)
  PRED<-rlang::enquo(ypred)
  
# Read observed DV
  obsDV<-rep(o$obs$y,max(o$sim$repl))
  obsIDV<-rep(o$obs$x,max(o$sim$repl))
  
# Read PRED and simulated DV:
 sim1<-simdata %>% 
  dplyr::mutate(obs_DV  = obsDV,
                sim_DV  = o$sim$y,
                obs_IDV = obsIDV,
                sim_IDV = !!IDV,
                sim_PRED = !!PRED,
                NREF = REP * 1E8 + REF)

 # Read PRED of IDV:
 sim2<-PRED_simdata %>%
   dplyr::mutate(NREF= REP * 1E8 + REF,
                 IDV_PRED= !!IDV) %>%  
   dplyr::select(NREF,IDV_PRED)
 

# Read simulated DVref & PREDref:
sim3<-refdata %>% 
  dplyr::mutate(NREF         = REP * 1E8 + REF,
                ref_DV       = !!DV,
                ref_PRED     = !!PRED,
                ref_IDV      = !!IDV) %>% 
  dplyr::select(NREF,ref_PRED,ref_DV,ref_IDV) 

# Read PRED of ref_IDV:
sim4<-PRED_refdata %>%
  dplyr::mutate(NREF= REP * 1E8 + REF,
                ref_IDV_PRED= !!IDV) %>%  
  dplyr::select(NREF,ref_IDV_PRED)

merged_data <- merge(merge(sim1, sim2, by = "NREF"), 
                     merge(sim4, sim3, by = "NREF"),
                     by = "NREF") %>% 
  mutate(RIDV_IPRED=ifelse(ref_IDV_PRED < 1E-6 | IDV_PRED < 1E-6, 1, 
                           ref_IDV_PRED / IDV_PRED)) %>% 
  filter(obs_IDV!=0) 

 ## Calculate prediction corrected DV and ODV:
 if (log) {
   merged_data<-merged_data %>% 
   dplyr::mutate(lnDVref    = ref_DV,
                 rpcDV      = sim_DV + (ref_PRED - sim_PRED),
                 lnrpcDV    = rpcDV,
                 rpcDVobs   = obs_DV + (ref_PRED - sim_PRED),
                 lnrpcDVobs = rpcDVobs,
                 lnIDVref    = log_protected(ref_IDV),
                 rpcIDV      = sim_IDV * RIDV_IPRED,
                 lnrpcIDV    = log_protected(rpcIDV),
                 rpcIDVobs   = obs_IDV * RIDV_IPRED,
                 lnrpcIDVobs = log_protected(rpcIDVobs))

 } else {
   merged_data<-merged_data %>% 
   dplyr::mutate(lnDVref     = log(ref_DV),
                 rpcDV       = sim_DV * ref_PRED / sim_PRED,
                 lnrpcDV     = log(rpcDV),
                 rpcDVobs    = obs_DV * ref_PRED / sim_PRED,
                 lnrpcDVobs  = log(rpcDVobs),
                 lnIDVref    = log_protected(ref_IDV),
                 rpcIDV      = sim_IDV * RIDV_IPRED,
                 lnrpcIDV    = log_protected(rpcIDV),
                 rpcIDVobs   = obs_IDV * RIDV_IPRED,
                 lnrpcIDVobs = log_protected(rpcIDVobs))
 }
 
## Calculate SD of DV, DVref
 merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnrpcDV", 
                                        by = "REF"),by = "REF")%>% rename("SDlnrpcDV" = "SD")
 merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnDVref", 
                                        by = "REF"),by = "REF")%>% rename("SDlnDVref" = "SD")
 
 merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnrpcIDV", 
                                               by = "REF"),by = "REF")%>% rename("SDlnrpcIDV" = "SD")
 merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnIDVref", 
                                               by = "REF"),by = "REF")%>% rename("SDlnIDVref" = "SD")
  
  ## Calculate rpvc for DV, ODV:
  if (log) {
    merged_data<-merged_data %>% 
      dplyr::mutate(rpvcDV     = ref_PRED + (lnrpcDV - ref_PRED) * (SDlnDVref / SDlnrpcDV),
                    rpvcDVobs  = ref_PRED + (lnrpcDVobs - ref_PRED) * (SDlnDVref /SDlnrpcDV),
                    rpvcIDV     = exp(log(ref_IDV_PRED) + (lnrpcIDV - log(ref_IDV_PRED)) * (SDlnIDVref / SDlnrpcIDV)),
                    rpvcIDVobs  = exp(log(ref_IDV_PRED) + (lnrpcIDVobs - log(ref_IDV_PRED)) * (SDlnIDVref /SDlnrpcIDV)))
  } else {
    merged_data<-merged_data %>% 
      dplyr::mutate(rpvcDV     = exp(log(ref_PRED) + (lnrpcDV - log(ref_PRED)) * (SDlnDVref / SDlnrpcDV)),
                    rpvcDVobs  = exp(log(ref_PRED) + (lnrpcDVobs - log(ref_PRED)) * (SDlnDVref /SDlnrpcDV)),
                    rpvcIDV     = exp(log(ref_IDV_PRED) + (lnrpcIDV - log(ref_IDV_PRED)) * (SDlnIDVref / SDlnrpcIDV)),
                    rpvcIDVobs  = exp(log(ref_IDV_PRED) + (lnrpcIDVobs - log(ref_IDV_PRED)) * (SDlnIDVref /SDlnrpcIDV)))
  }
  
    o$obs<-filter(o$obs,x!=0)
    o$sim<-filter(o$sim,x!=0)
    
    o$obs$ypc<-merged_data$rpvcDVobs[merged_data$REP==1]
    o$sim$ypc<-merged_data$rpvcDV
    o$obs$x<- merged_data$rpvcIDVobs[merged_data$REP==1]
    o$sim$x<- merged_data$rpvcIDV
    # o$obs[, x   := merged_data$rpvcIDVobs[merged_data$REP==1]]
    # o$sim[, x   := merged_data$rpvcIDV]
    
} else {
  
  DV <-rlang::enquo(yref)
  IDV<-rlang::enquo(xref)
  PRED<-rlang::enquo(ypred)

  # Read observed DV
  obsDV<-rep(o$obs$y,max(o$sim$repl))
  obsIDV<-rep(o$obs$x,max(o$sim$repl))
  # Read PRED and simulated DV:
  sim1<-simdata %>% 
    dplyr::mutate(obs_DV  = obsDV,
                  sim_DV  = o$sim$y,
                  obs_IDV = obsIDV,
                  sim_IDV = o$sim$x,
                  sim_PRED= !!PRED,
                  NREF    = REP * 1E8 + REF)
  
  # Read simulated DVref & PREDref:
  sim2<-refdata %>% 
    dplyr::mutate(NREF         = REP * 1E8 + REF,
                  ref_PRED     = !!PRED,
                  ref_DV       = !!DV,
                  ref_IDV      = !!IDV) %>% 
    dplyr::select(NREF,ref_PRED,ref_DV,ref_IDV) 
  
  merged_data <- merge(sim1, sim2, by = "NREF")
  #%>%mutate(RPRED=ifelse(ref_PRED < 0.0001 & sim_PRED < 0.0001, 1, ref_PRED / sim_PRED))
  
  ## Calculate prediction corrected DV and ODV:
  if (log) {
    merged_data<-merged_data %>% 
      dplyr::mutate(lnDVref    = ref_DV,
                    rpcDV      = sim_DV + (ref_PRED - sim_PRED),
                    lnrpcDV    = rpcDV,
                    rpcDVobs   = obs_DV + (ref_PRED - sim_PRED),
                    lnrpcDVobs = rpcDVobs
      )
  } else {
    merged_data<-merged_data %>% 
      dplyr::mutate(lnDVref    = log(ref_DV),
                    rpcDV      = sim_DV * ref_PRED / sim_PRED,
                    lnrpcDV    = log(rpcDV),
                    rpcDVobs   = obs_DV * ref_PRED / sim_PRED,
                    lnrpcDVobs = log(rpcDVobs))
  }
  
  ## Calculate SD of DV, DVref
  merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnrpcDV", 
                                                by = "REF"),by = "REF")%>% rename("SDlnrpcDV" = "SD")
  merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnDVref", 
                                                by = "REF"),by = "REF")%>% rename("SDlnDVref" = "SD")
  
  ## Calculate rpvc for DV, ODV:
  if (log) {
    merged_data<-merged_data %>% 
      dplyr::mutate(rpvcDV     = ref_PRED + (lnrpcDV - ref_PRED) * (SDlnDVref / SDlnrpcDV),
                    rpvcDVobs  = ref_PRED + (lnrpcDVobs - ref_PRED) * (SDlnDVref /SDlnrpcDV))
  } else {
    merged_data<-merged_data %>% 
      dplyr::mutate(rpvcDV     = exp(log(ref_PRED) + (lnrpcDV - log(ref_PRED)) * (SDlnDVref / SDlnrpcDV)),
                    rpvcDVobs  = exp(log(ref_PRED) + (lnrpcDVobs - log(ref_PRED)) * (SDlnDVref /SDlnrpcDV)))
  }
  
  o$obs$ypc<-merged_data$rpvcDVobs[merged_data$REP==1]
  o$sim$ypc<-merged_data$rpvcDV
  
  o$obs$x<-merged_data$obs_IDV[merged_data$REP==1]
  o$sim$x<-merged_data$ref_IDV
}
  
    
  update(o, cor=TRUE, cortype="Reference", cor.log=log)
  
  
}


#' No ref correction for Visual Predictive Check (VPC)
#' 
#' Optional function to use indicating no ref correction for VPC. 
#' 
#' @title norefcorrect
#' @param o tidyvpcobj
#' @param ... other arguments to include
#' @export
norefcorrect <- function(o, ...) UseMethod("norefcorrect")

#' @rdname norefcorrect
#' @export
norefcorrect.tidyvpcobj <- function(o,
                                     ...) {
  
  update(o, cor=FALSE)
}

#' reference corrected Visual Predictive Check (rcVPC) for the change from baseline
#' @details This function creates a reference corrected Visual Predictive Check (rcVPC) for the change from baseline.
#' @title refcorrectCFB
#' @param o tidyvpc object
#' @param lb lower bound for the change from baseline, e.g., -100.
#' @param yref variable name for the change from baseline data item, it should be the same name across the following three input data
#' @param xref independent variable
#' @param refdata the simulated reference data
#' @param PRED_refdata change from baseline prediction for the simulated reference data
#' @param PRED_simdata  change from baseline prediction for the simulated data
#' @param IDV_refcorr flag if the Independent variable should be reference corrected as well
#' @param ... Other arguments to include
#' @return Updates \code{tidyvpcobj} with required information to performing Reference correction which include \code{cor} logical indicating whether
#'   corrected VPC is to be performed.
#' @examples 
#' require(magrittr)
#' 
#' obs_data    <- obs_data[MDV == 0]
#' sim_data    <- sim_data[MDV == 0]
#' PCFB_sdata  <- PCFB_sim_data[MDV == 0]
#' ref_data    <- ref_data[MDV == 0]
#' PCFB_rdata  <- PCFB_ref_data[MDV == 0]
#' 
#' rcVPC <- observed(odata, x = CP, yobs = CDV) %>% 
#'  simulated(sdata, ysim = CDV) %>%
#'  refcorrectCFB(lb           = -1,
#'                xref         = CP,
#'                yref         = CDV,
#'                PRED_simdata = PCFB_sdata,
#'                refdata      = ref_data,
#'                PRED_refdata = PCFB_rdata) %>% 
#'  binning(bin = "centers", centers = c(-Inf,0.01,25,50,75,100,150,Inf)) %>%
#'  vpcstats(qpred = c(0.1, 0.5, 0.9))
#'        
#' @seealso \code{\link{observed}} \code{\link{simulated}} \code{\link{censoring}} \code{\link{binning}} \code{\link{vpcstats}}
#' @export
# Function to calculate rpvcDV and rpvcDVobs for Change from baseline (only on normal scale):
refcorrectCFB <- function(o, ...) UseMethod("refcorrectCFB")

#' @rdname refcorrectCFB
#' @export
refcorrectCFB.tidyvpcobj <- function(o,
                                     yref,
                                     xref,
                                     refdata,
                                     obsdata=o$data,
                                     simdata=o$simdata,
                                     PRED_simdata=NULL,
                                     PRED_refdata=NULL,
                                     lb=-1,
                                     IDV_refcorr=FALSE,
                                     log=o$lnDV,
                                     ...) {
  
  if (!is.null(o$vpc.method)){
    stop("refcorrectCFB() should be used before binning()")
  }
  
  if (IDV_refcorr) {

  CFB <-rlang::enquo(yref)
  IDV <-rlang::enquo(xref)
  
  # Read observed CFB
  obsCFB<-rep(o$obs$y,max(o$sim$repl))
  obsIDV<-rep(o$obs$x,max(o$sim$repl))
  
  # Read simulated CFB:
  sim1<-simdata %>% 
  dplyr::mutate(obs_DV = obsCFB,
                sim_DV = o$sim$y,
                obs_IDV = obsIDV,
                sim_IDV = !!IDV,
                NREF = REP * 1E8 + REF)
  
  # Read PRED of CFB & IDV:
  sim2<-PRED_simdata %>% 
    dplyr::mutate(NREF     = REP * 1E8 + REF,
                  sim_PRED = !!CFB,
                  IDV_PRED = !!IDV) %>% 
    dplyr::select(NREF, sim_PRED, IDV_PRED)
  
  # Read simulated CFBref:
  sim3<-refdata %>% 
    dplyr::mutate(NREF     = REP * 1E8 + REF,
                  ref_IDV  = !!IDV,
                  ref_DV   = !!CFB) %>% 
    dplyr::select(NREF,ref_IDV,ref_DV)
  
  # Read PRED of ref_CFB and IDV:
  sim4<-PRED_refdata %>%
    dplyr::mutate(NREF= REP * 1E8 + REF,
                  ref_PRED = !!CFB,
                  ref_IDV_PRED= !!IDV) %>%  
    dplyr::select(NREF,ref_PRED,ref_IDV_PRED)
  
  merged_data <- merge(merge(sim1, sim2, by = "NREF"), 
                       merge(sim4, sim3, by = "NREF"),
                       by = "NREF")%>% 
    dplyr::mutate(RIDV_IPRED=ifelse(ref_IDV_PRED<1E-6 | IDV_PRED<1E-6, 1, 
                             ref_IDV_PRED / IDV_PRED)) %>% 
    filter(obs_IDV!=0) 
  
  ## Calculate prediction corrected CFB and OCFB:
    merged_data<-merged_data %>% 
      dplyr::mutate(lnDVref     = log(ref_DV - lb),
                    rpcDV       = lb + (sim_DV - lb) * ((ref_PRED - lb)  /  (sim_PRED - lb)),
                    lnrpcDV     = log(rpcDV - lb),
                    rpcDVobs    = lb + (obs_DV - lb) * ((ref_PRED - lb)  / (sim_PRED - lb)),
                    lnrpcDVobs  = log(rpcDVobs - lb),
                    lnIDVref    = log_protected(ref_IDV),
                    rpcIDV      = sim_IDV * RIDV_IPRED,
                    lnrpcIDV    = log_protected(rpcIDV),
                    rpcIDVobs   = obs_IDV * RIDV_IPRED,
                    lnrpcIDVobs = log_protected(rpcIDVobs))
## Calculate SD of CFB, CFBref:

  merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnrpcDV", 
                                         by = "REF"),by = "REF")%>% rename("SDlnrpcDV" = "SD")
  merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnDVref", 
                                         by = "REF"),by = "REF")%>% rename("SDlnDVref" = "SD")
  merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnrpcIDV", 
                                                by = "REF"),by = "REF")%>% rename("SDlnrpcIDV" = "SD")
  merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnIDVref", 
                                                by = "REF"),by = "REF")%>% rename("SDlnIDVref" = "SD")
  
## Calculate rpvc for DV, ODV, CFB, OCFB:
    merged_data<-merged_data %>% 
      dplyr::mutate(SDr         = ifelse(SDlnrpcDV==SDlnDVref,1,(SDlnDVref/SDlnrpcDV))) %>% 
      dplyr:: mutate(rpvcDV     = lb + exp( log(ref_PRED - lb) + (lnrpcDV-log(ref_PRED - lb)) * SDr),
                     rpvcDVobs  = lb + exp( log(ref_PRED - lb) + (lnrpcDVobs-log(ref_PRED - lb)) * SDr),
                     rpvcIDV     = exp(log(ref_IDV_PRED) + (lnrpcIDV - log(ref_IDV_PRED)) * (SDlnIDVref / SDlnrpcIDV)),
                     rpvcIDVobs  = exp(log(ref_IDV_PRED) + (lnrpcIDVobs - log(ref_IDV_PRED)) * (SDlnIDVref /SDlnrpcIDV)))
  
  o$obs<-filter(o$obs,x!=0)
  o$sim<-filter(o$sim,x!=0)
  
  o$obs$ypc<-merged_data$rpvcDVobs[merged_data$REP==1]
  o$sim$ypc<-merged_data$rpvcDV

  o$obs$x<- merged_data$rpvcIDVobs[merged_data$REP==1]
  o$sim$x<- merged_data$rpvcIDV

  } else {
    
    CFB <-rlang::enquo(yref)
    IDV <-rlang::enquo(xref)
    
    # Read observed CFB
    obsCFB<-rep(o$obs$y,max(o$sim$repl))
    obsIDV<-rep(o$obs$x,max(o$sim$repl))
    
    # Read simulated CFB:
    sim1<-simdata %>% 
      dplyr::mutate(obs_DV = obsCFB,
                    sim_DV = o$sim$y,
                    obs_IDV = obsIDV,
                    sim_IDV = !!IDV,
                    NREF = REP * 1E8 + REF)
    
    # Read PRED of CFB & IDV:
    sim2<-PRED_simdata %>% 
      dplyr::mutate(NREF     = REP * 1E8 + REF,
                    sim_PRED = !!CFB) %>% 
      dplyr::select(NREF, sim_PRED)
    
    # Read simulated CFBref:
    sim3<-refdata %>% 
      dplyr::mutate(NREF     = REP * 1E8 + REF,
                    ref_IDV  = !!IDV,
                    ref_DV  = !!CFB) %>% 
      dplyr::select(NREF,ref_IDV,ref_DV)
    
    # Read PRED of ref_CFB:
    sim4<-PRED_refdata %>%
      dplyr::mutate(NREF= REP * 1E8 + REF,
                    ref_PRED = !!CFB) %>% 
      dplyr::select(NREF,ref_PRED)
    
    merged_data <- merge(merge(sim1, sim2, by = "NREF"), 
                         merge(sim4, sim3, by = "NREF"),
                         by = "NREF") 
    
    ## Calculate prediction corrected CFB and OCFB:
    merged_data<-merged_data %>% 
      dplyr::mutate(lnDVref   = log(ref_DV - lb),
                    rpcDV     = lb + (sim_DV - lb) * ((ref_PRED - lb)  /  (sim_PRED - lb)),
                    lnrpcDV   = log(rpcDV - lb),
                    rpcDVobs  = lb + (obs_DV - lb) * ((ref_PRED - lb)  / (sim_PRED - lb)),
                    lnrpcDVobs= log(rpcDVobs - lb))
    
    ## Calculate SD of CFB, CFBref:
    
    merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnrpcDV", 
                                                  by = "REF"),by = "REF")%>% rename("SDlnrpcDV" = "SD")
    merged_data<-dplyr::left_join(merged_data,sdX(merged_data,"lnDVref", 
                                                  by = "REF"),by = "REF")%>% rename("SDlnDVref" = "SD")
    
    ## Calculate rpvc for DV, ODV, CFB, OCFB:
    merged_data<-merged_data %>% 
      dplyr::mutate(SDr         = ifelse(SDlnrpcDV==SDlnDVref,1,(SDlnDVref/SDlnrpcDV))) %>% 
      dplyr:: mutate(rpvcDV     = lb + exp( log(ref_PRED - lb) + (lnrpcDV-log(ref_PRED - lb)) * SDr),
                     rpvcDVobs  = lb + exp( log(ref_PRED - lb) + (lnrpcDVobs-log(ref_PRED - lb)) * SDr))
    
    o$obs$ypc<-merged_data$rpvcDVobs[merged_data$REP==1]
    o$sim$ypc<-merged_data$rpvcDV
    o$obs$x<-merged_data$obs_IDV[merged_data$REP==1]
    o$sim$x<-merged_data$ref_IDV
    # o$obs[, x   := merged_data$obs_IDV[merged_data$REP==1]]
    # o$sim[, x   := merged_data$ref_IDV]
  }
  
  update(o, cor=TRUE, cortype="Reference", cor.log=log)
}


#' No ref correction for Visual Predictive Check (VPC) for the change from baseline.
#' 
#' Optional function to use indicating no ref correction for the change from baseline. 
#' 
#' @title norefcorrectCFB
#' @param o tidyvpcobj
#' @param ... other arguments to include
#' @export
norefcorrectCFB <- function(o, ...) UseMethod("norefcorrectCFB")

#' @rdname norefcorrectCFB
#' @export
norefcorrectCFB.tidyvpcobj <- function(o,
                                    ...) {
  
  update(o, cor=FALSE)
}


# Function to calculate SD for dependent variables at each independent variable bin:
sdX  <- function(data=data, y = y, by=x) {
  data %>%
    group_by(.dots = by) %>%
    summarise_(
      SD    = interp(~sd(v), v=as.name(y)))
}
# Function to calculate logarithm with protection against negative numbers:
log_protected <- function(x) {
  ifelse(x < 1E-5, log(1E-4), log(x))
}
