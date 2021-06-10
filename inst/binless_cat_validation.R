#Validation of tidyvpc methods for binless categorical VPC

##
rm(list = ls())   

## Specify quantile type 
quantileType = 6

## Load VPC results 
vpcJob <- readRDS("./OneCpt1stOrderAbsorp_Emax_CategoricalModel3Categories_QRPEM_VPC.RDS")

## Categorical/count observed data: predcheck0_cat.csv
dt_ObsData_CategoricalObs <- vpcJob$predcheck0_cat.csv[ObsName == "CategoricalObs"]
dt_SimData_CategoricalObs <- vpcJob$predout.csv[OBSNAME == "CategoricalObs"]

dt_ObsData_CategoricalObs <- dt_ObsData_CategoricalObs[order(ID5, IVAR)]
dt_SimData_CategoricalObs <- dt_SimData_CategoricalObs[order(REPLICATE, ID5, IVAR)]


##
## Binless VPC - using the user-specified sp
##
userSuppliedSP <- c(10, 7, 2)

binlessVPC_userSpecifiedSP_CategoricalObs <- observed(dt_ObsData_CategoricalObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_CategoricalObs, ysim = DV) %>%
  binless(optimize = FALSE, sp = userSuppliedSP) %>%
  vpcstats(vpc.type = "categorical"
           , conf.level = 0.9
           , quantile.type = quantileType
  )

## Statistics obtained by tidyVPC
dt_Stats_userSpecifiedSP_tidyVPC <- binlessVPC_userSpecifiedSP_CategoricalObs$stats %>%
  rename(IVAR = x, Obs = y, Sim5 = lo, Sim50 = md, Sim95 = hi) %>%
  mutate(pname = as.character(pname))

##==========================================================================================================================================
##
## Comparison the stats obtained by tidyVPC and manually calculated ones
##
##==========================================================================================================================================
## Prepare observed data 
dt_ObsData_IndictorEachCat <- dt_ObsData_CategoricalObs[, c("ID5", "IVAR", "DV"), with = FALSE] %>%
  mutate(indicatorCat0 = ifelse(DV == 0, 1, 0)
         , indicatorCat1 = ifelse(DV == 1, 1, 0)
         , indicatorCat2 = ifelse(DV == 2, 1, 0)
  )


## Prepare simulated data 
dt_SimData_IndictorEachCat <- dt_SimData_CategoricalObs[, c("REPLICATE", "ID5", "IVAR", "DV"), with = FALSE] %>%
  mutate(indicatorCat0 = ifelse(DV == 0, 1, 0)
         , indicatorCat1 = ifelse(DV == 1, 1, 0)
         , indicatorCat2 = ifelse(DV == 2, 1, 0)
  )

##---------------------------------------------------------------------------------------------------------------------------------
##    Manually calculate statistics 
##---------------------------------------------------------------------------------------------------------------------------------
## Statistics for observed data 
dt_Stats_fittedGAM_Obs <- dt_ObsData_IndictorEachCat[, .(IVAR
                                                         , prob0 = fitted(gam(indicatorCat0 ~ s(IVAR, k = 1), sp = userSuppliedSP[1], family = "binomial"))
                                                         , prob1 = fitted(gam(indicatorCat1 ~ s(IVAR, k = 1), sp = userSuppliedSP[2], family = "binomial"))
                                                         , prob2 = fitted(gam(indicatorCat2 ~ s(IVAR, k = 1), sp = userSuppliedSP[3], family = "binomial"))
)
] %>%
  distinct() %>%
  pivot_longer(cols = starts_with("prob"), names_to = "pname", values_to = "Obs")

##
## Statistics for simulated data 
##
dt_Stats_fittedGAM_Sim <- dt_SimData_IndictorEachCat[, .(IVAR
                                                         , prob0 = fitted(gam(indicatorCat0 ~ s(IVAR, k = 1), sp = c(userSuppliedSP[1]), family = "binomial"))
                                                         , prob1 = fitted(gam(indicatorCat1 ~ s(IVAR, k = 1), sp = c(userSuppliedSP[2]), family = "binomial"))
                                                         , prob2 = fitted(gam(indicatorCat2 ~ s(IVAR, k = 1), sp = c(userSuppliedSP[3]), family = "binomial"))
)
, by = REPLICATE] %>%
  distinct(IVAR, prob0, prob1, prob2, .keep_all = TRUE) 


## 5% quantile of these probabilities 
dt_Stats_fittedGAM_Sim5 <- dt_Stats_fittedGAM_Sim %>%
  group_by(IVAR) %>%
  summarise(prob0 = quantile(prob0, probs = 0.05, type = quantileType)
            , prob1 = quantile(prob1, probs = 0.05, type = quantileType)
            , prob2 = quantile(prob2, probs = 0.05, type = quantileType)
  ) %>%
  pivot_longer(cols = starts_with("prob"), names_to = "pname", values_to = "Sim5")


## median of these probabilities 
dt_Stats_fittedGAM_Sim50 <- dt_Stats_fittedGAM_Sim %>%
  group_by(IVAR) %>%
  summarise(prob0 = median(prob0)
            , prob1 = median(prob1)
            , prob2 = median(prob2)
  ) %>%
  pivot_longer(cols = starts_with("prob"), names_to = "pname", values_to = "Sim50")


## 95% quantile of these probabilities 
dt_Stats_fittedGAM_Sim95 <- dt_Stats_fittedGAM_Sim %>%
  group_by(IVAR) %>%
  summarise(prob0 = quantile(prob0, probs = 0.95, type = quantileType)
            , prob1 = quantile(prob1, probs = 0.95, type = quantileType)
            , prob2 = quantile(prob2, probs = 0.95, type = quantileType)
  ) %>%
  pivot_longer(cols = starts_with("prob"), names_to = "pname", values_to = "Sim95")


##---------------------------------------------------------------------------------------------------------------------------------
##    Comparison 
##---------------------------------------------------------------------------------------------------------------------------------
all.equal(dt_Stats_fittedGAM_Obs, dt_Stats_userSpecifiedSP_tidyVPC[, c("IVAR", "pname", "Obs"), with = FALSE], check.attributes = FALSE)

all.equal(dt_Stats_fittedGAM_Sim5, dt_Stats_userSpecifiedSP_tidyVPC[, c("IVAR", "pname", "Sim5"), with = FALSE], check.attributes = FALSE)

all.equal(dt_Stats_fittedGAM_Sim50, dt_Stats_userSpecifiedSP_tidyVPC[, c("IVAR", "pname", "Sim50"), with = FALSE], check.attributes = FALSE)

all.equal(dt_Stats_fittedGAM_Sim95, dt_Stats_userSpecifiedSP_tidyVPC[, c("IVAR", "pname", "Sim95"), with = FALSE], check.attributes = FALSE)

