rm(list=ls())
library(ggplot2)
library(gridExtra)
library(tidyr)
library(data.table)
library(openxlsx)
library(foreign)
library(sp) 
library(rworldmap)
library(RColorBrewer)
library(boot)
require(lhs)   
require(doParallel)
library(foreach)
library(plyr)
library(dplyr)

library(truncnorm)
library(shiny) 
library(shinydashboard)
library(ggiraph)
library(sparkline)
library(DT)
library(scales)
library(viridis)
library(shinydashboardPlus)
library(shinyWidgets)
library(sortable)
library(scales)

library(shiny.telemetry)
library(shinyBS)
library(DBI)
library(rsconnect)
library(shinyBS)
library(RPostgres)
library(digest)
library(khroma)


########## Read in data/curves ###########

if (!require("pacman")) install.packages("pacman")
pacman::p_load(reshape, reshape2,MASS,rlist,purrr,ggplot2,tidyr,data.table,openxlsx,foreign,RColorBrewer,boot,lhs,doParallel,foreach,utils,dplyr,plyr,lhs)

##########telemetry start############

Sys.setenv(R_DEFAULT_INTERNET_PROTOCOL = "ipv4")
# 
# telemetry <- Telemetry$new()

#########random username############

generate_anonymous_username <- function(ip) {
  # Hash the IP for privacy and consistency
  hashed_ip <- digest(ip, algo = "md5")
  set.seed(sum(utf8ToInt(hashed_ip)))

  numbers <- c("1234", "1243", "1324", "1342", "1423", "1432",
               "2134", "2143", "2314", "2341", "2413", "2431",
               "3124", "3142", "3214", "3241", "3412", "3421",
               "4123", "4132", "4213", "4231", "4312", "4321")
  letters <- c("abcd", "abdc", "acbd", "acdb", "adbc", "adcb",
               "bacd", "badc", "bcad", "bcda", "bdac", "bdca",
               "cabd", "cadb", "cbad", "cbda", "cdab", "cdba",
               "dabc", "dacb", "dbac", "dbca", "dcab", "dcba")

  username <- paste0(
    sample(numbers, 1),
    sample(letters, 1),
    sprintf("%03d", sample(100:999, 1))
  )

  return(username)
}

get_user_ip <- function(session) {
  if (!is.null(session$request)) {
    if (!is.null(session$request$HTTP_X_FORWARDED_FOR)) {
      # For proxied connections
      client_ip <- strsplit(session$request$HTTP_X_FORWARDED_FOR, ",")[[1]][1]
      return(trimws(client_ip))
    } else if (!is.null(session$request$REMOTE_ADDR)) {
      # Direct connections
      return(session$request$REMOTE_ADDR)
    }
  }
  return("unknown")  # Fallback
}

########## Cost function ############
### Function for cost simulations
f_DoseOpt_cost <- function(Merg,
                           pop,pop.m,cov.m,cov.s,mod.wasted){

  cost.all.varied.relapse<- function(i,Merg,
                                     pop,pop.m,cov.m,cov.s,mod.wasted...) {
    cbind(
      Merg[i,],
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  MANGO SAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      cost.rec.s3 = Merg$alpha.s3[i] * (pop + ((cov.s*mod.wasted * (1-cov.m))+pop.m)*Merg$tau.m[i]) * 
        (Merg$rec.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s3[i]/7),
      
      cost.sachet.rec.s3 = Merg$alpha.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$rec.s3[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.s3 = Merg$alpha.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.rec.s3 = Merg$alpha.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s3[i]/7,
      cost.nonlabor.rec.s3 = Merg$alpha.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s3[i]/7,
      cost.caregiver.rec.s3 = Merg$alpha.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s3[i]/7,
      
      ##### defaulted
      cost.def.s3 = Merg$eps.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$def.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s3[i]/7),
      
      cost.sachet.def.s3 = Merg$eps.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$def.s3[i]*Merg$cost.rutf[i] ,
      cost.drug.def.s3 = Merg$eps.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.def.s3 = Merg$eps.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s3[i]/7,
      cost.nonlabor.def.s3 = Merg$eps.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s3[i]/7,
      cost.caregiver.def.s3 = Merg$eps.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s3[i]/7,
      
      ##### never recovered
      cost.never.rec.s3 = Merg$eta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$never.rec.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s3[i]/7),
      
      cost.sachet.never.rec.s3 = Merg$eta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$never.rec.s3[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.s3 = Merg$eta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.never.rec.s3 = Merg$eta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s3[i]/7,
      cost.nonlabor.never.rec.s3 = Merg$eta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s3[i]/7,
      cost.caregiver.never.rec.s3 = Merg$eta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s3[i]/7,
      
      ##### died
      cost.died.s3 = Merg$theta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$died.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s3[i]/7),
      
      cost.sachet.died.s3 = Merg$theta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$died.s3[i]*Merg$cost.rutf[i] ,
      cost.drug.died.s3 = Merg$theta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.died.s3 = Merg$theta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s3[i]/7,
      cost.nonlabor.died.s3 = Merg$theta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s3[i]/7,
      cost.caregiver.died.s3 = Merg$theta.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s3[i]/7,
      
      ##### hospitalized
      cost.hosp.s3 = Merg$phi.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s3[i]/7),
      
      cost.sachet.hosp.s3 = Merg$phi.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.s3[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.s3 = Merg$phi.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.hosp.s3 = Merg$phi.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s3[i]/7,
      cost.nonlabor.hosp.s3 = Merg$phi.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s3[i]/7,
      cost.caregiver.hosp.s3 = Merg$phi.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s3[i]/7,
      
      ##### transferred
      cost.transf.s3 = Merg$mu.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$transf.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s3[i]/7),
      
      cost.sachet.transf.s3 = Merg$mu.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$transf.s3[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.s3 = Merg$mu.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.transf.s3 = Merg$mu.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s3[i]/7,
      cost.nonlabor.transf.s3 = Merg$mu.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s3[i]/7,
      cost.caregiver.transf.s3 = Merg$mu.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s3[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s3 = Merg$phi.s3[i] * Merg$alpha2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.rec.s3[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s3[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.rec.s3 = Merg$phi.s3[i] * Merg$alpha2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.rec.s3[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s3 = Merg$phi.s3[i] * Merg$alpha2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s3[i]/7,
      
      cost.nonlabor.hosp.rec.s3 = Merg$phi.s3[i] * Merg$alpha2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s3[i]/7,
      
      cost.caregiver.hosp.rec.s3 = Merg$phi.s3[i] * Merg$alpha2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s3[i]/7,
      
      cost.hospitalization.hosp.rec.s3 = Merg$phi.s3[i] * Merg$alpha2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s3 = Merg$phi.s3[i] * Merg$theta2.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.died.s3[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s3[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.died.s3 = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.died.s3[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s3 = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s3[i]/7,
      
      cost.nonlabor.hosp.died.s3 = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s3[i]/7,
      
      cost.caregiver.hosp.died.s3 = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s3[i]/7,
      
      cost.hospitalization.hosp.died.s3 = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s3 = Merg$phi.s3[i] * Merg$eta2.s3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.never.rec.s3[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s3[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.never.rec.s3 = Merg$phi.s3[i] * Merg$eta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.never.rec.s3[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s3 = Merg$phi.s3[i] * Merg$eta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s3[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.nonlabor.hosp.never.rec.s3 = Merg$phi.s3[i] * Merg$eta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s3[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.caregiver.hosp.never.rec.s3 = Merg$phi.s3[i] * Merg$eta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s3[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.hospitalization.hosp.never.rec.s3 = Merg$phi.s3[i] * Merg$eta2.s3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      ##### 
      ##### Relapse Post SAM Treatment - SAM to SAM ; SAM to MAM
      ##### 
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      
      ##### recovered
      cost.rec.s3.prss = Merg$alpha.s3.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * 
        (Merg$rec.s3.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s3.prss[i]/7),
      
      cost.sachet.rec.s3.prss = Merg$alpha.s3.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$rec.s3.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s3.prss = Merg$alpha.s3.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.rec.s3.prss = Merg$alpha.s3.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$cost.labor[i]*Merg$dur.rec.s3.prss[i]/7,
      
      cost.nonlabor.rec.s3.prss = Merg$alpha.s3.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.rec.s3.prss[i]/7,
      
      cost.caregiver.rec.s3.prss = Merg$alpha.s3.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$cost.caregiver[i]*Merg$dur.rec.s3.prss[i]/7,
      
      ##### defaulted
      cost.def.s3.prss = Merg$eps.s3[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *
        (Merg$def.s3.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s3.prss[i]/7),
      
      cost.sachet.def.s3.prss = Merg$eps.s3[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$def.s3.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s3.prss = Merg$eps.s3[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.def.s3.prss = Merg$eps.s3[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])  * Merg$cost.labor[i]*Merg$dur.def.s3.prss[i]/7,
      
      cost.nonlabor.def.s3.prss = Merg$eps.s3[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.def.s3.prss[i]/7,
      
      cost.caregiver.def.s3.prss = Merg$eps.s3[i] *  
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s3.prss[i]/7,
      
      ##### never recovered
      cost.never.rec.s3.prss = Merg$eta.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *
        (Merg$never.rec.s3.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s3.prss[i]/7),
      
      cost.sachet.never.rec.s3.prss = Merg$eta.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$never.rec.s3.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s3.prss =Merg$eta.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s3.prss = Merg$eta.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s3.prss[i]/7,
      
      cost.nonlabor.never.rec.s3.prss = Merg$eta.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s3.prss[i]/7,
      
      cost.caregiver.never.rec.s3.prss = Merg$eta.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s3.prss[i]/7,
      
      ##### died
      cost.died.s3.prss = Merg$theta.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *
        
        (Merg$died.s3.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s3.prss[i]/7),
      
      cost.sachet.died.s3.prss = Merg$theta.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$died.s3.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s3.prss = Merg$theta.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])  * Merg$cost.drug1[i],
      
      cost.labor.died.s3.prss = Merg$theta.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s3.prss[i]/7,
      
      cost.nonlabor.died.s3.prss = Merg$theta.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s3.prss[i]/7,
      
      cost.caregiver.died.s3.prss = Merg$theta.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s3.prss[i]/7,
      
      ##### hospitalized
      cost.hosp.s3.prss = Merg$phi.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * 
        
        (Merg$hosp.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s3[i]/7),
      
      cost.sachet.hosp.s3.prss = Merg$phi.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *  Merg$hosp.s3[i]*Merg$cost.rutf[i] ,
      
      cost.drug.hosp.s3.prss = Merg$phi.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *  Merg$cost.drug1[i],
      
      cost.labor.hosp.s3.prss = Merg$phi.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *  Merg$cost.labor[i]*Merg$dur.hosp.s3[i]/7,
      
      cost.nonlabor.hosp.s3.prss = Merg$phi.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *  Merg$cost.nonlabor[i]*Merg$dur.hosp.s3[i]/7,
      
      cost.caregiver.hosp.s3.prss = Merg$phi.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *  Merg$cost.caregiver[i]*Merg$dur.hosp.s3[i]/7,
      
      ##### transferred
      cost.transf.s3.prss = Merg$mu.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * 
        
        (Merg$transf.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s3[i]/7),
      
      cost.sachet.transf.s3.prss = Merg$mu.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$transf.s3[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s3.prss = Merg$mu.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s3.prss = Merg$mu.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.transf.s3[i]/7,
      
      cost.nonlabor.transf.s3.prss = Merg$mu.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s3[i]/7,
      
      cost.caregiver.transf.s3.prss = Merg$mu.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s3[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s3.prss = Merg$phi.s3[i] * Merg$alpha2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *
        
        (Merg$hosp.rec.s3.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s3.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.rec.s3.prss = Merg$phi.s3[i] * Merg$alpha2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i])   * Merg$hosp.rec.s3.prss[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s3.prss = Merg$phi.s3[i] * Merg$alpha2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.rec.s3.prss[i]/7,
      
      cost.nonlabor.hosp.rec.s3.prss = Merg$phi.s3[i] * Merg$alpha2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s3.prss[i]/7,
      
      cost.caregiver.hosp.rec.s3.prss = Merg$phi.s3[i] * Merg$alpha2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s3.prss[i]/7,
      
      cost.hospitalization.hosp.rec.s3.prss = Merg$phi.s3[i] * Merg$alpha2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s3.prss = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *
        
        (Merg$hosp.died.s3.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s3.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.died.s3.prss = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$hosp.died.s3.prss[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s3.prss = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s3.prss[i]/7,
      
      cost.nonlabor.hosp.died.s3.prss = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s3.prss[i]/7,
      
      cost.caregiver.hosp.died.s3.prss = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s3.prss[i]/7,
      
      cost.hospitalization.hosp.died.s3.prss = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s3.prss = Merg$phi.s3[i] * Merg$eta2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) *
        
        (Merg$hosp.never.rec.s3.prss[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s3.prss[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.never.rec.s3.prss = Merg$phi.s3[i] * Merg$eta2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$hosp.never.rec.s3.prss[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s3.prss = Merg$phi.s3[i] * Merg$eta2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s3.prss[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.nonlabor.hosp.never.rec.s3.prss = Merg$phi.s3[i] * Merg$eta2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s3.prss[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.caregiver.hosp.never.rec.s3.prss = Merg$phi.s3[i] * Merg$eta2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s3.prss[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.hospitalization.hosp.never.rec.s3.prss = Merg$phi.s3[i] * Merg$eta2.s3.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m3.prsm = Merg$cov[i]*Merg$alpha.m3.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  *  
        (Merg$dur.rec.m3.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m3.prsm[i]/7),
      
      cost.sachet.rec.m3.prsm = Merg$cov[i]*Merg$alpha.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$dur.rec.m3.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.rec.m3.prsm = Merg$cov[i]*Merg$alpha.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m3.prsm = Merg$cov[i]*Merg$alpha.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m3.prsm[i]/7,
      cost.nonlabor.rec.m3.prsm = Merg$cov[i]*Merg$alpha.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m3.prsm[i]/7,
      cost.caregiver.rec.m3.prsm = Merg$cov[i]*Merg$alpha.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m3.prsm[i]/7,
      
      ##### defaulted
      cost.def.m3.prsm = Merg$cov[i]*Merg$eps.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * 
        (Merg$dur.def.m3.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m3.prsm[i]/7),
      
      cost.sachet.def.m3.prsm = Merg$cov[i]*Merg$eps.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$dur.def.m3.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.def.m3.prsm = Merg$cov[i]*Merg$eps.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m3.prsm = Merg$cov[i]*Merg$eps.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m3.prsm[i]/7,
      cost.nonlabor.def.m3.prsm = Merg$cov[i]*Merg$eps.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m3.prsm[i]/7,
      cost.caregiver.def.m3.prsm = Merg$cov[i]*Merg$eps.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m3.prsm[i]/7,
      
      ##### never recovered
      cost.never.rec.m3.prsm = Merg$cov[i]*Merg$eta.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * 
        (Merg$dur.never.rec.m3.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m3.prsm[i]/7),
      
      cost.sachet.never.rec.m3.prsm = Merg$cov[i]*Merg$eta.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$dur.never.rec.m3.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.never.rec.m3.prsm = Merg$cov[i]*Merg$eta.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m3.prsm = Merg$cov[i]*Merg$eta.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m3.prsm[i]/7,
      cost.nonlabor.never.rec.m3.prsm = Merg$cov[i]*Merg$eta.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m3.prsm[i]/7,
      cost.caregiver.never.rec.m3.prsm = Merg$cov[i]*Merg$eta.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m3.prsm[i]/7,
      
      ##### died
      cost.died.m3.prsm = Merg$cov[i]*Merg$theta.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * 
        (Merg$dur.died.m3.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m3.prsm[i]/7),
      
      cost.sachet.died.m3.prsm = Merg$cov[i]*Merg$theta.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$dur.died.m3.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.died.m3.prsm = Merg$cov[i]*Merg$theta.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m3.prsm = Merg$cov[i]*Merg$theta.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m3.prsm[i]/7,
      cost.nonlabor.died.m3.prsm = Merg$cov[i]*Merg$theta.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m3.prsm[i]/7,
      cost.caregiver.died.m3.prsm = Merg$cov[i]*Merg$theta.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m3.prsm[i]/7,
      
      ##### hospitalized
      cost.hosp.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * 
        (Merg$dur.hosp.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m3[i]/7),
      
      cost.sachet.hosp.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$dur.hosp.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.hosp.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m3[i]/7,
      cost.nonlabor.hosp.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m3[i]/7,
      cost.caregiver.hosp.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m3[i]/7,
      
      ##### transferred
      cost.transf.m3.prsm = Merg$cov[i]*Merg$mu.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * 
        (Merg$dur.transf.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m3[i]/7),
      
      cost.sachet.transf.m3.prsm = Merg$cov[i]*Merg$mu.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$dur.transf.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.transf.m3.prsm = Merg$cov[i]*Merg$mu.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m3.prsm = Merg$cov[i]*Merg$mu.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m3[i]/7,
      cost.nonlabor.transf.m3.prsm = Merg$cov[i]*Merg$mu.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m3[i]/7,
      cost.caregiver.transf.m3.prsm = Merg$cov[i]*Merg$mu.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m3[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  * 
        ((Merg$dur.rec.m3.prsm[i]+Merg$hosp.stay.m3[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m3.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * (Merg$dur.rec.m3.prsm[i]+Merg$hosp.stay.m3[i]) *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m3.prsm[i]/7,
      
      cost.nonlabor.hosp.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m3.prsm[i]/7,
      
      cost.caregiver.hosp.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m3.prsm[i]/7,
      
      cost.hospitalization.hosp.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * 
        ((Merg$dur.died.m3.prsm[i]+Merg$hosp.stay.m3[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m3.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.died.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * (Merg$dur.died.m3.prsm[i]+Merg$hosp.stay.m3[i])  *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.died.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m3.prsm[i]/7,
      
      cost.nonlabor.hosp.died.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m3.prsm[i]/7,
      
      cost.caregiver.hosp.died.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m3.prsm[i]/7,
      
      cost.hospitalization.hosp.died.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * 
        ((Merg$dur.never.rec.m3.prsm[i]- Merg$dur.hosp.m3[i]) *Merg$cost.rusf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m3.prsm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.never.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   *  (Merg$dur.never.rec.m3.prsm[i]- Merg$dur.hosp.m3[i]) *Merg$cost.rusf[i], 
      
      cost.labor.hosp.never.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m3.prsm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.nonlabor.hosp.never.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m3.prsm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.caregiver.hosp.never.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m3.prsm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.hospitalization.hosp.never.rec.m3.prsm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m3.reg.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m3.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]*Merg$cost.rusf[i] * Merg$dur.m.reg.sam[i],
      cost.drug.m3.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m3.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m3.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m3.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  MANGO - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      cost.rec.m3 = Merg$alpha.m3[i] * pop.m * 
        (Merg$dur.rec.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m3[i]/7),
      
      cost.sachet.rec.m3 = Merg$alpha.m3[i] * pop.m * Merg$dur.rec.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.rec.m3 = Merg$alpha.m3[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.rec.m3 = Merg$alpha.m3[i] * pop.m * Merg$cost.labor[i]*Merg$dur.rec.m3[i]/7,
      cost.nonlabor.rec.m3 = Merg$alpha.m3[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m3[i]/7,
      cost.caregiver.rec.m3 = Merg$alpha.m3[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m3[i]/7,
      
      ##### defaulted
      cost.def.m3 = Merg$eps.m3[i] * pop.m * 
        (Merg$dur.def.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m3[i]/7),
      
      cost.sachet.def.m3 = Merg$eps.m3[i] * pop.m * Merg$dur.def.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.def.m3 = Merg$eps.m3[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.def.m3 = Merg$eps.m3[i] * pop.m * Merg$cost.labor[i]*Merg$dur.def.m3[i]/7,
      cost.nonlabor.def.m3 = Merg$eps.m3[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.def.m3[i]/7,
      cost.caregiver.def.m3 = Merg$eps.m3[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.def.m3[i]/7,
      
      ##### never recovered
      cost.never.rec.m3 = Merg$eta.m3[i] * pop.m * 
        (Merg$dur.never.rec.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m3[i]/7),
      
      cost.sachet.never.rec.m3 = Merg$eta.m3[i] * pop.m * Merg$dur.never.rec.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.never.rec.m3 = Merg$eta.m3[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.never.rec.m3 = Merg$eta.m3[i] * pop.m * Merg$cost.labor[i]*Merg$dur.never.rec.m3[i]/7,
      cost.nonlabor.never.rec.m3 = Merg$eta.m3[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m3[i]/7,
      cost.caregiver.never.rec.m3 = Merg$eta.m3[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.never.rec.m3[i]/7,
      
      ##### died
      cost.died.m3 = Merg$theta.m3[i] * pop.m * 
        (Merg$dur.died.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m3[i]/7),
      
      cost.sachet.died.m3 = Merg$theta.m3[i] * pop.m * Merg$dur.died.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.died.m3 = Merg$theta.m3[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.died.m3 = Merg$theta.m3[i] * pop.m * Merg$cost.labor[i]*Merg$dur.died.m3[i]/7,
      cost.nonlabor.died.m3 = Merg$theta.m3[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m3[i]/7,
      cost.caregiver.died.m3 = Merg$theta.m3[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m3[i]/7,
      
      ##### hospitalized
      cost.hosp.m3 = Merg$phi.m3[i] * pop.m * 
        (Merg$dur.hosp.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m3[i]/7),
      
      cost.sachet.hosp.m3 = Merg$phi.m3[i] * pop.m * Merg$dur.hosp.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.hosp.m3 = Merg$phi.m3[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.hosp.m3 = Merg$phi.m3[i] * pop.m * Merg$cost.labor[i]*Merg$dur.hosp.m3[i]/7,
      cost.nonlabor.hosp.m3 = Merg$phi.m3[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.hosp.m3[i]/7,
      cost.caregiver.hosp.m3 = Merg$phi.m3[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.hosp.m3[i]/7,
      
      ##### transferred
      cost.transf.m3 = Merg$mu.m3[i] * pop.m * 
        (Merg$dur.transf.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m3[i]/7),
      
      cost.sachet.transf.m3 = Merg$mu.m3[i] * pop.m * Merg$dur.transf.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.transf.m3 = Merg$mu.m3[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.transf.m3 = Merg$mu.m3[i] * pop.m * Merg$cost.labor[i]*Merg$dur.transf.m3[i]/7,
      cost.nonlabor.transf.m3 = Merg$mu.m3[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.transf.m3[i]/7,
      cost.caregiver.transf.m3 = Merg$mu.m3[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.transf.m3[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m3 = Merg$phi.m3[i] * Merg$alpha2.m3[i] * 
        pop.m * 
        ((Merg$dur.rec.m3[i]+Merg$hosp.stay.m3[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m3[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.rec.m3 = Merg$phi.m3[i] * Merg$alpha2.m3[i] * 
        pop.m * (Merg$dur.rec.m3[i]+Merg$hosp.stay.m3[i])*Merg$cost.rusf[i] ,
      
      cost.labor.hosp.rec.m3 = Merg$phi.m3[i] * Merg$alpha2.m3[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.rec.m3[i]/7,
      
      cost.nonlabor.hosp.rec.m3 = Merg$phi.m3[i] * Merg$alpha2.m3[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m3[i]/7,
      
      cost.caregiver.hosp.rec.m3 = Merg$phi.m3[i] * Merg$alpha2.m3[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m3[i]/7,
      
      cost.hospitalization.hosp.rec.m3 = Merg$phi.m3[i] * Merg$alpha2.m3[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m3 = Merg$phi.m3[i] * Merg$theta2.m3[i] * pop.m * 
        ((Merg$dur.died.m3[i]+Merg$hosp.stay.m3[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m3[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.died.m3 = Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * (Merg$dur.died.m3[i]+Merg$hosp.stay.m3[i]) *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.died.m3 = Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.died.m3[i]/7,
      
      cost.nonlabor.hosp.died.m3 = Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m3[i]/7,
      
      cost.caregiver.hosp.died.m3 = Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m3[i]/7,
      
      cost.hospitalization.hosp.died.m3 = Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m3 = Merg$phi.m3[i] * Merg$eta2.m3[i] * pop.m * 
        ((Merg$dur.never.rec.m3[i]- Merg$dur.hosp.m3[i])*Merg$cost.rusf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m3[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.never.rec.m3 = Merg$phi.m3[i] * Merg$eta2.m3[i] * 
        pop.m * (Merg$dur.never.rec.m3[i]- Merg$dur.hosp.m3[i]) *Merg$cost.rusf[i], 
      
      cost.labor.hosp.never.rec.m3 = Merg$phi.m3[i] * Merg$eta2.m3[i] * 
        pop.m * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m3[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.nonlabor.hosp.never.rec.m3 = Merg$phi.m3[i] * Merg$eta2.m3[i] * 
        pop.m * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m3[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.caregiver.hosp.never.rec.m3 = Merg$phi.m3[i] * Merg$eta2.m3[i] * 
        pop.m * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m3[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.hospitalization.hosp.never.rec.m3 = Merg$phi.m3[i] * Merg$eta2.m3[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m3.reg.sam = Merg$tau.m[i] * pop.m * 
        (Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m3.to.sam = Merg$tau.m[i] * pop.m * Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] ,
      cost.drug.m3.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.drug[i],
      cost.labor.m3.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m3.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m3.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      cost.rec.s3.prms = Merg$alpha.s3.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * 
        
        (Merg$rec.s3.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s3.prms[i]/7),
      
      cost.sachet.rec.s3.prms = Merg$alpha.s3.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$rec.s3.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s3.prms = Merg$alpha.s3.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.rec.s3.prms = Merg$alpha.s3.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s3.prms[i]/7,
      
      cost.nonlabor.rec.s3.prms = Merg$alpha.s3.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s3.prms[i]/7,
      
      cost.caregiver.rec.s3.prms = Merg$alpha.s3.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s3.prms[i]/7,
      
      ##### defaulted
      cost.def.s3.prms = Merg$eps.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$def.s3.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s3.prms[i]/7),
      
      cost.sachet.def.s3.prms = Merg$eps.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$def.s3.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s3.prms = Merg$eps.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.def.s3.prms = Merg$eps.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s3.prms[i]/7,
      
      cost.nonlabor.def.s3.prms = Merg$eps.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s3.prms[i]/7,
      
      cost.caregiver.def.s3.prms = Merg$eps.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s3.prms[i]/7,
      
      
      ##### never recovered
      cost.never.rec.s3.prms = Merg$eta.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$never.rec.s3.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s3.prms[i]/7),
      
      cost.sachet.never.rec.s3.prms = Merg$eta.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$never.rec.s3.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s3.prms = Merg$eta.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s3.prms = Merg$eta.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s3.prms[i]/7,
      
      cost.nonlabor.never.rec.s3.prms = Merg$eta.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s3.prms[i]/7,
      
      cost.caregiver.never.rec.s3.prms = Merg$eta.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s3.prms[i]/7,
      
      
      ##### died
      cost.died.s3.prms = Merg$theta.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$died.s3.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s3.prms[i]/7),
      
      cost.sachet.died.s3.prms = Merg$theta.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$died.s3.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s3.prms = Merg$theta.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.died.s3.prms = Merg$theta.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s3.prms[i]/7,
      
      cost.nonlabor.died.s3.prms = Merg$theta.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s3.prms[i]/7,
      
      cost.caregiver.died.s3.prms = Merg$theta.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s3.prms[i]/7,
      
      ##### hospitalized
      cost.hosp.s3.prms = Merg$phi.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s3[i]/7),
      
      cost.sachet.hosp.s3.prms = Merg$phi.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.s3[i]*Merg$cost.rutf[i] ,
      
      cost.drug.hosp.s3.prms = Merg$phi.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.hosp.s3.prms = Merg$phi.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s3[i]/7,
      
      cost.nonlabor.hosp.s3.prms = Merg$phi.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s3[i]/7,
      
      cost.caregiver.hosp.s3.prms = Merg$phi.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s3[i]/7,
      
      
      ##### transferred
      cost.transf.s3.prms = Merg$mu.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$transf.s3[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s3[i]/7),
      
      cost.sachet.transf.s3.prms = Merg$mu.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$transf.s3[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s3.prms = Merg$mu.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s3.prms = Merg$mu.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s3[i]/7,
      
      cost.nonlabor.transf.s3.prms = Merg$mu.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s3[i]/7,
      
      cost.caregiver.transf.s3.prms = Merg$mu.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s3[i]/7,
      
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s3.prms = Merg$phi.s3[i] * Merg$alpha2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.rec.s3.prms[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s3.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.rec.s3.prms = Merg$phi.s3[i] * Merg$alpha2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.rec.s3.prms[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s3.prms = Merg$phi.s3[i] * Merg$alpha2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s3.prms[i]/7,
      
      cost.nonlabor.hosp.rec.s3.prms = Merg$phi.s3[i] * Merg$alpha2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s3.prms[i]/7,
      
      cost.caregiver.hosp.rec.s3.prms = Merg$phi.s3[i] * Merg$alpha2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s3.prms[i]/7,
      
      cost.hospitalization.hosp.rec.s3.prms = Merg$phi.s3[i] * Merg$alpha2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s3.prms = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.died.s3.prms[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s3.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.died.s3.prms = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.died.s3.prms[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s3.prms = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s3.prms[i]/7,
      
      cost.nonlabor.hosp.died.s3.prms = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s3.prms[i]/7,
      
      cost.caregiver.hosp.died.s3.prms = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s3.prms[i]/7,
      
      cost.hospitalization.hosp.died.s3.prms = Merg$phi.s3[i] * Merg$theta2.s3[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s3.prms = Merg$phi.s3[i] * Merg$eta2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.never.rec.s3.prms[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s3.prms[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7),
      
      cost.sachet.hosp.never.rec.s3.prms = Merg$phi.s3[i] * Merg$eta2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.never.rec.s3.prms[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s3.prms = Merg$phi.s3[i] * Merg$eta2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s3.prms[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.nonlabor.hosp.never.rec.s3.prms = Merg$phi.s3[i] * Merg$eta2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s3.prms[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.caregiver.hosp.never.rec.s3.prms = Merg$phi.s3[i] * Merg$eta2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s3.prms[i] - Merg$dur.hosp.s3[i] - Merg$hosp.stay.s3[i])/7,
      
      cost.hospitalization.hosp.never.rec.s3.prms = Merg$phi.s3[i] * Merg$eta2.s3.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s3[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m3.prmm = Merg$cov[i]*Merg$alpha.m3.prmm[i] *  pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  *  
        (Merg$dur.rec.m3.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m3.prmm[i]/7),
      
      cost.sachet.rec.m3.prmm = Merg$cov[i]*Merg$alpha.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$dur.rec.m3.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.rec.m3.prmm = Merg$cov[i]*Merg$alpha.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m3.prmm = Merg$cov[i]*Merg$alpha.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m3.prmm[i]/7,
      cost.nonlabor.rec.m3.prmm = Merg$cov[i]*Merg$alpha.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m3.prmm[i]/7,
      cost.caregiver.rec.m3.prmm = Merg$cov[i]*Merg$alpha.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m3.prmm[i]/7,
      
      ##### defaulted
      cost.def.m3.prmm = Merg$cov[i]*Merg$eps.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * 
        (Merg$dur.def.m3.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m3.prmm[i]/7),
      
      cost.sachet.def.m3.prmm = Merg$cov[i]*Merg$eps.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$dur.def.m3.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.def.m3.prmm = Merg$cov[i]*Merg$eps.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m3.prmm = Merg$cov[i]*Merg$eps.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m3.prmm[i]/7,
      cost.nonlabor.def.m3.prmm = Merg$cov[i]*Merg$eps.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m3.prmm[i]/7,
      cost.caregiver.def.m3.prmm = Merg$cov[i]*Merg$eps.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m3.prmm[i]/7,
      
      ##### never recovered
      cost.never.rec.m3.prmm = Merg$cov[i]*Merg$eta.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * 
        (Merg$dur.never.rec.m3.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m3.prmm[i]/7),
      
      cost.sachet.never.rec.m3.prmm = Merg$cov[i]*Merg$eta.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$dur.never.rec.m3.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.never.rec.m3.prmm = Merg$cov[i]*Merg$eta.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m3.prmm = Merg$cov[i]*Merg$eta.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m3.prmm[i]/7,
      cost.nonlabor.never.rec.m3.prmm = Merg$cov[i]*Merg$eta.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m3.prmm[i]/7,
      cost.caregiver.never.rec.m3.prmm = Merg$cov[i]*Merg$eta.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m3.prmm[i]/7,
      
      ##### died
      cost.died.m3.prmm = Merg$cov[i]*Merg$theta.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * 
        (Merg$dur.died.m3.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m3.prmm[i]/7),
      
      cost.sachet.died.m3.prmm = Merg$cov[i]*Merg$theta.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$dur.died.m3.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.died.m3.prmm = Merg$cov[i]*Merg$theta.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m3.prmm = Merg$cov[i]*Merg$theta.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m3.prmm[i]/7,
      cost.nonlabor.died.m3.prmm = Merg$cov[i]*Merg$theta.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m3.prmm[i]/7,
      cost.caregiver.died.m3.prmm = Merg$cov[i]*Merg$theta.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m3.prmm[i]/7,
      
      ##### hospitalized
      cost.hosp.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * 
        (Merg$dur.hosp.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m3[i]/7),
      
      cost.sachet.hosp.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$dur.hosp.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.hosp.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m3[i]/7,
      cost.nonlabor.hosp.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m3[i]/7,
      cost.caregiver.hosp.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m3[i]/7,
      
      ##### transferred
      cost.transf.m3.prmm = Merg$cov[i]*Merg$mu.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * 
        (Merg$dur.transf.m3[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m3[i]/7),
      
      cost.sachet.transf.m3.prmm = Merg$cov[i]*Merg$mu.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$dur.transf.m3[i]*Merg$cost.rusf[i] ,
      cost.drug.transf.m3.prmm = Merg$cov[i]*Merg$mu.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m3.prmm = Merg$cov[i]*Merg$mu.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m3[i]/7,
      cost.nonlabor.transf.m3.prmm = Merg$cov[i]*Merg$mu.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m3[i]/7,
      cost.caregiver.transf.m3.prmm = Merg$cov[i]*Merg$mu.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m3[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  * 
        ((Merg$dur.rec.m3.prmm[i]+Merg$hosp.stay.m3[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m3.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * (Merg$dur.rec.m3.prmm[i]+Merg$hosp.stay.m3[i]) *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m3.prmm[i]/7,
      
      cost.nonlabor.hosp.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m3.prmm[i]/7,
      
      cost.caregiver.hosp.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m3.prmm[i]/7,
      
      cost.hospitalization.hosp.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$alpha2.m3.prmm[i] * 
        pop * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * 
        ((Merg$dur.died.m3.prmm[i]+Merg$hosp.stay.m3[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m3.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.died.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * (Merg$dur.died.m3.prmm[i]+Merg$hosp.stay.m3[i])  *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.died.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m3.prmm[i]/7,
      
      cost.nonlabor.hosp.died.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m3.prmm[i]/7,
      
      cost.caregiver.hosp.died.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m3.prmm[i]/7,
      
      cost.hospitalization.hosp.died.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prmm[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * 
        ((Merg$dur.never.rec.m3.prmm[i]- Merg$dur.hosp.m3[i]) *Merg$cost.rusf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m3.prmm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7),
      
      cost.sachet.hosp.never.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   *  (Merg$dur.never.rec.m3.prmm[i]- Merg$dur.hosp.m3[i]) *Merg$cost.rusf[i], 
      
      cost.labor.hosp.never.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m3.prmm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.nonlabor.hosp.never.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m3.prmm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.caregiver.hosp.never.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m3.prmm[i] - Merg$dur.hosp.m3[i] - Merg$hosp.stay.m3[i])/7,
      
      cost.hospitalization.hosp.never.rec.m3.prmm = Merg$cov[i]*Merg$phi.m3[i] * Merg$eta2.m3.prmm[i] * 
        pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m3[i]/7,
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m3.reg.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m3.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]*Merg$cost.rusf[i] * Merg$dur.m.reg.sam[i],
      cost.drug.m3.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m3.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m3.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m3.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  SAM OptiMA
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      cost.rec.s1 = Merg$alpha.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$rec.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s1[i]/7),
      
      cost.sachet.rec.s1 = Merg$alpha.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$rec.s1[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.s1 = Merg$alpha.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.rec.s1 = Merg$alpha.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s1[i]/7,
      cost.nonlabor.rec.s1 = Merg$alpha.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s1[i]/7,
      cost.caregiver.rec.s1 = Merg$alpha.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s1[i]/7,
      
      ##### defaulted
      cost.def.s1 = Merg$eps.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$def.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s1[i]/7),
      
      cost.sachet.def.s1 = Merg$eps.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$def.s1[i]*Merg$cost.rutf[i] ,
      cost.drug.def.s1 = Merg$eps.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.def.s1 = Merg$eps.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s1[i]/7,
      cost.nonlabor.def.s1 = Merg$eps.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s1[i]/7,
      cost.caregiver.def.s1 = Merg$eps.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s1[i]/7,
      
      ##### never recovered
      cost.never.rec.s1 = Merg$eta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$never.rec.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s1[i]/7),
      
      cost.sachet.never.rec.s1 = Merg$eta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$never.rec.s1[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.s1 = Merg$eta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.never.rec.s1 = Merg$eta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s1[i]/7,
      cost.nonlabor.never.rec.s1 = Merg$eta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s1[i]/7,
      cost.caregiver.never.rec.s1 = Merg$eta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s1[i]/7,
      
      ##### died
      cost.died.s1 = Merg$theta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$died.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s1[i]/7),
      
      cost.sachet.died.s1 = Merg$theta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$died.s1[i]*Merg$cost.rutf[i] ,
      cost.drug.died.s1 = Merg$theta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.died.s1 = Merg$theta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s1[i]/7,
      cost.nonlabor.died.s1 = Merg$theta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s1[i]/7,
      cost.caregiver.died.s1 = Merg$theta.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s1[i]/7,
      
      ##### hospitalized
      cost.hosp.s1 = Merg$phi.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s1[i]/7),
      
      cost.sachet.hosp.s1 = Merg$phi.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.s1[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.s1 = Merg$phi.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.hosp.s1 = Merg$phi.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s1[i]/7,
      cost.nonlabor.hosp.s1 = Merg$phi.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s1[i]/7,
      cost.caregiver.hosp.s1 = Merg$phi.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s1[i]/7,
      
      ##### transferred
      cost.transf.s1 = Merg$mu.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$transf.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s1[i]/7),
      
      cost.sachet.transf.s1 = Merg$mu.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$transf.s1[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.s1 = Merg$mu.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.transf.s1 = Merg$mu.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s1[i]/7,
      cost.nonlabor.transf.s1 = Merg$mu.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s1[i]/7,
      cost.caregiver.transf.s1 = Merg$mu.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s1[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s1 = Merg$phi.s1[i] * Merg$alpha2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.rec.s1[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s1[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.rec.s1 = Merg$phi.s1[i] * Merg$alpha2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.rec.s1[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s1 = Merg$phi.s1[i] * Merg$alpha2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s1[i]/7,
      
      cost.nonlabor.hosp.rec.s1 = Merg$phi.s1[i] * Merg$alpha2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s1[i]/7,
      
      cost.caregiver.hosp.rec.s1 = Merg$phi.s1[i] * Merg$alpha2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s1[i]/7,
      
      cost.hospitalization.hosp.rec.s1 = Merg$phi.s1[i] * Merg$alpha2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s1 = Merg$phi.s1[i] * Merg$theta2.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.died.s1[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s1[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.died.s1 = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.died.s1[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s1 = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s1[i]/7,
      
      cost.nonlabor.hosp.died.s1 = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s1[i]/7,
      
      cost.caregiver.hosp.died.s1 = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s1[i]/7,
      
      cost.hospitalization.hosp.died.s1 = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s1 = Merg$phi.s1[i] * Merg$eta2.s1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.never.rec.s1[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s1[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.never.rec.s1 = Merg$phi.s1[i] * Merg$eta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.never.rec.s1[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s1 = Merg$phi.s1[i] * Merg$eta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s1[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.nonlabor.hosp.never.rec.s1 = Merg$phi.s1[i] * Merg$eta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s1[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.caregiver.hosp.never.rec.s1 = Merg$phi.s1[i] * Merg$eta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s1[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.hospitalization.hosp.never.rec.s1 = Merg$phi.s1[i] * Merg$eta2.s1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      ##### 
      ##### Relapse Post SAM Treatment
      ##### 
      
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      
      ##### recovered
      cost.rec.s1.prss = Merg$alpha.s1.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * 
        (Merg$rec.s1.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s1.prss[i]/7),
      
      cost.sachet.rec.s1.prss = Merg$alpha.s1.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$rec.s1.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s1.prss = Merg$alpha.s1.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.rec.s1.prss = Merg$alpha.s1.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$cost.labor[i]*Merg$dur.rec.s1.prss[i]/7,
      
      cost.nonlabor.rec.s1.prss = Merg$alpha.s1.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.rec.s1.prss[i]/7,
      
      cost.caregiver.rec.s1.prss = Merg$alpha.s1.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$cost.caregiver[i]*Merg$dur.rec.s1.prss[i]/7,
      
      ##### defaulted
      cost.def.s1.prss = Merg$eps.s1[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *
        (Merg$def.s1.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s1.prss[i]/7),
      
      cost.sachet.def.s1.prss = Merg$eps.s1[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$def.s1.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s1.prss = Merg$eps.s1[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.def.s1.prss = Merg$eps.s1[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])  * Merg$cost.labor[i]*Merg$dur.def.s1.prss[i]/7,
      
      cost.nonlabor.def.s1.prss = Merg$eps.s1[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.def.s1.prss[i]/7,
      
      cost.caregiver.def.s1.prss = Merg$eps.s1[i] *  
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s1.prss[i]/7,
      
      ##### never recovered
      cost.never.rec.s1.prss = Merg$eta.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *
        (Merg$never.rec.s1.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s1.prss[i]/7),
      
      cost.sachet.never.rec.s1.prss = Merg$eta.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$never.rec.s1.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s1.prss =Merg$eta.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s1.prss = Merg$eta.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s1.prss[i]/7,
      
      cost.nonlabor.never.rec.s1.prss = Merg$eta.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s1.prss[i]/7,
      
      cost.caregiver.never.rec.s1.prss = Merg$eta.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s1.prss[i]/7,
      
      ##### died
      cost.died.s1.prss = Merg$theta.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *
        
        (Merg$died.s1.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s1.prss[i]/7),
      
      cost.sachet.died.s1.prss = Merg$theta.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$died.s1.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s1.prss = Merg$theta.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])  * Merg$cost.drug1[i],
      
      cost.labor.died.s1.prss = Merg$theta.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s1.prss[i]/7,
      
      cost.nonlabor.died.s1.prss = Merg$theta.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s1.prss[i]/7,
      
      cost.caregiver.died.s1.prss = Merg$theta.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s1.prss[i]/7,
      
      ##### hospitalized
      cost.hosp.s1.prss = Merg$phi.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * 
        
        (Merg$hosp.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s1[i]/7),
      
      cost.sachet.hosp.s1.prss = Merg$phi.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *  Merg$hosp.s1[i]*Merg$cost.rutf[i] ,
      
      cost.drug.hosp.s1.prss = Merg$phi.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *  Merg$cost.drug1[i],
      
      cost.labor.hosp.s1.prss = Merg$phi.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *  Merg$cost.labor[i]*Merg$dur.hosp.s1[i]/7,
      
      cost.nonlabor.hosp.s1.prss = Merg$phi.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *  Merg$cost.nonlabor[i]*Merg$dur.hosp.s1[i]/7,
      
      cost.caregiver.hosp.s1.prss = Merg$phi.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *  Merg$cost.caregiver[i]*Merg$dur.hosp.s1[i]/7,
      
      ##### transferred
      cost.transf.s1.prss = Merg$mu.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * 
        
        (Merg$transf.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s1[i]/7),
      
      cost.sachet.transf.s1.prss = Merg$mu.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$transf.s1[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s1.prss = Merg$mu.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s1.prss = Merg$mu.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.transf.s1[i]/7,
      
      cost.nonlabor.transf.s1.prss = Merg$mu.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s1[i]/7,
      
      cost.caregiver.transf.s1.prss = Merg$mu.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s1[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s1.prss = Merg$phi.s1[i] * Merg$alpha2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *
        
        (Merg$hosp.rec.s1.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s1.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.rec.s1.prss = Merg$phi.s1[i] * Merg$alpha2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i])   * Merg$hosp.rec.s1.prss[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s1.prss = Merg$phi.s1[i] * Merg$alpha2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.rec.s1.prss[i]/7,
      
      cost.nonlabor.hosp.rec.s1.prss = Merg$phi.s1[i] * Merg$alpha2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s1.prss[i]/7,
      
      cost.caregiver.hosp.rec.s1.prss = Merg$phi.s1[i] * Merg$alpha2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s1.prss[i]/7,
      
      cost.hospitalization.hosp.rec.s1.prss = Merg$phi.s1[i] * Merg$alpha2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s1.prss = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *
        
        (Merg$hosp.died.s1.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s1.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.died.s1.prss = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$hosp.died.s1.prss[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s1.prss = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s1.prss[i]/7,
      
      cost.nonlabor.hosp.died.s1.prss = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s1.prss[i]/7,
      
      cost.caregiver.hosp.died.s1.prss = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s1.prss[i]/7,
      
      cost.hospitalization.hosp.died.s1.prss = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s1.prss = Merg$phi.s1[i] * Merg$eta2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) *
        
        (Merg$hosp.never.rec.s1.prss[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s1.prss[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.never.rec.s1.prss = Merg$phi.s1[i] * Merg$eta2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$hosp.never.rec.s1.prss[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s1.prss = Merg$phi.s1[i] * Merg$eta2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s1.prss[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.nonlabor.hosp.never.rec.s1.prss = Merg$phi.s1[i] * Merg$eta2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s1.prss[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.caregiver.hosp.never.rec.s1.prss = Merg$phi.s1[i] * Merg$eta2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s1.prss[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.hospitalization.hosp.never.rec.s1.prss = Merg$phi.s1[i] * Merg$eta2.s1.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m1.prsm = Merg$cov[i]*Merg$alpha.m1.prsm[i] *   (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  *  
        (Merg$rec.m1.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m1.prsm[i]/7),
      
      cost.sachet.rec.m1.prsm = Merg$cov[i]*Merg$alpha.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$rec.m1.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.m1.prsm = Merg$cov[i]*Merg$alpha.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m1.prsm = Merg$cov[i]*Merg$alpha.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m1.prsm[i]/7,
      cost.nonlabor.rec.m1.prsm = Merg$cov[i]*Merg$alpha.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m1.prsm[i]/7,
      cost.caregiver.rec.m1.prsm = Merg$cov[i]*Merg$alpha.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m1.prsm[i]/7,
      
      ##### defaulted
      cost.def.m1.prsm = Merg$cov[i]*Merg$eps.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * 
        (Merg$def.m1.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m1.prsm[i]/7),
      
      cost.sachet.def.m1.prsm = Merg$cov[i]*Merg$eps.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$def.m1.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.def.m1.prsm = Merg$cov[i]*Merg$eps.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m1.prsm = Merg$cov[i]*Merg$eps.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m1.prsm[i]/7,
      cost.nonlabor.def.m1.prsm = Merg$cov[i]*Merg$eps.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m1.prsm[i]/7,
      cost.caregiver.def.m1.prsm = Merg$cov[i]*Merg$eps.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m1.prsm[i]/7,
      
      ##### never recovered
      cost.never.rec.m1.prsm = Merg$cov[i]*Merg$eta.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * 
        (Merg$never.rec.m1.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m1.prsm[i]/7),
      
      cost.sachet.never.rec.m1.prsm = Merg$cov[i]*Merg$eta.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$never.rec.m1.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.m1.prsm = Merg$cov[i]*Merg$eta.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m1.prsm = Merg$cov[i]*Merg$eta.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m1.prsm[i]/7,
      cost.nonlabor.never.rec.m1.prsm = Merg$cov[i]*Merg$eta.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m1.prsm[i]/7,
      cost.caregiver.never.rec.m1.prsm = Merg$cov[i]*Merg$eta.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m1.prsm[i]/7,
      
      ##### died
      cost.died.m1.prsm = Merg$cov[i]*Merg$theta.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * 
        (Merg$died.m1.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m1.prsm[i]/7),
      
      cost.sachet.died.m1.prsm = Merg$cov[i]*Merg$theta.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$died.m1.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.died.m1.prsm = Merg$cov[i]*Merg$theta.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m1.prsm = Merg$cov[i]*Merg$theta.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m1.prsm[i]/7,
      cost.nonlabor.died.m1.prsm = Merg$cov[i]*Merg$theta.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m1.prsm[i]/7,
      cost.caregiver.died.m1.prsm = Merg$cov[i]*Merg$theta.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m1.prsm[i]/7,
      
      ##### hospitalized
      cost.hosp.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * 
        (Merg$hosp.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m1[i]/7),
      
      cost.sachet.hosp.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$hosp.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m1[i]/7,
      cost.nonlabor.hosp.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m1[i]/7,
      cost.caregiver.hosp.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m1[i]/7,
      
      ##### transferred
      cost.transf.m1.prsm = Merg$cov[i]*Merg$mu.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * 
        (Merg$transf.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m1[i]/7),
      
      cost.sachet.transf.m1.prsm = Merg$cov[i]*Merg$mu.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$transf.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.m1.prsm = Merg$cov[i]*Merg$mu.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m1.prsm = Merg$cov[i]*Merg$mu.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m1[i]/7,
      cost.nonlabor.transf.m1.prsm = Merg$cov[i]*Merg$mu.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m1[i]/7,
      cost.caregiver.transf.m1.prsm = Merg$cov[i]*Merg$mu.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m1[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  * 
        (Merg$hosp.rec.m1.prsm[i]*Merg$cost.rutf[i] +  
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m1.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$hosp.rec.m1.prsm[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m1.prsm[i]/7,
      
      cost.nonlabor.hosp.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m1.prsm[i]/7,
      
      cost.caregiver.hosp.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m1.prsm[i]/7,
      
      cost.hospitalization.hosp.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * 
        (Merg$hosp.died.m1.prsm[i]*Merg$cost.rutf[i] +  
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m1.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.died.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$hosp.died.m1.prsm[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m1.prsm[i]/7,
      
      cost.nonlabor.hosp.died.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m1.prsm[i]/7,
      
      cost.caregiver.hosp.died.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m1.prsm[i]/7,
      
      cost.hospitalization.hosp.died.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * 
        (Merg$hosp.never.rec.m1.prsm[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m1.prsm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.never.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   *  Merg$hosp.never.rec.m1.prsm[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m1.prsm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.nonlabor.hosp.never.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m1.prsm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.caregiver.hosp.never.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m1.prsm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.hospitalization.hosp.never.rec.m1.prsm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m1.reg.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  *   
        (Merg$m1.reg.sam.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m1.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]*Merg$m1.reg.sam.prsm[i]*Merg$cost.rutf[i],
      cost.drug.m1.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m1.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m1.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m1.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  OptiMA - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      ##### recovered
      cost.rec.m1 = Merg$alpha.m1[i] * pop.m * 
        (Merg$rec.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m1[i]/7),
      
      cost.sachet.rec.m1 = Merg$alpha.m1[i] * pop.m * Merg$rec.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.m1 = Merg$alpha.m1[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.rec.m1 = Merg$alpha.m1[i] * pop.m * Merg$cost.labor[i]*Merg$dur.rec.m1[i]/7,
      cost.nonlabor.rec.m1 = Merg$alpha.m1[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m1[i]/7,
      cost.caregiver.rec.m1 = Merg$alpha.m1[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m1[i]/7,
      
      ##### defaulted
      cost.def.m1 = Merg$eps.m1[i] * pop.m * 
        (Merg$def.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m1[i]/7),
      
      cost.sachet.def.m1 = Merg$eps.m1[i] * pop.m * Merg$def.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.def.m1 = Merg$eps.m1[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.def.m1 = Merg$eps.m1[i] * pop.m * Merg$cost.labor[i]*Merg$dur.def.m1[i]/7,
      cost.nonlabor.def.m1 = Merg$eps.m1[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.def.m1[i]/7,
      cost.caregiver.def.m1 = Merg$eps.m1[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.def.m1[i]/7,
      
      ##### never recovered
      cost.never.rec.m1 = Merg$eta.m1[i] * pop.m * 
        (Merg$never.rec.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m1[i]/7),
      
      cost.sachet.never.rec.m1 = Merg$eta.m1[i] * pop.m * Merg$never.rec.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.m1 = Merg$eta.m1[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.never.rec.m1 = Merg$eta.m1[i] * pop.m * Merg$cost.labor[i]*Merg$dur.never.rec.m1[i]/7,
      cost.nonlabor.never.rec.m1 = Merg$eta.m1[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m1[i]/7,
      cost.caregiver.never.rec.m1 = Merg$eta.m1[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.never.rec.m1[i]/7,
      
      ##### died
      cost.died.m1 = Merg$theta.m1[i] * pop.m * 
        (Merg$died.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m1[i]/7),
      
      cost.sachet.died.m1 = Merg$theta.m1[i] * pop.m * Merg$died.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.died.m1 = Merg$theta.m1[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.died.m1 = Merg$theta.m1[i] * pop.m * Merg$cost.labor[i]*Merg$dur.died.m1[i]/7,
      cost.nonlabor.died.m1 = Merg$theta.m1[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m1[i]/7,
      cost.caregiver.died.m1 = Merg$theta.m1[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m1[i]/7,
      
      ##### hospitalized
      cost.hosp.m1 = Merg$phi.m1[i] * pop.m * 
        (Merg$hosp.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m1[i]/7),
      
      cost.sachet.hosp.m1 = Merg$phi.m1[i] * pop.m * Merg$hosp.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.m1 = Merg$phi.m1[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.hosp.m1 = Merg$phi.m1[i] * pop.m * Merg$cost.labor[i]*Merg$dur.hosp.m1[i]/7,
      cost.nonlabor.hosp.m1 = Merg$phi.m1[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.hosp.m1[i]/7,
      cost.caregiver.hosp.m1 = Merg$phi.m1[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.hosp.m1[i]/7,
      
      ##### transferred
      cost.transf.m1 = Merg$mu.m1[i] * pop.m * 
        (Merg$transf.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m1[i]/7),
      
      cost.sachet.transf.m1 = Merg$mu.m1[i] * pop.m * Merg$transf.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.m1 = Merg$mu.m1[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.transf.m1 = Merg$mu.m1[i] * pop.m * Merg$cost.labor[i]*Merg$dur.transf.m1[i]/7,
      cost.nonlabor.transf.m1 = Merg$mu.m1[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.transf.m1[i]/7,
      cost.caregiver.transf.m1 = Merg$mu.m1[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.transf.m1[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m1 = Merg$phi.m1[i] * Merg$alpha2.m1[i] * 
        pop.m * 
        (Merg$hosp.rec.m1[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m1[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.rec.m1 = Merg$phi.m1[i] * Merg$alpha2.m1[i] * 
        pop.m * Merg$hosp.rec.m1[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.m1 = Merg$phi.m1[i] * Merg$alpha2.m1[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.rec.m1[i]/7,
      
      cost.nonlabor.hosp.rec.m1 = Merg$phi.m1[i] * Merg$alpha2.m1[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m1[i]/7,
      
      cost.caregiver.hosp.rec.m1 = Merg$phi.m1[i] * Merg$alpha2.m1[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m1[i]/7,
      
      cost.hospitalization.hosp.rec.m1 = Merg$phi.m1[i] * Merg$alpha2.m1[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m1 = Merg$phi.m1[i] * Merg$theta2.m1[i] * pop.m * 
        (Merg$hosp.died.m1[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m1[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.died.m1 = Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$hosp.died.m1[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.m1 = Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.died.m1[i]/7,
      
      cost.nonlabor.hosp.died.m1 = Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m1[i]/7,
      
      cost.caregiver.hosp.died.m1 = Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m1[i]/7,
      
      cost.hospitalization.hosp.died.m1 = Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m1 = Merg$phi.m1[i] * Merg$eta2.m1[i] * pop.m * 
        (Merg$hosp.never.rec.m1[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m1[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.never.rec.m1 = Merg$phi.m1[i] * Merg$eta2.m1[i] * 
        pop.m * Merg$hosp.never.rec.m1[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.m1 = Merg$phi.m1[i] * Merg$eta2.m1[i] * 
        pop.m * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m1[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.nonlabor.hosp.never.rec.m1 = Merg$phi.m1[i] * Merg$eta2.m1[i] * 
        pop.m * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m1[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.caregiver.hosp.never.rec.m1 = Merg$phi.m1[i] * Merg$eta2.m1[i] * 
        pop.m * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m1[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.hospitalization.hosp.never.rec.m1 = Merg$phi.m1[i] * Merg$eta2.m1[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m1.reg.sam = Merg$tau.m[i] * pop.m * 
        (Merg$m1.reg.sam[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.opportunity[i] + 
              Merg$cost.transport[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m1.to.sam = Merg$tau.m[i] * pop.m * Merg$m1.reg.sam[i]*Merg$cost.rutf[i] ,
      cost.drug.m1.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.drug[i],
      cost.labor.m1.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m1.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m1.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      cost.rec.s1.prms = Merg$alpha.s1.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * 
        
        (Merg$rec.s1.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s1.prms[i]/7),
      
      cost.sachet.rec.s1.prms = Merg$alpha.s1.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$rec.s1.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s1.prms = Merg$alpha.s1.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.rec.s1.prms = Merg$alpha.s1.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s1.prms[i]/7,
      
      cost.nonlabor.rec.s1.prms = Merg$alpha.s1.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s1.prms[i]/7,
      
      cost.caregiver.rec.s1.prms = Merg$alpha.s1.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s1.prms[i]/7,
      
      ##### defaulted
      cost.def.s1.prms = Merg$eps.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$def.s1.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s1.prms[i]/7),
      
      cost.sachet.def.s1.prms = Merg$eps.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$def.s1.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s1.prms = Merg$eps.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.def.s1.prms = Merg$eps.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s1.prms[i]/7,
      
      cost.nonlabor.def.s1.prms = Merg$eps.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s1.prms[i]/7,
      
      cost.caregiver.def.s1.prms = Merg$eps.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s1.prms[i]/7,
      
      
      ##### never recovered
      cost.never.rec.s1.prms = Merg$eta.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$never.rec.s1.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s1.prms[i]/7),
      
      cost.sachet.never.rec.s1.prms = Merg$eta.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$never.rec.s1.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s1.prms = Merg$eta.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s1.prms = Merg$eta.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s1.prms[i]/7,
      
      cost.nonlabor.never.rec.s1.prms = Merg$eta.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s1.prms[i]/7,
      
      cost.caregiver.never.rec.s1.prms = Merg$eta.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s1.prms[i]/7,
      
      
      ##### died
      cost.died.s1.prms = Merg$theta.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$died.s1.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s1.prms[i]/7),
      
      cost.sachet.died.s1.prms = Merg$theta.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$died.s1.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s1.prms = Merg$theta.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.died.s1.prms = Merg$theta.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s1.prms[i]/7,
      
      cost.nonlabor.died.s1.prms = Merg$theta.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s1.prms[i]/7,
      
      cost.caregiver.died.s1.prms = Merg$theta.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s1.prms[i]/7,
      
      ##### hospitalized
      cost.hosp.s1.prms = Merg$phi.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s1[i]/7),
      
      cost.sachet.hosp.s1.prms = Merg$phi.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.s1[i]*Merg$cost.rutf[i] ,
      
      cost.drug.hosp.s1.prms = Merg$phi.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.hosp.s1.prms = Merg$phi.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s1[i]/7,
      
      cost.nonlabor.hosp.s1.prms = Merg$phi.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s1[i]/7,
      
      cost.caregiver.hosp.s1.prms = Merg$phi.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s1[i]/7,
      
      
      ##### transferred
      cost.transf.s1.prms = Merg$mu.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$transf.s1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s1[i]/7),
      
      cost.sachet.transf.s1.prms = Merg$mu.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$transf.s1[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s1.prms = Merg$mu.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s1.prms = Merg$mu.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s1[i]/7,
      
      cost.nonlabor.transf.s1.prms = Merg$mu.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s1[i]/7,
      
      cost.caregiver.transf.s1.prms = Merg$mu.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s1[i]/7,
      
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s1.prms = Merg$phi.s1[i] * Merg$alpha2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.rec.s1.prms[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s1.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.rec.s1.prms = Merg$phi.s1[i] * Merg$alpha2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.rec.s1.prms[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s1.prms = Merg$phi.s1[i] * Merg$alpha2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s1.prms[i]/7,
      
      cost.nonlabor.hosp.rec.s1.prms = Merg$phi.s1[i] * Merg$alpha2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s1.prms[i]/7,
      
      cost.caregiver.hosp.rec.s1.prms = Merg$phi.s1[i] * Merg$alpha2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s1.prms[i]/7,
      
      cost.hospitalization.hosp.rec.s1.prms = Merg$phi.s1[i] * Merg$alpha2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s1.prms = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.died.s1.prms[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s1.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.died.s1.prms = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.died.s1.prms[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s1.prms = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s1.prms[i]/7,
      
      cost.nonlabor.hosp.died.s1.prms = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s1.prms[i]/7,
      
      cost.caregiver.hosp.died.s1.prms = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s1.prms[i]/7,
      
      cost.hospitalization.hosp.died.s1.prms = Merg$phi.s1[i] * Merg$theta2.s1[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s1.prms = Merg$phi.s1[i] * Merg$eta2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.never.rec.s1.prms[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s1.prms[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7),
      
      cost.sachet.hosp.never.rec.s1.prms = Merg$phi.s1[i] * Merg$eta2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.never.rec.s1.prms[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s1.prms = Merg$phi.s1[i] * Merg$eta2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s1.prms[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.nonlabor.hosp.never.rec.s1.prms = Merg$phi.s1[i] * Merg$eta2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s1.prms[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.caregiver.hosp.never.rec.s1.prms = Merg$phi.s1[i] * Merg$eta2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s1.prms[i] - Merg$dur.hosp.s1[i] - Merg$hosp.stay.s1[i])/7,
      
      cost.hospitalization.hosp.never.rec.s1.prms = Merg$phi.s1[i] * Merg$eta2.s1.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s1[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m1.prmm = Merg$cov[i]*Merg$alpha.m1.prmm[i] *  pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  *  
        (Merg$rec.m1.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m1.prmm[i]/7),
      
      cost.sachet.rec.m1.prmm = Merg$cov[i]*Merg$alpha.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$rec.m1.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.m1.prmm = Merg$cov[i]*Merg$alpha.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m1.prmm = Merg$cov[i]*Merg$alpha.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m1.prmm[i]/7,
      cost.nonlabor.rec.m1.prmm = Merg$cov[i]*Merg$alpha.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m1.prmm[i]/7,
      cost.caregiver.rec.m1.prmm = Merg$cov[i]*Merg$alpha.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m1.prmm[i]/7,
      
      ##### defaulted
      cost.def.m1.prmm = Merg$cov[i]*Merg$eps.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * 
        (Merg$def.m1.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m1.prmm[i]/7),
      
      cost.sachet.def.m1.prmm = Merg$cov[i]*Merg$eps.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$def.m1.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.def.m1.prmm = Merg$cov[i]*Merg$eps.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m1.prmm = Merg$cov[i]*Merg$eps.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m1.prmm[i]/7,
      cost.nonlabor.def.m1.prmm = Merg$cov[i]*Merg$eps.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m1.prmm[i]/7,
      cost.caregiver.def.m1.prmm = Merg$cov[i]*Merg$eps.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m1.prmm[i]/7,
      
      ##### never recovered
      cost.never.rec.m1.prmm = Merg$cov[i]*Merg$eta.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * 
        (Merg$never.rec.m1.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m1.prmm[i]/7),
      
      cost.sachet.never.rec.m1.prmm = Merg$cov[i]*Merg$eta.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$never.rec.m1.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.m1.prmm = Merg$cov[i]*Merg$eta.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m1.prmm = Merg$cov[i]*Merg$eta.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m1.prmm[i]/7,
      cost.nonlabor.never.rec.m1.prmm = Merg$cov[i]*Merg$eta.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m1.prmm[i]/7,
      cost.caregiver.never.rec.m1.prmm = Merg$cov[i]*Merg$eta.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m1.prmm[i]/7,
      
      ##### died
      cost.died.m1.prmm = Merg$cov[i]*Merg$theta.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * 
        (Merg$died.m1.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m1.prmm[i]/7),
      
      cost.sachet.died.m1.prmm = Merg$cov[i]*Merg$theta.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$died.m1.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.died.m1.prmm = Merg$cov[i]*Merg$theta.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m1.prmm = Merg$cov[i]*Merg$theta.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m1.prmm[i]/7,
      cost.nonlabor.died.m1.prmm = Merg$cov[i]*Merg$theta.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m1.prmm[i]/7,
      cost.caregiver.died.m1.prmm = Merg$cov[i]*Merg$theta.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m1.prmm[i]/7,
      
      ##### hospitalized
      cost.hosp.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * 
        (Merg$hosp.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m1[i]/7),
      
      cost.sachet.hosp.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$hosp.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m1[i]/7,
      cost.nonlabor.hosp.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m1[i]/7,
      cost.caregiver.hosp.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m1[i]/7,
      
      ##### transferred
      cost.transf.m1.prmm = Merg$cov[i]*Merg$mu.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * 
        (Merg$transf.m1[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m1[i]/7),
      
      cost.sachet.transf.m1.prmm = Merg$cov[i]*Merg$mu.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$transf.m1[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.m1.prmm = Merg$cov[i]*Merg$mu.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m1.prmm = Merg$cov[i]*Merg$mu.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m1[i]/7,
      cost.nonlabor.transf.m1.prmm = Merg$cov[i]*Merg$mu.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m1[i]/7,
      cost.caregiver.transf.m1.prmm = Merg$cov[i]*Merg$mu.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m1[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  * 
        (Merg$hosp.rec.m1.prmm[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$rec.m1.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$hosp.rec.m1.prmm[i] *Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m1.prmm[i]/7,
      
      cost.nonlabor.hosp.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m1.prmm[i]/7,
      
      cost.caregiver.hosp.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m1.prmm[i]/7,
      
      cost.hospitalization.hosp.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$alpha2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * 
        (Merg$hosp.died.m1.prmm[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m1.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.died.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$hosp.died.m1.prmm[i]  *Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m1.prmm[i]/7,
      
      cost.nonlabor.hosp.died.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m1.prmm[i]/7,
      
      cost.caregiver.hosp.died.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m1.prmm[i]/7,
      
      cost.hospitalization.hosp.died.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$theta2.m1[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prmm[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * 
        (Merg$hosp.never.rec.m1.prmm[i] *Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m1.prmm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7),
      
      cost.sachet.hosp.never.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   *  Merg$hosp.never.rec.m1.prmm[i] *Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m1.prmm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.nonlabor.hosp.never.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m1.prmm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.caregiver.hosp.never.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m1.prmm[i] - Merg$dur.hosp.m1[i] - Merg$hosp.stay.m1[i])/7,
      
      cost.hospitalization.hosp.never.rec.m1.prmm = Merg$cov[i]*Merg$phi.m1[i] * Merg$eta2.m1.prmm[i] * 
        pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m1[i]/7,
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m1.reg.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] *   
        (Merg$m1.reg.sam.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m1.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]*Merg$cost.rutf[i] * Merg$m1.reg.sam.prmm[i],
      cost.drug.m1.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m1.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m1.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m1.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### ######### SAM Standard
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      ##### recovered
      cost.rec.s = Merg$alpha.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$rec.s[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s[i]/7),
      
      cost.sachet.rec.s = Merg$alpha.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$rec.s[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.s = Merg$alpha.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug[i],
      cost.labor.rec.s = Merg$alpha.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s[i]/7,
      cost.nonlabor.rec.s = Merg$alpha.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s[i]/7,
      cost.caregiver.rec.s = Merg$alpha.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s[i]/7,
      
      ##### defaulted
      cost.def.s = Merg$eps.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$def.s[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s[i]/7),
      
      cost.sachet.def.s = Merg$eps.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$def.s[i]*Merg$cost.rutf[i] ,
      cost.drug.def.s = Merg$eps.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug[i],
      cost.labor.def.s = Merg$eps.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s[i]/7,
      cost.nonlabor.def.s = Merg$eps.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s[i]/7,
      cost.caregiver.def.s = Merg$eps.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s[i]/7,
      
      ##### never recovered
      cost.never.rec.s = Merg$eta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$never.rec.s[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s[i]/7),
      
      cost.sachet.never.rec.s = Merg$eta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$never.rec.s[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.s = Merg$eta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug[i],
      cost.labor.never.rec.s = Merg$eta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s[i]/7,
      cost.nonlabor.never.rec.s = Merg$eta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s[i]/7,
      cost.caregiver.never.rec.s = Merg$eta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s[i]/7,
      
      ##### died
      cost.died.s = Merg$theta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$died.s[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s[i]/7),
      
      cost.sachet.died.s = Merg$theta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$died.s[i]*Merg$cost.rutf[i] ,
      cost.drug.died.s = Merg$theta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug[i],
      cost.labor.died.s = Merg$theta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s[i]/7,
      cost.nonlabor.died.s = Merg$theta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s[i]/7,
      cost.caregiver.died.s = Merg$theta.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s[i]/7,
      
      ##### hospitalized
      cost.hosp.s = Merg$phi.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.s[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s[i]/7),
      
      cost.sachet.hosp.s = Merg$phi.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.s[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.s = Merg$phi.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug[i],
      cost.labor.hosp.s = Merg$phi.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s[i]/7,
      cost.nonlabor.hosp.s = Merg$phi.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s[i]/7,
      cost.caregiver.hosp.s = Merg$phi.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s[i]/7,
      
      ##### transferred
      cost.transf.s = Merg$mu.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$transf.s[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s[i]/7),
      
      cost.sachet.transf.s = Merg$mu.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$transf.s[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.s = Merg$mu.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug[i],
      cost.labor.transf.s = Merg$mu.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s[i]/7,
      cost.nonlabor.transf.s = Merg$mu.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s[i]/7,
      cost.caregiver.transf.s = Merg$mu.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s[i]/7,
      
      ##### recovered Post-hospitalization
      ### hospitalization cost includes labor costs + medications/supplies  + opportunity cost 
      ### therefore costs of labor + nonlabor + caregiver added to recovery or died or never recovered duration post-hospitalization
      
      cost.hosp.rec.s = Merg$phi.s[i] * Merg$alpha2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.rec.s[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.rec.s = Merg$phi.s[i] * Merg$alpha2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.rec.s[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s = Merg$phi.s[i] * Merg$alpha2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s[i]/7,
      
      cost.nonlabor.hosp.rec.s = Merg$phi.s[i] * Merg$alpha2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s[i]/7,
      
      cost.caregiver.hosp.rec.s = Merg$phi.s[i] * Merg$alpha2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s[i]/7,
      
      cost.hospitalization.hosp.rec.s = Merg$phi.s[i] * Merg$alpha2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s = Merg$phi.s[i] * Merg$theta2.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.died.s[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.died.s = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.died.s[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s[i]/7,
      
      cost.nonlabor.hosp.died.s = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s[i]/7,
      
      cost.caregiver.hosp.died.s = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s[i]/7,
      
      cost.hospitalization.hosp.died.s = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s = Merg$phi.s[i] * Merg$eta2.s[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.never.rec.s[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.never.rec.s = Merg$phi.s[i] * Merg$eta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.never.rec.s[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s = Merg$phi.s[i] * Merg$eta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.nonlabor.hosp.never.rec.s = Merg$phi.s[i] * Merg$eta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.caregiver.hosp.never.rec.s = Merg$phi.s[i] * Merg$eta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.hospitalization.hosp.never.rec.s = Merg$phi.s[i] * Merg$eta2.s[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      ##### 
      ##### Relapse Post SAM Treatment
      ##### 
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      ### MAM children who regressed to SAM during the treatment, receive treatment for SAM, both in original treatment period, as well as treatment post relapse)
      
      ##### recovered
      cost.rec.s.prss = Merg$alpha.s.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * 
        (Merg$rec.s.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s.prss[i]/7),
      
      cost.sachet.rec.s.prss = Merg$alpha.s.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$rec.s.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s.prss = Merg$alpha.s.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.rec.s.prss = Merg$alpha.s.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$cost.labor[i]*Merg$dur.rec.s.prss[i]/7,
      
      cost.nonlabor.rec.s.prss = Merg$alpha.s.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.rec.s.prss[i]/7,
      
      cost.caregiver.rec.s.prss = Merg$alpha.s.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$cost.caregiver[i]*Merg$dur.rec.s.prss[i]/7,
      
      ##### defaulted
      cost.def.s.prss = Merg$eps.s[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *
        (Merg$def.s.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s.prss[i]/7),
      
      cost.sachet.def.s.prss = Merg$eps.s[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$def.s.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s.prss = Merg$eps.s[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.def.s.prss = Merg$eps.s[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])  * Merg$cost.labor[i]*Merg$dur.def.s.prss[i]/7,
      
      cost.nonlabor.def.s.prss = Merg$eps.s[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.def.s.prss[i]/7,
      
      cost.caregiver.def.s.prss = Merg$eps.s[i] *  
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s.prss[i]/7,
      
      ##### never recovered
      cost.never.rec.s.prss = Merg$eta.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *
        (Merg$never.rec.s.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s.prss[i]/7),
      
      cost.sachet.never.rec.s.prss = Merg$eta.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$never.rec.s.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s.prss =Merg$eta.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s.prss = Merg$eta.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s.prss[i]/7,
      
      cost.nonlabor.never.rec.s.prss = Merg$eta.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s.prss[i]/7,
      
      cost.caregiver.never.rec.s.prss = Merg$eta.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s.prss[i]/7,
      
      ##### died
      cost.died.s.prss = Merg$theta.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *
        
        (Merg$died.s.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s.prss[i]/7),
      
      cost.sachet.died.s.prss = Merg$theta.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$died.s.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s.prss = Merg$theta.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])  * Merg$cost.drug1[i],
      
      cost.labor.died.s.prss = Merg$theta.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s.prss[i]/7,
      
      cost.nonlabor.died.s.prss = Merg$theta.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s.prss[i]/7,
      
      cost.caregiver.died.s.prss = Merg$theta.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s.prss[i]/7,
      
      ##### hospitalized
      cost.hosp.s.prss = Merg$phi.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * 
        
        (Merg$hosp.s[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s[i]/7),
      
      cost.sachet.hosp.s.prss = Merg$phi.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *  Merg$hosp.s[i]*Merg$cost.rutf[i] ,
      
      cost.drug.hosp.s.prss = Merg$phi.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *  Merg$cost.drug1[i],
      
      cost.labor.hosp.s.prss = Merg$phi.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *  Merg$cost.labor[i]*Merg$dur.hosp.s[i]/7,
      
      cost.nonlabor.hosp.s.prss = Merg$phi.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *  Merg$cost.nonlabor[i]*Merg$dur.hosp.s[i]/7,
      
      cost.caregiver.hosp.s.prss = Merg$phi.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *  Merg$cost.caregiver[i]*Merg$dur.hosp.s[i]/7,
      
      ##### transferred
      cost.transf.s.prss = Merg$mu.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * 
        
        (Merg$transf.s[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s[i]/7),
      
      cost.sachet.transf.s.prss = Merg$mu.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$transf.s[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s.prss = Merg$mu.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s.prss = Merg$mu.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.transf.s[i]/7,
      
      cost.nonlabor.transf.s.prss = Merg$mu.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s[i]/7,
      
      cost.caregiver.transf.s.prss = Merg$mu.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s.prss = Merg$phi.s[i] * Merg$alpha2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *
        
        (Merg$hosp.rec.s.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.rec.s.prss = Merg$phi.s[i] * Merg$alpha2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i])   * Merg$hosp.rec.s.prss[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s.prss = Merg$phi.s[i] * Merg$alpha2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.rec.s.prss[i]/7,
      
      cost.nonlabor.hosp.rec.s.prss = Merg$phi.s[i] * Merg$alpha2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s.prss[i]/7,
      
      cost.caregiver.hosp.rec.s.prss = Merg$phi.s[i] * Merg$alpha2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s.prss[i]/7,
      
      cost.hospitalization.hosp.rec.s.prss = Merg$phi.s[i] * Merg$alpha2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s.prss = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *
        
        (Merg$hosp.died.s.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.died.s.prss = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$hosp.died.s.prss[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s.prss = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s.prss[i]/7,
      
      cost.nonlabor.hosp.died.s.prss = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s.prss[i]/7,
      
      cost.caregiver.hosp.died.s.prss = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s.prss[i]/7,
      
      cost.hospitalization.hosp.died.s.prss = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s.prss = Merg$phi.s[i] * Merg$eta2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) *
        
        (Merg$hosp.never.rec.s.prss[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s.prss[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.never.rec.s.prss = Merg$phi.s[i] * Merg$eta2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$hosp.never.rec.s.prss[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s.prss = Merg$phi.s[i] * Merg$eta2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s.prss[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.nonlabor.hosp.never.rec.s.prss = Merg$phi.s[i] * Merg$eta2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s.prss[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.caregiver.hosp.never.rec.s.prss = Merg$phi.s[i] * Merg$eta2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s.prss[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.hospitalization.hosp.never.rec.s.prss = Merg$phi.s[i] * Merg$eta2.s.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m.prsm = Merg$cov[i]*Merg$alpha.m.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  *  
        (Merg$dur.rec.m.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prsm[i]/7),
      
      cost.sachet.rec.m.prsm = Merg$cov[i]*Merg$alpha.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.rec.m.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.rec.m.prsm = Merg$cov[i]*Merg$alpha.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m.prsm = Merg$cov[i]*Merg$alpha.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m.prsm[i]/7,
      cost.nonlabor.rec.m.prsm = Merg$cov[i]*Merg$alpha.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m.prsm[i]/7,
      cost.caregiver.rec.m.prsm = Merg$cov[i]*Merg$alpha.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m.prsm[i]/7,
      
      ##### defaulted
      cost.def.m.prsm = Merg$cov[i]*Merg$eps.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.def.m.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m.prsm[i]/7),
      
      cost.sachet.def.m.prsm = Merg$cov[i]*Merg$eps.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.def.m.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.def.m.prsm = Merg$cov[i]*Merg$eps.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m.prsm = Merg$cov[i]*Merg$eps.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m.prsm[i]/7,
      cost.nonlabor.def.m.prsm = Merg$cov[i]*Merg$eps.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m.prsm[i]/7,
      cost.caregiver.def.m.prsm = Merg$cov[i]*Merg$eps.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m.prsm[i]/7,
      
      ##### never recovered
      cost.never.rec.m.prsm = Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.never.rec.m.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m.prsm[i]/7),
      
      cost.sachet.never.rec.m.prsm = Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.never.rec.m.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.never.rec.m.prsm = Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m.prsm = Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m.prsm[i]/7,
      cost.nonlabor.never.rec.m.prsm = Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m.prsm[i]/7,
      cost.caregiver.never.rec.m.prsm = Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m.prsm[i]/7,
      
      ##### died
      cost.died.m.prsm = Merg$cov[i]*Merg$theta.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.died.m.prsm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prsm[i]/7),
      
      cost.sachet.died.m.prsm = Merg$cov[i]*Merg$theta.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.died.m.prsm[i]*Merg$cost.rusf[i] ,
      cost.drug.died.m.prsm = Merg$cov[i]*Merg$theta.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m.prsm = Merg$cov[i]*Merg$theta.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m.prsm[i]/7,
      cost.nonlabor.died.m.prsm = Merg$cov[i]*Merg$theta.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m.prsm[i]/7,
      cost.caregiver.died.m.prsm = Merg$cov[i]*Merg$theta.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m.prsm[i]/7,
      
      ##### hospitalized
      cost.hosp.m.prsm = Merg$cov[i]*Merg$phi.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.hosp.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m[i]/7),
      
      cost.sachet.hosp.m.prsm = Merg$cov[i]*Merg$phi.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.hosp.m[i]*Merg$cost.rusf[i] ,
      cost.drug.hosp.m.prsm = Merg$cov[i]*Merg$phi.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m.prsm = Merg$cov[i]*Merg$phi.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m[i]/7,
      cost.nonlabor.hosp.m.prsm = Merg$cov[i]*Merg$phi.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m[i]/7,
      cost.caregiver.hosp.m.prsm = Merg$cov[i]*Merg$phi.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m[i]/7,
      
      ##### transferred
      cost.transf.m.prsm = Merg$cov[i]*Merg$mu.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.transf.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m[i]/7),
      
      cost.sachet.transf.m.prsm = Merg$cov[i]*Merg$mu.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.transf.m[i]*Merg$cost.rusf[i] ,
      cost.drug.transf.m.prsm = Merg$cov[i]*Merg$mu.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m.prsm = Merg$cov[i]*Merg$mu.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m[i]/7,
      cost.nonlabor.transf.m.prsm = Merg$cov[i]*Merg$mu.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m[i]/7,
      cost.caregiver.transf.m.prsm = Merg$cov[i]*Merg$mu.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        ((Merg$dur.rec.m.prsm[i]+Merg$hosp.stay.m[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * (Merg$dur.rec.m.prsm[i]+Merg$hosp.stay.m[i]) *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m.prsm[i]/7,
      
      cost.nonlabor.hosp.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m.prsm[i]/7,
      
      cost.caregiver.hosp.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m.prsm[i]/7,
      
      cost.hospitalization.hosp.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * 
        ((Merg$dur.died.m.prsm[i]+Merg$hosp.stay.m[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.died.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * (Merg$dur.died.m.prsm[i]+Merg$hosp.stay.m[i])  *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.died.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m.prsm[i]/7,
      
      cost.nonlabor.hosp.died.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m.prsm[i]/7,
      
      cost.caregiver.hosp.died.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m.prsm[i]/7,
      
      cost.hospitalization.hosp.died.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * 
        ((Merg$dur.never.rec.m.prsm[i]- Merg$dur.hosp.m[i]) *Merg$cost.rusf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m.prsm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.never.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   *  (Merg$dur.never.rec.m.prsm[i]- Merg$dur.hosp.m[i]) *Merg$cost.rusf[i], 
      
      cost.labor.hosp.never.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m.prsm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.nonlabor.hosp.never.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m.prsm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.caregiver.hosp.never.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m.prsm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.hospitalization.hosp.never.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m.reg.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]*Merg$cost.rusf[i] * Merg$dur.m.reg.sam[i],
      cost.drug.m.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-SAM to MAM CSB
      ## ## ## 
      
      ##### recovered
      cost.rec.m.prsm.csb= Merg$cov[i]*Merg$alpha.m.prsm[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  *  
        (Merg$dur.rec.m.prsm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prsm[i]/7),
      
      cost.csb.rec.m.prsm = Merg$cov[i]*Merg$alpha.m.prsm[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.rec.m.prsm[i]*Merg$cost.csb[i]/6 ,
      
      ##### defaulted
      cost.def.m.prsm.csb= Merg$cov[i]*Merg$eps.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.def.m.prsm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m.prsm[i]/7),
      
      cost.csb.def.m.prsm = Merg$cov[i]*Merg$eps.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.def.m.prsm[i]*Merg$cost.csb[i]/6 ,
      
      ##### never recovered
      cost.never.rec.m.prsm.csb= Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.never.rec.m.prsm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m.prsm[i]/7),
      
      cost.csb.never.rec.m.prsm = Merg$cov[i]*Merg$eta.m.prsm[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.never.rec.m.prsm[i]*Merg$cost.csb[i]/6 ,
      
      ##### died
      cost.died.m.prsm.csb= Merg$cov[i]*Merg$theta.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.died.m.prsm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prsm[i]/7),
      
      cost.csb.died.m.prsm = Merg$cov[i]*Merg$theta.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.died.m.prsm[i]*Merg$cost.csb[i]/6 ,
      
      ##### hospitalized
      cost.hosp.m.prsm.csb= Merg$cov[i]*Merg$phi.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.hosp.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m[i]/7),
      
      cost.csb.hosp.m.prsm = Merg$cov[i]*Merg$phi.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.hosp.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### transferred
      cost.transf.m.prsm.csb= Merg$cov[i]*Merg$mu.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        (Merg$dur.transf.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m[i]/7),
      
      cost.csb.transf.m.prsm = Merg$cov[i]*Merg$mu.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * Merg$dur.transf.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m.prsm.csb= Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  * 
        ((Merg$dur.rec.m.prsm[i]+Merg$hosp.stay.m[i])*Merg$cost.csb[i]/6 + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * 
        pop * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * (Merg$dur.rec.m.prsm[i]+Merg$hosp.stay.m[i]) *Merg$cost.csb[i]/6 ,
      
      ##### died Post-hospitalization
      cost.hosp.died.m.prsm.csb= Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * 
        ((Merg$dur.died.m.prsm[i]+Merg$hosp.stay.m[i])*Merg$cost.csb[i]/6 + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.died.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * (Merg$dur.died.m.prsm[i]+Merg$hosp.stay.m[i])  *Merg$cost.csb[i]/6 ,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m.prsm.csb= Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   * 
        ((Merg$dur.never.rec.m.prsm[i]- Merg$dur.hosp.m[i]) *Merg$cost.csb[i]/6 +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m.prsm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.never.rec.m.prsm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prsm[i] * 
        pop * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]   *  (Merg$dur.never.rec.m.prsm[i]- Merg$dur.hosp.m[i]) *Merg$cost.csb[i]/6, 
      
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m.reg.sam.prsm.csb= Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.csb[i]/6 + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.csb.m.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] * (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]*Merg$cost.csb[i]/6 * Merg$dur.m.reg.sam[i],
      
      
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  Standard - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      cost.rec.m = Merg$alpha.m[i] * pop.m * 
        (Merg$dur.rec.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m[i]/7),
      
      cost.sachet.rec.m = Merg$alpha.m[i] * pop.m * Merg$dur.rec.m[i]*Merg$cost.rusf[i] ,
      cost.drug.rec.m = Merg$alpha.m[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.rec.m = Merg$alpha.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.rec.m[i]/7,
      cost.nonlabor.rec.m = Merg$alpha.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m[i]/7,
      cost.caregiver.rec.m = Merg$alpha.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m[i]/7,
      
      ##### defaulted
      cost.def.m = Merg$eps.m[i] * pop.m * 
        (Merg$dur.def.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m[i]/7),
      
      cost.sachet.def.m = Merg$eps.m[i] * pop.m * Merg$dur.def.m[i]*Merg$cost.rusf[i] ,
      cost.drug.def.m = Merg$eps.m[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.def.m = Merg$eps.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.def.m[i]/7,
      cost.nonlabor.def.m = Merg$eps.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.def.m[i]/7,
      cost.caregiver.def.m = Merg$eps.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.def.m[i]/7,
      
      ##### never recovered
      cost.never.rec.m = Merg$eta.m[i] * pop.m * 
        (Merg$dur.never.rec.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m[i]/7),
      
      cost.sachet.never.rec.m = Merg$eta.m[i] * pop.m * Merg$dur.never.rec.m[i]*Merg$cost.rusf[i] ,
      cost.drug.never.rec.m = Merg$eta.m[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.never.rec.m = Merg$eta.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.never.rec.m[i]/7,
      cost.nonlabor.never.rec.m = Merg$eta.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m[i]/7,
      cost.caregiver.never.rec.m = Merg$eta.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.never.rec.m[i]/7,
      
      ##### died
      cost.died.m = Merg$theta.m[i] * pop.m * 
        (Merg$dur.died.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m[i]/7),
      
      cost.sachet.died.m = Merg$theta.m[i] * pop.m * Merg$dur.died.m[i]*Merg$cost.rusf[i] ,
      cost.drug.died.m = Merg$theta.m[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.died.m = Merg$theta.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.died.m[i]/7,
      cost.nonlabor.died.m = Merg$theta.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m[i]/7,
      cost.caregiver.died.m = Merg$theta.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m[i]/7,
      
      ##### hospitalized
      cost.hosp.m = Merg$phi.m[i] * pop.m * 
        (Merg$dur.hosp.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m[i]/7),
      
      cost.sachet.hosp.m = Merg$phi.m[i] * pop.m * Merg$dur.hosp.m[i]*Merg$cost.rusf[i] ,
      cost.drug.hosp.m = Merg$phi.m[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.hosp.m = Merg$phi.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.hosp.m[i]/7,
      cost.nonlabor.hosp.m = Merg$phi.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.hosp.m[i]/7,
      cost.caregiver.hosp.m = Merg$phi.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.hosp.m[i]/7,
      
      ##### transferred
      cost.transf.m = Merg$mu.m[i] * pop.m * 
        (Merg$dur.transf.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m[i]/7),
      
      cost.sachet.transf.m = Merg$mu.m[i] * pop.m * Merg$dur.transf.m[i]*Merg$cost.rusf[i] ,
      cost.drug.transf.m = Merg$mu.m[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.transf.m = Merg$mu.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.transf.m[i]/7,
      cost.nonlabor.transf.m = Merg$mu.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.transf.m[i]/7,
      cost.caregiver.transf.m = Merg$mu.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.transf.m[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * 
        ((Merg$dur.rec.m[i]+Merg$hosp.stay.m[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * (Merg$dur.rec.m[i]+Merg$hosp.stay.m[i])*Merg$cost.rusf[i] ,
      
      cost.labor.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.rec.m[i]/7,
      
      cost.nonlabor.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m[i]/7,
      
      cost.caregiver.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m[i]/7,
      
      cost.hospitalization.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * pop.m * 
        ((Merg$dur.died.m[i]+Merg$hosp.stay.m[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * (Merg$dur.died.m[i]+Merg$hosp.stay.m[i]) *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.died.m[i]/7,
      
      cost.nonlabor.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m[i]/7,
      
      cost.caregiver.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m[i]/7,
      
      cost.hospitalization.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * pop.m * 
        ((Merg$dur.never.rec.m[i]- Merg$dur.hosp.m[i])*Merg$cost.rusf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * 
        pop.m * (Merg$dur.never.rec.m[i]- Merg$dur.hosp.m[i]) *Merg$cost.rusf[i], 
      
      cost.labor.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * 
        pop.m * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.nonlabor.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * 
        pop.m * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.caregiver.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * 
        pop.m * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.hospitalization.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m.reg.sam = Merg$tau.m[i] * pop.m * 
        (Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m.to.sam = Merg$tau.m[i] * pop.m * Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] ,
      cost.drug.m.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.drug[i],
      cost.labor.m.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  Standard - MAM CSB
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      cost.rec.m.csb = Merg$alpha.m[i] * pop.m * 
        (Merg$dur.rec.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m[i]/7),
      
      cost.csb.rec.m = Merg$alpha.m[i] * pop.m * Merg$dur.rec.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### defaulted
      cost.def.m.csb = Merg$eps.m[i] * pop.m * 
        (Merg$dur.def.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m[i]/7),
      
      cost.csb.def.m = Merg$eps.m[i] * pop.m * Merg$dur.def.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### never recovered
      cost.never.rec.m.csb = Merg$eta.m[i] * pop.m * 
        (Merg$dur.never.rec.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m[i]/7),
      
      cost.csb.never.rec.m = Merg$eta.m[i] * pop.m * Merg$dur.never.rec.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### died
      cost.died.m.csb = Merg$theta.m[i] * pop.m * 
        (Merg$dur.died.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m[i]/7),
      
      cost.csb.died.m = Merg$theta.m[i] * pop.m * Merg$dur.died.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### hospitalized
      cost.hosp.m.csb = Merg$phi.m[i] * pop.m * 
        (Merg$dur.hosp.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m[i]/7),
      
      cost.csb.hosp.m = Merg$phi.m[i] * pop.m * Merg$dur.hosp.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### transferred
      cost.transf.m.csb = Merg$mu.m[i] * pop.m * 
        (Merg$dur.transf.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m[i]/7),
      
      cost.csb.transf.m = Merg$mu.m[i] * pop.m * Merg$dur.transf.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m.csb = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * 
        ((Merg$dur.rec.m[i]+Merg$hosp.stay.m[i])*Merg$cost.csb[i]/6 + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * 
        pop.m * (Merg$dur.rec.m[i]+Merg$hosp.stay.m[i])*Merg$cost.csb[i]/6 ,
      
      ##### died Post-hospitalization
      cost.hosp.died.m.csb = Merg$phi.m[i] * Merg$theta2.m[i] * pop.m * 
        ((Merg$dur.died.m[i]+Merg$hosp.stay.m[i])*Merg$cost.csb[i]/6 + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * (Merg$dur.died.m[i]+Merg$hosp.stay.m[i]) *Merg$cost.csb[i]/6 ,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m.csb = Merg$phi.m[i] * Merg$eta2.m[i] * pop.m * 
        ((Merg$dur.never.rec.m[i]- Merg$dur.hosp.m[i])*Merg$cost.csb[i]/6 +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * 
        pop.m * (Merg$dur.never.rec.m[i]- Merg$dur.hosp.m[i]) *Merg$cost.csb[i]/6, 
      
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m.reg.sam.csb = Merg$tau.m[i] * pop.m * 
        (Merg$dur.m.reg.sam[i]*Merg$cost.csb[i]/6 + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.csb.m.to.sam = Merg$tau.m[i] * pop.m * Merg$dur.m.reg.sam[i]*Merg$cost.csb[i]/6 ,
      
      
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      cost.rec.s.prms = Merg$alpha.s.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * 
        
        (Merg$rec.s.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s.prms[i]/7),
      
      cost.sachet.rec.s.prms = Merg$alpha.s.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$rec.s.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s.prms = Merg$alpha.s.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.rec.s.prms = Merg$alpha.s.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s.prms[i]/7,
      
      cost.nonlabor.rec.s.prms = Merg$alpha.s.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s.prms[i]/7,
      
      cost.caregiver.rec.s.prms = Merg$alpha.s.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s.prms[i]/7,
      
      ##### defaulted
      cost.def.s.prms = Merg$eps.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$def.s.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s.prms[i]/7),
      
      cost.sachet.def.s.prms = Merg$eps.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$def.s.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s.prms = Merg$eps.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.def.s.prms = Merg$eps.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s.prms[i]/7,
      
      cost.nonlabor.def.s.prms = Merg$eps.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s.prms[i]/7,
      
      cost.caregiver.def.s.prms = Merg$eps.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s.prms[i]/7,
      
      
      ##### never recovered
      cost.never.rec.s.prms = Merg$eta.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$never.rec.s.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s.prms[i]/7),
      
      cost.sachet.never.rec.s.prms = Merg$eta.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$never.rec.s.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s.prms = Merg$eta.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s.prms = Merg$eta.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s.prms[i]/7,
      
      cost.nonlabor.never.rec.s.prms = Merg$eta.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s.prms[i]/7,
      
      cost.caregiver.never.rec.s.prms = Merg$eta.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s.prms[i]/7,
      
      
      ##### died
      cost.died.s.prms = Merg$theta.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$died.s.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s.prms[i]/7),
      
      cost.sachet.died.s.prms = Merg$theta.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$died.s.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s.prms = Merg$theta.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.died.s.prms = Merg$theta.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s.prms[i]/7,
      
      cost.nonlabor.died.s.prms = Merg$theta.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s.prms[i]/7,
      
      cost.caregiver.died.s.prms = Merg$theta.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s.prms[i]/7,
      
      ##### hospitalized
      cost.hosp.s.prms = Merg$phi.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.s[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s[i]/7),
      
      cost.sachet.hosp.s.prms = Merg$phi.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.s[i]*Merg$cost.rutf[i] ,
      
      cost.drug.hosp.s.prms = Merg$phi.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.hosp.s.prms = Merg$phi.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s[i]/7,
      
      cost.nonlabor.hosp.s.prms = Merg$phi.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s[i]/7,
      
      cost.caregiver.hosp.s.prms = Merg$phi.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s[i]/7,
      
      
      ##### transferred
      cost.transf.s.prms = Merg$mu.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$transf.s[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s[i]/7),
      
      cost.sachet.transf.s.prms = Merg$mu.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$transf.s[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s.prms = Merg$mu.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s.prms = Merg$mu.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s[i]/7,
      
      cost.nonlabor.transf.s.prms = Merg$mu.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s[i]/7,
      
      cost.caregiver.transf.s.prms = Merg$mu.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s[i]/7,
      
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s.prms = Merg$phi.s[i] * Merg$alpha2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.rec.s.prms[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.rec.s.prms = Merg$phi.s[i] * Merg$alpha2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.rec.s.prms[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s.prms = Merg$phi.s[i] * Merg$alpha2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s.prms[i]/7,
      
      cost.nonlabor.hosp.rec.s.prms = Merg$phi.s[i] * Merg$alpha2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s.prms[i]/7,
      
      cost.caregiver.hosp.rec.s.prms = Merg$phi.s[i] * Merg$alpha2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s.prms[i]/7,
      
      cost.hospitalization.hosp.rec.s.prms = Merg$phi.s[i] * Merg$alpha2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s.prms = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.died.s.prms[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.died.s.prms = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.died.s.prms[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s.prms = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s.prms[i]/7,
      
      cost.nonlabor.hosp.died.s.prms = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s.prms[i]/7,
      
      cost.caregiver.hosp.died.s.prms = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s.prms[i]/7,
      
      cost.hospitalization.hosp.died.s.prms = Merg$phi.s[i] * Merg$theta2.s[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s.prms = Merg$phi.s[i] * Merg$eta2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.never.rec.s.prms[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s.prms[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7),
      
      cost.sachet.hosp.never.rec.s.prms = Merg$phi.s[i] * Merg$eta2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.never.rec.s.prms[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s.prms = Merg$phi.s[i] * Merg$eta2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s.prms[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.nonlabor.hosp.never.rec.s.prms = Merg$phi.s[i] * Merg$eta2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s.prms[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.caregiver.hosp.never.rec.s.prms = Merg$phi.s[i] * Merg$eta2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s.prms[i] - Merg$dur.hosp.s[i] - Merg$hosp.stay.s[i])/7,
      
      cost.hospitalization.hosp.never.rec.s.prms = Merg$phi.s[i] * Merg$eta2.s.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m.prmm = Merg$cov[i]*Merg$alpha.m.prmm[i] *  pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  *  
        (Merg$dur.rec.m.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prmm[i]/7),
      
      cost.sachet.rec.m.prmm = Merg$cov[i]*Merg$alpha.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.rec.m.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.rec.m.prmm = Merg$cov[i]*Merg$alpha.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m.prmm = Merg$cov[i]*Merg$alpha.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m.prmm[i]/7,
      cost.nonlabor.rec.m.prmm = Merg$cov[i]*Merg$alpha.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m.prmm[i]/7,
      cost.caregiver.rec.m.prmm = Merg$cov[i]*Merg$alpha.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m.prmm[i]/7,
      
      ##### defaulted
      cost.def.m.prmm = Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.def.m.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m.prmm[i]/7),
      
      cost.sachet.def.m.prmm = Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.def.m.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.def.m.prmm = Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m.prmm = Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m.prmm[i]/7,
      cost.nonlabor.def.m.prmm = Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m.prmm[i]/7,
      cost.caregiver.def.m.prmm = Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m.prmm[i]/7,
      
      ##### never recovered
      cost.never.rec.m.prmm = Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.never.rec.m.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m.prmm[i]/7),
      
      cost.sachet.never.rec.m.prmm = Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.never.rec.m.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.never.rec.m.prmm = Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m.prmm = Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m.prmm[i]/7,
      cost.nonlabor.never.rec.m.prmm = Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m.prmm[i]/7,
      cost.caregiver.never.rec.m.prmm = Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m.prmm[i]/7,
      
      ##### died
      cost.died.m.prmm = Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.died.m.prmm[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prmm[i]/7),
      
      cost.sachet.died.m.prmm = Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.died.m.prmm[i]*Merg$cost.rusf[i] ,
      cost.drug.died.m.prmm = Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m.prmm = Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m.prmm[i]/7,
      cost.nonlabor.died.m.prmm = Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m.prmm[i]/7,
      cost.caregiver.died.m.prmm = Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m.prmm[i]/7,
      
      ##### hospitalized
      cost.hosp.m.prmm = Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.hosp.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m[i]/7),
      
      cost.sachet.hosp.m.prmm = Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.hosp.m[i]*Merg$cost.rusf[i] ,
      cost.drug.hosp.m.prmm = Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m.prmm = Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m[i]/7,
      cost.nonlabor.hosp.m.prmm = Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m[i]/7,
      cost.caregiver.hosp.m.prmm = Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m[i]/7,
      
      ##### transferred
      cost.transf.m.prmm = Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.transf.m[i]*Merg$cost.rusf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m[i]/7),
      
      cost.sachet.transf.m.prmm = Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.transf.m[i]*Merg$cost.rusf[i] ,
      cost.drug.transf.m.prmm = Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m.prmm = Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m[i]/7,
      cost.nonlabor.transf.m.prmm = Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m[i]/7,
      cost.caregiver.transf.m.prmm = Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        ((Merg$dur.rec.m.prmm[i]+Merg$hosp.stay.m[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * (Merg$dur.rec.m.prmm[i]+Merg$hosp.stay.m[i]) *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m.prmm[i]/7,
      
      cost.nonlabor.hosp.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m.prmm[i]/7,
      
      cost.caregiver.hosp.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m.prmm[i]/7,
      
      cost.hospitalization.hosp.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * 
        ((Merg$dur.died.m.prmm[i]+Merg$hosp.stay.m[i])*Merg$cost.rusf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.died.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * (Merg$dur.died.m.prmm[i]+Merg$hosp.stay.m[i])  *Merg$cost.rusf[i] ,
      
      cost.labor.hosp.died.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m.prmm[i]/7,
      
      cost.nonlabor.hosp.died.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m.prmm[i]/7,
      
      cost.caregiver.hosp.died.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m.prmm[i]/7,
      
      cost.hospitalization.hosp.died.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * 
        ((Merg$dur.never.rec.m.prmm[i]- Merg$dur.hosp.m[i]) *Merg$cost.rusf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m.prmm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.sachet.hosp.never.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   *  (Merg$dur.never.rec.m.prmm[i]- Merg$dur.hosp.m[i]) *Merg$cost.rusf[i], 
      
      cost.labor.hosp.never.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m.prmm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.nonlabor.hosp.never.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m.prmm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.caregiver.hosp.never.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m.prmm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7,
      
      cost.hospitalization.hosp.never.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7,
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m.reg.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.rusf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]*Merg$cost.rusf[i] * Merg$dur.m.reg.sam[i],
      cost.drug.m.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      ## ## ## 
      ## ## ## POST-MAM to MAM CSB
      ## ## ## 
      
      ##### recovered
      cost.rec.m.prmm.csb = Merg$cov[i]*Merg$alpha.m.prmm[i] *  pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  *  
        (Merg$dur.rec.m.prmm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prmm[i]/7),
      
      cost.csb.rec.m.prmm = Merg$cov[i]*Merg$alpha.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.rec.m.prmm[i]*Merg$cost.csb[i]/6 ,
      
      ##### defaulted
      cost.def.m.prmm.csb= Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.def.m.prmm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m.prmm[i]/7),
      
      cost.csb.def.m.prmm = Merg$cov[i]*Merg$eps.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.def.m.prmm[i]*Merg$cost.csb[i]/6 ,
      
      ##### never recovered
      cost.never.rec.m.prmm.csb= Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.never.rec.m.prmm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m.prmm[i]/7),
      
      cost.csb.never.rec.m.prmm = Merg$cov[i]*Merg$eta.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.never.rec.m.prmm[i]*Merg$cost.csb[i]/6 ,
      
      ##### died
      cost.died.m.prmm.csb= Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.died.m.prmm[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prmm[i]/7),
      
      cost.csb.died.m.prmm = Merg$cov[i]*Merg$theta.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.died.m.prmm[i]*Merg$cost.csb[i]/6 ,
      
      ##### hospitalized
      cost.hosp.m.prmm.csb= Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.hosp.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m[i]/7),
      
      cost.csb.hosp.m.prmm = Merg$cov[i]*Merg$phi.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.hosp.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### transferred
      cost.transf.m.prmm.csb= Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        (Merg$dur.transf.m[i]*Merg$cost.csb[i]/6 + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m[i]/7),
      
      cost.csb.transf.m.prmm = Merg$cov[i]*Merg$mu.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * Merg$dur.transf.m[i]*Merg$cost.csb[i]/6 ,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m.prmm.csb= Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  * 
        ((Merg$dur.rec.m.prmm[i]+Merg$hosp.stay.m[i])*Merg$cost.csb[i]/6 + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * (Merg$dur.rec.m.prmm[i]+Merg$hosp.stay.m[i]) *Merg$cost.csb[i]/6 ,
      
      ##### died Post-hospitalization
      cost.hosp.died.m.prmm.csb= Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * 
        ((Merg$dur.died.m.prmm[i]+Merg$hosp.stay.m[i])*Merg$cost.csb[i]/6 + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.died.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$theta2.m[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * (Merg$dur.died.m.prmm[i]+Merg$hosp.stay.m[i])  *Merg$cost.csb[i]/6 ,
      
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m.prmm.csb= Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   * 
        ((Merg$dur.never.rec.m.prmm[i]- Merg$dur.hosp.m[i]) *Merg$cost.csb[i]/6 +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m.prmm[i] - Merg$dur.hosp.m[i] - Merg$hosp.stay.m[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m[i]/7),
      
      cost.csb.hosp.never.rec.m.prmm = Merg$cov[i]*Merg$phi.m[i] * Merg$eta2.m.prmm[i] * 
        pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]   *  (Merg$dur.never.rec.m.prmm[i]- Merg$dur.hosp.m[i]) *Merg$cost.csb[i]/6, 
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m.reg.sam.prmm.csb= Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.csb[i]/6 + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.csb.m.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]*Merg$cost.csb[i]/6 * Merg$dur.m.reg.sam[i],
      
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  SAM ComPAS
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      ##### recovered
      cost.rec.s2 = Merg$alpha.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$rec.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s2[i]/7),
      
      cost.sachet.rec.s2 = Merg$alpha.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$rec.s2[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.s2 = Merg$alpha.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.rec.s2 = Merg$alpha.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s2[i]/7,
      cost.nonlabor.rec.s2 = Merg$alpha.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s2[i]/7,
      cost.caregiver.rec.s2 = Merg$alpha.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s2[i]/7,
      
      ##### defaulted
      cost.def.s2 = Merg$eps.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$def.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s2[i]/7),
      
      cost.sachet.def.s2 = Merg$eps.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$def.s2[i]*Merg$cost.rutf[i] ,
      cost.drug.def.s2 = Merg$eps.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.def.s2 = Merg$eps.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s2[i]/7,
      cost.nonlabor.def.s2 = Merg$eps.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s2[i]/7,
      cost.caregiver.def.s2 = Merg$eps.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s2[i]/7,
      
      ##### never recovered
      cost.never.rec.s2 = Merg$eta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$never.rec.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s2[i]/7),
      
      cost.sachet.never.rec.s2 = Merg$eta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$never.rec.s2[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.s2 = Merg$eta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.never.rec.s2 = Merg$eta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s2[i]/7,
      cost.nonlabor.never.rec.s2 = Merg$eta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s2[i]/7,
      cost.caregiver.never.rec.s2 = Merg$eta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s2[i]/7,
      
      ##### died
      cost.died.s2 = Merg$theta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$died.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s2[i]/7),
      
      cost.sachet.died.s2 = Merg$theta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$died.s2[i]*Merg$cost.rutf[i] ,
      cost.drug.died.s2 = Merg$theta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.died.s2 = Merg$theta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s2[i]/7,
      cost.nonlabor.died.s2 = Merg$theta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s2[i]/7,
      cost.caregiver.died.s2 = Merg$theta.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s2[i]/7,
      
      ##### hospitalized
      cost.hosp.s2 = Merg$phi.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s2[i]/7),
      
      cost.sachet.hosp.s2 = Merg$phi.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.s2[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.s2 = Merg$phi.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.hosp.s2 = Merg$phi.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s2[i]/7,
      cost.nonlabor.hosp.s2 = Merg$phi.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s2[i]/7,
      cost.caregiver.hosp.s2 = Merg$phi.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s2[i]/7,
      
      ##### transferred
      cost.transf.s2 = Merg$mu.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$transf.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s2[i]/7),
      
      cost.sachet.transf.s2 = Merg$mu.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$transf.s2[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.s2 = Merg$mu.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.drug1[i],
      cost.labor.transf.s2 = Merg$mu.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s2[i]/7,
      cost.nonlabor.transf.s2 = Merg$mu.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s2[i]/7,
      cost.caregiver.transf.s2 = Merg$mu.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s2[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s2 = Merg$phi.s2[i] * Merg$alpha2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.rec.s2[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s2[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.rec.s2 = Merg$phi.s2[i] * Merg$alpha2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.rec.s2[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s2 = Merg$phi.s2[i] * Merg$alpha2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s2[i]/7,
      
      cost.nonlabor.hosp.rec.s2 = Merg$phi.s2[i] * Merg$alpha2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s2[i]/7,
      
      cost.caregiver.hosp.rec.s2 = Merg$phi.s2[i] * Merg$alpha2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s2[i]/7,
      
      cost.hospitalization.hosp.rec.s2 = Merg$phi.s2[i] * Merg$alpha2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s2 = Merg$phi.s2[i] * Merg$theta2.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.died.s2[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s2[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.died.s2 = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) *Merg$hosp.died.s2[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.s2 = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s2[i]/7,
      
      cost.nonlabor.hosp.died.s2 = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s2[i]/7,
      
      cost.caregiver.hosp.died.s2 = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s2[i]/7,
      
      cost.hospitalization.hosp.died.s2 = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s2 = Merg$phi.s2[i] * Merg$eta2.s2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * 
        (Merg$hosp.never.rec.s2[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s2[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.never.rec.s2 = Merg$phi.s2[i] * Merg$eta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$hosp.never.rec.s2[i]*Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.s2 = Merg$phi.s2[i] * Merg$eta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s2[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.nonlabor.hosp.never.rec.s2 = Merg$phi.s2[i] * Merg$eta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s2[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.caregiver.hosp.never.rec.s2 = Merg$phi.s2[i] * Merg$eta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s2[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.hospitalization.hosp.never.rec.s2 = Merg$phi.s2[i] * Merg$eta2.s2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      ##### 
      ##### Relapse Post SAM Treatment
      ##### 
      
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      ### MAM children who regressed to SAM during the treatment, receive treatment for SAM, both in original treatment period, as well as treatment post relapse)
      
      ##### recovered
      cost.rec.s2.prss = Merg$alpha.s2.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * 
        (Merg$rec.s2.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s2.prss[i]/7),
      
      cost.sachet.rec.s2.prss = Merg$alpha.s2.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$rec.s2.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s2.prss = Merg$alpha.s2.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.rec.s2.prss = Merg$alpha.s2.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$cost.labor[i]*Merg$dur.rec.s2.prss[i]/7,
      
      cost.nonlabor.rec.s2.prss = Merg$alpha.s2.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.rec.s2.prss[i]/7,
      
      cost.caregiver.rec.s2.prss = Merg$alpha.s2.prss[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$cost.caregiver[i]*Merg$dur.rec.s2.prss[i]/7,
      
      ##### defaulted
      cost.def.s2.prss = Merg$eps.s2[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *
        (Merg$def.s2.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s2.prss[i]/7),
      
      cost.sachet.def.s2.prss = Merg$eps.s2[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$def.s2.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s2.prss = Merg$eps.s2[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$cost.drug1[i],
      
      cost.labor.def.s2.prss = Merg$eps.s2[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])  * Merg$cost.labor[i]*Merg$dur.def.s2.prss[i]/7,
      
      cost.nonlabor.def.s2.prss = Merg$eps.s2[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$cost.nonlabor[i]*Merg$dur.def.s2.prss[i]/7,
      
      cost.caregiver.def.s2.prss = Merg$eps.s2[i] *  
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s2.prss[i]/7,
      
      ##### never recovered
      cost.never.rec.s2.prss = Merg$eta.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *
        (Merg$never.rec.s2.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s2.prss[i]/7),
      
      cost.sachet.never.rec.s2.prss = Merg$eta.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$never.rec.s2.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s2.prss =Merg$eta.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s2.prss = Merg$eta.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s2.prss[i]/7,
      
      cost.nonlabor.never.rec.s2.prss = Merg$eta.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s2.prss[i]/7,
      
      cost.caregiver.never.rec.s2.prss = Merg$eta.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s2.prss[i]/7,
      
      ##### died
      cost.died.s2.prss = Merg$theta.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *
        
        (Merg$died.s2.prss[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s2.prss[i]/7),
      
      cost.sachet.died.s2.prss = Merg$theta.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$died.s2.prss[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s2.prss = Merg$theta.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])  * Merg$cost.drug1[i],
      
      cost.labor.died.s2.prss = Merg$theta.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s2.prss[i]/7,
      
      cost.nonlabor.died.s2.prss = Merg$theta.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s2.prss[i]/7,
      
      cost.caregiver.died.s2.prss = Merg$theta.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s2.prss[i]/7,
      
      ##### hospitalized
      cost.hosp.s2.prss = Merg$phi.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * 
        
        (Merg$hosp.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s2[i]/7),
      
      cost.sachet.hosp.s2.prss = Merg$phi.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *  Merg$hosp.s2[i]*Merg$cost.rutf[i] ,
      
      cost.drug.hosp.s2.prss = Merg$phi.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *  Merg$cost.drug1[i],
      
      cost.labor.hosp.s2.prss = Merg$phi.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *  Merg$cost.labor[i]*Merg$dur.hosp.s2[i]/7,
      
      cost.nonlabor.hosp.s2.prss = Merg$phi.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *  Merg$cost.nonlabor[i]*Merg$dur.hosp.s2[i]/7,
      
      cost.caregiver.hosp.s2.prss = Merg$phi.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *  Merg$cost.caregiver[i]*Merg$dur.hosp.s2[i]/7,
      
      ##### transferred
      cost.transf.s2.prss = Merg$mu.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * 
        
        (Merg$transf.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s2[i]/7),
      
      cost.sachet.transf.s2.prss = Merg$mu.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$transf.s2[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s2.prss = Merg$mu.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s2.prss = Merg$mu.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.transf.s2[i]/7,
      
      cost.nonlabor.transf.s2.prss = Merg$mu.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s2[i]/7,
      
      cost.caregiver.transf.s2.prss = Merg$mu.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s2[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s2.prss = Merg$phi.s2[i] * Merg$alpha2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *
        
        (Merg$hosp.rec.s2.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s2.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.rec.s2.prss = Merg$phi.s2[i] * Merg$alpha2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i])   * Merg$hosp.rec.s2.prss[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s2.prss = Merg$phi.s2[i] * Merg$alpha2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.rec.s2.prss[i]/7,
      
      cost.nonlabor.hosp.rec.s2.prss = Merg$phi.s2[i] * Merg$alpha2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s2.prss[i]/7,
      
      cost.caregiver.hosp.rec.s2.prss = Merg$phi.s2[i] * Merg$alpha2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s2.prss[i]/7,
      
      cost.hospitalization.hosp.rec.s2.prss = Merg$phi.s2[i] * Merg$alpha2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s2.prss = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *
        
        (Merg$died.s2.prss[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s2.prss[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.died.s2.prss = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$died.s2.prss[i]*Merg$cost.rutf[i],
      
      cost.labor.hosp.died.s2.prss = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.labor[i]*Merg$dur.died.s2.prss[i]/7,
      
      cost.nonlabor.hosp.died.s2.prss = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s2.prss[i]/7,
      
      cost.caregiver.hosp.died.s2.prss = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s2.prss[i]/7,
      
      cost.hospitalization.hosp.died.s2.prss = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s2.prss = Merg$phi.s2[i] * Merg$eta2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) *
        
        (Merg$never.rec.s2.prss[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s2.prss[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.never.rec.s2.prss = Merg$phi.s2[i] * Merg$eta2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$never.rec.s2.prss[i]*Merg$cost.rutf[i] , 
      
      cost.labor.hosp.never.rec.s2.prss = Merg$phi.s2[i] * Merg$eta2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s2.prss[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.nonlabor.hosp.never.rec.s2.prss = Merg$phi.s2[i] * Merg$eta2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s2.prss[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.caregiver.hosp.never.rec.s2.prss = Merg$phi.s2[i] * Merg$eta2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s2.prss[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.hospitalization.hosp.never.rec.s2.prss = Merg$phi.s2[i] * Merg$eta2.s2.prss[i] * 
        (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
           Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m2.prsm = Merg$cov[i]*Merg$alpha.m2.prsm[i] *   (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  *  
        (Merg$dur.rec.m2.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m2.prsm[i]/7),
      
      cost.sachet.rec.m2.prsm = Merg$cov[i]*Merg$alpha.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$dur.rec.m2.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.m2.prsm = Merg$cov[i]*Merg$alpha.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m2.prsm = Merg$cov[i]*Merg$alpha.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m2.prsm[i]/7,
      cost.nonlabor.rec.m2.prsm = Merg$cov[i]*Merg$alpha.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m2.prsm[i]/7,
      cost.caregiver.rec.m2.prsm = Merg$cov[i]*Merg$alpha.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m2.prsm[i]/7,
      
      ##### defaulted
      cost.def.m2.prsm = Merg$cov[i]*Merg$eps.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * 
        (Merg$dur.def.m2.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m2.prsm[i]/7),
      
      cost.sachet.def.m2.prsm = Merg$cov[i]*Merg$eps.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$dur.def.m2.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.def.m2.prsm = Merg$cov[i]*Merg$eps.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m2.prsm = Merg$cov[i]*Merg$eps.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m2.prsm[i]/7,
      cost.nonlabor.def.m2.prsm = Merg$cov[i]*Merg$eps.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m2.prsm[i]/7,
      cost.caregiver.def.m2.prsm = Merg$cov[i]*Merg$eps.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m2.prsm[i]/7,
      
      ##### never recovered
      cost.never.rec.m2.prsm = Merg$cov[i]*Merg$eta.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * 
        (Merg$dur.never.rec.m2.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m2.prsm[i]/7),
      
      cost.sachet.never.rec.m2.prsm = Merg$cov[i]*Merg$eta.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$dur.never.rec.m2.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.m2.prsm = Merg$cov[i]*Merg$eta.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m2.prsm = Merg$cov[i]*Merg$eta.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m2.prsm[i]/7,
      cost.nonlabor.never.rec.m2.prsm = Merg$cov[i]*Merg$eta.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m2.prsm[i]/7,
      cost.caregiver.never.rec.m2.prsm = Merg$cov[i]*Merg$eta.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m2.prsm[i]/7,
      
      ##### died
      cost.died.m2.prsm = Merg$cov[i]*Merg$theta.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * 
        (Merg$dur.died.m2.prsm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m2.prsm[i]/7),
      
      cost.sachet.died.m2.prsm = Merg$cov[i]*Merg$theta.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$dur.died.m2.prsm[i]*Merg$cost.rutf[i] ,
      cost.drug.died.m2.prsm = Merg$cov[i]*Merg$theta.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m2.prsm = Merg$cov[i]*Merg$theta.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m2.prsm[i]/7,
      cost.nonlabor.died.m2.prsm = Merg$cov[i]*Merg$theta.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m2.prsm[i]/7,
      cost.caregiver.died.m2.prsm = Merg$cov[i]*Merg$theta.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m2.prsm[i]/7,
      
      ##### hospitalized
      cost.hosp.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * 
        (Merg$dur.hosp.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m2[i]/7),
      
      cost.sachet.hosp.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$dur.hosp.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m2[i]/7,
      cost.nonlabor.hosp.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m2[i]/7,
      cost.caregiver.hosp.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m2[i]/7,
      
      ##### transferred
      cost.transf.m2.prsm = Merg$cov[i]*Merg$mu.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * 
        (Merg$dur.transf.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m2[i]/7),
      
      cost.sachet.transf.m2.prsm = Merg$cov[i]*Merg$mu.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$dur.transf.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.m2.prsm = Merg$cov[i]*Merg$mu.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m2.prsm = Merg$cov[i]*Merg$mu.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m2[i]/7,
      cost.nonlabor.transf.m2.prsm = Merg$cov[i]*Merg$mu.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m2[i]/7,
      cost.caregiver.transf.m2.prsm = Merg$cov[i]*Merg$mu.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m2[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  * 
        ((Merg$dur.rec.m2.prsm[i]+Merg$hosp.stay.m2[i])*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m2.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * (Merg$dur.rec.m2.prsm[i]+Merg$hosp.stay.m2[i]) *Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m2.prsm[i]/7,
      
      cost.nonlabor.hosp.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m2.prsm[i]/7,
      
      cost.caregiver.hosp.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m2.prsm[i]/7,
      
      cost.hospitalization.hosp.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * 
        ((Merg$dur.died.m2.prsm[i]+Merg$hosp.stay.m2[i])*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m2.prsm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.died.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * (Merg$dur.died.m2.prsm[i]+Merg$hosp.stay.m2[i])  *Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m2.prsm[i]/7,
      
      cost.nonlabor.hosp.died.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m2.prsm[i]/7,
      
      cost.caregiver.hosp.died.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m2.prsm[i]/7,
      
      cost.hospitalization.hosp.died.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prsm[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * 
        ((Merg$dur.never.rec.m2.prsm[i]- Merg$dur.hosp.m2[i]) *Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m2.prsm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.never.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   *  (Merg$dur.never.rec.m2.prsm[i]- Merg$dur.hosp.m2[i]) *Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m2.prsm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.nonlabor.hosp.never.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m2.prsm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.caregiver.hosp.never.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m2.prsm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.hospitalization.hosp.never.rec.m2.prsm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prsm[i] * 
        (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m2.reg.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m2.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]*Merg$cost.rutf[i] * Merg$dur.m.reg.sam[i],
      cost.drug.m2.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m2.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m2.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m2.to.sam.prsm = Merg$cov[i] * Merg$tau.m[i] *  (pop + pop.m*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  ComPAS - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      cost.rec.m2 = Merg$alpha.m2[i] * pop.m * 
        (Merg$dur.rec.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m2[i]/7),
      
      cost.sachet.rec.m2 = Merg$alpha.m2[i] * pop.m * Merg$dur.rec.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.m2 = Merg$alpha.m2[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.rec.m2 = Merg$alpha.m2[i] * pop.m * Merg$cost.labor[i]*Merg$dur.rec.m2[i]/7,
      cost.nonlabor.rec.m2 = Merg$alpha.m2[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m2[i]/7,
      cost.caregiver.rec.m2 = Merg$alpha.m2[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m2[i]/7,
      
      ##### defaulted
      cost.def.m2 = Merg$eps.m2[i] * pop.m * 
        (Merg$dur.def.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m2[i]/7),
      
      cost.sachet.def.m2 = Merg$eps.m2[i] * pop.m * Merg$dur.def.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.def.m2 = Merg$eps.m2[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.def.m2 = Merg$eps.m2[i] * pop.m * Merg$cost.labor[i]*Merg$dur.def.m2[i]/7,
      cost.nonlabor.def.m2 = Merg$eps.m2[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.def.m2[i]/7,
      cost.caregiver.def.m2 = Merg$eps.m2[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.def.m2[i]/7,
      
      ##### never recovered
      cost.never.rec.m2 = Merg$eta.m2[i] * pop.m * 
        (Merg$dur.never.rec.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m2[i]/7),
      
      cost.sachet.never.rec.m2 = Merg$eta.m2[i] * pop.m * Merg$dur.never.rec.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.m2 = Merg$eta.m2[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.never.rec.m2 = Merg$eta.m2[i] * pop.m * Merg$cost.labor[i]*Merg$dur.never.rec.m2[i]/7,
      cost.nonlabor.never.rec.m2 = Merg$eta.m2[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m2[i]/7,
      cost.caregiver.never.rec.m2 = Merg$eta.m2[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.never.rec.m2[i]/7,
      
      ##### died
      cost.died.m2 = Merg$theta.m2[i] * pop.m * 
        (Merg$dur.died.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m2[i]/7),
      
      cost.sachet.died.m2 = Merg$theta.m2[i] * pop.m * Merg$dur.died.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.died.m2 = Merg$theta.m2[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.died.m2 = Merg$theta.m2[i] * pop.m * Merg$cost.labor[i]*Merg$dur.died.m2[i]/7,
      cost.nonlabor.died.m2 = Merg$theta.m2[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m2[i]/7,
      cost.caregiver.died.m2 = Merg$theta.m2[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m2[i]/7,
      
      ##### hospitalized
      cost.hosp.m2 = Merg$phi.m2[i] * pop.m * 
        (Merg$dur.hosp.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m2[i]/7),
      
      cost.sachet.hosp.m2 = Merg$phi.m2[i] * pop.m * Merg$dur.hosp.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.m2 = Merg$phi.m2[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.hosp.m2 = Merg$phi.m2[i] * pop.m * Merg$cost.labor[i]*Merg$dur.hosp.m2[i]/7,
      cost.nonlabor.hosp.m2 = Merg$phi.m2[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.hosp.m2[i]/7,
      cost.caregiver.hosp.m2 = Merg$phi.m2[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.hosp.m2[i]/7,
      
      ##### transferred
      cost.transf.m2 = Merg$mu.m2[i] * pop.m * 
        (Merg$dur.transf.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m2[i]/7),
      
      cost.sachet.transf.m2 = Merg$mu.m2[i] * pop.m * Merg$dur.transf.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.m2 = Merg$mu.m2[i] * pop.m * Merg$cost.drug1[i],
      cost.labor.transf.m2 = Merg$mu.m2[i] * pop.m * Merg$cost.labor[i]*Merg$dur.transf.m2[i]/7,
      cost.nonlabor.transf.m2 = Merg$mu.m2[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.transf.m2[i]/7,
      cost.caregiver.transf.m2 = Merg$mu.m2[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.transf.m2[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m2 = Merg$phi.m2[i] * Merg$alpha2.m2[i] * 
        pop.m * 
        ((Merg$dur.rec.m2[i]+Merg$hosp.stay.m2[i])*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m2[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.rec.m2 = Merg$phi.m2[i] * Merg$alpha2.m2[i] * 
        pop.m * (Merg$dur.rec.m2[i]+Merg$hosp.stay.m2[i])*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.m2 = Merg$phi.m2[i] * Merg$alpha2.m2[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.rec.m2[i]/7,
      
      cost.nonlabor.hosp.rec.m2 = Merg$phi.m2[i] * Merg$alpha2.m2[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.rec.m2[i]/7,
      
      cost.caregiver.hosp.rec.m2 = Merg$phi.m2[i] * Merg$alpha2.m2[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.rec.m2[i]/7,
      
      cost.hospitalization.hosp.rec.m2 = Merg$phi.m2[i] * Merg$alpha2.m2[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m2 = Merg$phi.m2[i] * Merg$theta2.m2[i] * pop.m * 
        ((Merg$dur.died.m2[i]+Merg$hosp.stay.m2[i])*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m2[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.died.m2 = Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * (Merg$dur.died.m2[i]+Merg$hosp.stay.m2[i]) *Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.m2 = Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$cost.labor[i]*Merg$dur.died.m2[i]/7,
      
      cost.nonlabor.hosp.died.m2 = Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$cost.nonlabor[i]*Merg$dur.died.m2[i]/7,
      
      cost.caregiver.hosp.died.m2 = Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$cost.caregiver[i]*Merg$dur.died.m2[i]/7,
      
      cost.hospitalization.hosp.died.m2 = Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m2 = Merg$phi.m2[i] * Merg$eta2.m2[i] * pop.m * 
        ((Merg$dur.never.rec.m2[i]- Merg$dur.hosp.m2[i])*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m2[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.never.rec.m2 = Merg$phi.m2[i] * Merg$eta2.m2[i] * 
        pop.m * (Merg$dur.never.rec.m2[i]- Merg$dur.hosp.m2[i]) *Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.m2 = Merg$phi.m2[i] * Merg$eta2.m2[i] * 
        pop.m * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m2[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.nonlabor.hosp.never.rec.m2 = Merg$phi.m2[i] * Merg$eta2.m2[i] * 
        pop.m * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m2[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.caregiver.hosp.never.rec.m2 = Merg$phi.m2[i] * Merg$eta2.m2[i] * 
        pop.m * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m2[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.hospitalization.hosp.never.rec.m2 = Merg$phi.m2[i] * Merg$eta2.m2[i] * 
        pop.m * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m2.reg.sam = Merg$tau.m[i] * pop.m * 
        (Merg$dur.m.reg.sam[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m2.to.sam = Merg$tau.m[i] * pop.m * Merg$dur.m.reg.sam[i]*Merg$cost.rutf[i] ,
      cost.drug.m2.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.drug[i],
      cost.labor.m2.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m2.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m2.to.sam = Merg$tau.m[i] * pop.m * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      cost.rec.s2.prms = Merg$alpha.s2.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * 
        
        (Merg$rec.s2.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s2.prms[i]/7),
      
      cost.sachet.rec.s2.prms = Merg$alpha.s2.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$rec.s2.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.rec.s2.prms = Merg$alpha.s2.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.rec.s2.prms = Merg$alpha.s2.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s2.prms[i]/7,
      
      cost.nonlabor.rec.s2.prms = Merg$alpha.s2.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s2.prms[i]/7,
      
      cost.caregiver.rec.s2.prms = Merg$alpha.s2.prms[i] *  
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s2.prms[i]/7,
      
      ##### defaulted
      cost.def.s2.prms = Merg$eps.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$def.s2.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.s2.prms[i]/7),
      
      cost.sachet.def.s2.prms = Merg$eps.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$def.s2.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.def.s2.prms = Merg$eps.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.def.s2.prms = Merg$eps.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.def.s2.prms[i]/7,
      
      cost.nonlabor.def.s2.prms = Merg$eps.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.def.s2.prms[i]/7,
      
      cost.caregiver.def.s2.prms = Merg$eps.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.def.s2.prms[i]/7,
      
      
      ##### never recovered
      cost.never.rec.s2.prms = Merg$eta.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$never.rec.s2.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.s2.prms[i]/7),
      
      cost.sachet.never.rec.s2.prms = Merg$eta.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$never.rec.s2.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.never.rec.s2.prms = Merg$eta.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.never.rec.s2.prms = Merg$eta.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.never.rec.s2.prms[i]/7,
      
      cost.nonlabor.never.rec.s2.prms = Merg$eta.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.never.rec.s2.prms[i]/7,
      
      cost.caregiver.never.rec.s2.prms = Merg$eta.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.never.rec.s2.prms[i]/7,
      
      
      ##### died
      cost.died.s2.prms = Merg$theta.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$died.s2.prms[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s2.prms[i]/7),
      
      cost.sachet.died.s2.prms = Merg$theta.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$died.s2.prms[i]*Merg$cost.rutf[i] ,
      
      cost.drug.died.s2.prms = Merg$theta.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.died.s2.prms = Merg$theta.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s2.prms[i]/7,
      
      cost.nonlabor.died.s2.prms = Merg$theta.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s2.prms[i]/7,
      
      cost.caregiver.died.s2.prms = Merg$theta.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s2.prms[i]/7,
      
      ##### hospitalized
      cost.hosp.s2.prms = Merg$phi.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.s2[i]/7),
      
      cost.sachet.hosp.s2.prms = Merg$phi.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.s2[i]*Merg$cost.rutf[i],
      
      cost.drug.hosp.s2.prms = Merg$phi.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.hosp.s2.prms = Merg$phi.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.hosp.s2[i]/7,
      
      cost.nonlabor.hosp.s2.prms = Merg$phi.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.hosp.s2[i]/7,
      
      cost.caregiver.hosp.s2.prms = Merg$phi.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.hosp.s2[i]/7,
      
      
      ##### transferred
      cost.transf.s2.prms = Merg$mu.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$transf.s2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.s2[i]/7),
      
      cost.sachet.transf.s2.prms = Merg$mu.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$transf.s2[i]*Merg$cost.rutf[i] ,
      
      cost.drug.transf.s2.prms = Merg$mu.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.drug1[i],
      
      cost.labor.transf.s2.prms = Merg$mu.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.transf.s2[i]/7,
      
      cost.nonlabor.transf.s2.prms = Merg$mu.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.transf.s2[i]/7,
      
      cost.caregiver.transf.s2.prms = Merg$mu.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.transf.s2[i]/7,
      
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.s2.prms = Merg$phi.s2[i] * Merg$alpha2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.rec.s2.prms[i]*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.s2.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.rec.s2.prms = Merg$phi.s2[i] * Merg$alpha2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.rec.s2.prms[i]*Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.s2.prms = Merg$phi.s2[i] * Merg$alpha2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.rec.s2.prms[i]/7,
      
      cost.nonlabor.hosp.rec.s2.prms = Merg$phi.s2[i] * Merg$alpha2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.rec.s2.prms[i]/7,
      
      cost.caregiver.hosp.rec.s2.prms = Merg$phi.s2[i] * Merg$alpha2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.rec.s2.prms[i]/7,
      
      cost.hospitalization.hosp.rec.s2.prms = Merg$phi.s2[i] * Merg$alpha2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.s2.prms = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.died.s2.prms[i]*Merg$cost.rutf[i]+ 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.s2.prms[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.died.s2.prms = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$hosp.died.s2.prms[i]*Merg$cost.rutf[i],
      
      cost.labor.hosp.died.s2.prms = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i]*Merg$dur.died.s2.prms[i]/7,
      
      cost.nonlabor.hosp.died.s2.prms = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i]*Merg$dur.died.s2.prms[i]/7,
      
      cost.caregiver.hosp.died.s2.prms = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*Merg$dur.died.s2.prms[i]/7,
      
      cost.hospitalization.hosp.died.s2.prms = Merg$phi.s2[i] * Merg$theta2.s2[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.s2.prms = Merg$phi.s2[i] * Merg$eta2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *
        
        (Merg$hosp.never.rec.s2.prms[i]*Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.s2.prms[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7),
      
      cost.sachet.hosp.never.rec.s2.prms = Merg$phi.s2[i] * Merg$eta2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) *Merg$hosp.never.rec.s2.prms[i]*Merg$cost.rutf[i] , 
      
      cost.labor.hosp.never.rec.s2.prms = Merg$phi.s2[i] * Merg$eta2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.labor[i] *
        (Merg$dur.never.rec.s2.prms[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.nonlabor.hosp.never.rec.s2.prms = Merg$phi.s2[i] * Merg$eta2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.s2.prms[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.caregiver.hosp.never.rec.s2.prms = Merg$phi.s2[i] * Merg$eta2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.s2.prms[i] - Merg$dur.hosp.s2[i] - Merg$hosp.stay.s2[i])/7,
      
      cost.hospitalization.hosp.never.rec.s2.prms = Merg$phi.s2[i] * Merg$eta2.s2.prms[i] * 
        (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
           Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]) * Merg$cost.hosp[i]*Merg$hosp.stay.s2[i]/7,
      
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      cost.rec.m2.prmm = Merg$cov[i]*Merg$alpha.m2.prmm[i] *  pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  *  
        (Merg$dur.rec.m2.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m2.prmm[i]/7),
      
      cost.sachet.rec.m2.prmm = Merg$cov[i]*Merg$alpha.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$dur.rec.m2.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.rec.m2.prmm = Merg$cov[i]*Merg$alpha.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.rec.m2.prmm = Merg$cov[i]*Merg$alpha.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.rec.m2.prmm[i]/7,
      cost.nonlabor.rec.m2.prmm = Merg$cov[i]*Merg$alpha.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.rec.m2.prmm[i]/7,
      cost.caregiver.rec.m2.prmm = Merg$cov[i]*Merg$alpha.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.rec.m2.prmm[i]/7,
      
      ##### defaulted
      cost.def.m2.prmm = Merg$cov[i]*Merg$eps.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * 
        (Merg$dur.def.m2.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.def.m2.prmm[i]/7),
      
      cost.sachet.def.m2.prmm = Merg$cov[i]*Merg$eps.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$dur.def.m2.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.def.m2.prmm = Merg$cov[i]*Merg$eps.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.def.m2.prmm = Merg$cov[i]*Merg$eps.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.def.m2.prmm[i]/7,
      cost.nonlabor.def.m2.prmm = Merg$cov[i]*Merg$eps.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.def.m2.prmm[i]/7,
      cost.caregiver.def.m2.prmm = Merg$cov[i]*Merg$eps.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.def.m2.prmm[i]/7,
      
      ##### never recovered
      cost.never.rec.m2.prmm = Merg$cov[i]*Merg$eta.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * 
        (Merg$dur.never.rec.m2.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.never.rec.m2.prmm[i]/7),
      
      cost.sachet.never.rec.m2.prmm = Merg$cov[i]*Merg$eta.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$dur.never.rec.m2.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.never.rec.m2.prmm = Merg$cov[i]*Merg$eta.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.never.rec.m2.prmm = Merg$cov[i]*Merg$eta.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.never.rec.m2.prmm[i]/7,
      cost.nonlabor.never.rec.m2.prmm = Merg$cov[i]*Merg$eta.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.never.rec.m2.prmm[i]/7,
      cost.caregiver.never.rec.m2.prmm = Merg$cov[i]*Merg$eta.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.never.rec.m2.prmm[i]/7,
      
      ##### died
      cost.died.m2.prmm = Merg$cov[i]*Merg$theta.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * 
        (Merg$dur.died.m2.prmm[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m2.prmm[i]/7),
      
      cost.sachet.died.m2.prmm = Merg$cov[i]*Merg$theta.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$dur.died.m2.prmm[i]*Merg$cost.rutf[i] ,
      cost.drug.died.m2.prmm = Merg$cov[i]*Merg$theta.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.died.m2.prmm = Merg$cov[i]*Merg$theta.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.died.m2.prmm[i]/7,
      cost.nonlabor.died.m2.prmm = Merg$cov[i]*Merg$theta.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.died.m2.prmm[i]/7,
      cost.caregiver.died.m2.prmm = Merg$cov[i]*Merg$theta.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.died.m2.prmm[i]/7,
      
      ##### hospitalized
      cost.hosp.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * 
        (Merg$dur.hosp.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.hosp.m2[i]/7),
      
      cost.sachet.hosp.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$dur.hosp.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.hosp.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.hosp.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.hosp.m2[i]/7,
      cost.nonlabor.hosp.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.hosp.m2[i]/7,
      cost.caregiver.hosp.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.hosp.m2[i]/7,
      
      ##### transferred
      cost.transf.m2.prmm = Merg$cov[i]*Merg$mu.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * 
        (Merg$dur.transf.m2[i]*Merg$cost.rutf[i] + Merg$cost.drug1[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.transf.m2[i]/7),
      
      cost.sachet.transf.m2.prmm = Merg$cov[i]*Merg$mu.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$dur.transf.m2[i]*Merg$cost.rutf[i] ,
      cost.drug.transf.m2.prmm = Merg$cov[i]*Merg$mu.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.drug1[i],
      cost.labor.transf.m2.prmm = Merg$cov[i]*Merg$mu.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.labor[i]*Merg$dur.transf.m2[i]/7,
      cost.nonlabor.transf.m2.prmm = Merg$cov[i]*Merg$mu.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.nonlabor[i]*Merg$dur.transf.m2[i]/7,
      cost.caregiver.transf.m2.prmm = Merg$cov[i]*Merg$mu.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * Merg$cost.caregiver[i]*Merg$dur.transf.m2[i]/7,
      
      ##### recovered Post-hospitalization
      cost.hosp.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  * 
        ((Merg$dur.rec.m2.prmm[i]+Merg$hosp.stay.m2[i])*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.rec.m2.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * (Merg$dur.rec.m2.prmm[i]+Merg$hosp.stay.m2[i]) *Merg$cost.rutf[i] ,
      
      cost.labor.hosp.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.rec.m2.prmm[i]/7,
      
      cost.nonlabor.hosp.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.rec.m2.prmm[i]/7,
      
      cost.caregiver.hosp.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.rec.m2.prmm[i]/7,
      
      cost.hospitalization.hosp.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$alpha2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      ##### died Post-hospitalization
      cost.hosp.died.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * 
        ((Merg$dur.died.m2.prmm[i]+Merg$hosp.stay.m2[i])*Merg$cost.rutf[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.died.m2.prmm[i]/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.died.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * (Merg$dur.died.m2.prmm[i]+Merg$hosp.stay.m2[i])  *Merg$cost.rutf[i] ,
      
      cost.labor.hosp.died.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.labor[i]*Merg$dur.died.m2.prmm[i]/7,
      
      cost.nonlabor.hosp.died.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.nonlabor[i]*Merg$dur.died.m2.prmm[i]/7,
      
      cost.caregiver.hosp.died.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.caregiver[i]*Merg$dur.died.m2.prmm[i]/7,
      
      cost.hospitalization.hosp.died.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$theta2.m2[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      ##### never recovered Post-hospitalization
      cost.hosp.never.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prmm[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * 
        ((Merg$dur.never.rec.m2.prmm[i]- Merg$dur.hosp.m2[i]) *Merg$cost.rutf[i] +
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*
           (Merg$dur.never.rec.m2.prmm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7 + 
           Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7),
      
      cost.sachet.hosp.never.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   *  (Merg$dur.never.rec.m2.prmm[i]- Merg$dur.hosp.m2[i]) *Merg$cost.rutf[i], 
      
      cost.labor.hosp.never.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.labor[i] *
        (Merg$dur.never.rec.m2.prmm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.nonlabor.hosp.never.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.nonlabor[i] *
        (Merg$dur.never.rec.m2.prmm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.caregiver.hosp.never.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.caregiver[i]*
        (Merg$dur.never.rec.m2.prmm[i] - Merg$dur.hosp.m2[i] - Merg$hosp.stay.m2[i])/7,
      
      cost.hospitalization.hosp.never.rec.m2.prmm = Merg$cov[i]*Merg$phi.m2[i] * Merg$eta2.m2.prmm[i] * 
        pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]   * Merg$cost.hosp[i]*Merg$hosp.stay.m2[i]/7,
      
      ##### MAM children who regress to SAM during treatment
      
      cost.m2.reg.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] *   
        (Merg$dur.m.reg.sam[i]*Merg$cost.rutf[i] + Merg$cost.drug[i] + 
           (Merg$cost.labor[i] + Merg$cost.nonlabor[i] + Merg$cost.caregiver[i])*Merg$dur.m.reg.sam[i]/7),
      
      cost.sachet.m2.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]*Merg$cost.rutf[i] * Merg$dur.m.reg.sam[i],
      cost.drug.m2.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$cost.drug[i],
      cost.labor.m2.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$cost.labor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.nonlabor.m2.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$cost.nonlabor[i]*Merg$dur.m.reg.sam[i]/7,
      cost.caregiver.m2.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$cost.caregiver[i]*Merg$dur.m.reg.sam[i]/7,
      
      ################# Sachet Code ###################
      
      pop.s = pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i], #pop.s1 = pop.s2 = pop.s3
      
      pop.s3.prss = (Merg$cov[i]*Merg$alpha.s3[i] * Merg$sigma.s3.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
                       Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]) ,
      
      pop.s2.prss = (Merg$cov[i]*Merg$alpha.s2[i] * Merg$sigma.s2.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
                       Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]),
      
      pop.s1.prss = (Merg$cov[i]*Merg$alpha.s1[i] * Merg$sigma.s1.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
                       Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]),
      
      pop.s.prss = (Merg$cov[i]*Merg$alpha.s[i] * Merg$sigma.s.to.sam[i]  *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) +    
                      Merg$cov[i] * Merg$tau.m[i] * (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]) ,
      
      pop.s3.prsm = Merg$cov[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s3[i] * Merg$sigma.s3.to.mam[i]  , 
      pop.s2.prsm = Merg$cov[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s2[i] * Merg$sigma.s2.to.mam[i]  , 
      pop.s1.prsm = Merg$cov[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s1[i] * Merg$sigma.s1.to.mam[i]  , 
      pop.s.prsm = Merg$cov[i] *  (pop + (cov.s*mod.wasted * (1-cov.m))*Merg$tau.m[i]) * Merg$alpha.s[i] * Merg$sigma.s.to.mam[i]  , 
      
      pop.s3.prms = (Merg$cov[i]* pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.sam[i]  +
                       Merg$cov[i] * pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i] * Merg$tau.m[i]),
      
      pop.s2.prms = (Merg$cov[i]* pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.sam[i]  +
                       Merg$cov[i] * pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i] * Merg$tau.m[i]),
      
      pop.s1.prms = (Merg$cov[i]* pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.sam[i]  +
                       Merg$cov[i] * pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i] * Merg$tau.m[i]) ,
      
      pop.s.prms = (Merg$cov[i]* pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.sam[i]  +
                      Merg$cov[i] * pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i] * Merg$tau.m[i]),
      
      
      pop.s3.prmm = Merg$cov[i] *  pop.m * Merg$alpha.m3[i] * Merg$sigma.m3.to.mam[i]  , 
      pop.s2.prmm = Merg$cov[i] *  pop.m * Merg$alpha.m2[i] * Merg$sigma.m2.to.mam[i]  , 
      pop.s1.prmm = Merg$cov[i] *  pop.m * Merg$alpha.m1[i] * Merg$sigma.m1.to.mam[i]  , 
      pop.s.prmm = Merg$cov[i] *  pop.m * Merg$alpha.m[i] * Merg$sigma.m.to.mam[i]  , 
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  MANGO SAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      number.sachet.rec.s3 = Merg$alpha.s3[i] * Merg$rec.s3[i] ,
      
      ##### defaulted
      number.sachet.def.s3 = Merg$eps.s3[i] * Merg$def.s3[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s3 = Merg$eta.s3[i] * Merg$never.rec.s3[i] ,
      
      ##### died
      number.sachet.died.s3 = Merg$theta.s3[i] * Merg$died.s3[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s3 = Merg$phi.s3[i] * Merg$hosp.s3[i] ,
      
      ##### transferred
      number.sachet.transf.s3 = Merg$mu.s3[i] * Merg$transf.s3[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s3 = Merg$phi.s3[i] * Merg$alpha2.s3[i] * Merg$hosp.rec.s3[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s3 = Merg$phi.s3[i] * Merg$theta2.s3[i] * Merg$hosp.died.s3[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s3 = Merg$phi.s3[i] * Merg$eta2.s3[i] * Merg$hosp.never.rec.s3[i], 
      
      ##### 
      ##### Relapse Post SAM Treatment - SAM to SAM ; SAM to MAM
      ##### 
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      
      ##### recovered
      number.sachet.rec.s3.prss = Merg$alpha.s3.prss[i] * Merg$rec.s3.prss[i] ,
      
      ##### defaulted
      number.sachet.def.s3.prss = Merg$eps.s3[i] * Merg$def.s3.prss[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s3.prss = Merg$eta.s3.prss[i] * Merg$never.rec.s3.prss[i] ,
      
      ##### died
      number.sachet.died.s3.prss = Merg$theta.s3[i] * Merg$died.s3.prss[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s3.prss = Merg$phi.s3[i] * Merg$hosp.s3[i] ,
      
      ##### transferred
      number.sachet.transf.s3.prss = Merg$mu.s3[i] * Merg$transf.s3[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s3.prss = Merg$phi.s3[i] * Merg$alpha2.s3.prss[i] * Merg$hosp.rec.s3.prss[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s3.prss = Merg$phi.s3[i] * Merg$theta2.s3[i] * Merg$hosp.died.s3.prss[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s3.prss = Merg$phi.s3[i] * Merg$eta2.s3.prss[i] * Merg$hosp.never.rec.s3.prss[i], 
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m3.prsm = Merg$alpha.m3.prsm[i] * Merg$dur.rec.m3.prsm[i] ,
      
      ##### defaulted
      number.sachet.def.m3.prsm = Merg$eps.m3[i] * Merg$dur.def.m3.prsm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m3.prsm = Merg$eta.m3.prsm[i] * Merg$dur.never.rec.m3.prsm[i] ,
      
      ##### died
      number.sachet.died.m3.prsm = Merg$theta.m3[i] * Merg$dur.died.m3.prsm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m3.prsm = Merg$phi.m3[i] * Merg$dur.hosp.m3[i] ,
      
      ##### transferred
      number.sachet.transf.m3.prsm = Merg$mu.m3[i] * Merg$dur.transf.m3[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m3.prsm = Merg$phi.m3[i] * Merg$alpha2.m3.prsm[i] * (Merg$dur.rec.m3.prsm[i]+Merg$hosp.stay.m3[i])  ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m3.prsm = Merg$phi.m3[i] * Merg$theta2.m3[i] * (Merg$dur.died.m3.prsm[i]+Merg$hosp.stay.m3[i])   ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m3.prsm = Merg$phi.m3[i] * Merg$eta2.m3.prsm[i] * (Merg$dur.never.rec.m3.prsm[i]- Merg$dur.hosp.m3[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      
      number.sachet.m3.to.sam.prsm = Merg$tau.m[i] * Merg$dur.m.reg.sam[i],
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  MANGO - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      number.sachet.rec.m3 = Merg$alpha.m3[i] * Merg$dur.rec.m3[i] ,
      
      ##### defaulted
      number.sachet.def.m3 = Merg$eps.m3[i] * Merg$dur.def.m3[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m3 = Merg$eta.m3[i] * Merg$dur.never.rec.m3[i] ,
      
      ##### died
      number.sachet.died.m3 = Merg$theta.m3[i] * Merg$dur.died.m3[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m3 = Merg$phi.m3[i] * Merg$dur.hosp.m3[i] ,
      
      ##### transferred
      number.sachet.transf.m3 = Merg$mu.m3[i] * Merg$dur.transf.m3[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m3 = Merg$phi.m3[i] * Merg$alpha2.m3[i] * 
        pop.m * (Merg$dur.rec.m3[i]+Merg$hosp.stay.m3[i]) ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m3 = Merg$phi.m3[i] * Merg$theta2.m3[i] * 
        pop.m * (Merg$dur.died.m3[i]+Merg$hosp.stay.m3[i])  ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m3 = Merg$phi.m3[i] * Merg$eta2.m3[i] * 
        pop.m * (Merg$dur.never.rec.m3[i]- Merg$dur.hosp.m3[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      
      number.sachet.m3.to.sam = Merg$tau.m[i] * Merg$dur.m.reg.sam[i] ,
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.s3.prms = Merg$alpha.s3.prms[i] * Merg$rec.s3.prms[i] ,
      
      ##### defaulted
      number.sachet.def.s3.prms = Merg$eps.s3[i] * Merg$def.s3.prms[i] ,
      
      
      ##### never recovered
      number.sachet.never.rec.s3.prms = Merg$eta.s3.prms[i] *  Merg$never.rec.s3.prms[i] ,
      
      ##### died
      number.sachet.died.s3.prms = Merg$theta.s3[i] * Merg$died.s3.prms[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s3.prms = Merg$phi.s3[i] * Merg$hosp.s3[i] ,
      
      ##### transferred
      number.sachet.transf.s3.prms = Merg$mu.s3[i] * Merg$transf.s3[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s3.prms = Merg$phi.s3[i] * Merg$alpha2.s3.prms[i] * Merg$hosp.rec.s3.prms[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s3.prms = Merg$phi.s3[i] * Merg$theta2.s3[i] * Merg$hosp.died.s3.prms[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s3.prms = Merg$phi.s3[i] * Merg$eta2.s3.prms[i] * Merg$hosp.never.rec.s3.prms[i], 
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m3.prmm = Merg$alpha.m3.prmm[i] * Merg$dur.rec.m3.prmm[i] ,
      
      ##### defaulted
      number.sachet.def.m3.prmm = Merg$eps.m3[i] * Merg$dur.def.m3.prmm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m3.prmm = Merg$eta.m3.prmm[i] * Merg$dur.never.rec.m3.prmm[i] ,
      
      ##### died
      number.sachet.died.m3.prmm = Merg$theta.m3[i] * Merg$dur.died.m3.prmm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m3.prmm = Merg$phi.m3[i] * Merg$dur.hosp.m3[i] ,
      
      ##### transferred
      number.sachet.transf.m3.prmm = Merg$mu.m3[i] * Merg$dur.transf.m3[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m3.prmm = Merg$phi.m3[i] * Merg$alpha2.m3.prmm[i] * (Merg$dur.rec.m3.prmm[i]+Merg$hosp.stay.m3[i])  ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m3.prmm = Merg$phi.m3[i] * Merg$theta2.m3[i] *(Merg$dur.died.m3.prmm[i]+Merg$hosp.stay.m3[i])   ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m3.prmm = Merg$phi.m3[i] * Merg$eta2.m3.prmm[i] * (Merg$dur.never.rec.m3.prmm[i]- Merg$dur.hosp.m3[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      
      number.sachet.m3.to.sam.prmm = Merg$tau.m[i] * Merg$dur.m.reg.sam[i],
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  SAM OptimA
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      number.sachet.rec.s1 = Merg$alpha.s1[i] *  Merg$rec.s1[i] ,
      
      ##### defaulted
      number.sachet.def.s1 = Merg$eps.s1[i] *  Merg$def.s1[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s1 = Merg$eta.s1[i] *  Merg$never.rec.s1[i] ,
      
      ##### died
      number.sachet.died.s1 = Merg$theta.s1[i] *  Merg$died.s1[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s1 = Merg$phi.s1[i] *  Merg$hosp.s1[i] ,
      
      ##### transferred
      number.sachet.transf.s1 = Merg$mu.s1[i] *  Merg$transf.s1[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s1 = Merg$phi.s1[i] * Merg$alpha2.s1[i] * Merg$hosp.rec.s1[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s1 = Merg$phi.s1[i] * Merg$theta2.s1[i] * Merg$hosp.died.s1[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s1 = Merg$phi.s1[i] * Merg$eta2.s1[i] * Merg$hosp.never.rec.s1[i], 
      
      ##### 
      ##### Relapse Post SAM Treatment
      ##### 
      
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      
      ##### recovered
      number.sachet.rec.s1.prss = Merg$alpha.s1.prss[i] * Merg$rec.s1.prss[i] ,
      
      ##### defaulted
      number.sachet.def.s1.prss = Merg$eps.s1[i] *  Merg$def.s1.prss[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s1.prss = Merg$eta.s1.prss[i] * Merg$never.rec.s1.prss[i] ,
      
      ##### died
      number.sachet.died.s1.prss = Merg$theta.s1[i] * Merg$died.s1.prss[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s1.prss = Merg$phi.s1[i] * Merg$hosp.s1[i] ,
      
      ##### transferred
      number.sachet.transf.s1.prss = Merg$mu.s1[i] * Merg$transf.s1[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s1.prss = Merg$phi.s1[i] * Merg$alpha2.s1.prss[i] * Merg$hosp.rec.s1.prss[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s1.prss = Merg$phi.s1[i] * Merg$theta2.s1[i] * Merg$hosp.died.s1.prss[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s1.prss = Merg$phi.s1[i] * Merg$eta2.s1.prss[i] * Merg$hosp.never.rec.s1.prss[i], 
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m1.prsm = Merg$alpha.m1.prsm[i] * Merg$rec.m1.prsm[i] ,
      
      ##### defaulted
      number.sachet.def.m1.prsm = Merg$eps.m1[i] *  Merg$def.m1.prsm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m1.prsm = Merg$eta.m1.prsm[i] *  Merg$never.rec.m1.prsm[i] ,
      
      ##### died
      number.sachet.died.m1.prsm = Merg$theta.m1[i] *  Merg$died.m1.prsm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m1.prsm =Merg$phi.m1[i] * Merg$hosp.m1[i] ,
      
      ##### transferred
      number.sachet.transf.m1.prsm = Merg$mu.m1[i] *  Merg$transf.m1[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m1.prsm = Merg$phi.m1[i] * Merg$alpha2.m1.prsm[i] * Merg$hosp.rec.m1.prsm[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m1.prsm = Merg$phi.m1[i] * Merg$theta2.m1[i] * Merg$hosp.died.m1.prsm[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m1.prsm = Merg$phi.m1[i] * Merg$eta2.m1.prsm[i] *  Merg$hosp.never.rec.m1.prsm[i], 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m1.to.sam.prsm = Merg$tau.m[i] *  Merg$m1.reg.sam.prsm[i],
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  OptimA - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      ##### recovered
      number.sachet.rec.m1 = Merg$alpha.m1[i] * Merg$rec.m1[i] ,
      
      ##### defaulted
      number.sachet.def.m1 = Merg$eps.m1[i] * Merg$def.m1[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m1 = Merg$eta.m1[i] * Merg$never.rec.m1[i] ,
      
      ##### died
      number.sachet.died.m1 = Merg$theta.m1[i] * Merg$died.m1[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m1 = Merg$phi.m1[i] * Merg$hosp.m1[i] ,
      
      ##### transferred
      number.sachet.transf.m1 = Merg$mu.m1[i] * Merg$transf.m1[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m1 = Merg$phi.m1[i] * Merg$alpha2.m1[i] * Merg$hosp.rec.m1[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m1 = Merg$phi.m1[i] * Merg$theta2.m1[i] * Merg$hosp.died.m1[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m1 = Merg$phi.m1[i] * Merg$eta2.m1[i] * Merg$hosp.never.rec.m1[i], 
      
      ##### MAM children who regress to SAM during treatment
      
      number.sachet.m1.to.sam = Merg$tau.m[i] * Merg$m1.reg.sam[i] ,
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.s1.prms = Merg$alpha.s1.prms[i] *  Merg$rec.s1.prms[i] ,
      
      ##### defaulted
      number.sachet.def.s1.prms = Merg$eps.s1[i] * Merg$def.s1.prms[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s1.prms = Merg$eta.s1.prms[i] * Merg$never.rec.s1.prms[i] ,
      
      ##### died
      number.sachet.died.s1.prms = Merg$theta.s1[i] * Merg$died.s1.prms[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s1.prms = Merg$phi.s1[i] * Merg$hosp.s1[i] ,
      
      ##### transferred
      number.sachet.transf.s1.prms = Merg$mu.s1[i] * Merg$transf.s1[i] ,
      
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s1.prms = Merg$phi.s1[i] * Merg$alpha2.s1.prms[i] * Merg$hosp.rec.s1.prms[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s1.prms = Merg$phi.s1[i] * Merg$theta2.s1[i] * Merg$hosp.died.s1.prms[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s1.prms = Merg$phi.s1[i] * Merg$eta2.s1.prms[i] * Merg$hosp.never.rec.s1.prms[i], 
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m1.prmm = Merg$alpha.m1.prmm[i] * Merg$rec.m1.prmm[i] ,
      
      ##### defaulted
      number.sachet.def.m1.prmm = Merg$eps.m1[i] * Merg$def.m1.prmm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m1.prmm = Merg$eta.m1.prmm[i] * Merg$never.rec.m1.prmm[i] ,
      
      ##### died
      number.sachet.died.m1.prmm = Merg$theta.m1[i] * Merg$died.m1.prmm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m1.prmm = Merg$phi.m1[i] * Merg$hosp.m1[i] ,
      
      ##### transferred
      number.sachet.transf.m1.prmm = Merg$mu.m1[i] * Merg$transf.m1[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m1.prmm = Merg$phi.m1[i] * Merg$alpha2.m1.prmm[i] * Merg$hosp.rec.m1.prmm[i]  ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m1.prmm = Merg$phi.m1[i] * Merg$theta2.m1[i] * Merg$hosp.died.m1.prmm[i]   ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m1.prmm = Merg$phi.m1[i] * Merg$eta2.m1.prmm[i]  *  Merg$hosp.never.rec.m1.prmm[i] , 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m1.to.sam.prmm = Merg$tau.m[i] * Merg$m1.reg.sam.prmm[i],
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### ######### SAM Standard
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      ##### MAM children not in treatment who regress to SAM
      #number.sachet.rec.s = Merg$alpha.s[i] *  Merg$rec.s[i] ,
      
      ##### recovered
      number.sachet.rec.s = Merg$alpha.s[i] *  Merg$rec.s[i] ,
      
      ##### defaulted
      number.sachet.def.s = Merg$eps.s[i] *  Merg$def.s[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s = Merg$eta.s[i] *  Merg$never.rec.s[i] ,
      
      ##### died
      number.sachet.died.s = Merg$theta.s[i] *  Merg$died.s[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s = Merg$phi.s[i] *  Merg$hosp.s[i] ,
      
      ##### transferred
      number.sachet.transf.s = Merg$mu.s[i] *  Merg$transf.s[i] ,
      
      ##### recovered Post-hospitalization
      ### hospitalization cost includes labor costs + medications/supplies  + opportunity cost 
      ### therefore costs of labor + nonlabor + caregiver added to recovery or died or never recovered duration post-hospitalization
      number.sachet.hosp.rec.s = Merg$phi.s[i] * Merg$alpha2.s[i] * Merg$hosp.rec.s[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s = Merg$phi.s[i] * Merg$theta2.s[i] * Merg$hosp.died.s[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s = Merg$phi.s[i] * Merg$eta2.s[i] * Merg$hosp.never.rec.s[i], 
      
      ##### 
      ##### Relapse Post SAM Treatment
      ##### 
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      ### MAM children who regressed to SAM during the treatment, receive treatment for SAM, both in original treatment period, as well as treatment post relapse)
      
      ##### recovered
      number.sachet.rec.s.prss = Merg$alpha.s.prss[i] * Merg$rec.s.prss[i] ,
      
      ##### defaulted
      number.sachet.def.s.prss = Merg$eps.s[i] * Merg$def.s.prss[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s.prss = Merg$eta.s.prss[i] * Merg$never.rec.s.prss[i] ,
      
      ##### died
      number.sachet.died.s.prss = Merg$theta.s[i] * Merg$died.s.prss[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s.prss = Merg$phi.s[i] * Merg$hosp.s[i] ,
      
      ##### transferred
      number.sachet.transf.s.prss = Merg$mu.s[i] * Merg$transf.s[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s.prss = Merg$phi.s[i] * Merg$alpha2.s.prss[i] * Merg$hosp.rec.s.prss[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s.prss = Merg$phi.s[i] * Merg$theta2.s[i] * Merg$hosp.died.s.prss[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s.prss = Merg$phi.s[i] * Merg$eta2.s.prss[i] * Merg$hosp.never.rec.s.prss[i], 
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m.prsm = Merg$alpha.m.prsm[i] * Merg$dur.rec.m.prsm[i] ,
      
      ##### defaulted
      number.sachet.def.m.prsm = Merg$eps.m[i] * Merg$dur.def.m.prsm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m.prsm = Merg$eta.m.prsm[i] * Merg$dur.never.rec.m.prsm[i] ,
      
      ##### died
      number.sachet.died.m.prsm = Merg$theta.m[i] * Merg$dur.died.m.prsm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m.prsm = Merg$phi.m[i] * Merg$dur.hosp.m[i] ,
      
      ##### transferred
      number.sachet.transf.m.prsm = Merg$mu.m[i] * Merg$dur.transf.m[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m.prsm = Merg$phi.m[i] * Merg$alpha2.m.prsm[i] *(Merg$dur.rec.m.prsm[i]+Merg$hosp.stay.m[i])  ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m.prsm = Merg$phi.m[i] * Merg$theta2.m[i] * (Merg$dur.died.m.prsm[i]+Merg$hosp.stay.m[i])   ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m.prsm = Merg$phi.m[i] * Merg$eta2.m.prsm[i] * (Merg$dur.never.rec.m.prsm[i]- Merg$dur.hosp.m[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m.to.sam.prsm = Merg$tau.m[i] * Merg$dur.m.reg.sam[i],
      
      ## ## ## 
      ## ## ## POST-SAM to MAM CSB
      ## ## ## checked until here
      
      ##### recovered
      number.csb.rec.m.prsm = Merg$alpha.m.prsm[i] * Merg$dur.rec.m.prsm[i] ,
      
      ##### defaulted
      number.csb.def.m.prsm = Merg$eps.m[i] * Merg$dur.def.m.prsm[i] ,
      
      ##### never recovered
      number.csb.never.rec.m.prsm = Merg$eta.m.prsm[i] * Merg$dur.never.rec.m.prsm[i] ,
      
      ##### died
      number.csb.died.m.prsm = Merg$theta.m[i] * Merg$dur.died.m.prsm[i] ,
      
      ##### hospitalized
      number.csb.hosp.m.prsm = Merg$phi.m[i] * Merg$dur.hosp.m[i] ,
      
      ##### transferred
      number.csb.transf.m.prsm = Merg$mu.m[i] * Merg$dur.transf.m[i] ,
      
      ##### recovered Post-hospitalization
      number.csb.hosp.rec.m.prsm = Merg$phi.m[i] * Merg$alpha2.m.prsm[i] * (Merg$dur.rec.m.prsm[i]+Merg$hosp.stay.m[i])  ,
      
      ##### died Post-hospitalization
      number.csb.hosp.died.m.prsm = Merg$phi.m[i] * Merg$theta2.m[i] *  (Merg$dur.died.m.prsm[i]+Merg$hosp.stay.m[i])   ,
      
      ##### never recovered Post-hospitalization
      number.csb.hosp.never.rec.m.prsm = Merg$phi.m[i] * Merg$eta2.m.prsm[i] *  (Merg$dur.never.rec.m.prsm[i]- Merg$dur.hosp.m[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.csb.m.to.sam.prsm = Merg$tau.m[i] * Merg$dur.m.reg.sam[i],
      
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  Standard - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      number.sachet.rec.m = Merg$alpha.m[i] * Merg$dur.rec.m[i] ,    
      
      ##### defaulted
      number.sachet.def.m = Merg$eps.m[i] * Merg$dur.def.m[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m = Merg$eta.m[i] * Merg$dur.never.rec.m[i] ,
      
      ##### died
      number.sachet.died.m = Merg$theta.m[i] * Merg$dur.died.m[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m = Merg$phi.m[i] * Merg$dur.hosp.m[i] ,
      
      ##### transferred
      number.sachet.transf.m = Merg$mu.m[i] * Merg$dur.transf.m[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * (Merg$dur.rec.m[i]+Merg$hosp.stay.m[i]) ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * (Merg$dur.died.m[i]+Merg$hosp.stay.m[i])  ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * (Merg$dur.never.rec.m[i]- Merg$dur.hosp.m[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m.to.sam = Merg$tau.m[i] * Merg$dur.m.reg.sam[i] ,
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  Standard - MAM CSB
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      number.csb.rec.m = Merg$alpha.m[i] * Merg$dur.rec.m[i] ,
      
      ##### defaulted
      number.csb.def.m = Merg$eps.m[i] * Merg$dur.def.m[i] ,
      
      ##### never recovered
      number.csb.never.rec.m = Merg$eta.m[i] * Merg$dur.never.rec.m[i] ,
      
      ##### died
      number.csb.died.m = Merg$theta.m[i] * Merg$dur.died.m[i] ,
      
      ##### hospitalized
      number.csb.hosp.m = Merg$phi.m[i] * Merg$dur.hosp.m[i] ,
      
      ##### transferred
      number.csb.transf.m = Merg$mu.m[i] * Merg$dur.transf.m[i] ,
      
      ##### recovered Post-hospitalization
      number.csb.hosp.rec.m = Merg$phi.m[i] * Merg$alpha2.m[i] * (Merg$dur.rec.m[i]+Merg$hosp.stay.m[i]) ,
      
      ##### died Post-hospitalization
      number.csb.hosp.died.m = Merg$phi.m[i] * Merg$theta2.m[i] * (Merg$dur.died.m[i]+Merg$hosp.stay.m[i])  ,
      
      ##### never recovered Post-hospitalization
      number.csb.hosp.never.rec.m = Merg$phi.m[i] * Merg$eta2.m[i] * (Merg$dur.never.rec.m[i]- Merg$dur.hosp.m[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.csb.m.to.sam = Merg$tau.m[i] * Merg$dur.m.reg.sam[i] ,
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.s.prms = Merg$alpha.s.prms[i] *  Merg$rec.s.prms[i] ,
      
      ##### defaulted
      number.sachet.def.s.prms = Merg$eps.s[i] * Merg$def.s.prms[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s.prms = Merg$eta.s.prms[i] * Merg$never.rec.s.prms[i] ,
      
      ##### died
      number.sachet.died.s.prms = Merg$theta.s[i] * Merg$died.s.prms[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s.prms = Merg$phi.s[i] * Merg$hosp.s[i] ,
      
      ##### transferred
      number.sachet.transf.s.prms = Merg$mu.s[i] * Merg$transf.s[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s.prms = Merg$phi.s[i] * Merg$alpha2.s.prms[i] * Merg$hosp.rec.s.prms[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s.prms = Merg$phi.s[i] * Merg$theta2.s[i] * Merg$hosp.died.s.prms[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s.prms = Merg$phi.s[i] * Merg$eta2.s.prms[i] * Merg$hosp.never.rec.s.prms[i], 
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m.prmm = Merg$alpha.m.prmm[i] * Merg$dur.rec.m.prmm[i] ,
      
      ##### defaulted
      number.sachet.def.m.prmm = Merg$eps.m[i] * Merg$dur.def.m.prmm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m.prmm = Merg$eta.m.prmm[i] * Merg$dur.never.rec.m.prmm[i] ,
      
      ##### died
      number.sachet.died.m.prmm = Merg$theta.m[i] * Merg$dur.died.m.prmm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m.prmm = Merg$phi.m[i] * Merg$dur.hosp.m[i] ,
      
      ##### transferred
      number.sachet.transf.m.prmm = Merg$mu.m[i] * Merg$dur.transf.m[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m.prmm = Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * (Merg$dur.rec.m.prmm[i]+Merg$hosp.stay.m[i])  ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m.prmm = Merg$phi.m[i] * Merg$theta2.m[i] * (Merg$dur.died.m.prmm[i]+Merg$hosp.stay.m[i])   ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m.prmm = Merg$phi.m[i] * Merg$eta2.m.prmm[i] *  (Merg$dur.never.rec.m.prmm[i]- Merg$dur.hosp.m[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m.to.sam.prmm = Merg$tau.m[i] * Merg$dur.m.reg.sam[i],
      
      ## ## ## 
      ## ## ## POST-MAM to MAM CSB
      ## ## ## 
      
      ##### recovered
      number.csb.rec.m.prmm = Merg$alpha.m.prmm[i] * Merg$dur.rec.m.prmm[i] ,
      
      ##### defaulted
      number.csb.def.m.prmm = Merg$eps.m[i] * Merg$dur.def.m.prmm[i] ,
      
      ##### never recovered
      number.csb.never.rec.m.prmm = Merg$eta.m.prmm[i] * Merg$dur.never.rec.m.prmm[i] ,
      
      ##### died
      number.csb.died.m.prmm = Merg$theta.m[i] * Merg$dur.died.m.prmm[i] ,
      
      ##### hospitalized
      number.csb.hosp.m.prmm = Merg$phi.m[i] * Merg$dur.hosp.m[i] ,
      
      ##### transferred
      number.csb.transf.m.prmm = Merg$mu.m[i] * Merg$dur.transf.m[i] ,
      
      ##### recovered Post-hospitalization
      number.csb.hosp.rec.m.prmm = Merg$phi.m[i] * Merg$alpha2.m.prmm[i] * (Merg$dur.rec.m.prmm[i]+Merg$hosp.stay.m[i])  ,
      
      ##### died Post-hospitalization
      number.csb.hosp.died.m.prmm = Merg$phi.m[i] * Merg$theta2.m[i] * (Merg$dur.died.m.prmm[i]+Merg$hosp.stay.m[i])   ,
      
      ##### never recovered Post-hospitalization
      number.csb.hosp.never.rec.m.prmm = Merg$phi.m[i] * Merg$eta2.m.prmm[i] * (Merg$dur.never.rec.m.prmm[i]- Merg$dur.hosp.m[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.csb.m.to.sam.prmm = Merg$cov[i] * Merg$tau.m[i] * Merg$dur.m.reg.sam[i],
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  SAM ComPAS
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      ##### recovered
      number.sachet.rec.s2 = Merg$alpha.s2[i] *  Merg$rec.s2[i] ,
      
      ##### defaulted
      number.sachet.def.s2 = Merg$eps.s2[i] *  Merg$def.s2[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s2 = Merg$eta.s2[i] *  Merg$never.rec.s2[i] ,
      
      ##### died
      number.sachet.died.s2 = Merg$theta.s2[i] *  Merg$died.s2[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s2 = Merg$phi.s2[i] *  Merg$hosp.s2[i] ,
      
      ##### transferred
      number.sachet.transf.s2 = Merg$mu.s2[i] *  Merg$transf.s2[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s2 = Merg$phi.s2[i] * Merg$alpha2.s2[i] * Merg$hosp.rec.s2[i] ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s2 = Merg$phi.s2[i] * Merg$theta2.s2[i] * Merg$hosp.died.s2[i] ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s2 = Merg$phi.s2[i] * Merg$eta2.s2[i] * Merg$hosp.never.rec.s2[i], 
      
      ##### 
      ##### Relapse Post SAM Treatment
      ##### 
      
      
      ## ## ## 
      ## ## ## POST-SAM to SAM
      ## ## ## 
      
      ### MAM children who regressed to SAM during the treatment, receive treatment for SAM, both in original treatment period, as well as treatment post relapse)
      
      ##### recovered
      number.sachet.rec.s2.prss = Merg$alpha.s2.prss[i] * Merg$dur.rec.s2.prss[i] ,
      
      ##### defaulted
      number.sachet.def.s2.prss = Merg$eps.s2[i] * Merg$dur.def.s2.prss[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s2.prss = Merg$eta.s2.prss[i] * Merg$dur.never.rec.s2.prss[i] ,
      
      ##### died
      number.sachet.died.s2.prss = Merg$theta.s2[i] * Merg$dur.died.s2.prss[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s2.prss = Merg$phi.s2[i] * Merg$dur.hosp.s2[i] ,
      
      ##### transferred
      number.sachet.transf.s2.prss = Merg$mu.s2[i] * Merg$dur.transf.s2[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s2.prss = Merg$phi.s2[i] * Merg$alpha2.s2.prss[i] * (Merg$dur.rec.s2.prss[i]+ Merg$hosp.stay.s2[i]) ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s2.prss = Merg$phi.s2[i] * Merg$theta2.s2[i] * (Merg$dur.died.s2.prss[i] + Merg$hosp.stay.s2[i]),
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s2.prss = Merg$phi.s2[i] * Merg$eta2.s2.prss[i] * (Merg$dur.never.rec.s2.prss[i] - Merg$hosp.stay.s2[i]) , 
      
      ## ## ## 
      ## ## ## POST-SAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m2.prsm = Merg$alpha.m2.prsm[i] *  Merg$dur.rec.m2.prsm[i] ,
      
      ##### defaulted
      number.sachet.def.m2.prsm = Merg$eps.m2[i] *  Merg$dur.def.m2.prsm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m2.prsm = Merg$eta.m2.prsm[i] *  Merg$dur.never.rec.m2.prsm[i] ,
      
      ##### died
      number.sachet.died.m2.prsm = Merg$theta.m2[i] *  Merg$dur.died.m2.prsm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m2.prsm = Merg$phi.m2[i] *  Merg$dur.hosp.m2[i] ,
      
      ##### transferred
      number.sachet.transf.m2.prsm = Merg$mu.m2[i] *  Merg$dur.transf.m2[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m2.prsm = Merg$phi.m2[i] * Merg$alpha2.m2.prsm[i] * (Merg$dur.rec.m2.prsm[i]+Merg$hosp.stay.m2[i])  ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m2.prsm = Merg$phi.m2[i] * Merg$theta2.m2[i] * (Merg$dur.died.m2.prsm[i]+Merg$hosp.stay.m2[i])   ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m2.prsm = Merg$phi.m2[i] * Merg$eta2.m2.prsm[i] *  (Merg$dur.never.rec.m2.prsm[i]- Merg$dur.hosp.m2[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m2.to.sam.prsm = Merg$tau.m[i] * Merg$dur.m.reg.sam[i],
      
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      ######### #########  ComPAS - MAM
      ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
      
      
      ##### recovered
      number.sachet.rec.m2 = Merg$alpha.m2[i] * Merg$dur.rec.m2[i] ,
      
      ##### defaulted
      number.sachet.def.m2 = Merg$eps.m2[i] * Merg$dur.def.m2[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m2 = Merg$eta.m2[i] * Merg$dur.never.rec.m2[i] ,
      
      ##### died
      number.sachet.died.m2 = Merg$theta.m2[i] * Merg$dur.died.m2[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m2 = Merg$phi.m2[i] * Merg$dur.hosp.m2[i] ,
      
      ##### transferred
      number.sachet.transf.m2 = Merg$mu.m2[i] * Merg$dur.transf.m2[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m2 = Merg$phi.m2[i] * Merg$alpha2.m2[i] * (Merg$dur.rec.m2[i]+Merg$hosp.stay.m2[i]) ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m2 = Merg$phi.m2[i] * Merg$theta2.m2[i] * (Merg$dur.died.m2[i]+Merg$hosp.stay.m2[i])  ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m2 = Merg$phi.m2[i] * Merg$eta2.m2[i] * (Merg$dur.never.rec.m2[i]- Merg$dur.hosp.m2[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m2.to.sam = Merg$tau.m[i] * Merg$dur.m.reg.sam[i] ,
      
      ## ## ## 
      ## ## ## POST-MAM to SAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.s2.prms = Merg$alpha.s2.prms[i] * Merg$dur.rec.s2.prms[i] ,
      
      ##### defaulted
      number.sachet.def.s2.prms = Merg$eps.s2[i] * Merg$dur.def.s2.prms[i] ,
      
      ##### never recovered
      number.sachet.never.rec.s2.prms = Merg$eta.s2.prms[i] * Merg$dur.never.rec.s2.prms[i] ,
      
      ##### died
      number.sachet.died.s2.prms = Merg$theta.s2[i] * Merg$dur.died.s2.prms[i] ,
      
      ##### hospitalized
      number.sachet.hosp.s2.prms = Merg$phi.s2[i] * Merg$dur.hosp.s2[i],
      
      ##### transferred
      number.sachet.transf.s2.prms = Merg$mu.s2[i] * Merg$dur.transf.s2[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.s2.prms = Merg$phi.s2[i] * Merg$alpha2.s2.prms[i] * (Merg$dur.rec.s2.prms[i]+ Merg$hosp.stay.s2[i]) ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.s2.prms = Merg$phi.s2[i] * Merg$theta2.s2[i] * (Merg$dur.died.s2.prms[i]+ Merg$hosp.stay.s2[i]),
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.s2.prms = Merg$phi.s2[i] * Merg$eta2.s2.prms[i] * (Merg$dur.never.rec.s2.prms[i] - Merg$hosp.stay.s2[i]) , 
      
      ## ## ## 
      ## ## ## POST-MAM to MAM
      ## ## ## 
      
      ##### recovered
      number.sachet.rec.m2.prmm = Merg$alpha.m2.prmm[i] * Merg$dur.rec.m2.prmm[i] ,
      
      ##### defaulted
      number.sachet.def.m2.prmm = Merg$eps.m2[i] * Merg$dur.def.m2.prmm[i] ,
      
      ##### never recovered
      number.sachet.never.rec.m2.prmm = Merg$eta.m2.prmm[i] * Merg$dur.never.rec.m2.prmm[i] ,
      
      ##### died
      number.sachet.died.m2.prmm = Merg$theta.m2[i] * Merg$dur.died.m2.prmm[i] ,
      
      ##### hospitalized
      number.sachet.hosp.m2.prmm = Merg$phi.m2[i] * Merg$dur.hosp.m2[i] ,
      
      ##### transferred
      number.sachet.transf.m2.prmm = Merg$mu.m2[i] * Merg$dur.transf.m2[i] ,
      
      ##### recovered Post-hospitalization
      number.sachet.hosp.rec.m2.prmm = Merg$phi.m2[i] * Merg$alpha2.m2.prmm[i] * (Merg$dur.rec.m2.prmm[i]+Merg$hosp.stay.m2[i])  ,
      
      ##### died Post-hospitalization
      number.sachet.hosp.died.m2.prmm = Merg$phi.m2[i] * Merg$theta2.m2[i] * (Merg$dur.died.m2.prmm[i]+Merg$hosp.stay.m2[i])   ,
      
      ##### never recovered Post-hospitalization
      number.sachet.hosp.never.rec.m2.prmm = Merg$phi.m2[i] * Merg$eta2.m2.prmm[i] * (Merg$dur.never.rec.m2.prmm[i]- Merg$dur.hosp.m2[i]) , 
      
      ##### MAM children who regress to SAM during treatment
      number.sachet.m2.to.sam.prmm = Merg$tau.m[i] * Merg$dur.m.reg.sam[i]
      
    )}
  
cost.s.varied.relapse <- cost.all.varied.relapse(1:dim(Merg)[1],Merg,pop,pop.m,cov.m,cov.s,mod.wasted)
return(cost.s.varied.relapse)
}


########## Model function ############
### Function for parameters

f_DoseOpt_params_lhs <- function(weight.adm.s,muac.s,sev.wasted,mod.wasted,cov.s,cov.m,
                                 weight.gain.standard,weight.gain.optima,muac.gain.standard,muac.gain.optima,
                                 weight.gain.mango,muac.gain.compas,mango,sc,treatDist.s,treatDist.m,
                                 OptimaDist,
                                 CompasDist, 
                                 MangoDist,
                                 ppp,exch,
                                 costDist.min,costDist.max,
                                 startingPoint,country.name,reducedRecovery) {
  
  ##### simulation size
  h <- 15       
  
  ### standard vs. OptimA vs. ComPAS vs. MANGO - Severe wasting
  
  #if (startingPoint %in% 0){
  ### standard 
  alpha.s <- treatDist.s[1] # recovered 
  eps.s <- treatDist.s[2] # defaulted
  theta.s <- treatDist.s[3] # dead
  phi.s <- treatDist.s[4] # hospitalized 
  eta.s <- treatDist.s[5] # never recovered
  mu.s <- 1 - alpha.s - eps.s - eta.s - theta.s - phi.s # transferred
  
  alpha.m <- treatDist.m[1] # recovered 
  eps.m <- treatDist.m[2] # defaulted
  theta.m <- treatDist.m[3] # dead
  phi.m <- treatDist.m[4] # hospitalized 
  eta.m <- treatDist.m[5] # never recovered
  
  ### MAM children who regress to SAM during treatment, based on convo with Kevin Phelan: 10% Isanaka Mali,15% DRC Cazes, 30%...
  ## Isanaka Table 2: Probability of developing SAM among children with MAM	9.3%	range: 3.0–19.5%: https://pmc.ncbi.nlm.nih.gov/articles/PMC6509694/
  
  # Cazes Suppl table 5: Children with at least one follow-up visit with indication for reference to hospital; Main reason for reference to hospital: stagnant or weight loss:
  # https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(22)00041-9/fulltext?trk=organization_guest_main-feed-card_feed-article-content
  # 12.5% (37/296) OptimA, 5.2% (15/288) Standard 
  
  tau.m.min = tau.m1.min = tau.m2.min = tau.m3.min =  treatDist.m[6] 
  tau.m.max = tau.m1.max = tau.m2.max = tau.m3.max =  treatDist.m[7]
  
  ### OptimA 
  alpha.s1 <- OptimaDist[1]#0.681 # recovered 
  eps.s1 <- OptimaDist[2]#0.058 # defaulted
  theta.s1 <- OptimaDist[3]#0.021 # dead
  phi.s1 <- OptimaDist[4]#0 # hospitalized
  eta.s1 <- OptimaDist[5]#0.132 # never recovered
  mu.s1 <- 1 - alpha.s1 - eps.s1 - eta.s1 - theta.s1 - phi.s1 # transferred
  
  alpha.m1 <- OptimaDist[6]#0.497 # recovered 
  eps.m1 <- OptimaDist[7]#0.113 # defaulted
  theta.m1 <- OptimaDist[8]#0.056 # dead
  phi.m1 <- OptimaDist[9]#0 # hospitalized
  eta.m1 <- OptimaDist[10]#0.08 # never recovered
  
  ### ComPAS 
  alpha.s2 <- CompasDist[1]#0.694 # recovered 
  eps.s2 <- CompasDist[2]#0.061 # defaulted
  theta.s2 <- CompasDist[3]#0.013 # dead
  phi.s2 <- CompasDist[4]#0 # hospitalized 
  eta.s2 <- CompasDist[5]#0.121 # never recovered
  mu.s2 <- 1 - alpha.s2 - eps.s2 - eta.s2 - theta.s2 - phi.s2 # transferred
  
  alpha.m2 <- CompasDist[6]#0.519 # recovered 
  eps.m2 <- CompasDist[7]#0.095 # defaulted
  eta.m2 <- CompasDist[8]#0.074 # never recovered
  theta.m2 <- CompasDist[9]#0.03 # dead
  phi.m2 <- CompasDist[10]#0 # hospitalized
  
  ### MANGO 
  alpha.s3 <- MangoDist[1]#mango[dosage==1 & am=="SAM" & exit_rsn == 1,]$proportion # recovered
  eps.s3 <- MangoDist[2]#mango[dosage==1 & am=="SAM" & exit_rsn == 3,]$proportion # defaulted
  eta.s3 <- MangoDist[3]#mango[dosage==1 & am=="SAM" & exit_rsn == 6,]$proportion # never recovered
  theta.s3 <- MangoDist[4]#0 # dead
  phi.s3 <- MangoDist[5]#mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$proportion # hospitalized
  mu.s3 <- 1 - alpha.s3 - eps.s3 - eta.s3 - theta.s3 - phi.s3 # transferred

  alpha.m3 <- MangoDist[6]#mango[dosage==1 & am=="MAM" & exit_rsn == 1,]$proportion # recovered 
  eps.m3 <- MangoDist[7]#mango[dosage==1 & am=="MAM" & exit_rsn == 3,]$proportion # defaulted
  eta.m3 <- MangoDist[8]#mango[dosage==1 & am=="MAM" & exit_rsn == 6,]$proportion # never recovered
  theta.m3 <- MangoDist[9]#mango[dosage==1 & am=="MAM" & exit_rsn == 5,]$proportion # dead
  phi.m3 <- MangoDist[10]#mango[dosage==1 & am=="MAM" & exit_rsn == 2,]$proportion # hospitalized 
  
  ###########
  ########### Post-hospitalization
  ###########
  
  theta2.s1 =  theta2.s2 =  theta2.s3 =  theta2.s =  
    theta2.m1 =  theta2.m2 =  theta2.m3 =  theta2.m =  0.11# died post-hospitalization
  
  eta2.s1 =  eta2.s2 =  eta2.s3 =  eta2.s =  
    eta2.m1 =  eta2.m2 =  eta2.m3 =  eta2.m = 0.039# stayed wasted post-hospitalization
  
  alpha2.s1 =  alpha2.s2 =  alpha2.s3 =  alpha2.s =  
    alpha2.m1 =  alpha2.m2 =  alpha2.m3 =  alpha2.m = 1  - theta2.s - eta2.s # recovered post-hospitalization
  
  
  ###########
  ########### Post-discharge
  ########### 

  delta.s1.min = delta.s2.min = delta.s3.min = delta.s.min =  
    delta.m1.min = delta.m2.min = delta.m3.min = delta.m.min = 0.0006 # died within 6 months post-discharge
  
  delta.s1.max = delta.s2.max = delta.s3.max = delta.s.max =  
    delta.m1.max = delta.m2.max = delta.m3.max = delta.m.max =  0.104   # died within 6 months post-discharge
  
  ### relapse rate
  
  # ######## Standard 
  sigma.s.to.sam.min <- treatDist.s[7]# 0.015 relapsed from SAM to SAM
  sigma.s.to.sam.max <-  treatDist.s[8]# 0.136 relapsed from SAM to SAM
  sigma.s.to.mam.min <-  treatDist.s[9]#0.0725# relapsed from SAM to MAM
  sigma.s.to.mam.max <-  treatDist.s[10]#0.226# relapsed from SAM to MAM
  
  sigma.m.to.mam.min <- treatDist.m[8]#0.1 # relapsed from MAM to MAM (Assumed)
  sigma.m.to.mam.max <- treatDist.m[9]#0.3# relapsed from MAM to MAM (Assumed)
  sigma.m.to.sam.min <- treatDist.m[10]#0 # relapsed from MAM to SAM (Assumed) - Deterioration
  sigma.m.to.sam.max <- treatDist.m[11]#0.1 # relapsed from MAM to SAM (Assumed) - Deterioration
  
  # ######## OptimA
  sigma.s1.to.sam.min <-  OptimaDist[11]#0.111# relapsed from SAM to SAM
  sigma.s1.to.sam.max <-  OptimaDist[12]#0.111# relapsed from SAM to SAM
  sigma.s1.to.mam.min <-  OptimaDist[13]#0.01# relapsed from SAM to MAM
  sigma.s1.to.mam.max <- OptimaDist[14]# 0.01# relapsed from SAM to MAM
  
  sigma.m1.to.mam.min <-  OptimaDist[15]#0.24 # relapsed from MAM to MAM (Cazes et al, 2022, Table S5, MAM to AM)
  sigma.m1.to.mam.max <-  OptimaDist[16]#0.24 # relapsed from MAM to MAM (Cazes et al, 2022, Table S5, MAM to AM)
  sigma.m1.to.sam.min <-  OptimaDist[17]#0 # relapsed from MAM to SAM  (assumed)
  sigma.m1.to.sam.max <-  OptimaDist[18]#0.1 # relapsed from MAM to SAM  (assumed)
  
  # ######## ComPAS
  sigma.s2.to.sam.min <-  CompasDist[11]#0.03# relapsed from SAM to SAM
  sigma.s2.to.sam.max <-  CompasDist[12]#0.124# relapsed from SAM to SAM
  sigma.s2.to.mam.min <-  CompasDist[13]#0.174# relapsed from SAM to MAM (Kangas et al, 2023, table 2, SAM to AM)
  sigma.s2.to.mam.max <-  CompasDist[14]#0.351# relapsed from SAM to MAM (Kangas et al, 2023, table 2, SAM to AM)
  
  sigma.m2.to.mam.min <-  CompasDist[15]#0.212# relapsed from MAM to MAM (Kangas et al, 2023, table 2, MAM to AM)
  sigma.m2.to.mam.max <-  CompasDist[16]#0.318# relapsed from MAM to MAM (Kangas et al, 2023, table 2, MAM to AM)
  sigma.m2.to.sam.min <-  CompasDist[17]#0.001 # relapsed from MAM to SAM (Kangas et al, 2023, table S1, MAM to SAM)
  sigma.m2.to.sam.max <-  CompasDist[18]#0.027 # relapsed from MAM to SAM (Kangas et al, 2023, table S1, MAM to SAM)
  
  # ######## MANGO
  sigma.s3.to.sam.min <-  MangoDist[11]#0.024# relapsed from SAM to SAM https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002887
  sigma.s3.to.sam.max <-  MangoDist[12]#0.024# relapsed from SAM to SAM https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002887
  sigma.s3.to.mam.min <-  MangoDist[13]#0.0725# relapsed from SAM to MAM  assume to be same as standard protocol
  sigma.s3.to.mam.max <-  MangoDist[14]#0.226# relapsed from SAM to MAM   assume to be same as standard protocol
  
  sigma.m3.to.mam.min <- MangoDist[15]#0.1 # relapsed from MAM to MAM (Assumed to be same as standard protocol)
  sigma.m3.to.mam.max <- MangoDist[16]#0.3# relapsed from MAM to MAM (Assumed to be same as standard protocol)
  sigma.m3.to.sam.min <- MangoDist[17]#0 # relapsed from MAM to SAM (Assumed to be same as standard protocol)
  sigma.m3.to.sam.max <- MangoDist[18]#0.1 # relapsed from MAM to SAM (Assumed to be same as standard protocol)
  
  # beta = 1 - delta - sigma # stayed recovered 
  
  ## ######## Vary the relapse rate to be a wider range per Sarah's comment
  #s.to.sam.min <- s1.to.sam.min <- s2.to.sam.min <- s3.to.sam.min <-  0   # relapsed from SAM to SAM
  #s.to.sam.max <-  s1.to.sam.max <- s2.to.sam.max <- s3.to.sam.max <- 0.6 # relapsed from SAM to SAM
  
  ###################################################
  ###################################################
  
  ### ### ### time to recovery 
  dur.rec.s3.min <- mango[dosage==1 & am=="SAM" & exit_rsn == 1,]$min.los
  dur.rec.s3.max <- ifelse(mango[dosage==1 & am=="SAM" & exit_rsn == 1,]$max.los > 112, 112, mango[dosage==1 & am=="SAM" & exit_rsn == 1,]$max.los)
  
  dur.rec.s2.min <- 3*7
  dur.rec.s2.max <- 12*7
  
  dur.rec.s1.min <- 21
  dur.rec.s1.max <- 84
  
  dur.rec.s.min <- 21
  dur.rec.s.max <- 84
  
  dur.rec.m3.min <- mango[dosage==1 & am=="MAM" & exit_rsn == 1,]$min.los
  dur.rec.m3.max <- ifelse(mango[dosage==1 & am=="MAM" & exit_rsn == 1,]$max.los > 112, 112, mango[dosage==1 & am=="MAM" & exit_rsn == 1,]$max.los)
  
  dur.rec.m2.min <- 21
  dur.rec.m2.max <- 35
  
  dur.rec.m1.min <- 7*3
  dur.rec.m1.max <- 7*6
  
  dur.rec.m.min <- 7*5
  dur.rec.m.max <- 7*7
  
  ### ### ### time to default 
  dur.def.s3.min <- mango[dosage==1 & am=="SAM" & exit_rsn == 3,]$min.los
  dur.def.s3.max <- ifelse(mango[dosage==1 & am=="SAM" & exit_rsn == 3,]$max.los > 112, 112,mango[dosage==1 & am=="SAM" & exit_rsn == 3,]$max.los)
  
  dur.def.s2.min <- 34
  dur.def.s2.max <- 54
  
  dur.def.s1.min <- 28
  dur.def.s1.max <- 56
  
  dur.def.s.min <- 28
  dur.def.s.max <- 56
  
  dur.def.m3.min <- mango[dosage==1 & am=="MAM" & exit_rsn == 3,]$min.los
  dur.def.m3.max <- ifelse(mango[dosage==1 & am=="MAM" & exit_rsn == 3,]$max.los > 112, 112,mango[dosage==1 & am=="MAM" & exit_rsn == 3,]$max.los)
  
  dur.def.m2.min <- 16.8
  dur.def.m2.max <- 28
  
  dur.def.m1.min <- 16.8
  dur.def.m1.max <- 33.6
  
  dur.def.m.min <- 28
  dur.def.m.max <- 72.8
  
  ### ### ### time to died
  dur.died.s3.min <- 14
  dur.died.s3.max <- 28
  
  dur.died.s2.min <- 17
  dur.died.s2.max <- 27
  
  dur.died.s1.min <- 14
  dur.died.s1.max <- 28
  
  dur.died.s.min <- 14
  dur.died.s.max <- 28
  
  dur.died.m3.min <- mango[dosage==1 & am=="MAM" & exit_rsn == 5,]$min.los
  dur.died.m3.max <- ifelse(mango[dosage==1 & am=="MAM" & exit_rsn == 5,]$max.los > 112, 112,mango[dosage==1 & am=="MAM" & exit_rsn == 3,]$max.los)
  
  dur.died.m2.min <- 8.4
  dur.died.m2.max <- 14
  
  dur.died.m1.min <- 8.4
  dur.died.m1.max <- 16.8
  
  dur.died.m.min <- 14
  dur.died.m.max <- 36.4
  
  ### ### ### time to hospitalization 
  dur.hosp.s3.min <- mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$min.los
  dur.hosp.s3.max <- ifelse(mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$max.los > 112, 112,mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$max.los)
  
  dur.hosp.s2.min <- dur.hosp.s1.min <- dur.hosp.s.min <- 
    dur.hosp.m2.min <- dur.hosp.m1.min <- dur.hosp.m.min <-3
  
  dur.hosp.s2.max <- dur.hosp.s1.max <- dur.hosp.s.max <- 
    dur.hosp.m2.max <- dur.hosp.m1.max <- dur.hosp.m.max <- 7
  
  dur.hosp.m3.min <- mango[dosage==1 & am=="MAM" & exit_rsn == 2,]$min.los
  dur.hosp.m3.max <- ifelse(mango[dosage==1 & am=="MAM" & exit_rsn == 2,]$max.los > 112, 112,mango[dosage==1 & am=="MAM" & exit_rsn == 2,]$max.los)
  
  ### ### ### time to transfer 
  dur.transf.s3.min <- mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$min.los
  dur.transf.s3.max <- ifelse(mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$max.los > 112, 112, mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$max.los)
  
  dur.transf.s2.min <- dur.transf.s1.min <- dur.transf.s.min <- 
    dur.transf.m2.min <- dur.transf.m1.min <- dur.transf.m.min <- 3
  
  dur.transf.s2.max <- dur.transf.s1.max <- dur.transf.s.max <- 
    dur.transf.m2.max <- dur.transf.m1.max <- dur.transf.m.max <- 7
  
  dur.transf.m3.min <- 3
  dur.transf.m3.max <- 7
  
  ### ### ### time to never recovered
  dur.never.rec.s3.min <- mango[dosage==1 & am=="SAM" & exit_rsn == 6,]$min.los
  dur.never.rec.s3.max <- ifelse(mango[dosage==1 & am=="SAM" & exit_rsn == 6,]$max.los > 112, 112, mango[dosage==1 & am=="SAM" & exit_rsn == 6,]$max.los)
  
  dur.never.rec.s2 <- dur.never.rec.s1 <- dur.never.rec.s <- 
    dur.never.rec.m2 <- dur.never.rec.m1 <- dur.never.rec.m <- 112
  
  dur.never.rec.m3.min <- mango[dosage==1 & am=="MAM" & exit_rsn == 6,]$min.los
  dur.never.rec.m3.max <- ifelse(mango[dosage==1 & am=="MAM" & exit_rsn == 6,]$max.los > 112, 112, mango[dosage==1 & am=="MAM" & exit_rsn == 6,]$max.los)
  
  ### ### ### length of stay in inpatient care in days
  hosp.stay.s.min = hosp.stay.m.min = 3 
  hosp.stay.s.max = hosp.stay.m.max = 7
  
  hosp.stay.m.min = 5 
  hosp.stay.m.max = 8
  
  hosp.stay.m1.min = 4
  hosp.stay.m1.max = 7
  
  hosp.stay.s1.min = hosp.stay.s2.min = hosp.stay.s3.min = 
    hosp.stay.m2.min = hosp.stay.m3.min = 3
  
  hosp.stay.s1.max = hosp.stay.s2.max = hosp.stay.s3.max = 
    hosp.stay.m2.max = hosp.stay.m3.max = 5
  
  ### time for MAM children who regress to SAM during treatment, unpublished study in South Sudan
  
  dur.m.reg.sam.min <- dur.m.reg.sam1.min <- dur.m.reg.sam2.min <- dur.m.reg.sam3.min <- 10
  dur.m.reg.sam.max <- dur.m.reg.sam1.max <- dur.m.reg.sam2.max <- dur.m.reg.sam3.max <- 20
  
  ### ### ### time to recovery post-hosp
  ## hosp.stay + dur.rec
  
  ### ### ### time to death post-hosp
  ## hosp.stay + dur.died
  
  ### ### ### time to stay wasted post-hosp
  ## dur.never.rec - hosp.stay - dur.hosp
  
  
  ### ### ### ### ### ### time to outcomes post SAM to SAM relapse
  ## time to relapse 6 months
  ## we assume time to recovery is 15% more post relapse
  ## prss means post relapse sam to sam
  ## prmm for mam means post relapse mam to mam
  
  ### ### rec
  dur.rec.s3.prss.min <- (1+reducedRecovery)*dur.rec.s3.min
  dur.rec.s3.prss.max <- ifelse((1+reducedRecovery)*dur.rec.s3.max > 112, 112,(1+reducedRecovery)*dur.rec.s3.max)
  
  dur.rec.s2.prss.min <- (1+reducedRecovery)*dur.rec.s2.min
  dur.rec.s2.prss.max <- (1+reducedRecovery)*dur.rec.s2.max
  
  dur.rec.s1.prss.min <- (1+reducedRecovery)*dur.rec.s1.min
  dur.rec.s1.prss.max <- (1+reducedRecovery)*dur.rec.s1.max
  
  dur.rec.s.prss.min <- (1+reducedRecovery)*dur.rec.s.min
  dur.rec.s.prss.max <- (1+reducedRecovery)*dur.rec.s.max
  
  dur.rec.m3.prsm.min <- (1+reducedRecovery)*dur.rec.m3.min
  dur.rec.m3.prsm.max <- ifelse((1+reducedRecovery)*dur.rec.m3.max > 112, 112,(1+reducedRecovery)*dur.rec.m3.max)
  
  dur.rec.m2.prsm.min <- (1+reducedRecovery)*dur.rec.m2.min
  dur.rec.m2.prsm.max <- (1+reducedRecovery)*dur.rec.m2.max
  
  dur.rec.m1.prsm.min <- (1+reducedRecovery)*dur.rec.m1.min
  dur.rec.m1.prsm.max <- (1+reducedRecovery)*dur.rec.m1.max
  
  dur.rec.m.prsm.min <- (1+reducedRecovery)*dur.rec.m.min
  dur.rec.m.prsm.max <- (1+reducedRecovery)*dur.rec.m.max
  
  ### ### def
  dur.def.s3.prss.min <- dur.def.s3.min
  dur.def.s3.prss.max <- dur.def.s3.max
  
  dur.def.s2.prss.min <- 39
  dur.def.s2.prss.max <- 62
  
  dur.def.s1.prss.min <- 32
  dur.def.s1.prss.max <- 64
  
  dur.def.s.prss.min <- 32
  dur.def.s.prss.max <- 64
  
  dur.def.m3.prsm.min <- dur.def.m3.min
  dur.def.m3.prsm.max <- dur.def.m3.max
  
  dur.def.m2.prsm.min <-  19
  dur.def.m2.prsm.max <-  32
  
  dur.def.m1.prsm.min <- 19
  dur.def.m1.prsm.max <- 38
  
  dur.def.m.prsm.min <- 32
  dur.def.m.prsm.max <- 83
  
  ### ### died
  dur.died.s3.prss.min <- dur.died.s3.min
  dur.died.s3.prss.max <- dur.died.s3.max
  
  dur.died.s2.prss.min <- 20
  dur.died.s2.prss.max <- 32
  
  dur.died.s1.prss.min <- 16
  dur.died.s1.prss.max <- 32
  
  dur.died.s.prss.min <- 16
  dur.died.s.prss.max <- 32
  
  dur.died.m3.prsm.min <- dur.died.m3.min
  dur.died.m3.prsm.max <- dur.died.m3.max
  
  dur.died.m2.prsm.min <- 9
  dur.died.m2.prsm.max <- 16
  
  dur.died.m1.prsm.min <- 9
  dur.died.m1.prsm.max <- 19
  
  dur.died.m.prsm.min <- 16
  dur.died.m.prsm.max <- 42
  
  ### ### ### time to never recovered
  dur.never.rec.s3.prss <- dur.never.rec.s2.prss <- dur.never.rec.s1.prss <- dur.never.rec.s.prss <- 
    dur.never.rec.m3.prsm <- dur.never.rec.m2.prsm <- dur.never.rec.m1.prsm <- dur.never.rec.m.prsm <- 
    dur.never.rec.m3.prmm <- dur.never.rec.m2.prmm <- dur.never.rec.m1.prmm <- dur.never.rec.m.prmm <- 
    dur.never.rec.m3.prms <- dur.never.rec.m2.prms <- dur.never.rec.m1.prms <- dur.never.rec.m.prms <- 112
  
  ### we assume time to hosp, transf, and length of stay in hosp, 
  ### time to rec post-hosp, time to stay wasted post-hosp and 
  ### time to death post-hosp is same as before relapse. 
  
  ###################################################
  ## Cost estimation
  ###################################################
  #pop = 5000
  
  # https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
  cpi <- 320.795 # Apr 2025
  
  #rutf.cost <- c(0.31*cpi/292.655 ,0.32*cpi/292.655,0.39*cpi/292.655,0.34*cpi/292.655,0.354*cpi/258.811 ,0.369*cpi/258.811)*1.15
  rutf.cost.min <- costDist.min[1]#min(c(0.31*cpi/292.655 ,0.32*cpi/292.655,0.39*cpi/292.655,0.34*cpi/292.655,0.354*cpi/258.811 ,0.369*cpi/258.811)*1.15)
  rutf.cost.max <- costDist.max[1]#max(c(0.31*cpi/292.655 ,0.32*cpi/292.655,0.39*cpi/292.655,0.34*cpi/292.655,0.354*cpi/258.811 ,0.369*cpi/258.811)*1.15)
  
  #rusf.cost <- c(0.3*cpi/292.655,0.38*cpi/292.655,0.35*cpi/292.655 ,0.387*cpi/258.811,0.438*cpi/258.811)*1.15
  rusf.cost.min <- costDist.min[2]#min(c(0.3*cpi/292.655,0.38*cpi/292.655,0.35*cpi/292.655 ,0.387*cpi/258.811,0.438*cpi/258.811)*1.15)
  rusf.cost.max <- costDist.max[2]#max(c(0.3*cpi/292.655,0.38*cpi/292.655,0.35*cpi/292.655 ,0.387*cpi/258.811,0.438*cpi/258.811)*1.15)
  
  # CSB cost per packet (1 packet is 1.5 kg)
  csb.cost.min <- costDist.min[10]#
  csb.cost.max <- costDist.max[10]#csb.cost <- c(2.17*cpi/292.655,4.23*cpi/292.655,1.36*cpi/224.939)*1.15
  
  #drug.cost1 <- 0.24*cpi/245.120
  drug.cost1.min <- costDist.min[3]#*cpi/245.120#0.14*cpi/245.120
  drug.cost1.max <- costDist.max[3]#*cpi/245.120#0.26*cpi/245.120
  
  #drug.cost <- 0.22*cpi/245.120
  drug.cost.min <- costDist.min[3]#*cpi/245.120#0.14*cpi/245.120
  drug.cost.max <- costDist.max[3]#*cpi/245.120#0.24*cpi/245.120
  
  ##labor.cost <- 1.57*cpi/258.811 # labor costs (outpatient) per visit DRC, ALIMA
  #labor.cost.min <- costDist.min[4]#0.3*cpi/245.120 # MANGO
  #labor.cost.max <- costDist.max[4]#3.5*cpi/245.120 # MANGO
  
  #nonlabor.cost <- (1.2*cpi/258.811)/30 # non labor costs (outpatient) per child visit per month (consulation) DRC, ALIMA
  nonlabor.cost.min <- (costDist.min[5])/30#*cpi/258.811)/30#(1.2*cpi/258.811)/30
  nonlabor.cost.max <- (costDist.max[5])/30#*cpi/258.811)/30#(1.2*cpi/258.811)/30
  
  ## caregiver cost
  # Caregiver opportunity costs for outpatient visits
  opportunity.cost.min <- costDist.min[7]#*cpi/258.811#0.06*cpi/258.811
  opportunity.cost.max <- costDist.max[7]#*cpi/245.120#2.01*cpi/245.120
  
  # Caregiver transportation and food costs for outpatient visits
  transport.cost.min <- costDist.min[6]#*cpi/245.120#0.67*cpi/245.120
  transport.cost.max <- costDist.max[6]#*cpi/305.691#1.41*cpi/305.691
  
  #inp.hosp.cost.min <- costDist.min[8]#34.72*cpi/258.811 #Total cost of inpatient stay per day per child hospitalized DRC, ALIMA
  #inp.hosp.cost.max <- costDist.max[8]
  
  inp.opp.med.suppl.cost.min <- costDist.min[9]+costDist.min[7]#(9.51+1.53)*cpi/258.811 #medications / supplies and opportunity cost of inpatient stay per day per child hospitalized DRC, ALIMA
  inp.opp.med.suppl.cost.max <- costDist.max[9]+costDist.max[7]
  ############ 
  ############ WHO Choice https://www.who.int/teams/health-systems-governance-and-financing/economic-analysis/costing-and-technical-efficiency/quantities-and-unit-prices-(cost-inputs)/econometric-estimation-of-who-choice-country-specific-costs-for-inpatient-and-outpatient-health-service-delivery
  ############ 
  # unit cost values for service delivery at country and regional level. 
  # Inpatient Unit Costs presents the estimated cost per hospital bed-day, excluding the cost of drugs and diagnostic tests but including costs such as personnel, capital and food costs.
  # Outpatient Unit Costs presents the estimated cost per outpatient visit, and include all cost components except drugs and diagnostics. 
  # Estimates have been updated to 2010 international dollars, local currency units and US$, with 95% confidence intervals.
  # cost includes Drugs, salaries, lab, food, other costs
  
  ## For Ethiopia from the WHO CHOICE pdf file; 2010 PPP$I
  
  # # health center with outpatient services only (without bed); 
  # hc.outp.cost.min <- 0.37
  # hc.outp.cost.max  <- 5.87
  # 
  # # health center with bed, 
  # hc.b.outp.cost.min <- 0.52
  # hc.b.outp.cost.max  <- 7.14
  # 
  # # primary-level hospital for treating basic cases (district hospital), 
  # pr.inp.cost.min <- 2.91
  # pr.inp.cost.max  <- 17.06
  # 
  # pr.outp.cost.min <- 0.52
  # pr.outp.cost.max  <- 8.54
  # 
  # # secondary-level hospital for treating referral cases (specialist hospital), 
  # sec.inp.cost.min <- 3.25
  # sec.inp.cost.max  <- 18.11
  # 
  # sec.outp.cost.min <- 0.57
  # sec.outp.cost.max  <- 9.17
  # 
  # # teaching hospital primarily for treating referral cases with teaching component
  # tert.inp.cost.min <- 4.14
  # tert.inp.cost.max  <- 22.96
  # 
  # tert.outp.cost.min <- 0.53
  # tert.outp.cost.max  <- 8.13
  # 
  
  # get PPP conversation factor per country https://databank.worldbank.org/source/world-development-indicators/Series/PA.NUS.PPP#
  #ppp <- data.table(read.csv("ppp-conversion-factor.csv"))
  ppp.country <- as.numeric(ppp[Country.Name==country.name,]$X2010)
  
  ## exchange rate from Worldbank
  #exch <- data.table(read.csv("exchange-rate.csv"))
  exch.country <- as.numeric(exch[Country.Name==country.name,]$X2010)
  
  ## For inpatient & outpatient care: I assumed it's primary hospital (district hospital)
  pr.inp.cost.min <- costDist.min[8]#(costDist.min[8] * ppp.country /exch.country) *cpi/218.056
  pr.inp.cost.max  <- costDist.max[8]#(costDist.max[8] * ppp.country /exch.country) *cpi/218.056
  
  pr.outp.cost.min <- costDist.min[4]#(costDist.min[4] * ppp.country /exch.country) *cpi/218.056
  pr.outp.cost.max  <- costDist.max[4]#(costDist.max[4] * ppp.country /exch.country) *cpi/218.056
  
  ## the labor and food cost comes from WHO-CHOICE estimate and we include non-labor, drug, sachets, transport and opportunity costs  in the analysis
  
  ############ 
  ############ increase in recovery rate & time to recovery with increase in weight at admission and increase in MUAC at admission
  ############ 
  
  # Standard Protocol
  #  CHR: 1.53, 95% CI (1.12, 2.08) 12% to 108% increase in recovery rate with with admission weight ≥ 6.5 Kg than <6.5 kg (Mengesha et al, 2016)
  #  CHR: 2.094, 95 % CI: 1.627–2.694: 62.7% to 169.4% increase in recovery rate with 1 cm increase in MUAC (Gebremichael, 2015)
  #  CHR: 1.85, 95 % CI: 1.5–2.28 50% to 128% faster recovery with admission weight >7kg than admission weight <7kg (Yadeta et al, 2024)
  #  CHR: 1.67, 95 % CI: 1.24–2.24: 24% to 124% faster recovery with 115 ≤ MUAC ≤ 125 mm than MUAC < 115 mm (Yadeta et al, 2024)
  
  # OptimA Protocol: Daures et al, 2020 in Burkina Faso
  #            MUAC <115 mm or oedema MUAC 115–119 mm MUAC 120–124 mm (n 3064)
  #Recovered (%)	70.4, (67.5-73.5)	  84.1, (82.1-86.2)	 91.4, (90.4-92.2)
  #Deceased	      1.5, (0.0-4.5)	    0.6, (0.0-2.6)	   0.1, (0.0-1.0)
  #Defaulted	    9.3, (6.4-12.4)	    5.5, (3.6-7.6)	   3.2, (2.2-4.1)
  #Non-responder	10.6, (7.6-13.6)	  4.3, (2.3-6.4)	   1.2, (0.2-2.1)
  #Transferred	  0.5, (0.0-3.5)	    0.1, (0.0-2.2)	  0.2, (0.0-1.1)
  #Average time to 
  #recovery (week) 8.1 ± 3.4	        6.5 ± 2.7	        5.2 ± 1.9
  #(mean ± sd)	
  
  
  # ComPAS Protocol: Kangas et al, 2022 in Mali
  # •	MUAC < 110 mm → 81.6% recovery rate; 63 length of stay 
  # •	MUAC ≥ 110 mm → 89.9% recovery rate; 49 length of stay
  # •	Weight at admission ≤ 7 kg → 86.5% recovery rate; 56 length of stay
  # •	Weight at admission > 7 kg → 88.6% recovery rate; 48 length of stay
  
  # MANGO protocol:Kangas et al, 2022 in Burkina Faso:
  # •	1 mm increase in MUAC → 1% faster recovery rate (CHR: 1.01, 95 % CI: 1.00–1.03)
  # we assume the rest is same as standard protocol
  
  ##### ##### 
  ##### Standard Protocol multipliers
  ##### ##### 
  
  ## we make an average of CHR for a combination of MUAC and weight at admission
  
  wm.alpha.s.min <- wm.alpha.m.min <- mean(c(1.12,1.627)) #admission weight ≥ 6.5 Kg than <6.5 kg & 1 cm increase in MUAC 
  wm.alpha.s.max <- wm.alpha.m.max <- mean(c(2.694,2.08)) #admission weight ≥ 6.5 Kg than <6.5 kg & 1 cm increase in MUAC
  
  wm.dur.rec.s.min <- wm.dur.rec.m.min <- mean(c(1.5,1.24)) #admission weight >= 7kg than admission weight < 7kg & 115 ≤ MUAC ≤ 125 mm than MUAC < 115 mm 
  wm.dur.rec.s.max <- wm.dur.rec.m.max <- mean(c(2.28,2.24)) #admission weight >7kg than admission weight <7kg & 115 ≤ MUAC ≤ 125 mm than MUAC < 115 mm 
  
  ##### ##### 
  ##### ##### OptimA Protocol multipliers
  ##### ##### 
  
  ## we assume recovery rate and time to recovery change with weight at admission for OptimA changes at the same rate as standard protocol
  
  wm.alpha.s1.min <- wm.alpha.m1.min <- mean(c(1.12,86.2/73.5)) #admission weight ≥ 6.5 Kg than <6.5 kg & muac 115–119 mm vs. muac < 115 mm
  wm.alpha.s1.max <- wm.alpha.m1.max <- mean(c(2.08,82.1/67.5)) #admission weight ≥ 6.5 Kg than <6.5 kg & muac 115–119 mm vs. muac < 115 mm
  
  wm.dur.rec.s1 <- wm.dur.rec.m1 <- mean(c(1.85,8.1/6.5)) #admission weight >= 7kg than admission weight < 7kg & muac 115–119 mm vs. muac < 115 mm
  
  ##### ##### 
  ##### ##### ComPAS Protocol multipliers
  ##### ##### 
  
  wm.alpha.s2 <- wm.alpha.m2 <- mean(c(88.6/86.5,89.9/81.6)) 
  
  wm.dur.rec.s2 <- wm.dur.rec.m2 <- mean(c(56/48,63/49)) #admission weight > 7 kg than ≤ 7 kg & MUAC ≥ 110 mm than MUAC < 110 mm 
  
  ##### ##### 
  ##### ##### MANGO Protocol multipliers
  ##### ##### 
  
  wm.alpha.s3.min <- wm.alpha.m3.min <- mean(c(1.12,1.627,1.00)) #admission weight ≥ 6.5 Kg than <6.5 kg & 1 cm increase in MUAC 
  wm.alpha.s3.max <- wm.alpha.m3.min <- mean(c(2.694,2.08,1.03)) #admission weight ≥ 6.5 Kg than <6.5 kg & 1 cm increase in MUAC
  
  wm.dur.rec.s3.min <- wm.dur.rec.m3.min <- mean(c(1.5,1.24)) #admission weight >= 7kg than admission weight < 7kg & 115 ≤ MUAC ≤ 125 mm than MUAC < 115 mm 
  wm.dur.rec.s3.max <- wm.dur.rec.m3.max <- mean(c(2.28,2.24)) #admission weight >7kg than admission weight <7kg & 115 ≤ MUAC ≤ 125 mm than MUAC < 115 mm 
  
  ###################################################
  ################################################### Latin Hypercube for model parameters & costs
  ###################################################
  lhs<-maximinLHS(h,53)   #simulate
  
  params <- cbind(
    alpha.s3 = lhs[,1]*(0.2*alpha.s3)+alpha.s3*0.9,
    eps.s3 = lhs[,2]*(0.2*eps.s3)+eps.s3*0.9,
    eta.s3 = lhs[,3]*(0.2*eta.s3)+eta.s3*0.9,
    theta.s3 = lhs[,4]*(0.2*theta.s3)+theta.s3*0.9,
    phi.s3 = lhs[,5]*(0.2*phi.s3)+phi.s3*0.9,
    theta2.s3 = lhs[,6]*(0.2*theta2.s3)+theta2.s3*0.9,
    eta2.s3 = lhs[,7]*(0.2*eta2.s3)+eta2.s3*0.9,
    delta.s3 = lhs[,8]*(delta.s3.max-delta.s3.min)+delta.s3.min,
    sigma.s3.to.mam = lhs[,9]*(sigma.s3.to.mam.max-sigma.s3.to.mam.min)+sigma.s3.to.mam.min,
    sigma.s3.to.sam = lhs[,10]*(sigma.s3.to.sam.max-sigma.s3.to.sam.min)+sigma.s3.to.sam.min,
    
    alpha.s2 = lhs[,1]*(0.2*alpha.s2)+alpha.s2*0.9,
    eps.s2 = lhs[,2]*(0.2*eps.s2)+eps.s2*0.9,
    eta.s2 = lhs[,3]*(0.2*eta.s2)+eta.s2*0.9,
    theta.s2 = lhs[,4]*(0.2*theta.s2)+theta.s2*0.9,
    phi.s2 = lhs[,5]*(0.2*phi.s2)+phi.s2*0.9,
    theta2.s2 = lhs[,6]*(0.2*theta2.s2)+theta2.s2*0.9,
    eta2.s2 = lhs[,7]*(0.2*eta2.s2)+eta2.s2*0.9,
    delta.s2 = lhs[,8]*(delta.s2.max-delta.s2.min)+delta.s2.min,
    sigma.s2.to.mam = lhs[,9]*(sigma.s2.to.mam.max-sigma.s2.to.mam.min)+sigma.s2.to.mam.min,
    sigma.s2.to.sam = lhs[,10]*(sigma.s2.to.sam.max-sigma.s2.to.sam.min)+sigma.s2.to.sam.min,
    
    alpha.s1 = lhs[,1]*(0.2*alpha.s1)+alpha.s1*0.9,
    eps.s1 = lhs[,2]*(0.2*eps.s1)+eps.s1*0.9,
    eta.s1 = lhs[,3]*(0.2*eta.s1)+eta.s1*0.9,
    theta.s1 = lhs[,4]*(0.2*theta.s1)+theta.s1*0.9,
    phi.s1 = lhs[,5]*(0.2*phi.s1)+phi.s1*0.9,
    theta2.s1 = lhs[,6]*(0.2*theta2.s1)+theta2.s1*0.9,
    eta2.s1 = lhs[,7]*(0.2*eta2.s1)+eta2.s1*0.9,
    delta.s1 = lhs[,8]*(delta.s1.max-delta.s1.min)+delta.s1.min,
    sigma.s1.to.mam = lhs[,9]*(sigma.s1.to.mam.max-sigma.s1.to.mam.min)+sigma.s1.to.mam.min,
    sigma.s1.to.sam = lhs[,10]*(sigma.s1.to.sam.max-sigma.s1.to.sam.min)+sigma.s1.to.sam.min,
    
    alpha.s = lhs[,1]*(0.2*alpha.s)+alpha.s*0.9,
    eps.s = lhs[,2]*(0.2*eps.s)+eps.s*0.9,
    eta.s = lhs[,3]*(0.2*eta.s)+eta.s*0.9,
    theta.s = lhs[,4]*(0.2*theta.s)+theta.s*0.9,
    phi.s = lhs[,5]*(0.2*phi.s)+phi.s*0.9,
    theta2.s = lhs[,6]*(0.2*theta2.s1)+theta2.s1*0.9,
    eta2.s = lhs[,7]*(0.2*eta2.s)+eta2.s*0.9,
    delta.s = lhs[,8]*(delta.s.max-delta.s.min)+delta.s.min,
    sigma.s.to.mam = lhs[,9]*(sigma.s.to.mam.max-sigma.s.to.mam.min)+sigma.s.to.mam.min,
    sigma.s.to.sam = lhs[,10]*(sigma.s.to.sam.max-sigma.s.to.sam.min)+sigma.s.to.sam.min,
    
    dur.rec.s3 = lhs[,11]*(dur.rec.s3.max-dur.rec.s3.min)+dur.rec.s3.min,  
    dur.def.s3 = lhs[,12]*(dur.def.s3.max-dur.def.s3.min)+dur.def.s3.min,
    dur.died.s3 = lhs[,13]*(dur.died.s3.max-dur.died.s3.min)+dur.died.s3.min,
    dur.hosp.s3 = lhs[,14]*(dur.hosp.s3.max-dur.hosp.s3.min)+dur.hosp.s3.min,  
    dur.transf.s3 = lhs[,15]*(dur.transf.s3.max-dur.transf.s3.min)+dur.transf.s3.min,  
    hosp.stay.s3 = lhs[,16]*(hosp.stay.s3.max-hosp.stay.s3.min)+hosp.stay.s3.min,
    dur.rec.s3.prss = lhs[,17]*(dur.rec.s3.prss.max-dur.rec.s3.prss.min)+dur.rec.s3.prss.min,  
    dur.def.s3.prss = lhs[,18]*(dur.def.s3.prss.max-dur.def.s3.prss.min)+dur.def.s3.prss.min,  
    dur.died.s3.prss = lhs[,19]*(dur.died.s3.prss.max-dur.died.s3.prss.min)+dur.died.s3.prss.min,  
    
    dur.rec.s2 = lhs[,11]*(dur.rec.s2.max-dur.rec.s2.min)+dur.rec.s2.min,  
    dur.def.s2 = lhs[,12]*(dur.def.s2.max-dur.def.s2.min)+dur.def.s2.min,
    dur.died.s2 = lhs[,13]*(dur.died.s2.max-dur.died.s2.min)+dur.died.s2.min,
    dur.hosp.s2 = lhs[,14]*(dur.hosp.s2.max-dur.hosp.s2.min)+dur.hosp.s2.min,  
    dur.transf.s2 = lhs[,15]*(dur.transf.s2.max-dur.transf.s2.min)+dur.transf.s2.min,  
    hosp.stay.s2 = lhs[,16]*(hosp.stay.s2.max-hosp.stay.s2.min)+hosp.stay.s2.min,
    dur.rec.s2.prss = lhs[,17]*(dur.rec.s2.prss.max-dur.rec.s2.prss.min)+dur.rec.s2.prss.min,  
    dur.def.s2.prss = lhs[,18]*(dur.def.s2.prss.max-dur.def.s2.prss.min)+dur.def.s2.prss.min,  
    dur.died.s2.prss = lhs[,19]*(dur.died.s2.prss.max-dur.died.s2.prss.min)+dur.died.s2.prss.min,  
    
    dur.rec.s1 = lhs[,11]*(dur.rec.s1.max-dur.rec.s1.min)+dur.rec.s1.min,  
    dur.def.s1 = lhs[,12]*(dur.def.s1.max-dur.def.s1.min)+dur.def.s1.min,
    dur.died.s1 = lhs[,13]*(dur.died.s1.max-dur.died.s1.min)+dur.died.s1.min,
    dur.hosp.s1 = lhs[,14]*(dur.hosp.s1.max-dur.hosp.s1.min)+dur.hosp.s1.min,  
    dur.transf.s1 = lhs[,15]*(dur.transf.s1.max-dur.transf.s1.min)+dur.transf.s1.min,  
    hosp.stay.s1 = lhs[,16]*(hosp.stay.s1.max-hosp.stay.s1.min)+hosp.stay.s1.min,
    dur.rec.s1.prss = lhs[,17]*(dur.rec.s1.prss.max-dur.rec.s1.prss.min)+dur.rec.s1.prss.min,  
    dur.def.s1.prss = lhs[,18]*(dur.def.s1.prss.max-dur.def.s1.prss.min)+dur.def.s1.prss.min,  
    dur.died.s1.prss = lhs[,19]*(dur.died.s1.prss.max-dur.died.s1.prss.min)+dur.died.s1.prss.min,  
    
    dur.rec.s = lhs[,11]*(dur.rec.s.max-dur.rec.s.min)+dur.rec.s.min,  
    dur.def.s = lhs[,12]*(dur.def.s.max-dur.def.s.min)+dur.def.s.min,
    dur.died.s = lhs[,13]*(dur.died.s.max-dur.died.s.min)+dur.died.s.min,
    dur.hosp.s = lhs[,14]*(dur.hosp.s.max-dur.hosp.s.min)+dur.hosp.s.min,  
    dur.transf.s = lhs[,15]*(dur.transf.s.max-dur.transf.s.min)+dur.transf.s.min,  
    hosp.stay.s = lhs[,16]*(hosp.stay.s.max-hosp.stay.s.min)+hosp.stay.s.min,
    dur.rec.s.prss = lhs[,17]*(dur.rec.s.prss.max-dur.rec.s.prss.min)+dur.rec.s.prss.min,  
    dur.def.s.prss = lhs[,18]*(dur.def.s.prss.max-dur.def.s.prss.min)+dur.def.s.prss.min,  
    dur.died.s.prss = lhs[,19]*(dur.died.s.prss.max-dur.died.s.prss.min)+dur.died.s.prss.min,  
    
    cost.rutf = lhs[,20]*(rutf.cost.max-rutf.cost.min)+rutf.cost.min,
    cost.rusf = lhs[,21]*(rusf.cost.max-rusf.cost.min)+rusf.cost.min,
    cost.drug = lhs[,22]*(drug.cost.max-drug.cost.min)+drug.cost.min,
    cost.labor = lhs[,23]*(pr.outp.cost.max-pr.outp.cost.min)+pr.outp.cost.min,
    cost.nonlabor = lhs[,24]*(nonlabor.cost.max-nonlabor.cost.min)+nonlabor.cost.min,
    cost.opportunity = lhs[,25]*(opportunity.cost.max-opportunity.cost.min)+opportunity.cost.min,
    cost.transport = lhs[,26]*(transport.cost.max-transport.cost.min)+transport.cost.min,
    cost.hosp1 = lhs[,27]*(inp.opp.med.suppl.cost.max-inp.opp.med.suppl.cost.min) + inp.opp.med.suppl.cost.min,
    cost.hosp2 = lhs[,28]*(pr.inp.cost.max-pr.inp.cost.min)+pr.inp.cost.min,
    cost.drug1 = lhs[,29]*(drug.cost1.min-drug.cost1.max)+drug.cost1.min,
    
    alpha.m3 = lhs[,30]*(0.2*alpha.m3)+alpha.m3*0.9,
    eps.m3 = lhs[,31]*(0.2*eps.m3)+eps.m3*0.9,
    eta.m3 = lhs[,32]*(0.2*eta.m3)+eta.m3*0.9,
    theta.m3 = lhs[,33]*(0.2*theta.m3)+theta.m3*0.9,
    phi.m3 = lhs[,34]*(0.2*phi.m3)+phi.m3*0.9,
    theta2.m3 = lhs[,35]*(0.2*theta2.m3)+theta2.m3*0.9,
    eta2.m3 = lhs[,36]*(0.2*eta2.m3)+eta2.m3*0.9,
    delta.m3 = lhs[,37]*(delta.m3.max-delta.m3.min)+delta.m3.min,
    sigma.m3.to.mam = lhs[,38]*(sigma.m3.to.mam.max-sigma.m3.to.mam.min)+sigma.m3.to.mam.min,
    sigma.m3.to.sam = lhs[,39]*(sigma.m3.to.sam.max-sigma.m3.to.sam.min)+sigma.m3.to.sam.min,
    tau.m = lhs[,40]*(tau.m.max-tau.m.min)+tau.m.max,
    
    alpha.m2 = lhs[,30]*(0.2*alpha.m2)+alpha.m2*0.9,
    eps.m2 = lhs[,31]*(0.2*eps.m2)+eps.m2*0.9,
    eta.m2 = lhs[,32]*(0.2*eta.m2)+eta.m2*0.9,
    theta.m2 = lhs[,33]*(0.2*theta.m2)+theta.m2*0.9,
    phi.m2 = lhs[,34]*(0.2*phi.m2)+phi.m2*0.9,
    theta2.m2 = lhs[,35]*(0.2*theta2.m2)+theta2.m2*0.9,
    eta2.m2 = lhs[,36]*(0.2*eta2.m2)+eta2.m2*0.9,
    delta.m2 = lhs[,37]*(delta.m2.max-delta.m2.min)+delta.m2.min,
    sigma.m2.to.mam = lhs[,38]*(sigma.m2.to.mam.max-sigma.m2.to.mam.min)+sigma.m2.to.mam.min,
    sigma.m2.to.sam = lhs[,39]*(sigma.m2.to.sam.max-sigma.m2.to.sam.min)+sigma.m2.to.sam.min,
    
    alpha.m1 = lhs[,30]*(0.2*alpha.m1)+alpha.m1*0.9,
    eps.m1 = lhs[,31]*(0.2*eps.m1)+eps.m1*0.9,
    eta.m1 = lhs[,32]*(0.2*eta.m1)+eta.m1*0.9,
    theta.m1 = lhs[,33]*(0.2*theta.m1)+theta.m1*0.9,
    phi.m1 = lhs[,34]*(0.2*phi.m1)+phi.m1*0.9,
    theta2.m1 = lhs[,35]*(0.2*theta2.m1)+theta2.m1*0.9,
    eta2.m1 = lhs[,36]*(0.2*eta2.m1)+eta2.m1*0.9,
    delta.m1 = lhs[,37]*(delta.m1.max-delta.m1.min)+delta.m1.min,
    sigma.m1.to.mam = lhs[,38]*(sigma.m1.to.mam.max-sigma.m1.to.mam.min)+sigma.m1.to.mam.min,
    sigma.m1.to.sam = lhs[,39]*(sigma.m1.to.sam.max-sigma.m1.to.sam.min)+sigma.m1.to.sam.min,
    
    alpha.m = lhs[,30]*(0.2*alpha.m)+alpha.m*0.9,
    eps.m = lhs[,31]*(0.2*eps.m)+eps.m*0.9,
    eta.m = lhs[,32]*(0.2*eta.m)+eta.m*0.9,
    theta.m = lhs[,33]*(0.2*theta.m)+theta.m*0.9,
    phi.m = lhs[,34]*(0.2*phi.m)+phi.m*0.9,
    theta2.m = lhs[,35]*(0.2*theta2.m1)+theta2.m1*0.9,
    eta2.m = lhs[,36]*(0.2*eta2.m)+eta2.m*0.9,
    delta.m = lhs[,37]*(delta.m.max-delta.m.min)+delta.m.min,
    sigma.m.to.mam = lhs[,38]*(sigma.m.to.mam.max-sigma.m.to.mam.min)+sigma.m.to.mam.min,
    sigma.m.to.sam = lhs[,39]*(sigma.m.to.sam.max-sigma.m.to.sam.min)+sigma.m.to.sam.min,
    
    dur.rec.m3 = lhs[,41]*(dur.rec.m3.max-dur.rec.m3.min)+dur.rec.m3.min,  
    dur.def.m3 = lhs[,42]*(dur.def.m3.max-dur.def.m3.min)+dur.def.m3.min,
    dur.died.m3 = lhs[,43]*(dur.died.m3.max-dur.died.m3.min)+dur.died.m3.min,
    dur.hosp.m3 = lhs[,44]*(dur.hosp.m3.max-dur.hosp.m3.min)+dur.hosp.m3.min,  
    dur.transf.m3 = lhs[,45]*(dur.transf.m3.max-dur.transf.m3.min)+dur.transf.m3.min,  
    hosp.stay.m3 = lhs[,46]*(hosp.stay.m3.max-hosp.stay.m3.min)+hosp.stay.m3.min,
    dur.rec.m3.prsm = lhs[,47]*(dur.rec.m3.prsm.max-dur.rec.m3.prsm.min)+dur.rec.m3.prsm.min,  
    dur.def.m3.prsm = lhs[,48]*(dur.def.m3.prsm.max-dur.def.m3.prsm.min)+dur.def.m3.prsm.min,  
    dur.died.m3.prsm = lhs[,49]*(dur.died.m3.prsm.max-dur.died.m3.prsm.min)+dur.died.m3.prsm.min,  
    
    dur.rec.m2 = lhs[,41]*(dur.rec.m2.max-dur.rec.m2.min)+dur.rec.m2.min,  
    dur.def.m2 = lhs[,42]*(dur.def.m2.max-dur.def.m2.min)+dur.def.m2.min,
    dur.died.m2 = lhs[,43]*(dur.died.m2.max-dur.died.m2.min)+dur.died.m2.min,
    dur.hosp.m2 = lhs[,44]*(dur.hosp.m2.max-dur.hosp.m2.min)+dur.hosp.m2.min,  
    dur.transf.m2 = lhs[,45]*(dur.transf.m2.max-dur.transf.m2.min)+dur.transf.m2.min,  
    hosp.stay.m2 = lhs[,46]*(hosp.stay.m2.max-hosp.stay.m2.min)+hosp.stay.m2.min,
    dur.rec.m2.prsm = lhs[,47]*(dur.rec.m2.prsm.max-dur.rec.m2.prsm.min)+dur.rec.m2.prsm.min,  
    dur.def.m2.prsm = lhs[,48]*(dur.def.m2.prsm.max-dur.def.m2.prsm.min)+dur.def.m2.prsm.min,  
    dur.died.m2.prsm = lhs[,49]*(dur.died.m2.prsm.max-dur.died.m2.prsm.min)+dur.died.m2.prsm.min,  
    
    dur.rec.m1 = lhs[,41]*(dur.rec.m1.max-dur.rec.m1.min)+dur.rec.m1.min,  
    dur.def.m1 = lhs[,42]*(dur.def.m1.max-dur.def.m1.min)+dur.def.m1.min,
    dur.died.m1 = lhs[,43]*(dur.died.m1.max-dur.died.m1.min)+dur.died.m1.min,
    dur.hosp.m1 = lhs[,44]*(dur.hosp.m1.max-dur.hosp.m1.min)+dur.hosp.m1.min,  
    dur.transf.m1 = lhs[,45]*(dur.transf.m1.max-dur.transf.m1.min)+dur.transf.m1.min,  
    hosp.stay.m1 = lhs[,46]*(hosp.stay.m1.max-hosp.stay.m1.min)+hosp.stay.m1.min,
    dur.rec.m1.prsm = lhs[,47]*(dur.rec.m1.prsm.max-dur.rec.m1.prsm.min)+dur.rec.m1.prsm.min,  
    dur.def.m1.prsm = lhs[,48]*(dur.def.m1.prsm.max-dur.def.m1.prsm.min)+dur.def.m1.prsm.min,  
    dur.died.m1.prsm = lhs[,49]*(dur.died.m1.prsm.max-dur.died.m1.prsm.min)+dur.died.m1.prsm.min,  
    
    dur.rec.m = lhs[,41]*(dur.rec.m.max-dur.rec.m.min)+dur.rec.m.min,  
    dur.def.m = lhs[,42]*(dur.def.m.max-dur.def.m.min)+dur.def.m.min,
    dur.died.m = lhs[,43]*(dur.died.m.max-dur.died.m.min)+dur.died.m.min,
    dur.hosp.m = lhs[,44]*(dur.hosp.m.max-dur.hosp.m.min)+dur.hosp.m.min,  
    dur.transf.m = lhs[,45]*(dur.transf.m.max-dur.transf.m.min)+dur.transf.m.min,  
    hosp.stay.m = lhs[,46]*(hosp.stay.m.max-hosp.stay.m.min)+hosp.stay.m.min,
    dur.rec.m.prsm = lhs[,47]*(dur.rec.m.prsm.max-dur.rec.m.prsm.min)+dur.rec.m.prsm.min,  
    dur.def.m.prsm = lhs[,48]*(dur.def.m.prsm.max-dur.def.m.prsm.min)+dur.def.m.prsm.min,  
    dur.died.m.prsm = lhs[,49]*(dur.died.m.prsm.max-dur.died.m.prsm.min)+dur.died.m.prsm.min,  
    
    wm.alpha.s3 = lhs[,50]*(wm.alpha.s3.max-wm.alpha.s3.min)+wm.alpha.s3.min,
    wm.dur.rec.s3 = lhs[,51]*(wm.dur.rec.s3.max-wm.dur.rec.s3.min)+wm.dur.rec.s3.min,
    
    wm.alpha.s2 = lhs[,50]*(0.2*wm.alpha.s2)+wm.alpha.s2*0.9,
    wm.dur.rec.s2 = lhs[,51]*(0.2*wm.dur.rec.s2)+wm.dur.rec.s2*0.9,
    
    wm.alpha.s1 = lhs[,50]*(wm.alpha.s1.max-wm.alpha.s1.min)+wm.alpha.s1.min,
    wm.dur.rec.s1 = lhs[,51]*(0.2*wm.dur.rec.s1)+wm.dur.rec.s1*0.9,
    
    wm.alpha.s = lhs[,50]*(wm.alpha.s.max-wm.alpha.s.min)+wm.alpha.s.min,
    wm.dur.rec.s = lhs[,51]*(wm.dur.rec.s.max-wm.dur.rec.s.min)+wm.dur.rec.s.min,
    
    dur.m.reg.sam = lhs[,52]*(dur.m.reg.sam.max - dur.m.reg.sam.min) + dur.m.reg.sam.min,
    
    cost.csb = lhs[,53]*(csb.cost.max - csb.cost.min) + csb.cost.min
    
  )   
  
  params <- data.table(params)
  
  params$cost.caregiver <- params$cost.opportunity + params$cost.transport
  params$cost.hosp <- params$cost.hosp1 + params$cost.hosp2
  
  # transferred
  params[,mu.s3 := (1 - alpha.s3 - eps.s3 - eta.s3 - theta.s3 - phi.s3)]
  params[,mu.s2 := (1 - alpha.s2 - eps.s2 - eta.s2 - theta.s2 - phi.s2)]
  params[,mu.s1 := (1 - alpha.s1 - eps.s1 - eta.s1 - theta.s1 - phi.s1)]
  params[,mu.s := (1 - alpha.s - eps.s - eta.s - theta.s - phi.s)]
  
  params[,alpha.s3 := ifelse(mu.s3 <0 , alpha.s3 + mu.s3,alpha.s3)]
  params[,alpha.s2 := ifelse(mu.s2 <0 , alpha.s2 + mu.s2,alpha.s2)]
  params[,alpha.s1 := ifelse(mu.s1 <0 , alpha.s1 + mu.s1,alpha.s1)]
  params[,alpha.s := ifelse(mu.s <0 , alpha.s + mu.s,alpha.s)]
  
  params$mu.s3 <- ifelse(params$mu.s3 <0, 0,params$mu.s3)
  params$mu.s2 <- ifelse(params$mu.s2 <0, 0,params$mu.s2)
  params$mu.s1 <- ifelse(params$mu.s1 <0, 0,params$mu.s1)
  params$mu.s <- ifelse(params$mu.s <0, 0,params$mu.s)
  
  # recovered post-hospitalization
  params[,alpha2.s3 := (1 - theta2.s3 - eta2.s3)]
  params[,alpha2.s2 := (1 - theta2.s2 - eta2.s2)]
  params[,alpha2.s1 := (1 - theta2.s1 - eta2.s1)]
  params[,alpha2.s := (1 - theta2.s - eta2.s)]
  
  # stayed recovered post-recovery
  params[,beta.s3 := (1 - delta.s3 - sigma.s3.to.mam - sigma.s3.to.sam)]
  params[,beta.s2 := (1 - delta.s2 - sigma.s2.to.mam - sigma.s2.to.sam)]
  params[,beta.s1 := (1 - delta.s1 - sigma.s1.to.mam - sigma.s1.to.sam)]
  params[,beta.s := (1 - delta.s - sigma.s.to.mam - sigma.s.to.sam)]
  
  # transferred
  params[,mu.m3 := (1 - alpha.m3 - eps.m3 - eta.m3 - theta.m3 - phi.m3 - tau.m)]
  params[,mu.m2 := (1 - alpha.m2 - eps.m2 - eta.m2 - theta.m2 - phi.m2 - tau.m)]
  params[,mu.m1 := (1 - alpha.m1 - eps.m1 - eta.m1 - theta.m1 - phi.m1 - tau.m)]
  params[,mu.m := (1 - alpha.m - eps.m - eta.m - theta.m - phi.m - tau.m)]
  
  params[,alpha.m3 := ifelse(mu.m3 <0 , alpha.m3 + mu.m3,alpha.m3)]
  params[,alpha.m2 := ifelse(mu.m2 <0 , alpha.m2 + mu.m2,alpha.m2)]
  params[,alpha.m1 := ifelse(mu.m1 <0 , alpha.m1 + mu.m1,alpha.m1)]
  params[,alpha.m := ifelse(mu.m <0 , alpha.m + mu.m,alpha.m)]
  
  params$mu.m3 <- ifelse(params$mu.m3 <0, 0,params$mu.m3)
  params$mu.m2 <- ifelse(params$mu.m2 <0, 0,params$mu.m2)
  params$mu.m1 <- ifelse(params$mu.m1 <0, 0,params$mu.m1)
  params$mu.m <- ifelse(params$mu.m <0, 0,params$mu.m)
  
  # recovered post-hospitalization
  params[,alpha2.m3 := (1 - theta2.m3 - eta2.m3)]
  params[,alpha2.m2 := (1 - theta2.m2 - eta2.m2)]
  params[,alpha2.m1 := (1 - theta2.m1 - eta2.m1)]
  params[,alpha2.m := (1 - theta2.m - eta2.m)]
  
  # stayed recovered post-recovery
  params[,beta.m3 := (1 - delta.m3 - sigma.m3.to.mam - sigma.m3.to.mam)]
  params[,beta.m2 := (1 - delta.m2 - sigma.m2.to.mam - sigma.m2.to.mam)]
  params[,beta.m1 := (1 - delta.m1 - sigma.m1.to.mam - sigma.m1.to.mam)]
  params[,beta.m := (1 - delta.m - sigma.m.to.mam - sigma.m.to.sam)]
  
  ###########
  ########### Post- SAM to SAM & SAM to MAM relapse
  ########### 
  ## assume recovery rate is 15% lower than the original recovery rate
  ## which means alpha.s, alpha.s1 and alpha.s2 all multipled by 0.85
  ## assumed never recovered outcomes will be the original value + 0.15*alpha.s
  ## assumed other outcomes will be same as the original outcomes
  ## prss means post relapse sam to sam
  ## prsm means post relapse sam to mam
  ## prmm means post relapse mam to mam
  ## prms means post relapse mam to sam
  
  ## relapse SAM to SAM
  
  params$alpha.s.prss <- (1-reducedRecovery)*params$alpha.s # recovered 
  params$eta.s.prss <- params$eta.s + reducedRecovery*params$alpha.s # never recovered
  
  params$alpha.s1.prss <- (1-reducedRecovery)*params$alpha.s1 # recovered 
  params$eta.s1.prss <- params$eta.s1 + reducedRecovery*params$alpha.s1 # never recovered
  
  params$alpha.s2.prss <- (1-reducedRecovery)*params$alpha.s2 # recovered 
  params$eta.s2.prss <- params$eta.s2 + reducedRecovery*params$alpha.s2 # never recovered
  
  params$alpha.s3.prss <- (1-reducedRecovery)*params$alpha.s3 # recovered 
  params$eta.s3.prss <- params$eta.s3 + reducedRecovery*params$alpha.s3 # never recovered
  
  ##### relapse SAM to MAM
  params$alpha.m.prsm <- (1-reducedRecovery)*params$alpha.m # recovered 
  params$eta.m.prsm <- params$eta.m + reducedRecovery*params$alpha.m # never recovered
  
  params$alpha.m1.prsm <- (1-reducedRecovery)*params$alpha.m1 # recovered 
  params$eta.m1.prsm <- params$eta.m1 + reducedRecovery*params$alpha.m1 # never recovered
  
  params$alpha.m2.prsm <- (1-reducedRecovery)*params$alpha.m2 # recovered 
  params$eta.m2.prsm <- params$eta.m2 + reducedRecovery*params$alpha.m2 # never recovered
  
  params$alpha.m3.prsm <- (1-reducedRecovery)*params$alpha.m3 # recovered 
  params$eta.m3.prsm <- params$eta.m3 + reducedRecovery*params$alpha.m3 # never recovered
  
  ## relapse MAM to SAM
  params$alpha.s.prms <- (1-reducedRecovery)*params$alpha.s # recovered 
  params$eta.s.prms <- params$eta.s + reducedRecovery*params$alpha.s # never recovered
  
  params$alpha.s1.prms <- (1-reducedRecovery)*params$alpha.s1 # recovered 
  params$eta.s1.prms <- params$eta.s1 + reducedRecovery*params$alpha.s1 # never recovered
  
  params$alpha.s2.prms <- (1-reducedRecovery)*params$alpha.s2 # recovered 
  params$eta.s2.prms <- params$eta.s2 + reducedRecovery*params$alpha.s2 # never recovered
  
  params$alpha.s3.prms <- (1-reducedRecovery)*params$alpha.s3 # recovered 
  params$eta.s3.prms <- params$eta.s3 + reducedRecovery*params$alpha.s3 # never recovered
  
  
  ## relapse MAM to MAM
  params$alpha.m.prmm <- (1-reducedRecovery)*params$alpha.m # recovered 
  params$eta.m.prmm <- params$eta.m + reducedRecovery*params$alpha.m # never recovered
  
  params$alpha.m1.prmm <- (1-reducedRecovery)*params$alpha.m1 # recovered 
  params$eta.m1.prmm <- params$eta.m1 + reducedRecovery*params$alpha.m1 # never recovered
  
  params$alpha.m2.prmm <- (1-reducedRecovery)*params$alpha.m2 # recovered 
  params$eta.m2.prmm <- params$eta.m2 + reducedRecovery*params$alpha.m2 # never recovered
  
  params$alpha.m3.prmm <- (1-reducedRecovery)*params$alpha.m3 # recovered 
  params$eta.m3.prmm <- params$eta.m3 + reducedRecovery*params$alpha.m3 # never recovered
  
  
  ###########
  ########### Post-hospitalization Post- relapse
  ###########
  
  # stayed wasted post-hospitalization
  params$eta2.s3.prss <- eta2.s + reducedRecovery*params$alpha2.s3  
  params$eta2.s2.prss <- eta2.s + reducedRecovery*params$alpha2.s2  
  params$eta2.s1.prss <- eta2.s + reducedRecovery*params$alpha2.s1    
  params$eta2.s.prss <- eta2.s + reducedRecovery*params$alpha2.s 
  
  params$eta2.m3.prsm <- eta2.m + reducedRecovery*params$alpha2.m3  
  params$eta2.m2.prsm <- eta2.m + reducedRecovery*params$alpha2.m2  
  params$eta2.m1.prsm <- eta2.m + reducedRecovery*params$alpha2.m1    
  params$eta2.m.prsm <- eta2.m + reducedRecovery*params$alpha2.m 
  
  params$eta2.m3.prmm <- eta2.m + reducedRecovery*params$alpha2.m3  
  params$eta2.m2.prmm <- eta2.m + reducedRecovery*params$alpha2.m2  
  params$eta2.m1.prmm <- eta2.m + reducedRecovery*params$alpha2.m1    
  params$eta2.m.prmm <- eta2.m + reducedRecovery*params$alpha2.m 
  
  params$eta2.s3.prms <- eta2.s + reducedRecovery*params$alpha2.s3  
  params$eta2.s2.prms <- eta2.s + reducedRecovery*params$alpha2.s2  
  params$eta2.s1.prms <- eta2.s + reducedRecovery*params$alpha2.s1    
  params$eta2.s.prms <- eta2.s + reducedRecovery*params$alpha2.s 
  
  # recovered post-hospitalization
  params$alpha2.s3.prss <- (1-reducedRecovery)*params$alpha2.s3 
  params$alpha2.s2.prss <- (1-reducedRecovery)*params$alpha2.s2 
  params$alpha2.s1.prss <- (1-reducedRecovery)*params$alpha2.s1 
  params$alpha2.s.prss <- (1-reducedRecovery)*params$alpha2.s
  
  params$alpha2.m3.prsm <- (1-reducedRecovery)*params$alpha2.m3 
  params$alpha2.m2.prsm <- (1-reducedRecovery)*params$alpha2.m2 
  params$alpha2.m1.prsm <- (1-reducedRecovery)*params$alpha2.m1 
  params$alpha2.m.prsm <- (1-reducedRecovery)*params$alpha2.m
  
  params$alpha2.s3.prms <- (1-reducedRecovery)*params$alpha2.s3 
  params$alpha2.s2.prms <- (1-reducedRecovery)*params$alpha2.s2 
  params$alpha2.s1.prms <- (1-reducedRecovery)*params$alpha2.s1 
  params$alpha2.s.prms <- (1-reducedRecovery)*params$alpha2.s
  
  params$alpha2.m3.prmm <- (1-reducedRecovery)*params$alpha2.m3 
  params$alpha2.m2.prmm <- (1-reducedRecovery)*params$alpha2.m2 
  params$alpha2.m1.prmm <- (1-reducedRecovery)*params$alpha2.m1 
  params$alpha2.m.prmm <- (1-reducedRecovery)*params$alpha2.m
  
  params[,dur.never.rec.s3 := 112]  
  params[,dur.never.rec.s2 := 112]  
  params[,dur.never.rec.s1 := 112]  
  params[,dur.never.rec.s := 112]  
  
  params[,dur.never.rec.s3.prss := 112]  
  params[,dur.never.rec.s2.prss := 112]  
  params[,dur.never.rec.s1.prss := 112]  
  params[,dur.never.rec.s.prss := 112]      
  
  params[,dur.never.rec.m3.prsm := 112]  
  params[,dur.never.rec.m2.prsm := 112]  
  params[,dur.never.rec.m1.prsm := 112]  
  params[,dur.never.rec.m.prsm := 112]      
  
  params[,dur.never.rec.m3 := 112]  
  params[,dur.never.rec.m2 := 112]  
  params[,dur.never.rec.m1 := 112]  
  params[,dur.never.rec.m := 112]  
  
  params[,dur.never.rec.s3.prms := 112]  
  params[,dur.never.rec.s2.prms := 112]  
  params[,dur.never.rec.s1.prms := 112]  
  params[,dur.never.rec.s.prms := 112]      
  
  params[,dur.never.rec.m3.prmm := 112]  
  params[,dur.never.rec.m2.prmm := 112]  
  params[,dur.never.rec.m1.prmm := 112]  
  params[,dur.never.rec.m.prmm := 112]      
  
  params[,dur.rec.s3.prms := dur.rec.s3.prss]  
  params[,dur.rec.s2.prms := dur.rec.s2.prss]  
  params[,dur.rec.s1.prms := dur.rec.s1.prss]  
  params[,dur.rec.s.prms := dur.rec.s.prss]  
  
  params[,dur.def.s3.prms := dur.def.s3.prss]  
  params[,dur.def.s2.prms := dur.def.s2.prss]  
  params[,dur.def.s1.prms := dur.def.s1.prss]  
  params[,dur.def.s.prms := dur.def.s.prss]  
  
  params[,dur.died.s3.prms := dur.died.s3.prss]  
  params[,dur.died.s2.prms := dur.died.s2.prss]  
  params[,dur.died.s1.prms := dur.died.s1.prss]  
  params[,dur.died.s.prms := dur.died.s.prss]  
  
  
  params[,dur.rec.m3.prmm := dur.rec.m3.prsm]  
  params[,dur.rec.m2.prmm := dur.rec.m2.prsm]  
  params[,dur.rec.m1.prmm := dur.rec.m1.prsm]  
  params[,dur.rec.m.prmm := dur.rec.m.prsm]  
  
  params[,dur.def.m3.prmm := dur.def.m3.prsm]  
  params[,dur.def.m2.prmm := dur.def.m2.prsm]  
  params[,dur.def.m1.prmm := dur.def.m1.prsm]  
  params[,dur.def.m.prmm := dur.def.m.prsm]  
  
  params[,dur.died.m3.prmm := dur.died.m3.prsm]  
  params[,dur.died.m2.prmm := dur.died.m2.prsm]  
  params[,dur.died.m1.prmm := dur.died.m1.prsm]  
  params[,dur.died.m.prmm := dur.died.m.prsm] 
  
  ############ 
  ############ weight at admission (kg)
  ############ 
  
  ## severe wasting
  weight.adm.s <- weight.adm.s 
  
  ############ 
  ############ MUAC at admission (mm)
  ############ 
  
  ## severe wasting
  muac.s <- muac.s
  
  ## severe wasting
  
  ############ 
  ############ 
  
  muac.wt.params <- function(i,j,k,weight.adm.s,muac.s,params,...) {
    
    cbind(
      weight.adm.s = weight.adm.s[i],
      muac.s = muac.s[j],
      params[k,]
    )
  }
  
  
  system.time(params.all <- (
    expand.grid(1:length(weight.adm.s),1:length(muac.s),1:dim(params)[1])|> 
      pmap( 
        ~muac.wt.params(..1,..2,..3,weight.adm.s,muac.s,params)
      ) |> 
      list.rbind()
  ))   
  
  params <- data.table(params.all)
  
  setkey(params,weight.adm.s,muac.s)
  
  ########################## adjust recovery rate and time to recovery with muac and weight at admission
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  OptimA
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ## never recovered 
  params$eta.s1 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                          params$eta.s1 + params$alpha.s1 * (1 - 1/params$wm.alpha.s1),
                          params$eta.s1)
  
  params$alpha.s1 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                            params$alpha.s1/params$wm.alpha.s1,
                            params$alpha.s1)
  
  params$dur.rec.s1 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                              params$wm.dur.rec.s1*params$dur.rec.s1,
                              params$dur.rec.s1)
  
  params$eta.m1 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                          params$eta.m1 + params$alpha.m1 * (1 - 1/params$wm.alpha.s1),
                          params$eta.m1)
  
  params$alpha.m1 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                            params$alpha.m1/params$wm.alpha.s1,
                            params$alpha.m1)
  
  params$dur.rec.m1 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                              params$wm.dur.rec.s1*params$dur.rec.m1,
                              params$dur.rec.m1)
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  Standard
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  ## never recovered 
  params$eta.s <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                         params$eta.s + params$alpha.s * (1 - 1/params$wm.alpha.s),
                         params$eta.s)
  
  params$alpha.s <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                           params$alpha.s/params$wm.alpha.s,
                           params$alpha.s)
  
  params$dur.rec.s <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                             params$wm.dur.rec.s*params$dur.rec.s,
                             params$dur.rec.s)
  
  params$eta.m <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                         params$eta.m + params$alpha.m * (1 - 1/params$wm.alpha.s),
                         params$eta.m)
  
  params$alpha.m <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                           params$alpha.m/params$wm.alpha.s,
                           params$alpha.m)
  
  params$dur.rec.m <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                             params$wm.dur.rec.s*params$dur.rec.m,
                             params$dur.rec.m)
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  ComPAS
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  ## never recovered 
  params$eta.s2 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                          params$eta.s2 + params$alpha.s2 * (1 - 1/params$wm.alpha.s2),
                          params$eta.s2)
  
  params$alpha.s2 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                            params$alpha.s2/params$wm.alpha.s2,
                            params$alpha.s2)
  
  params$dur.rec.s2 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                              params$wm.dur.rec.s2*params$dur.rec.s2,
                              params$dur.rec.s2)
  
  
  params$eta.m2 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                          params$eta.m2 + params$alpha.m2 * (1 - 1/params$wm.alpha.s2),
                          params$eta.m2)
  
  params$alpha.m2 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                            params$alpha.m2/params$wm.alpha.s2,
                            params$alpha.m2)
  
  params$dur.rec.m2 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                              params$wm.dur.rec.s2*params$dur.rec.m2,
                              params$dur.rec.m2)
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  MANGO
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  ## never recovered 
  params$eta.s3 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                          params$eta.s3 + params$alpha.s3 * (1 - 1/params$wm.alpha.s3),
                          params$eta.s3)
  
  params$alpha.s3 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                            params$alpha.s3/params$wm.alpha.s3,
                            params$alpha.s3)
  
  params$dur.rec.s3 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                              params$wm.dur.rec.s3*params$dur.rec.s3,
                              params$dur.rec.s3)
  
  
  
  params$eta.m3 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                          params$eta.m3 + params$alpha.m3 * (1 - 1/params$wm.alpha.s3),
                          params$eta.m3)
  
  params$alpha.m3 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                            params$alpha.m3/params$wm.alpha.s3,
                            params$alpha.m3)
  
  params$dur.rec.m3 <- ifelse(params$weight.adm.s < 7 & params$muac.s < 115,
                              params$wm.dur.rec.s3*params$dur.rec.m3,
                              params$dur.rec.m3)
  
  ############ 
  ############ Vary % treated post-relapse 
  ############ 
  
  #coverage.mam <- c(0,.5*cov.s,cov.s,cov.m)
  #cov <- rep(treatDist.s[9],length(coverage.mam))
  #cov <- seq(0,treatDist.s[9],0.2*(treatDist.s[9]))
  cov <- treatDist.s[11]
  cov.post.relapse <- function(i,j,cov,params,...) {
    
    cbind(
      cov = cov[i],
      #cov.m = coverage.mam[i],
      params[j,]
    )
  }
  
  
  system.time(params.all <- (
    expand.grid(1:length(cov),1:dim(params)[1])|> 
      pmap( 
        ~cov.post.relapse(..1,..2,cov,params)
      ) |> 
      list.rbind()
  ))   

  
  ############ ############ ############ 
  ############ number of sachets per week
  ############ ############ ############ 
  
  ## from OptimA in DRC study https://www.thelancet.com/journals/eclinm/article/PIIS2589-5370%2823%2900055-X/fulltext#secsectitle0105
  ## cumulative weight gain in grams, in standard & OptimA group
  ## cumulative muac gain in mm, in standard & OptimA group
  
  ##1. We estimate the weight gain per week until recovery/default/etc. from the curve
  ## 2. We calculate weight at each week until recovery/default/et. By summing 1 + weight at admission
  ## 3. we use the number of sachets per week based on the calculated weight at each week for each outcome
  
  # ggplot(weight.gain.standard,aes(x=x,y=estimate))+geom_point() + geom_smooth(span=0.6)
  # ggplot(weight.gain.optima ,aes(x=x,y=estimate))+geom_point() + geom_smooth(span=0.6)
  # ggplot(muac.gain.standard,aes(x=x,y=estimate))+geom_point() + geom_smooth(span=0.6)
  # ggplot(muac.gain.optima ,aes(x=x,y=estimate))+geom_point() + geom_smooth(span=0.6)
  # ggplot(weight.gain.mango,aes(x=x,y=estimate))+geom_point() + geom_smooth(span=0.6)
  
  ############ standard
  Loe1.gain.weight.s <- loess(estimate ~ x, weight.gain.standard,span=0.6)
  Loe1.gain.muac.s <- loess(estimate ~ x, muac.gain.standard,span=0.6)
  
  weekly.gain.weight.s <- data.frame(predict(Loe1.gain.weight.s,data.frame(x=seq(1,16,1)), se = TRUE)) #112 days = 16 weeks
  weekly.gain.muac.s <- data.frame(predict(Loe1.gain.muac.s,data.frame(x=seq(1,16,1)), se = TRUE)) #112 days = 16 weeks
  weekly.gain.muac.s$time <- weekly.gain.weight.s$time <- seq(1,16,1) #week
  weekly.gain.s <- weekly.gain.weight.s
  weekly.gain.s$fit.muac <- weekly.gain.muac.s$fit
  weekly.gain.s$se.fit.muac <- weekly.gain.muac.s$se.fit
  
  ############ OptimA
  Loe1.gain.weight.s1 <- loess(estimate ~ x, weight.gain.optima,span=0.6)
  Loe1.gain.muac.s1 <- loess(estimate ~ x, muac.gain.optima,span=0.6)
  
  weekly.gain.weight.s1 <- data.frame(predict(Loe1.gain.weight.s1,data.frame(x=seq(1,16,1)), se = TRUE)) #112 days = 16 weeks
  weekly.gain.muac.s1 <- data.frame(predict(Loe1.gain.muac.s1,data.frame(x=seq(1,16,1)), se = TRUE)) #112 days = 16 weeks
  weekly.gain.muac.s1$time <- weekly.gain.weight.s1$time <- seq(1,16,1) #week
  weekly.gain.s1 <- weekly.gain.weight.s1
  weekly.gain.s1$fit.muac <- weekly.gain.muac.s1$fit
  weekly.gain.s1$se.fit.muac <- weekly.gain.muac.s1$se.fit
  
  ############ ComPAS
  ### MUAC < 115 mm, two RUTF sachets per day,
  ### 115 = MUAC <125 mm, one RUTF sachet per day
  
  Loe1.gain.muac.s2 <- loess(estimate ~ x, muac.gain.compas,span=1)
  
  weekly.gain.muac.s2 <- data.frame(predict(Loe1.gain.muac.s2,data.frame(x=seq(1,16,1)), se = TRUE)) #112 days ~ 16 weeks
  weekly.gain.muac.s2$time <- seq(1,16,1) #week
  weekly.gain.s2 <- weekly.gain.muac.s2
  
  ############ MANGO
  weight.gain.mango$velocity.weekly <- weight.gain.mango$estimate * 7
  
  Loe1.weight.velocity.s3 <- loess(velocity.weekly ~ x, weight.gain.mango,span=0.6)
  weekly.weight.velocity.s3 <- data.frame(predict(Loe1.weight.velocity.s3,data.frame(x=seq(1,16,1)), se = TRUE)) #112 days = 16 weeks
  weekly.weight.velocity.s3$time <- seq(1,16,1) #week
  
  #### converting the weight velocity to weight gain per week
  ## Weekly Weight Gain (grams)=Weight Gain Velocity (g/kg/week)× Initial body Weight (kg)
  
  weekly.weight.velocity.s3 <- data.table(weekly.weight.velocity.s3)
  weekly.weight.velocity.s3[, weight.velocity.cum := cumsum(fit)]
  
  ## from ComPAS Kenya & South Sudan Study
  rutf <- function(weight){
    ifelse(weight >=4 & weight <= 4.9, 14,  
           ifelse(weight >4.9 & weight <= 6.9, 18,  
                  ifelse(weight >6.9 & weight <= 8.4, 21,  
                         ifelse(weight >8.4 & weight <= 9.4, 25,  
                                ifelse(weight >9.4 & weight <= 10.4, 28,  
                                       ifelse(weight >10.4 & weight <= 11.9, 32,  
                                              ifelse(weight >11.9, 35,NA)))))))}
  
  ## ## ## 
  ## ## ## standard # from https://www.thelancet.com/journals/eclinm/article/PIIS2589-5370%2823%2900055-X/fulltext#supplementaryMaterial paper (suppl mat table 2)
  ## ## ## 
  ## ## ## In the Cazes et al, study children aged 6?59 months with a MUAC < 115 mm or WHZ < -3 or nutritional oedema grade 1 or 2 were eligible
  
  
  rutf.s <- function(weight){
    ifelse(weight >=3 & weight <= 3.4, 9,  
           ifelse(weight >3.4 & weight <= 4.9, 11,  
                  ifelse(weight >4.9 & weight <= 6.9, 14,  
                         ifelse(weight >6.9 & weight <= 9.9, 21,  
                                ifelse(weight >9.9 & weight <= 14.9, 28,35  
                                )))))}
  
  ## ## ## 
  ## ## ## ComPAS # from https://onlinelibrary.wiley.com/doi/full/10.1111/mcn.13771
  ## ## ## 
  
  ### MUAC < 115 mm, two RUTF sachets per day,
  ### 115 = MUAC <125 mm, one RUTF sachet per day
  
  rutf.s2 <- function(muac){
    ifelse(muac <115, 14,7)
  }
  
  
  ## ## ## 
  ## ## ## OptimA # from https://www.thelancet.com/journals/eclinm/article/PIIS2589-5370%2823%2900055-X/fulltext #supplementaryMaterial paper (suppl mat table 2)
  ## ## ## 
  
  
  rutf.s1.muac <- function(weight,muac){
    ifelse(muac <115,
           ifelse(weight >=3 & weight <= 3.4, 10 ,  
                  ifelse(weight >3.4 & weight <= 4, 11,  
                         ifelse(weight >4 & weight <= 4.4, 12,  
                                ifelse(weight >4.4 & weight <= 4.9, 13,  
                                       
                                       ifelse(weight >4.9 & weight <= 5.4, 14,  
                                              ifelse(weight >5.4 & weight <= 5.9, 15,  
                                                     ifelse(weight >5.9 & weight <= 6.4, 16,  
                                                            ifelse(weight >6.4 & weight <= 6.9, 17,  
                                                                   
                                                                   ifelse(weight >6.9 & weight <= 7.4, 18,  
                                                                          ifelse(weight >7.4 & weight <= 7.9, 19,  
                                                                                 ifelse(weight >7.9 & weight <= 8.4, 20,  
                                                                                        ifelse(weight >8.4 & weight <= 8.9, 22,  
                                                                                               ifelse(weight >8.9 & weight <= 9.4, 43,  
                                                                                                      ifelse(weight >9.4 & weight <= 9.9, 24,  
                                                                                                             
                                                                                                             ifelse(weight >9.9 & weight <= 10.4, 25,  
                                                                                                                    ifelse(weight >10.4 & weight <= 10.9, 26,  
                                                                                                                           ifelse(weight >10.9 & weight <= 11.4, 27,  
                                                                                                                                  ifelse(weight >11.4 & weight <= 11.9, 29,  
                                                                                                                                         ifelse(weight >11.9 & weight <= 12.4, 30,  
                                                                                                                                                ifelse(weight >12.4 & weight <= 12.9, 31,  
                                                                                                                                                       ifelse(weight >12.9 & weight <= 13.4, 33,  
                                                                                                                                                              ifelse(weight >13.4 & weight <= 13.9, 34,  
                                                                                                                                                                     ifelse(weight >13.9 & weight <= 14.4, 35,  
                                                                                                                                                                            ifelse(weight >14.4 & weight <= 14.9, 36,36  
                                                                                                                                                                            )))))))))))))))))))))))),
           ifelse(muac >= 115 & muac <=119, 
                  ifelse(weight >=3 & weight <= 3.4, 8 ,  
                         ifelse(weight >3.4 & weight <= 4, 8,  
                                ifelse(weight >4 & weight <= 4.4, 9,  
                                       ifelse(weight >4.4 & weight <= 4.9, 10,  
                                              
                                              ifelse(weight >4.9 & weight <= 5.4, 11,  
                                                     ifelse(weight >5.4 & weight <= 5.9, 12,  
                                                            ifelse(weight >5.9 & weight <= 6.4, 12,  
                                                                   ifelse(weight >6.4 & weight <= 6.9, 13,  
                                                                          
                                                                          ifelse(weight >6.9 & weight <= 7.4, 13,  
                                                                                 ifelse(weight >7.4 & weight <= 7.9, 14,  
                                                                                        ifelse(weight >7.9 & weight <= 8.4, 15,  
                                                                                               ifelse(weight >8.4 & weight <= 8.9, 15,  
                                                                                                      ifelse(weight >8.9 & weight <= 9.4, 16,  
                                                                                                             ifelse(weight >9.4 & weight <= 9.9, 17,  
                                                                                                                    
                                                                                                                    ifelse(weight >9.9 & weight <= 10.4, 18,  
                                                                                                                           ifelse(weight >10.4 & weight <= 10.9, 19,  
                                                                                                                                  ifelse(weight >10.9 & weight <= 11.4, 20,  
                                                                                                                                         ifelse(weight >11.4 & weight <= 11.9, 21,  
                                                                                                                                                ifelse(weight >11.9 & weight <= 12.4, 22,  
                                                                                                                                                       ifelse(weight >12.4 & weight <= 12.9, 22,  
                                                                                                                                                              ifelse(weight >12.9 & weight <= 13.4, 23,  
                                                                                                                                                                     ifelse(weight >13.4 & weight <= 13.9, 24,  
                                                                                                                                                                            ifelse(weight >13.9 & weight <= 14.4, 25,  
                                                                                                                                                                                   ifelse(weight >14.4 & weight <= 14.9, 26,28  
                                                                                                                                                                                   )))))))))))))))))))))))), 
                  #muac >= 120  
                  ifelse(weight >=3 & weight <= 6.9, 7,  
                         ifelse(weight >6.9 & weight <= 7.4, 8,  
                                ifelse(weight >7.4 & weight <= 7.9, 8,  
                                       ifelse(weight >7.9 & weight <= 8.4, 9,  
                                              ifelse(weight >8.4 & weight <= 8.9, 9,  
                                                     ifelse(weight >8.9 & weight <= 9.4, 9,  
                                                            ifelse(weight >9.4 & weight <= 9.9, 9,  
                                                                   
                                                                   ifelse(weight >9.9 & weight <= 10.4, 10,  
                                                                          ifelse(weight >10.4 & weight <= 10.9, 10,  
                                                                                 ifelse(weight >10.9 & weight <= 11.4, 10,  
                                                                                        ifelse(weight >11.4 & weight <= 11.9, 10,  
                                                                                               ifelse(weight >11.9 & weight <= 12.4, 11,  
                                                                                                      ifelse(weight >12.4 & weight <= 12.9, 11,  
                                                                                                             ifelse(weight >12.9 & weight <= 13.4, 12,  
                                                                                                                    ifelse(weight >13.4 & weight <= 13.9, 12,  
                                                                                                                           ifelse(weight >13.9 & weight <= 14.4, 12,  
                                                                                                                                  ifelse(weight >14.4 & weight <= 14.9, 13,14  
                                                                                                                                  )))))))))))))))))))}
  
  
  ## ## ## 
  ## ## ## MANGO # from https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002887 paper (table 1)
  ## ## ## 
  
  rutf.s3 <- function(weight,time){
    ifelse(time < 3 & weight >=3 & weight <= 3.4, 8,
           ifelse(time >= 3 & weight >=3 & weight <= 3.4, 7,
                  
                  ifelse(time < 3 & weight >3.4 & weight <= 4.9, 10,
                         ifelse(time >= 3 & weight >3.4 & weight <= 4.9, 7,
                                
                                ifelse(time < 3 & weight >4.9 & weight <= 6.9, 15,
                                       ifelse(time >= 3 & weight >4.9 & weight <= 6.9, 7,
                                              
                                              ifelse(time < 3 & weight >6.9 & weight <= 9.9, 20,
                                                     ifelse(time >= 3 & weight >6.9 & weight <= 9.9, 14,
                                                            
                                                            ifelse(time < 3 & weight >9.9 & weight <= 14.9, 30,14)))))))))
  }
  
  
  ############ 
  ############ 
  ############ 
  
  ## ## ## ##  
  ## ## ## ## OptimA & Standard & MANGO # of sachets
  ## ## ## ## 
  
  rutf.number <- NULL
  for (i in 1:dim(params.all)[1]){
    
    
    #### MANGO
    
    rec.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.rec.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000 + params.all$weight.adm.s[i],
                          weekly.weight.velocity.s3[which((round_any(params.all$dur.rec.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    def.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.def.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000 + params.all$weight.adm.s[i],
                          weekly.weight.velocity.s3[which((round_any(params.all$dur.def.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    died.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.died.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000 + params.all$weight.adm.s[i],
                           weekly.weight.velocity.s3[which((round_any(params.all$dur.died.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    hosp.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.hosp.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000 + params.all$weight.adm.s[i],
                           weekly.weight.velocity.s3[which((round_any(params.all$dur.hosp.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    transf.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.transf.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000 + params.all$weight.adm.s[i],
                             weekly.weight.velocity.s3[which((round_any(params.all$dur.transf.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    never.rec.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.never.rec.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000 + params.all$weight.adm.s[i],
                                weekly.weight.velocity.s3[which((round_any(params.all$dur.never.rec.s3/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    ### we assume #of rutfs include those consumed while hospitalized and those consumed post-hosp until recovery, or death or never recovered
    ### minus weight gain and rutfs consumption up to time to hospitalization
    
    hosp.rec.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any((params.all$dur.hosp.s3+params.all$dur.rec.s3+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                 weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                               weekly.weight.velocity.s3[which((round_any((params.all$dur.hosp.s3+params.all$dur.rec.s3+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                 weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$time))
    
    hosp.died.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any((params.all$dur.died.s3+params.all$dur.rec.s3+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                  weekly.weight.velocity.s3$time > (round_any(params.all$dur.died.s3/7,1))[i]),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                                weekly.weight.velocity.s3[which((round_any((params.all$dur.died.s3+params.all$dur.rec.s3+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                  weekly.weight.velocity.s3$time > (round_any(params.all$dur.died.s3/7,1))[i]),]$time))
    
    hosp.never.rec.s3 <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any((params.all$dur.never.rec.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                       weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                                     weekly.weight.velocity.s3[which((round_any((params.all$dur.never.rec.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                       weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$time))
    
    rec.s3.prss <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.rec.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                               weekly.weight.velocity.s3[which((round_any(params.all$dur.rec.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$time ))
    
    
    def.s3.prss <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.def.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                               weekly.weight.velocity.s3[which((round_any(params.all$dur.def.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    
    died.s3.prss <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.died.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                                weekly.weight.velocity.s3[which((round_any(params.all$dur.died.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    
    never.rec.s3.prss <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any(params.all$dur.never.rec.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                                     weekly.weight.velocity.s3[which((round_any(params.all$dur.never.rec.s3.prss/7,1))[i] >= weekly.weight.velocity.s3$time),]$time))
    
    
    hosp.rec.s3.prss <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any((params.all$dur.hosp.s3+params.all$dur.rec.s3.prss+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                      weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                                    weekly.weight.velocity.s3[which((round_any((params.all$dur.hosp.s3+params.all$dur.rec.s3.prss+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                      weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$time))
    
    hosp.died.s3.prss <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any((params.all$dur.hosp.s3+params.all$dur.died.s3.prss+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                       weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                                     weekly.weight.velocity.s3[which((round_any((params.all$dur.hosp.s3+params.all$dur.died.s3.prss+params.all$hosp.stay.s3)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                       weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$time))
    
    hosp.never.rec.s3.prss <- sum(rutf.s3(weekly.weight.velocity.s3[which((round_any((params.all$dur.never.rec.s3.prss)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                            weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$weight.velocity.cum*params.all$weight.adm.s[i]/1000  + params.all$weight.adm.s[i],
                                          weekly.weight.velocity.s3[which((round_any((params.all$dur.never.rec.s3.prss)/7,1))[i] >= weekly.weight.velocity.s3$time &
                                                                            weekly.weight.velocity.s3$time > (round_any(params.all$dur.hosp.s3/7,1))[i]),]$time))
    
    
    ### standard 
    rec.s <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.rec.s/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    def.s <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.def.s/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    died.s <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.died.s/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    hosp.s <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.hosp.s/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    transf.s <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.transf.s/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    never.rec.s <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.never.rec.s/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    
    ### we assume #of rutf sachets include those consumed while hospitalized and those consumed post-hosp until recovery, or death or never recovered
    ### minus weight gain and rutf consumption up to time to hospitalization
    
    hosp.rec.s <- sum(rutf.s(weekly.gain.s[which((round_any((params.all$dur.hosp.s+params.all$dur.rec.s+params.all$hosp.stay.s)/7,1))[i] >= weekly.gain.s$time &
                                                   weekly.gain.s$time > (round_any(params.all$dur.hosp.s/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i]))
    
    hosp.died.s <- sum(rutf.s(weekly.gain.s[which((round_any((params.all$dur.hosp.s+params.all$dur.died.s+params.all$hosp.stay.s)/7,1))[i] >= weekly.gain.s$time &
                                                    weekly.gain.s$time > (round_any(params.all$dur.hosp.s/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i]))
    
    hosp.never.rec.s <- sum(rutf.s(weekly.gain.s[which((round_any((params.all$dur.never.rec.s)/7,1))[i] >= weekly.gain.s$time &
                                                         weekly.gain.s$time > (round_any(params.all$dur.hosp.s/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i]))
    
    rec.s.prss <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.rec.s.prss/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    def.s.prss <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.def.s.prss/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    died.s.prss <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.died.s.prss/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    never.rec.s.prss <- sum(rutf.s(weekly.gain.s[which((round_any(params.all$dur.never.rec.s.prss/7,1))[i] >= weekly.gain.s$time),]$fit/1000 + params.all$weight.adm.s[i]))
    
    hosp.rec.s.prss <- sum(rutf.s(weekly.gain.s[which((round_any((params.all$dur.hosp.s+params.all$dur.rec.s.prss+params.all$hosp.stay.s)/7,1))[i] >= weekly.gain.s$time &
                                                        weekly.gain.s$time > (round_any(params.all$dur.hosp.s/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i]))
    
    hosp.died.s.prss <- sum(rutf.s(weekly.gain.s[which((round_any((params.all$dur.hosp.s+params.all$dur.hosp.s+params.all$dur.died.s.prss+params.all$hosp.stay.s)/7,1))[i] >= weekly.gain.s$time &
                                                         weekly.gain.s$time > (round_any(params.all$dur.hosp.s/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i]))
    
    hosp.never.rec.s.prss <- sum(rutf.s(weekly.gain.s[which((round_any((params.all$dur.never.rec.s.prss)/7,1))[i] >= weekly.gain.s$time &
                                                              weekly.gain.s$time > (round_any(params.all$dur.hosp.s/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i]))
    
    ### OptimA
    rec.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.rec.s1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                               weekly.gain.s1[which((round_any(params.all$dur.rec.s1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    def.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.def.s1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                               weekly.gain.s1[which((round_any(params.all$dur.def.s1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    died.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.died.s1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                weekly.gain.s1[which((round_any(params.all$dur.died.s1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    hosp.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.hosp.s1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                weekly.gain.s1[which((round_any(params.all$dur.hosp.s1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    transf.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.transf.s1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                  weekly.gain.s1[which((round_any(params.all$dur.transf.s1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    never.rec.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.never.rec.s1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                     weekly.gain.s1[which((round_any(params.all$dur.never.rec.s1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    hosp.rec.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.rec.s1+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                           weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i], 
                                    weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.rec.s1+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                           weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.died.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.died.s1+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                            weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                     weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.died.s1+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                            weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.never.rec.s1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.never.rec.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                          weekly.gain.s1[which((round_any((params.all$dur.never.rec.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    rec.s1.prss <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.rec.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                    weekly.gain.s1[which((round_any(params.all$dur.rec.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    def.s1.prss <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.def.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                    weekly.gain.s1[which((round_any(params.all$dur.def.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    died.s1.prss <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.died.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                     weekly.gain.s1[which((round_any(params.all$dur.died.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    never.rec.s1.prss <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.never.rec.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                          weekly.gain.s1[which((round_any(params.all$dur.never.rec.s1.prss/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    hosp.rec.s1.prss <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.rec.s1.prss+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                                weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i], 
                                         weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.rec.s1.prss+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                                weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.died.s1.prss <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.died.s1.prss+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                          weekly.gain.s1[which((round_any((params.all$dur.hosp.s1+params.all$dur.died.s1.prss+params.all$hosp.stay.s1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.never.rec.s1.prss <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.never.rec.s1.prss)/7,1))[i] >= weekly.gain.s1$time &
                                                                      weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                               weekly.gain.s1[which((round_any((params.all$dur.never.rec.s1.prss)/7,1))[i] >= weekly.gain.s1$time &
                                                                      weekly.gain.s1$time > (round_any(params.all$dur.hosp.s1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    
    rec.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.rec.m1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                               weekly.gain.s1[which((round_any(params.all$dur.rec.m1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    def.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.def.m1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                               weekly.gain.s1[which((round_any(params.all$dur.def.m1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    died.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.died.m1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                weekly.gain.s1[which((round_any(params.all$dur.died.m1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    hosp.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.hosp.m1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                weekly.gain.s1[which((round_any(params.all$dur.hosp.m1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    transf.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.transf.m1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                  weekly.gain.s1[which((round_any(params.all$dur.transf.m1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    m1.reg.sam <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.m.reg.sam/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                   weekly.gain.s1[which((round_any(params.all$dur.m.reg.sam/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    never.rec.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.never.rec.m1/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                     weekly.gain.s1[which((round_any(params.all$dur.never.rec.m1/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    hosp.rec.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.rec.m1+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                           weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i], 
                                    weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.rec.m1+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                           weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.died.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.died.m1+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                            weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                     weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.died.m1+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                            weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.never.rec.m1 <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.never.rec.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                          weekly.gain.s1[which((round_any((params.all$dur.never.rec.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    rec.m1.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.rec.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                    weekly.gain.s1[which((round_any(params.all$dur.rec.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    def.m1.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.def.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                    weekly.gain.s1[which((round_any(params.all$dur.def.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    died.m1.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.died.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                     weekly.gain.s1[which((round_any(params.all$dur.died.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    never.rec.m1.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.never.rec.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                          weekly.gain.s1[which((round_any(params.all$dur.never.rec.m1.prmm/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    hosp.rec.m1.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.rec.m1.prmm+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                                weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i], 
                                         weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.rec.m1.prmm+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                                weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.died.m1.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.died.m1.prmm+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                          weekly.gain.s1[which((round_any((params.all$dur.hosp.m1+params.all$dur.died.m1.prmm+params.all$hosp.stay.m1)/7,1))[i] >= weekly.gain.s1$time &
                                                                 weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    hosp.never.rec.m1.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any((params.all$dur.never.rec.m1.prmm)/7,1))[i] >= weekly.gain.s1$time &
                                                                      weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit/1000 + params.all$weight.adm.s[i],
                                               weekly.gain.s1[which((round_any((params.all$dur.never.rec.m1.prmm)/7,1))[i] >= weekly.gain.s1$time &
                                                                      weekly.gain.s1$time > (round_any(params.all$dur.hosp.m1/7,1))[i]),]$fit.muac + params.all$muac.s[i]))
    
    m1.reg.sam.prmm <- sum(rutf.s1.muac(weekly.gain.s1[which((round_any(params.all$dur.m.reg.sam/7,1))[i] >= weekly.gain.s1$time),]$fit/1000 + params.all$weight.adm.s[i],
                                        weekly.gain.s1[which((round_any(params.all$dur.m.reg.sam/7,1))[i] >= weekly.gain.s1$time),]$fit.muac + params.all$muac.s[i]))
    
    ### comPAS 
    rec.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.rec.s2/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    def.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.def.s2/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    died.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.died.s2/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    hosp.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.hosp.s2/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    transf.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.transf.s2/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    never.rec.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.never.rec.s2/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    
    ### we assume #of rutfs include those consumed while hospitalized and those consumed post-hosp until recovery, or death or never recovered
    ### minus weight gain and rutfs consumption up to time to hospitalization
    
    hosp.rec.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any((params.all$dur.hosp.s2+params.all$dur.rec.s2+params.all$hosp.stay.s2)/7,1))[i] >= weekly.gain.s2$time &
                                                      weekly.gain.s2$time > (round_any(params.all$dur.hosp.s2/7,1))[i]),]$fit + params.all$muac.s[i]))
    
    hosp.died.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any((params.all$dur.hosp.s2+params.all$dur.died.s2+params.all$hosp.stay.s2)/7,1))[i] >= weekly.gain.s2$time &
                                                       weekly.gain.s2$time > (round_any(params.all$dur.hosp.s2/7,1))[i]),]$fit + params.all$muac.s[i]))
    
    hosp.never.rec.s2 <- sum(rutf.s2(weekly.gain.s2[which((round_any((params.all$dur.never.rec.s2)/7,1))[i] >= weekly.gain.s2$time &
                                                            weekly.gain.s2$time > (round_any(params.all$dur.hosp.s2/7,1))[i]),]$fit + params.all$muac.s[i]))
    
    rec.s2.prss <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.rec.s2.prss/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    def.s2.prss <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.def.s2.prss/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    died.s2.prss <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.died.s2.prss/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    never.rec.s2.prss <- sum(rutf.s2(weekly.gain.s2[which((round_any(params.all$dur.never.rec.s2.prss/7,1))[i] >= weekly.gain.s2$time),]$fit + params.all$muac.s[i]))
    
    hosp.rec.s2.prss <- sum(rutf.s2(weekly.gain.s2[which((round_any((params.all$dur.hosp.s2+params.all$dur.rec.s2.prss+params.all$hosp.stay.s2)/7,1))[i] >= weekly.gain.s2$time &
                                                           weekly.gain.s2$time > (round_any(params.all$dur.hosp.s2/7,1))[i]),]$fit + params.all$muac.s[i]))
    
    hosp.died.s2.prss <- sum(rutf.s2(weekly.gain.s2[which((round_any((params.all$dur.hosp.s2+params.all$dur.hosp.s2+params.all$dur.died.s2.prss+params.all$hosp.stay.s2)/7,1))[i] >= weekly.gain.s2$time &
                                                            weekly.gain.s2$time > (round_any(params.all$dur.hosp.s2/7,1))[i]),]$fit + params.all$muac.s[i]))
    
    hosp.never.rec.s2.prss <- sum(rutf.s2(weekly.gain.s2[which((round_any((params.all$dur.never.rec.s2.prss)/7,1))[i] >= weekly.gain.s2$time &
                                                                 weekly.gain.s2$time > (round_any(params.all$dur.hosp.s2/7,1))[i]),]$fit + params.all$muac.s[i]))
    
    rutf.number <-rbind(rutf.number, c(params.all$weight.adm.s[i],params.all$muac.s[i],
                                       params.all$dur.rec.s3[i], params.all$dur.def.s3[i],params.all$dur.died.s3[i],params.all$dur.hosp.s3[i],params.all$dur.transf.s3[i],
                                       params.all$dur.never.rec.s3[i],params.all$hosp.stay.s3[i],params.all$dur.rec.s3.prss[i],params.all$dur.def.s3.prss[i],
                                       params.all$dur.died.s3.prss[i],params.all$dur.never.rec.s3.prss[i],
                                       
                                       params.all$dur.rec.s[i], params.all$dur.def.s[i],params.all$dur.died.s[i],params.all$dur.hosp.s[i],params.all$dur.transf.s[i],
                                       params.all$dur.never.rec.s[i],params.all$hosp.stay.s[i],params.all$dur.rec.s.prss[i],params.all$dur.def.s.prss[i],
                                       params.all$dur.died.s.prss[i],params.all$dur.never.rec.s.prss[i],
                                       
                                       params.all$dur.rec.s1[i], params.all$dur.def.s1[i],params.all$dur.died.s1[i],params.all$dur.hosp.s1[i],params.all$dur.transf.s1[i],
                                       params.all$dur.never.rec.s1[i],params.all$hosp.stay.s1[i],params.all$dur.rec.s1.prss[i],params.all$dur.def.s1.prss[i],
                                       params.all$dur.died.s1.prss[i],params.all$dur.never.rec.s1.prss[i],
                                       
                                       rec.s,def.s,died.s,hosp.s,transf.s,never.rec.s,hosp.rec.s,hosp.died.s,hosp.never.rec.s,
                                       rec.s.prss,def.s.prss,died.s.prss,never.rec.s.prss,hosp.rec.s.prss,hosp.died.s.prss,hosp.never.rec.s.prss,
                                       
                                       rec.s1,def.s1,died.s1,hosp.s1,transf.s1,never.rec.s1,hosp.rec.s1,hosp.died.s1,hosp.never.rec.s1,
                                       rec.s1.prss,def.s1.prss,died.s1.prss,never.rec.s1.prss,hosp.rec.s1.prss,hosp.died.s1.prss,hosp.never.rec.s1.prss,
                                       
                                       rec.m1,def.m1,died.m1,hosp.m1,transf.m1,never.rec.m1,hosp.rec.m1,hosp.died.m1,hosp.never.rec.m1,m1.reg.sam,
                                       rec.m1.prmm,def.m1.prmm,died.m1.prmm,never.rec.m1.prmm,hosp.rec.m1.prmm,hosp.died.m1.prmm,hosp.never.rec.m1.prmm,m1.reg.sam.prmm,
                                       
                                       rec.s2,def.s2,died.s2,hosp.s2,transf.s2,never.rec.s2,hosp.rec.s2,hosp.died.s2,hosp.never.rec.s2,
                                       rec.s2.prss,def.s2.prss,died.s2.prss,never.rec.s2.prss,hosp.rec.s2.prss,hosp.died.s2.prss,hosp.never.rec.s2.prss,
                                       
                                       rec.s3,def.s3,died.s3,hosp.s3,transf.s3,never.rec.s3,hosp.rec.s3,hosp.died.s3,hosp.never.rec.s3,
                                       rec.s3.prss,def.s3.prss,died.s3.prss,never.rec.s3.prss,hosp.rec.s3.prss,hosp.died.s3.prss,hosp.never.rec.s3.prss)
    )
  }
  
  rutf.number <- data.table(rutf.number)
  colnames(rutf.number) <- c("weight.adm.s","muac.s",
                             "dur.rec.s3", "dur.def.s3","dur.died.s3","dur.hosp.s3","dur.transf.s3",
                             "dur.never.rec.s3","hosp.stay.s3","dur.rec.s3.prss","dur.def.s3.prss",
                             "dur.died.s3.prss","dur.never.rec.s3.prss",
                             
                             "dur.rec.s", "dur.def.s","dur.died.s","dur.hosp.s","dur.transf.s",
                             "dur.never.rec.s","hosp.stay.s","dur.rec.s.prss","dur.def.s.prss",
                             "dur.died.s.prss","dur.never.rec.s.prss",
                             
                             "dur.rec.s1","dur.def.s1","dur.died.s1","dur.hosp.s1","dur.transf.s1","dur.never.rec.s1",
                             "hosp.stay.s1","dur.rec.s1.prss","dur.def.s1.prss","dur.died.s1.prss",
                             "dur.never.rec.s1.prss",
                             
                             "rec.s","def.s","died.s","hosp.s","transf.s","never.rec.s","hosp.rec.s","hosp.died.s","hosp.never.rec.s",
                             "rec.s.prss","def.s.prss","died.s.prss","never.rec.s.prss","hosp.rec.s.prss","hosp.died.s.prss","hosp.never.rec.s.prss",
                             
                             "rec.s1","def.s1","died.s1","hosp.s1","transf.s1","never.rec.s1", "hosp.rec.s1","hosp.died.s1","hosp.never.rec.s1",
                             "rec.s1.prss","def.s1.prss","died.s1.prss","never.rec.s1.prss","hosp.rec.s1.prss","hosp.died.s1.prss","hosp.never.rec.s1.prss",
                             
                             "rec.m1","def.m1","died.m1","hosp.m1","transf.m1","never.rec.m1","hosp.rec.m1","hosp.died.m1","hosp.never.rec.m1","m1.reg.sam",
                             "rec.m1.prmm","def.m1.prmm","died.m1.prmm","never.rec.m1.prmm","hosp.rec.m1.prmm","hosp.died.m1.prmm","hosp.never.rec.m1.prmm","m1.reg.sam.prmm",
                             
                             "rec.s2","def.s2","died.s2","hosp.s2","transf.s2","never.rec.s2","hosp.rec.s2","hosp.died.s2","hosp.never.rec.s2",
                             "rec.s2.prss","def.s2.prss","died.s2.prss","never.rec.s2.prss","hosp.rec.s2.prss","hosp.died.s2.prss","hosp.never.rec.s2.prss",
                             
                             "rec.s3","def.s3","died.s3","hosp.s3","transf.s3","never.rec.s3", "hosp.rec.s3","hosp.died.s3","hosp.never.rec.s3",
                             "rec.s3.prss","def.s3.prss","died.s3.prss","never.rec.s3.prss","hosp.rec.s3.prss","hosp.died.s3.prss","hosp.never.rec.s3.prss"
  )
  
  rutf.number$transf.s.prss <- rutf.number$transf.s
  rutf.number$hosp.s.prss <- rutf.number$hosp.s
  
  rutf.number$transf.s1.prss <- rutf.number$transf.s1
  rutf.number$hosp.s1.prss <- rutf.number$hosp.s1
  
  rutf.number$transf.s2.prss <- rutf.number$transf.s2
  rutf.number$hosp.s2.prss <- rutf.number$hosp.s2
  
  rutf.number$transf.s3.prss <- rutf.number$transf.s3
  rutf.number$hosp.s3.prss <- rutf.number$hosp.s3
  
  
  rutf.number$rec.s.prms <- rutf.number$rec.s.prss 
  rutf.number$def.s.prms <- rutf.number$def.s.prss 
  rutf.number$died.s.prms <- rutf.number$died.s.prss 
  rutf.number$never.rec.s.prms <- rutf.number$never.rec.s.prss 
  rutf.number$hosp.rec.s.prms <- rutf.number$hosp.rec.s.prss 
  rutf.number$hosp.died.s.prms <- rutf.number$hosp.died.s.prss 
  rutf.number$hosp.never.rec.s.prms <- rutf.number$hosp.never.rec.s.prss 
  
  rutf.number$rec.s1.prms <- rutf.number$rec.s1.prss 
  rutf.number$def.s1.prms <- rutf.number$def.s1.prss 
  rutf.number$died.s1.prms <- rutf.number$died.s1.prss 
  rutf.number$never.rec.s1.prms <- rutf.number$never.rec.s1.prss 
  rutf.number$hosp.rec.s1.prms <- rutf.number$hosp.rec.s1.prss 
  rutf.number$hosp.died.s1.prms <- rutf.number$hosp.died.s1.prss 
  rutf.number$hosp.never.rec.s1.prms <- rutf.number$hosp.never.rec.s1.prss 
  
  rutf.number$rec.m1.prsm <- rutf.number$rec.m1.prmm 
  rutf.number$def.m1.prsm <- rutf.number$def.m1.prmm 
  rutf.number$died.m1.prsm <- rutf.number$died.m1.prmm 
  rutf.number$never.rec.m1.prsm <- rutf.number$never.rec.m1.prmm 
  rutf.number$hosp.rec.m1.prsm <- rutf.number$hosp.rec.m1.prmm 
  rutf.number$hosp.died.m1.prsm <- rutf.number$hosp.died.m1.prmm 
  rutf.number$hosp.never.rec.m1.prsm <- rutf.number$hosp.never.rec.m1.prmm 
  rutf.number$m1.reg.sam.prsm <- rutf.number$m1.reg.sam.prmm 
  
  rutf.number$rec.s2.prms <- rutf.number$rec.s2.prss 
  rutf.number$def.s2.prms <- rutf.number$def.s2.prss 
  rutf.number$died.s2.prms <- rutf.number$died.s2.prss 
  rutf.number$never.rec.s2.prms <- rutf.number$never.rec.s2.prss 
  rutf.number$hosp.rec.s2.prms <- rutf.number$hosp.rec.s2.prss 
  rutf.number$hosp.died.s2.prms <- rutf.number$hosp.died.s2.prss 
  rutf.number$hosp.never.rec.s2.prms <- rutf.number$hosp.never.rec.s2.prss 
  
  rutf.number$rec.s3.prms <- rutf.number$rec.s3.prss 
  rutf.number$def.s3.prms <- rutf.number$def.s3.prss 
  rutf.number$died.s3.prms <- rutf.number$died.s3.prss 
  rutf.number$never.rec.s3.prms <- rutf.number$never.rec.s3.prss 
  rutf.number$hosp.rec.s3.prms <- rutf.number$hosp.rec.s3.prss 
  rutf.number$hosp.died.s3.prms <- rutf.number$hosp.died.s3.prss 
  rutf.number$hosp.never.rec.s3.prms <- rutf.number$hosp.never.rec.s3.prss 
  
  ## 1000-1200 kcal/day  CSB given to children with MAM in standard protocol 
  ## 1 packet of CSB is 1.5kg, 
  ## 100 grams of CBS provides 400 kcal https://supply.unicef.org/s0000295.html, 
  ## therefore one packet provides about 6000 kcal. 
  ## one packet is consumed for about 6 days. 
  
  ## One sachet of RUSF per day was given to children with MAM in standard protocol 
  ## One sachet of RUTF per day was given children with MAM in ComPAS protocol 
  ## two sachets of RUTF per day was given to children with MAM in ComPAS protocol 
  
  Merg <-  cbind(params.all,rutf.number)#rutf.number %>% left_join(params.all,relationship = "many-to-many")
  
  return(Merg)
} 

########## Model function ############
### Function for parameters

f_DoseOpt_fixedbudget_calc <- function(cost.s.varied.relapse,program.budget,scenario) {

    cost.sch <- cost.s.varied.relapse
    
    budget = program.budget
    
    ######### ######### 
    ######### standard
    ######### ######### 
    
    sw.s <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      s <- (cost.sch$number.sachet.s[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
              cost.sch$number.sachet.prss[i] * (cost.sch$cov[i] * cost.sch$alpha.s[i] * cost.sch$sigma.s.to.sam[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
                                                  cost.sch$cov[i] * cost.sch$tau.m[i] * (pop + pop.m * cost.sch$tau.m[i]) * cost.sch$alpha.s[i] * cost.sch$sigma.s.to.mam[i]) + 
              cost.sch$number.sachet.prms[i] * (cost.sch$cov[i] * pop.m * cost.sch$alpha.m[i] * cost.sch$sigma.m.to.sam[i] + 
                                                  cost.sch$cov[i] * pop.m * cost.sch$alpha.m[i] * cost.sch$sigma.m.to.mam[i] * cost.sch$tau.m[i]))*cost.sch$cost.rutf[i] - budget
      
      return(s^2)  # Return the squared residual to minimize
    }
    
    mw.m <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      m <- (cost.sch$number.sachet.m[i] * pop.m +
              cost.sch$number.sachet.prsm[i] * (cost.sch$cov[i] *  (pop + pop.m*cost.sch$tau.m[i]) * cost.sch$alpha.s[i] * cost.sch$sigma.s.to.mam[i]) + 
              cost.sch$number.sachet.prmm[i] * (cost.sch$cov[i] *  pop.m * cost.sch$alpha.m[i] * cost.sch$sigma.m.to.mam[i]))*cost.sch$cost.rusf[i] - budget
      
      return(m^2)  # Return the squared residual to minimize
    }
    
    mw.m.csb <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      m <- (cost.sch$number.sachet.m[i] * pop.m +
              cost.sch$number.sachet.prsm[i] * (cost.sch$cov[i] *  (pop + pop.m*cost.sch$tau.m[i]) * cost.sch$alpha.s[i] * cost.sch$sigma.s.to.mam[i]) + 
              cost.sch$number.sachet.prmm[i] * (cost.sch$cov[i] *  pop.m * cost.sch$alpha.m[i] * cost.sch$sigma.m.to.mam[i]))*cost.sch$cost.csb[i]/6 - budget
      
      return(m^2)  # Return the squared residual to minimize
    }
    
    ######### ######### 
    ######### OptimA
    ######### ######### 
    
    sw.s1 <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      s1 <- (cost.sch$number.sachet.s1[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
               cost.sch$number.sachet.prss1[i] * (cost.sch$cov[i] * cost.sch$alpha.s1[i] * cost.sch$sigma.s1.to.sam[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
                                                    cost.sch$cov[i] * cost.sch$tau.m[i] * (pop + pop.m * cost.sch$tau.m[i]) * cost.sch$alpha.s1[i] * cost.sch$sigma.s1.to.mam[i]) + 
               cost.sch$number.sachet.prms1[i] * (cost.sch$cov[i] * pop.m * cost.sch$alpha.m1[i] * cost.sch$sigma.m1.to.sam[i] + 
                                                    cost.sch$cov[i] * pop.m * cost.sch$alpha.m1[i] * cost.sch$sigma.m1.to.mam[i] * cost.sch$tau.m[i]))*cost.sch$cost.rutf[i] - budget
      
      return(s1^2)  # Return the squared residual to minimize
    }
    
    mw.m1 <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      m1 <- (cost.sch$number.sachet.m1[i] * pop.m +
               cost.sch$number.sachet.prsm1[i] * (cost.sch$cov[i] *  (pop + pop.m*cost.sch$tau.m[i]) * cost.sch$alpha.s1[i] * cost.sch$sigma.s1.to.mam[i]) + 
               cost.sch$number.sachet.prmm1[i] * (cost.sch$cov[i] *  pop.m * cost.sch$alpha.m1[i] * cost.sch$sigma.m1.to.mam[i]))*cost.sch$cost.rutf[i] - budget
      
      return(m1^2)  # Return the squared residual to minimize
    }
    
    ######### ######### 
    ######### ComPAS
    ######### ######### 
    
    sw.s2 <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      s2 <- (cost.sch$number.sachet.s2[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
               cost.sch$number.sachet.prss2[i] * (cost.sch$cov[i] * cost.sch$alpha.s2[i] * cost.sch$sigma.s2.to.sam[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
                                                    cost.sch$cov[i] * cost.sch$tau.m[i] * (pop + pop.m * cost.sch$tau.m[i]) * cost.sch$alpha.s2[i] * cost.sch$sigma.s2.to.mam[i]) + 
               cost.sch$number.sachet.prms2[i] * (cost.sch$cov[i] * pop.m * cost.sch$alpha.m2[i] * cost.sch$sigma.m2.to.sam[i] + 
                                                    cost.sch$cov[i] * pop.m * cost.sch$alpha.m2[i] * cost.sch$sigma.m2.to.mam[i] * cost.sch$tau.m[i]))*cost.sch$cost.rutf[i] - budget
      
      return(s2^2)  # Return the squared residual to minimize
    }
    
    mw.m2 <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      m2 <- (cost.sch$number.sachet.m2[i] * pop.m +
               cost.sch$number.sachet.prsm2[i] * (cost.sch$cov[i] *  (pop + pop.m*cost.sch$tau.m[i]) * cost.sch$alpha.s2[i] * cost.sch$sigma.s2.to.mam[i]) + 
               cost.sch$number.sachet.prmm2[i] * (cost.sch$cov[i] *  pop.m * cost.sch$alpha.m2[i] * cost.sch$sigma.m2.to.mam[i]))*cost.sch$cost.rutf[i] - budget
      
      return(m2^2)  # Return the squared residual to minimize
    }
    
    ######### ######### 
    ######### MANGO
    ######### ######### 
    
    sw.s3 <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      s3 <- (cost.sch$number.sachet.s3[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
               cost.sch$number.sachet.prss3[i] * (cost.sch$cov[i] * cost.sch$alpha.s3[i] * cost.sch$sigma.s3.to.sam[i] * (pop + pop.m * cost.sch$tau.m[i]) + 
                                                    cost.sch$cov[i] * cost.sch$tau.m[i] * (pop + pop.m * cost.sch$tau.m[i]) * cost.sch$alpha.s3[i] * cost.sch$sigma.s3.to.mam[i]) + 
               cost.sch$number.sachet.prms3[i] * (cost.sch$cov[i] * pop.m * cost.sch$alpha.m3[i] * cost.sch$sigma.m3.to.sam[i] + 
                                                    cost.sch$cov[i] * pop.m * cost.sch$alpha.m3[i] * cost.sch$sigma.m3.to.mam[i] * cost.sch$tau.m[i]))*cost.sch$cost.rutf[i] - budget
      
      return(s3^2)  # Return the squared residual to minimize
    }
    
    mw.m3 <- function(params, i) {
      # Extract pop and pop.m from params
      pop <- params[1]
      pop.m <- params[2]
      
      # Equation with both pop and pop.m
      m3 <- (cost.sch$number.sachet.m3[i] * pop.m +
               cost.sch$number.sachet.prsm3[i] * (cost.sch$cov[i] *  (pop + pop.m*cost.sch$tau.m[i]) * cost.sch$alpha.s3[i] * cost.sch$sigma.s3.to.mam[i]) + 
               cost.sch$number.sachet.prmm3[i] * (cost.sch$cov[i] *  pop.m * cost.sch$alpha.m3[i] * cost.sch$sigma.m3.to.mam[i]))*cost.sch$cost.rusf[i] - budget
      
      return(m3^2)  # Return the squared residual to minimize
    }

    ######### ######### ################## ######### ######### ######### ######### ################## ######### ######### ##### ######### ################## ######### ########
    ######### ######### ################## ######### ######### Define population ################## ######### ######### ##### ######### ################## ######### ########
    ######### ######### ################## ######### ######### ######### ######### ################## ######### ######### ##### ######### ################## ######### ########
    
    # Initial guesses for pop and pop.m (you can adjust these)
    initial_guess <- c(1000, 500)  # Initial guesses for pop and pop.m
    
    
    optimal_values_s <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = sw.s, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    
    optimal_values_s1 <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = sw.s1, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    
    optimal_values_s2 <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = sw.s2, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    optimal_values_s3 <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = sw.s3, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    optimal_values_m <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = mw.m, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    optimal_values_m_csb <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = mw.m.csb, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    optimal_values_m1 <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = mw.m1, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    optimal_values_m2 <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = mw.m2, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    optimal_values_m3 <- lapply(1:nrow(cost.sch), function(i) {
      optimal_values <- optim(par = initial_guess, fn = mw.m3, i = i, method = "BFGS")
      return(optimal_values$par)
    })
    
    
    optimal_pop_s <- sapply(optimal_values_s, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_s <- sapply(optimal_values_s, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_s1 <- sapply(optimal_values_s1, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_s1 <- sapply(optimal_values_s1, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_s2 <- sapply(optimal_values_s2, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_s2 <- sapply(optimal_values_s2, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_s3 <- sapply(optimal_values_s3, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_s3 <- sapply(optimal_values_s3, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_m <- sapply(optimal_values_m, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_m <- sapply(optimal_values_m, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_m_csb <- sapply(optimal_values_m_csb, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_m_csb <- sapply(optimal_values_m_csb, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_m1 <- sapply(optimal_values_m1, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_m1 <- sapply(optimal_values_m1, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_m2 <- sapply(optimal_values_m2, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_m2 <- sapply(optimal_values_m2, function(x) x[2])  # Extract pop.m values for all i
    
    optimal_pop_m3 <- sapply(optimal_values_m3, function(x) x[1])  # Extract pop values for all i
    optimal_pop_m_m3 <- sapply(optimal_values_m3, function(x) x[2])  # Extract pop.m values for all i
    
    #### given budget
    if (scenario == 1){
    cost.sch$optimal.pop.s.givenbudget <- optimal_pop_s
    cost.sch$optimal.pop.m.s.givenbudget <- optimal_pop_m_s
    
    cost.sch$optimal.pop.s1.givenbudget <- optimal_pop_s1
    cost.sch$optimal.pop.m.s1.givenbudget <- optimal_pop_m_s1
    
    cost.sch$optimal.pop.s2.givenbudget <- optimal_pop_s2
    cost.sch$optimal.pop.m.s2.givenbudget <- optimal_pop_m_s2
    
    cost.sch$optimal.pop.s3.givenbudget <- optimal_pop_s3
    cost.sch$optimal.pop.m.s3.givenbudget <- optimal_pop_m_s3
    
    cost.sch$optimal.pop.m.givenbudget <- optimal_pop_m
    cost.sch$optimal.pop.m.m.givenbudget <- optimal_pop_m_m
    
    cost.sch$optimal.pop.m.csb.givenbudget <- optimal_pop_m_csb
    cost.sch$optimal.pop.m.m.csb.givenbudget <- optimal_pop_m_m_csb
    
    cost.sch$optimal.pop.m1.givenbudget <- optimal_pop_m1
    cost.sch$optimal.pop.m.m1.givenbudget <- optimal_pop_m_m1
    
    cost.sch$optimal.pop.m2.givenbudget <- optimal_pop_m2
    cost.sch$optimal.pop.m.m2.givenbudget <- optimal_pop_m_m2
    
    cost.sch$optimal.pop.m3.givenbudget <- optimal_pop_m3
    cost.sch$optimal.pop.m.m3.givenbudget <- optimal_pop_m_m3
    
    cost.sch$optimal.trt.s.givenbudget <- (cost.sch$optimal.pop.s.givenbudget+ cost.sch$optimal.pop.m.s.givenbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s * cost.sch$sigma.s.to.sam  *  (cost.sch$optimal.pop.s.givenbudget+ cost.sch$optimal.pop.m.s.givenbudget*cost.sch$tau.m) + 
                                      cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s.givenbudget+ cost.sch$optimal.pop.m.s.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                      cost.sch$cov* cost.sch$optimal.pop.m.s.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.sam  + cost.sch$cov * cost.sch$optimal.pop.m.s.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m.givenbudget <- (cost.sch$optimal.pop.m.m.givenbudget + cost.sch$cov *  (cost.sch$optimal.pop.m.givenbudget + cost.sch$optimal.pop.m.m.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                      cost.sch$cov *  cost.sch$optimal.pop.m.m.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam)
    
    cost.sch$optimal.trt.m.csb.givenbudget <- (cost.sch$optimal.pop.m.m.csb.givenbudget + cost.sch$cov *  (cost.sch$optimal.pop.m.csb.givenbudget + cost.sch$optimal.pop.m.m.csb.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                          cost.sch$cov *  cost.sch$optimal.pop.m.m.csb.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam)
    
    cost.sch$optimal.trt.s1.givenbudget <- (cost.sch$optimal.pop.s1.givenbudget + cost.sch$optimal.pop.m.s1.givenbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s1 * cost.sch$sigma.s1.to.sam  *  (cost.sch$optimal.pop.s1.givenbudget + cost.sch$optimal.pop.m.s1.givenbudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s1.givenbudget + cost.sch$optimal.pop.m.s1.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam + 
                                       cost.sch$cov* cost.sch$optimal.pop.m.s1.givenbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s1.givenbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m1.givenbudget <- (cost.sch$optimal.pop.m.m1.givenbudget + cost.sch$cov *  (cost.sch$optimal.pop.m1.givenbudget + cost.sch$optimal.pop.m.m1.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m1.givenbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam)
    
    
    cost.sch$optimal.trt.s2.givenbudget <- (cost.sch$optimal.pop.s2.givenbudget + cost.sch$optimal.pop.m.s2.givenbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s2 * cost.sch$sigma.s2.to.sam  *  (cost.sch$optimal.pop.s2.givenbudget + cost.sch$optimal.pop.m.s2.givenbudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s2.givenbudget + cost.sch$optimal.pop.m.s2.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam +
                                       cost.sch$cov* cost.sch$optimal.pop.m.s2.givenbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s2.givenbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m2.givenbudget <- (cost.sch$optimal.pop.m.m2.givenbudget + cost.sch$cov *  (cost.sch$optimal.pop.m2.givenbudget + cost.sch$optimal.pop.m.m2.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m2.givenbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam  )
    
    
    cost.sch$optimal.trt.s3.givenbudget <- (cost.sch$optimal.pop.s3.givenbudget + cost.sch$optimal.pop.m.s3.givenbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s3 * cost.sch$sigma.s3.to.sam  *  (cost.sch$optimal.pop.s3.givenbudget + cost.sch$optimal.pop.m.s3.givenbudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s3.givenbudget + cost.sch$optimal.pop.m.s3.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam + 
                                       cost.sch$cov* cost.sch$optimal.pop.m.s3.givenbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s3.givenbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m3.givenbudget <- (cost.sch$optimal.pop.m.m3.givenbudget + cost.sch$cov *  (cost.sch$optimal.pop.m3.givenbudget + cost.sch$optimal.pop.m.m3.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m3.givenbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam)    
    
    cost.sch$optimal.sachet.s.givenbudget <- (cost.sch$number.sachet.s * (cost.sch$optimal.pop.s.givenbudget+ cost.sch$optimal.pop.m.s.givenbudget*cost.sch$tau.m) + 
                                         cost.sch$number.sachet.prss *  (cost.sch$cov*cost.sch$alpha.s * cost.sch$sigma.s.to.sam  *  (cost.sch$optimal.pop.s.givenbudget+ cost.sch$optimal.pop.m.s.givenbudget*cost.sch$tau.m) + 
                                                                           cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s.givenbudget+ cost.sch$optimal.pop.m.s.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                         cost.sch$number.sachet.prms * (cost.sch$cov* cost.sch$optimal.pop.m.s.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.sam  + 
                                                                          cost.sch$cov * cost.sch$optimal.pop.m.s.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m.givenbudget <- (cost.sch$number.sachet.m* cost.sch$optimal.pop.m.m.givenbudget + 
                                         cost.sch$number.sachet.prsm * (cost.sch$cov *  (cost.sch$optimal.pop.m.givenbudget + cost.sch$optimal.pop.m.m.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                         cost.sch$number.sachet.prmm * (cost.sch$cov *  cost.sch$optimal.pop.m.m.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam))
    
    cost.sch$optimal.sachet.m.csb.givenbudget <- (cost.sch$number.csb.m * cost.sch$optimal.pop.m.m.csb.givenbudget + 
                                             cost.sch$number.csb.prsm * (cost.sch$cov *  (cost.sch$optimal.pop.m.csb.givenbudget + cost.sch$optimal.pop.m.m.csb.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                             cost.sch$number.csb.prmm * (cost.sch$cov *  cost.sch$optimal.pop.m.m.csb.givenbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam))
    
    cost.sch$optimal.sachet.s1.givenbudget <- (cost.sch$number.sachet.s1 * (cost.sch$optimal.pop.s1.givenbudget + cost.sch$optimal.pop.m.s1.givenbudget*cost.sch$tau.m) + 
                                          cost.sch$number.sachet.prss1 * (cost.sch$cov*cost.sch$alpha.s1 * cost.sch$sigma.s1.to.sam  *  (cost.sch$optimal.pop.s1.givenbudget + cost.sch$optimal.pop.m.s1.givenbudget*cost.sch$tau.m) +    
                                                                            cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s1.givenbudget + cost.sch$optimal.pop.m.s1.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam) + 
                                          cost.sch$number.sachet.prms1 * (cost.sch$cov* cost.sch$optimal.pop.m.s1.givenbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s1.givenbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m1.givenbudget <- (cost.sch$number.sachet.m1 * cost.sch$optimal.pop.m.m1.givenbudget + 
                                          cost.sch$number.sachet.prsm1 * (cost.sch$cov *  (cost.sch$optimal.pop.m1.givenbudget + cost.sch$optimal.pop.m.m1.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam) +
                                          cost.sch$number.sachet.prmm1 * (cost.sch$cov *  cost.sch$optimal.pop.m.m1.givenbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam))
    
    cost.sch$optimal.sachet.s2.givenbudget <- (cost.sch$number.sachet.s2 * (cost.sch$optimal.pop.s2.givenbudget + cost.sch$optimal.pop.m.s2.givenbudget*cost.sch$tau.m) + 
                                          cost.sch$number.sachet.prss2 * (cost.sch$cov*cost.sch$alpha.s2 * cost.sch$sigma.s2.to.sam  *  (cost.sch$optimal.pop.s2.givenbudget + cost.sch$optimal.pop.m.s2.givenbudget*cost.sch$tau.m) +    
                                                                            cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s2.givenbudget + cost.sch$optimal.pop.m.s2.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam) +
                                          cost.sch$number.sachet.prms2 * (cost.sch$cov* cost.sch$optimal.pop.m.s2.givenbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s2.givenbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m2.givenbudget <- (cost.sch$number.sachet.m2 * cost.sch$optimal.pop.m.m2.givenbudget + 
                                          cost.sch$number.sachet.prsm2 * (cost.sch$cov *  (cost.sch$optimal.pop.m2.givenbudget + cost.sch$optimal.pop.m.m2.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam) +
                                          cost.sch$number.sachet.prmm2 * (cost.sch$cov *  cost.sch$optimal.pop.m.m2.givenbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam) )
    
    cost.sch$optimal.sachet.s3.givenbudget <- (cost.sch$number.sachet.s3 * (cost.sch$optimal.pop.s3.givenbudget + cost.sch$optimal.pop.m.s3.givenbudget*cost.sch$tau.m) + 
                                          cost.sch$cov*cost.sch$alpha.s3 * cost.sch$sigma.s3.to.sam  *  (cost.sch$optimal.pop.s3.givenbudget + cost.sch$optimal.pop.m.s3.givenbudget*cost.sch$tau.m) +    
                                          cost.sch$number.sachet.prss3 * (cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s3.givenbudget + cost.sch$optimal.pop.m.s3.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam) + 
                                          cost.sch$number.sachet.prms3 * (cost.sch$cov* cost.sch$optimal.pop.m.s3.givenbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s3.givenbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m3.givenbudget <- (cost.sch$number.sachet.m3 * cost.sch$optimal.pop.m.m3.givenbudget + 
                                          cost.sch$number.sachet.prsm3 * (cost.sch$cov *  (cost.sch$optimal.pop.m3.givenbudget + cost.sch$optimal.pop.m.m3.givenbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam) +
                                          cost.sch$number.sachet.prmm3 * (cost.sch$cov *  cost.sch$optimal.pop.m.m3.givenbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam))    
    }
    
    #### half budget
    else if (scenario == 2){
    cost.sch$optimal.pop.s.halfbudget <- optimal_pop_s
    cost.sch$optimal.pop.m.s.halfbudget <- optimal_pop_m_s
    
    cost.sch$optimal.pop.s1.halfbudget <- optimal_pop_s1
    cost.sch$optimal.pop.m.s1.halfbudget <- optimal_pop_m_s1
    
    cost.sch$optimal.pop.s2.halfbudget <- optimal_pop_s2
    cost.sch$optimal.pop.m.s2.halfbudget <- optimal_pop_m_s2
    
    cost.sch$optimal.pop.s3.halfbudget <- optimal_pop_s3
    cost.sch$optimal.pop.m.s3.halfbudget <- optimal_pop_m_s3
    
    cost.sch$optimal.pop.m.halfbudget <- optimal_pop_m
    cost.sch$optimal.pop.m.m.halfbudget <- optimal_pop_m_m
    
    cost.sch$optimal.pop.m.csb.halfbudget <- optimal_pop_m_csb
    cost.sch$optimal.pop.m.m.csb.halfbudget <- optimal_pop_m_m_csb
    
    cost.sch$optimal.pop.m1.halfbudget <- optimal_pop_m1
    cost.sch$optimal.pop.m.m1.halfbudget <- optimal_pop_m_m1
    
    cost.sch$optimal.pop.m2.halfbudget <- optimal_pop_m2
    cost.sch$optimal.pop.m.m2.halfbudget <- optimal_pop_m_m2
    
    cost.sch$optimal.pop.m3.halfbudget <- optimal_pop_m3
    cost.sch$optimal.pop.m.m3.halfbudget <- optimal_pop_m_m3
    
    cost.sch$optimal.trt.s.halfbudget <- (cost.sch$optimal.pop.s.halfbudget+ cost.sch$optimal.pop.m.s.halfbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s * cost.sch$sigma.s.to.sam  *  (cost.sch$optimal.pop.s.halfbudget+ cost.sch$optimal.pop.m.s.halfbudget*cost.sch$tau.m) + 
                                      cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s.halfbudget+ cost.sch$optimal.pop.m.s.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                      cost.sch$cov* cost.sch$optimal.pop.m.s.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.sam  + cost.sch$cov * cost.sch$optimal.pop.m.s.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m.halfbudget <- (cost.sch$optimal.pop.m.m.halfbudget + cost.sch$cov *  (cost.sch$optimal.pop.m.halfbudget + cost.sch$optimal.pop.m.m.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                      cost.sch$cov *  cost.sch$optimal.pop.m.m.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam)
    
    cost.sch$optimal.trt.m.csb.halfbudget <- (cost.sch$optimal.pop.m.m.csb.halfbudget + cost.sch$cov *  (cost.sch$optimal.pop.m.csb.halfbudget + cost.sch$optimal.pop.m.m.csb.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                          cost.sch$cov *  cost.sch$optimal.pop.m.m.csb.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam)
    
    cost.sch$optimal.trt.s1.halfbudget <- (cost.sch$optimal.pop.s1.halfbudget + cost.sch$optimal.pop.m.s1.halfbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s1 * cost.sch$sigma.s1.to.sam  *  (cost.sch$optimal.pop.s1.halfbudget + cost.sch$optimal.pop.m.s1.halfbudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s1.halfbudget + cost.sch$optimal.pop.m.s1.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam + 
                                       cost.sch$cov* cost.sch$optimal.pop.m.s1.halfbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s1.halfbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m1.halfbudget <- (cost.sch$optimal.pop.m.m1.halfbudget + cost.sch$cov *  (cost.sch$optimal.pop.m1.halfbudget + cost.sch$optimal.pop.m.m1.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m1.halfbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam)
    
    
    cost.sch$optimal.trt.s2.halfbudget <- (cost.sch$optimal.pop.s2.halfbudget + cost.sch$optimal.pop.m.s2.halfbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s2 * cost.sch$sigma.s2.to.sam  *  (cost.sch$optimal.pop.s2.halfbudget + cost.sch$optimal.pop.m.s2.halfbudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s2.halfbudget + cost.sch$optimal.pop.m.s2.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam +
                                       cost.sch$cov* cost.sch$optimal.pop.m.s2.halfbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s2.halfbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m2.halfbudget <- (cost.sch$optimal.pop.m.m2.halfbudget + cost.sch$cov *  (cost.sch$optimal.pop.m2.halfbudget + cost.sch$optimal.pop.m.m2.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m2.halfbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam  )
    
    
    cost.sch$optimal.trt.s3.halfbudget <- (cost.sch$optimal.pop.s3.halfbudget + cost.sch$optimal.pop.m.s3.halfbudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s3 * cost.sch$sigma.s3.to.sam  *  (cost.sch$optimal.pop.s3.halfbudget + cost.sch$optimal.pop.m.s3.halfbudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s3.halfbudget + cost.sch$optimal.pop.m.s3.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam + 
                                       cost.sch$cov* cost.sch$optimal.pop.m.s3.halfbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s3.halfbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m3.halfbudget <- (cost.sch$optimal.pop.m.m3.halfbudget + cost.sch$cov *  (cost.sch$optimal.pop.m3.halfbudget + cost.sch$optimal.pop.m.m3.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m3.halfbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam)    
    
    cost.sch$optimal.sachet.s.halfbudget <- (cost.sch$number.sachet.s * (cost.sch$optimal.pop.s.halfbudget+ cost.sch$optimal.pop.m.s.halfbudget*cost.sch$tau.m) + 
                                         cost.sch$number.sachet.prss *  (cost.sch$cov*cost.sch$alpha.s * cost.sch$sigma.s.to.sam  *  (cost.sch$optimal.pop.s.halfbudget+ cost.sch$optimal.pop.m.s.halfbudget*cost.sch$tau.m) + 
                                                                           cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s.halfbudget+ cost.sch$optimal.pop.m.s.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                         cost.sch$number.sachet.prms * (cost.sch$cov* cost.sch$optimal.pop.m.s.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.sam  + 
                                                                          cost.sch$cov * cost.sch$optimal.pop.m.s.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m.halfbudget <- (cost.sch$number.sachet.m* cost.sch$optimal.pop.m.m.halfbudget + 
                                         cost.sch$number.sachet.prsm * (cost.sch$cov *  (cost.sch$optimal.pop.m.halfbudget + cost.sch$optimal.pop.m.m.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                         cost.sch$number.sachet.prmm * (cost.sch$cov *  cost.sch$optimal.pop.m.m.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam))
    
    cost.sch$optimal.sachet.m.csb.halfbudget <- (cost.sch$number.csb.m * cost.sch$optimal.pop.m.m.csb.halfbudget + 
                                             cost.sch$number.csb.prsm * (cost.sch$cov *  (cost.sch$optimal.pop.m.csb.halfbudget + cost.sch$optimal.pop.m.m.csb.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                             cost.sch$number.csb.prmm * (cost.sch$cov *  cost.sch$optimal.pop.m.m.csb.halfbudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam))
    
    cost.sch$optimal.sachet.s1.halfbudget <- (cost.sch$number.sachet.s1 * (cost.sch$optimal.pop.s1.halfbudget + cost.sch$optimal.pop.m.s1.halfbudget*cost.sch$tau.m) + 
                                          cost.sch$number.sachet.prss1 * (cost.sch$cov*cost.sch$alpha.s1 * cost.sch$sigma.s1.to.sam  *  (cost.sch$optimal.pop.s1.halfbudget + cost.sch$optimal.pop.m.s1.halfbudget*cost.sch$tau.m) +    
                                                                            cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s1.halfbudget + cost.sch$optimal.pop.m.s1.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam) + 
                                          cost.sch$number.sachet.prms1 * (cost.sch$cov* cost.sch$optimal.pop.m.s1.halfbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s1.halfbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m1.halfbudget <- (cost.sch$number.sachet.m1 * cost.sch$optimal.pop.m.m1.halfbudget + 
                                          cost.sch$number.sachet.prsm1 * (cost.sch$cov *  (cost.sch$optimal.pop.m1.halfbudget + cost.sch$optimal.pop.m.m1.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam) +
                                          cost.sch$number.sachet.prmm1 * (cost.sch$cov *  cost.sch$optimal.pop.m.m1.halfbudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam))
    
    cost.sch$optimal.sachet.s2.halfbudget <- (cost.sch$number.sachet.s2 * (cost.sch$optimal.pop.s2.halfbudget + cost.sch$optimal.pop.m.s2.halfbudget*cost.sch$tau.m) + 
                                          cost.sch$number.sachet.prss2 * (cost.sch$cov*cost.sch$alpha.s2 * cost.sch$sigma.s2.to.sam  *  (cost.sch$optimal.pop.s2.halfbudget + cost.sch$optimal.pop.m.s2.halfbudget*cost.sch$tau.m) +    
                                                                            cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s2.halfbudget + cost.sch$optimal.pop.m.s2.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam) +
                                          cost.sch$number.sachet.prms2 * (cost.sch$cov* cost.sch$optimal.pop.m.s2.halfbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s2.halfbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m2.halfbudget <- (cost.sch$number.sachet.m2 * cost.sch$optimal.pop.m.m2.halfbudget + 
                                          cost.sch$number.sachet.prsm2 * (cost.sch$cov *  (cost.sch$optimal.pop.m2.halfbudget + cost.sch$optimal.pop.m.m2.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam) +
                                          cost.sch$number.sachet.prmm2 * (cost.sch$cov *  cost.sch$optimal.pop.m.m2.halfbudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam) )
    
    cost.sch$optimal.sachet.s3.halfbudget <- (cost.sch$number.sachet.s3 * (cost.sch$optimal.pop.s3.halfbudget + cost.sch$optimal.pop.m.s3.halfbudget*cost.sch$tau.m) + 
                                          cost.sch$cov*cost.sch$alpha.s3 * cost.sch$sigma.s3.to.sam  *  (cost.sch$optimal.pop.s3.halfbudget + cost.sch$optimal.pop.m.s3.halfbudget*cost.sch$tau.m) +    
                                          cost.sch$number.sachet.prss3 * (cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s3.halfbudget + cost.sch$optimal.pop.m.s3.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam) + 
                                          cost.sch$number.sachet.prms3 * (cost.sch$cov* cost.sch$optimal.pop.m.s3.halfbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s3.halfbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m3.halfbudget <- (cost.sch$number.sachet.m3 * cost.sch$optimal.pop.m.m3.halfbudget + 
                                          cost.sch$number.sachet.prsm3 * (cost.sch$cov *  (cost.sch$optimal.pop.m3.halfbudget + cost.sch$optimal.pop.m.m3.halfbudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam) +
                                          cost.sch$number.sachet.prmm3 * (cost.sch$cov *  cost.sch$optimal.pop.m.m3.halfbudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam))    
    }
    
    #### double budget
    else if (scenario == 3){
    cost.sch$optimal.pop.s.doublebudget <- optimal_pop_s
    cost.sch$optimal.pop.m.s.doublebudget <- optimal_pop_m_s
    
    cost.sch$optimal.pop.s1.doublebudget <- optimal_pop_s1
    cost.sch$optimal.pop.m.s1.doublebudget <- optimal_pop_m_s1
    
    cost.sch$optimal.pop.s2.doublebudget <- optimal_pop_s2
    cost.sch$optimal.pop.m.s2.doublebudget <- optimal_pop_m_s2
    
    cost.sch$optimal.pop.s3.doublebudget <- optimal_pop_s3
    cost.sch$optimal.pop.m.s3.doublebudget <- optimal_pop_m_s3
    
    cost.sch$optimal.pop.m.doublebudget <- optimal_pop_m
    cost.sch$optimal.pop.m.m.doublebudget <- optimal_pop_m_m
    
    cost.sch$optimal.pop.m.csb.doublebudget <- optimal_pop_m_csb
    cost.sch$optimal.pop.m.m.csb.doublebudget <- optimal_pop_m_m_csb
    
    cost.sch$optimal.pop.m1.doublebudget <- optimal_pop_m1
    cost.sch$optimal.pop.m.m1.doublebudget <- optimal_pop_m_m1
    
    cost.sch$optimal.pop.m2.doublebudget <- optimal_pop_m2
    cost.sch$optimal.pop.m.m2.doublebudget <- optimal_pop_m_m2
    
    cost.sch$optimal.pop.m3.doublebudget <- optimal_pop_m3
    cost.sch$optimal.pop.m.m3.doublebudget <- optimal_pop_m_m3
    
    cost.sch$optimal.trt.s.doublebudget <- (cost.sch$optimal.pop.s.doublebudget+ cost.sch$optimal.pop.m.s.doublebudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s * cost.sch$sigma.s.to.sam  *  (cost.sch$optimal.pop.s.doublebudget+ cost.sch$optimal.pop.m.s.doublebudget*cost.sch$tau.m) + 
                                      cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s.doublebudget+ cost.sch$optimal.pop.m.s.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                      cost.sch$cov* cost.sch$optimal.pop.m.s.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.sam  + cost.sch$cov * cost.sch$optimal.pop.m.s.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m.doublebudget <- (cost.sch$optimal.pop.m.m.doublebudget + cost.sch$cov *  (cost.sch$optimal.pop.m.doublebudget + cost.sch$optimal.pop.m.m.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                      cost.sch$cov *  cost.sch$optimal.pop.m.m.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam)
    
    cost.sch$optimal.trt.m.csb.doublebudget <- (cost.sch$optimal.pop.m.m.csb.doublebudget + cost.sch$cov *  (cost.sch$optimal.pop.m.csb.doublebudget + cost.sch$optimal.pop.m.m.csb.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam + 
                                          cost.sch$cov *  cost.sch$optimal.pop.m.m.csb.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam)
    
    cost.sch$optimal.trt.s1.doublebudget <- (cost.sch$optimal.pop.s1.doublebudget + cost.sch$optimal.pop.m.s1.doublebudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s1 * cost.sch$sigma.s1.to.sam  *  (cost.sch$optimal.pop.s1.doublebudget + cost.sch$optimal.pop.m.s1.doublebudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s1.doublebudget + cost.sch$optimal.pop.m.s1.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam + 
                                       cost.sch$cov* cost.sch$optimal.pop.m.s1.doublebudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s1.doublebudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m1.doublebudget <- (cost.sch$optimal.pop.m.m1.doublebudget + cost.sch$cov *  (cost.sch$optimal.pop.m1.doublebudget + cost.sch$optimal.pop.m.m1.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m1.doublebudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam)
    
    
    cost.sch$optimal.trt.s2.doublebudget <- (cost.sch$optimal.pop.s2.doublebudget + cost.sch$optimal.pop.m.s2.doublebudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s2 * cost.sch$sigma.s2.to.sam  *  (cost.sch$optimal.pop.s2.doublebudget + cost.sch$optimal.pop.m.s2.doublebudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s2.doublebudget + cost.sch$optimal.pop.m.s2.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam +
                                       cost.sch$cov* cost.sch$optimal.pop.m.s2.doublebudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s2.doublebudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m2.doublebudget <- (cost.sch$optimal.pop.m.m2.doublebudget + cost.sch$cov *  (cost.sch$optimal.pop.m2.doublebudget + cost.sch$optimal.pop.m.m2.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m2.doublebudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam  )
    
    
    cost.sch$optimal.trt.s3.doublebudget <- (cost.sch$optimal.pop.s3.doublebudget + cost.sch$optimal.pop.m.s3.doublebudget*cost.sch$tau.m + cost.sch$cov*cost.sch$alpha.s3 * cost.sch$sigma.s3.to.sam  *  (cost.sch$optimal.pop.s3.doublebudget + cost.sch$optimal.pop.m.s3.doublebudget*cost.sch$tau.m) +    
                                       cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s3.doublebudget + cost.sch$optimal.pop.m.s3.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam + 
                                       cost.sch$cov* cost.sch$optimal.pop.m.s3.doublebudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.sam  +
                                       cost.sch$cov * cost.sch$optimal.pop.m.s3.doublebudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam * cost.sch$tau.m)
    
    cost.sch$optimal.trt.m3.doublebudget <- (cost.sch$optimal.pop.m.m3.doublebudget + cost.sch$cov *  (cost.sch$optimal.pop.m3.doublebudget + cost.sch$optimal.pop.m.m3.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam +
                                       cost.sch$cov *  cost.sch$optimal.pop.m.m3.doublebudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam)    
    
    cost.sch$optimal.sachet.s.doublebudget <- (cost.sch$number.sachet.s * (cost.sch$optimal.pop.s.doublebudget+ cost.sch$optimal.pop.m.s.doublebudget*cost.sch$tau.m) + 
                                         cost.sch$number.sachet.prss *  (cost.sch$cov*cost.sch$alpha.s * cost.sch$sigma.s.to.sam  *  (cost.sch$optimal.pop.s.doublebudget+ cost.sch$optimal.pop.m.s.doublebudget*cost.sch$tau.m) + 
                                                                           cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s.doublebudget+ cost.sch$optimal.pop.m.s.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                         cost.sch$number.sachet.prms * (cost.sch$cov* cost.sch$optimal.pop.m.s.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.sam  + 
                                                                          cost.sch$cov * cost.sch$optimal.pop.m.s.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m.doublebudget <- (cost.sch$number.sachet.m* cost.sch$optimal.pop.m.m.doublebudget + 
                                         cost.sch$number.sachet.prsm * (cost.sch$cov *  (cost.sch$optimal.pop.m.doublebudget + cost.sch$optimal.pop.m.m.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                         cost.sch$number.sachet.prmm * (cost.sch$cov *  cost.sch$optimal.pop.m.m.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam))
    
    cost.sch$optimal.sachet.m.csb.doublebudget <- (cost.sch$number.csb.m * cost.sch$optimal.pop.m.m.csb.doublebudget + 
                                             cost.sch$number.csb.prsm * (cost.sch$cov *  (cost.sch$optimal.pop.m.csb.doublebudget + cost.sch$optimal.pop.m.m.csb.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s * cost.sch$sigma.s.to.mam) + 
                                             cost.sch$number.csb.prmm * (cost.sch$cov *  cost.sch$optimal.pop.m.m.csb.doublebudget * cost.sch$alpha.m * cost.sch$sigma.m.to.mam))
    
    cost.sch$optimal.sachet.s1.doublebudget <- (cost.sch$number.sachet.s1 * (cost.sch$optimal.pop.s1.doublebudget + cost.sch$optimal.pop.m.s1.doublebudget*cost.sch$tau.m) + 
                                          cost.sch$number.sachet.prss1 * (cost.sch$cov*cost.sch$alpha.s1 * cost.sch$sigma.s1.to.sam  *  (cost.sch$optimal.pop.s1.doublebudget + cost.sch$optimal.pop.m.s1.doublebudget*cost.sch$tau.m) +    
                                                                            cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s1.doublebudget + cost.sch$optimal.pop.m.s1.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam) + 
                                          cost.sch$number.sachet.prms1 * (cost.sch$cov* cost.sch$optimal.pop.m.s1.doublebudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s1.doublebudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m1.doublebudget <- (cost.sch$number.sachet.m1 * cost.sch$optimal.pop.m.m1.doublebudget + 
                                          cost.sch$number.sachet.prsm1 * (cost.sch$cov *  (cost.sch$optimal.pop.m1.doublebudget + cost.sch$optimal.pop.m.m1.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s1 * cost.sch$sigma.s1.to.mam) +
                                          cost.sch$number.sachet.prmm1 * (cost.sch$cov *  cost.sch$optimal.pop.m.m1.doublebudget * cost.sch$alpha.m1 * cost.sch$sigma.m1.to.mam))
    
    cost.sch$optimal.sachet.s2.doublebudget <- (cost.sch$number.sachet.s2 * (cost.sch$optimal.pop.s2.doublebudget + cost.sch$optimal.pop.m.s2.doublebudget*cost.sch$tau.m) + 
                                          cost.sch$number.sachet.prss2 * (cost.sch$cov*cost.sch$alpha.s2 * cost.sch$sigma.s2.to.sam  *  (cost.sch$optimal.pop.s2.doublebudget + cost.sch$optimal.pop.m.s2.doublebudget*cost.sch$tau.m) +    
                                                                            cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s2.doublebudget + cost.sch$optimal.pop.m.s2.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam) +
                                          cost.sch$number.sachet.prms2 * (cost.sch$cov* cost.sch$optimal.pop.m.s2.doublebudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s2.doublebudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m2.doublebudget <- (cost.sch$number.sachet.m2 * cost.sch$optimal.pop.m.m2.doublebudget + 
                                          cost.sch$number.sachet.prsm2 * (cost.sch$cov *  (cost.sch$optimal.pop.m2.doublebudget + cost.sch$optimal.pop.m.m2.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s2 * cost.sch$sigma.s2.to.mam) +
                                          cost.sch$number.sachet.prmm2 * (cost.sch$cov *  cost.sch$optimal.pop.m.m2.doublebudget * cost.sch$alpha.m2 * cost.sch$sigma.m2.to.mam) )
    
    cost.sch$optimal.sachet.s3.doublebudget <- (cost.sch$number.sachet.s3 * (cost.sch$optimal.pop.s3.doublebudget + cost.sch$optimal.pop.m.s3.doublebudget*cost.sch$tau.m) + 
                                          cost.sch$cov*cost.sch$alpha.s3 * cost.sch$sigma.s3.to.sam  *  (cost.sch$optimal.pop.s3.doublebudget + cost.sch$optimal.pop.m.s3.doublebudget*cost.sch$tau.m) +    
                                          cost.sch$number.sachet.prss3 * (cost.sch$cov * cost.sch$tau.m * (cost.sch$optimal.pop.s3.doublebudget + cost.sch$optimal.pop.m.s3.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam) + 
                                          cost.sch$number.sachet.prms3 * (cost.sch$cov* cost.sch$optimal.pop.m.s3.doublebudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.sam  +
                                                                            cost.sch$cov * cost.sch$optimal.pop.m.s3.doublebudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam * cost.sch$tau.m))
    
    cost.sch$optimal.sachet.m3.doublebudget <- (cost.sch$number.sachet.m3 * cost.sch$optimal.pop.m.m3.doublebudget + 
                                          cost.sch$number.sachet.prsm3 * (cost.sch$cov *  (cost.sch$optimal.pop.m3.doublebudget + cost.sch$optimal.pop.m.m3.doublebudget*cost.sch$tau.m) * cost.sch$alpha.s3 * cost.sch$sigma.s3.to.mam) +
                                          cost.sch$number.sachet.prmm3 * (cost.sch$cov *  cost.sch$optimal.pop.m.m3.doublebudget * cost.sch$alpha.m3 * cost.sch$sigma.m3.to.mam))    
    }
    
    return (cost.sch)
}

########## Wrapper function ############
f_wrapper <- function(
    
  weight.adm.s = weight.adm.s,
  muac.s = muac.s,
  sev.wasted = sev.wasted,
  mod.wasted = mod.wasted,
  cov.s = cov.s,
  cov.m = cov.m,
  treatDist.s = treatDist.s, 
  treatDist.m = treatDist.m, 
  OptimaDist = OptimaDist,
  CompasDist = CompasDist, 
  MangoDist = MangoDist,
  startingPoint = 0,
  costDist.min = costDist.min,
  costDist.max = costDist.max,
  country.name = country.name,
  reducedRecovery=reducedRecovery
){
  #setwd("/Users/lauraskrip/Dropbox/Nutrition_RUTF Protocol Cascade Model/Shiny App Code/Shiny Dashboard v5/")
  ppp = data.table(read.csv("ppp-conversion-factor.csv"))
  exch = data.table(read.csv("exchange-rate.csv"))
  weight.gain.standard = read.csv("Casez-standard-weight-gain-cumulated.csv")
  weight.gain.optima = read.csv("Casez-OptimA-weight-gain-cumulated.csv")
  muac.gain.standard = read.csv("Casez-standard-Muac-gain-cumulated.csv")
  muac.gain.optima = read.csv("Casez-OptimA-Muac-gain-cumulated.csv")
  weight.gain.mango = read.csv("mango-weight-gain.csv")
  muac.gain.compas = read.csv("ComPAS-muac-gain.csv")
  mango = data.table(read.csv("mango.data.csv"))
  sc = data.table(read.csv("scenarios.csv"))
  # need to specify environment of inner functions (to use outer function environment)
  environment(f_DoseOpt_params_lhs)   <- environment()
  environment(f_DoseOpt_cost)   <- environment()
  
  #-- Create Inputs --#
  Merg <- f_DoseOpt_params_lhs(weight.adm.s = weight.adm.s,
                              muac.s = muac.s,
                               sev.wasted = sev.wasted,
                               mod.wasted = mod.wasted,
                               cov.s = cov.s,
                               cov.m = cov.m,
                               weight.gain.standard = weight.gain.standard, 
                               weight.gain.optima = weight.gain.optima,
                               muac.gain.standard = muac.gain.standard,
                               muac.gain.optima = muac.gain.optima,
                               weight.gain.mango = weight.gain.mango,
                               muac.gain.compas = muac.gain.compas,
                               mango = mango,
                               ppp = ppp,
                               exch = exch,
                               sc = sc,
                               treatDist.s = treatDist.s,
                               treatDist.m = treatDist.m,
                               OptimaDist = OptimaDist,
                               CompasDist = CompasDist, 
                               MangoDist = MangoDist,
                               costDist.min = costDist.min,
                               costDist.max = costDist.max,
                               country.name = country.name,
                               startingPoint = 0,
                              reducedRecovery = reducedRecovery)
  pop <- sev.wasted*cov.s
  pop.m <- mod.wasted*cov.m
  
  # ifelse(sum(MangoDist)<0,
  #        MangoDist=c(mango[dosage==1 & am=="SAM" & exit_rsn == 1,]$proportion,
  #                    mango[dosage==1 & am=="SAM" & exit_rsn == 3,]$proportion,
  #                    mango[dosage==1 & am=="SAM" & exit_rsn == 6,]$proportion,
  #                    0,
  #                    mango[dosage==1 & am=="SAM" & exit_rsn == 2,]$proportion,
  #                    mango[dosage==1 & am=="MAM" & exit_rsn == 1,]$proportion,
  #                    mango[dosage==1 & am=="MAM" & exit_rsn == 3,]$proportion,
  #                    mango[dosage==1 & am=="MAM" & exit_rsn == 6,]$proportion,
  #                    mango[dosage==1 & am=="MAM" & exit_rsn == 5,]$proportion,
  #                    mango[dosage==1 & am=="MAM" & exit_rsn == 2,]$proportion),
  #        MangoDist = MangoDist)
  
  #--  Run Cost Model  --#
  cost.s.varied.relapse <- f_DoseOpt_cost(Merg,
                                          pop,pop.m,cov.m,cov.s,mod.wasted)
  
  #-- Return results --#
  
  # convert matrix to dataframe (for plots)
  #############################
  #memory.limit(2000000)
  #memory.size(NA)
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  Standard
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########

  cost.s.varied.relapse[,totalcost.s := (cost.rec.s+cost.def.s+cost.never.rec.s +cost.died.s +cost.hosp.s+cost.transf.s+cost.hosp.rec.s+
                            cost.hosp.died.s+cost.hosp.never.rec.s+ cost.rec.s.prss + cost.def.s.prss + cost.never.rec.s.prss + 
                            cost.died.s.prss + cost.hosp.s.prss + cost.transf.s.prss + cost.hosp.rec.s.prss + cost.hosp.died.s.prss + cost.hosp.never.rec.s.prss + 
                            cost.died.s.prms + cost.hosp.s.prms + cost.transf.s.prms + cost.hosp.rec.s.prms + cost.hosp.died.s.prms + cost.hosp.never.rec.s.prms),]
  
  cost.s.varied.relapse[,cost.sachet.s := (cost.sachet.rec.s + cost.sachet.def.s + cost.sachet.never.rec.s + cost.sachet.died.s + cost.sachet.hosp.s  +             
                              cost.sachet.transf.s + cost.sachet.hosp.rec.s + cost.sachet.hosp.died.s + cost.sachet.hosp.never.rec.s + cost.sachet.rec.s.prss +          
                              cost.sachet.def.s.prss + cost.sachet.never.rec.s.prss + cost.sachet.died.s.prss + cost.sachet.hosp.s.prss + cost.sachet.transf.s.prss +         
                              cost.sachet.hosp.rec.s.prss + cost.sachet.hosp.died.s.prss + cost.sachet.hosp.never.rec.s.prss+
                              cost.sachet.rec.s.prms + cost.sachet.def.s.prms + cost.sachet.never.rec.s.prms + cost.sachet.died.s.prms + cost.sachet.hosp.s.prms + cost.sachet.transf.s.prms +         
                              cost.sachet.hosp.rec.s.prms + cost.sachet.hosp.died.s.prms + cost.sachet.hosp.never.rec.s.prms),]
  
  cost.s.varied.relapse[,cost.drug.s := (cost.drug.rec.s + cost.drug.def.s + cost.drug.never.rec.s + cost.drug.died.s + 
                            cost.drug.hosp.s + cost.drug.transf.s + cost.drug.rec.s.prss + cost.drug.def.s.prss + 
                            cost.drug.never.rec.s.prss + cost.drug.died.s.prss + cost.drug.hosp.s.prss + cost.drug.transf.s.prss + 
                            cost.drug.rec.s.prms + cost.drug.def.s.prms + 
                            cost.drug.never.rec.s.prms + cost.drug.died.s.prms + cost.drug.hosp.s.prms + cost.drug.transf.s.prms),]
  
  cost.s.varied.relapse[,cost.labor.s := (cost.labor.rec.s + cost.labor.def.s + cost.labor.never.rec.s + cost.labor.died.s + cost.labor.hosp.s  +             
                             cost.labor.transf.s + cost.labor.hosp.rec.s + cost.labor.hosp.died.s + cost.labor.hosp.never.rec.s + cost.labor.rec.s.prss +          
                             cost.labor.def.s.prss + cost.labor.never.rec.s.prss + cost.labor.died.s.prss + cost.labor.hosp.s.prss + cost.labor.transf.s.prss +         
                             cost.labor.hosp.rec.s.prss + cost.labor.hosp.died.s.prss + cost.labor.hosp.never.rec.s.prss + 
                             cost.labor.rec.s.prms +          
                             cost.labor.def.s.prms + cost.labor.never.rec.s.prms + cost.labor.died.s.prms + cost.labor.hosp.s.prms + cost.labor.transf.s.prms +         
                             cost.labor.hosp.rec.s.prms + cost.labor.hosp.died.s.prms + cost.labor.hosp.never.rec.s.prms),]
  
  cost.s.varied.relapse[,cost.nonlabor.s := (cost.nonlabor.rec.s + cost.nonlabor.def.s + cost.nonlabor.never.rec.s + cost.nonlabor.died.s + cost.nonlabor.hosp.s  +             
                                cost.nonlabor.transf.s + cost.nonlabor.hosp.rec.s + cost.nonlabor.hosp.died.s + cost.nonlabor.hosp.never.rec.s + cost.nonlabor.rec.s.prss +          
                                cost.nonlabor.def.s.prss + cost.nonlabor.never.rec.s.prss + cost.nonlabor.died.s.prss + cost.nonlabor.hosp.s.prss + cost.nonlabor.transf.s.prss +         
                                cost.nonlabor.hosp.rec.s.prss + cost.nonlabor.hosp.died.s.prss + cost.nonlabor.hosp.never.rec.s.prss + 
                                cost.nonlabor.rec.s.prms +          
                                cost.nonlabor.def.s.prms + cost.nonlabor.never.rec.s.prms + cost.nonlabor.died.s.prms + cost.nonlabor.hosp.s.prms + cost.nonlabor.transf.s.prms +         
                                cost.nonlabor.hosp.rec.s.prms + cost.nonlabor.hosp.died.s.prms + cost.nonlabor.hosp.never.rec.s.prms),]
  
  cost.s.varied.relapse[,cost.caregiver.s := (cost.caregiver.rec.s + cost.caregiver.def.s + cost.caregiver.never.rec.s + cost.caregiver.died.s + cost.caregiver.hosp.s  +             
                                 cost.caregiver.transf.s + cost.caregiver.hosp.rec.s + cost.caregiver.hosp.died.s + cost.caregiver.hosp.never.rec.s + cost.caregiver.rec.s.prss +          
                                 cost.caregiver.def.s.prss + cost.caregiver.never.rec.s.prss + cost.caregiver.died.s.prss + cost.caregiver.hosp.s.prss + cost.caregiver.transf.s.prss +         
                                 cost.caregiver.hosp.rec.s.prss + cost.caregiver.hosp.died.s.prss + cost.caregiver.hosp.never.rec.s.prss + 
                                 cost.caregiver.rec.s.prms +          
                                 cost.caregiver.def.s.prms + cost.caregiver.never.rec.s.prms + cost.caregiver.died.s.prms + cost.caregiver.hosp.s.prms + cost.caregiver.transf.s.prms +         
                                 cost.caregiver.hosp.rec.s.prms + cost.caregiver.hosp.died.s.prms + cost.caregiver.hosp.never.rec.s.prms),]
  
  cost.s.varied.relapse[,cost.hospitalization.s := (cost.hospitalization.hosp.rec.s  + cost.hospitalization.hosp.died.s + 
                                       cost.hospitalization.hosp.never.rec.s + cost.hospitalization.hosp.rec.s.prss + 
                                       cost.hospitalization.hosp.died.s.prss + cost.hospitalization.hosp.never.rec.s.prss + 
                                       cost.hospitalization.hosp.rec.s.prms + 
                                       cost.hospitalization.hosp.died.s.prms + cost.hospitalization.hosp.never.rec.s.prms),]
  
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  ComPAS
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  cost.s.varied.relapse[,totalcost.s2 := (cost.rec.s2+cost.def.s2+cost.never.rec.s2 +cost.died.s2 +cost.hosp.s2+cost.transf.s2+cost.hosp.rec.s2+
                             cost.hosp.died.s2+cost.hosp.never.rec.s2+ cost.rec.s2.prss + cost.def.s2.prss + cost.never.rec.s2.prss + 
                             cost.died.s2.prss + cost.hosp.s2.prss + cost.transf.s2.prss + cost.hosp.rec.s2.prss + cost.hosp.died.s2.prss + cost.hosp.never.rec.s2.prss + 
                             cost.died.s2.prms + cost.hosp.s2.prms + cost.transf.s2.prms + cost.hosp.rec.s2.prms + cost.hosp.died.s2.prms + cost.hosp.never.rec.s2.prms),]
  
  cost.s.varied.relapse[,cost.sachet.s2 := (cost.sachet.rec.s2 + cost.sachet.def.s2 + cost.sachet.never.rec.s2 + cost.sachet.died.s2 + cost.sachet.hosp.s2  +             
                               cost.sachet.transf.s2 + cost.sachet.hosp.rec.s2 + cost.sachet.hosp.died.s2 + cost.sachet.hosp.never.rec.s2 + cost.sachet.rec.s2.prss +          
                               cost.sachet.def.s2.prss + cost.sachet.never.rec.s2.prss + cost.sachet.died.s2.prss + cost.sachet.hosp.s2.prss + cost.sachet.transf.s2.prss +         
                               cost.sachet.hosp.rec.s2.prss + cost.sachet.hosp.died.s2.prss + cost.sachet.hosp.never.rec.s2.prss+
                               cost.sachet.rec.s2.prms + cost.sachet.def.s2.prms + cost.sachet.never.rec.s2.prms + cost.sachet.died.s2.prms + cost.sachet.hosp.s2.prms + cost.sachet.transf.s2.prms +         
                               cost.sachet.hosp.rec.s2.prms + cost.sachet.hosp.died.s2.prms + cost.sachet.hosp.never.rec.s2.prms),]
  
  cost.s.varied.relapse[,cost.drug.s2 := (cost.drug.rec.s2 + cost.drug.def.s2 + cost.drug.never.rec.s2 + cost.drug.died.s2 + 
                             cost.drug.hosp.s2 + cost.drug.transf.s2 + cost.drug.rec.s2.prss + cost.drug.def.s2.prss + 
                             cost.drug.never.rec.s2.prss + cost.drug.died.s2.prss + cost.drug.hosp.s2.prss + cost.drug.transf.s2.prss + 
                             cost.drug.rec.s2.prms + cost.drug.def.s2.prms + 
                             cost.drug.never.rec.s2.prms + cost.drug.died.s2.prms + cost.drug.hosp.s2.prms + cost.drug.transf.s2.prms),]
  
  cost.s.varied.relapse[,cost.labor.s2 := (cost.labor.rec.s2 + cost.labor.def.s2 + cost.labor.never.rec.s2 + cost.labor.died.s2 + cost.labor.hosp.s2  +             
                              cost.labor.transf.s2 + cost.labor.hosp.rec.s2 + cost.labor.hosp.died.s2 + cost.labor.hosp.never.rec.s2 + cost.labor.rec.s2.prss +          
                              cost.labor.def.s2.prss + cost.labor.never.rec.s2.prss + cost.labor.died.s2.prss + cost.labor.hosp.s2.prss + cost.labor.transf.s2.prss +         
                              cost.labor.hosp.rec.s2.prss + cost.labor.hosp.died.s2.prss + cost.labor.hosp.never.rec.s2.prss + 
                              cost.labor.rec.s2.prms +          
                              cost.labor.def.s2.prms + cost.labor.never.rec.s2.prms + cost.labor.died.s2.prms + cost.labor.hosp.s2.prms + cost.labor.transf.s2.prms +         
                              cost.labor.hosp.rec.s2.prms + cost.labor.hosp.died.s2.prms + cost.labor.hosp.never.rec.s2.prms),]
  
  cost.s.varied.relapse[,cost.nonlabor.s2 := (cost.nonlabor.rec.s2 + cost.nonlabor.def.s2 + cost.nonlabor.never.rec.s2 + cost.nonlabor.died.s2 + cost.nonlabor.hosp.s2  +             
                                 cost.nonlabor.transf.s2 + cost.nonlabor.hosp.rec.s2 + cost.nonlabor.hosp.died.s2 + cost.nonlabor.hosp.never.rec.s2 + cost.nonlabor.rec.s2.prss +          
                                 cost.nonlabor.def.s2.prss + cost.nonlabor.never.rec.s2.prss + cost.nonlabor.died.s2.prss + cost.nonlabor.hosp.s2.prss + cost.nonlabor.transf.s2.prss +         
                                 cost.nonlabor.hosp.rec.s2.prss + cost.nonlabor.hosp.died.s2.prss + cost.nonlabor.hosp.never.rec.s2.prss + 
                                 cost.nonlabor.rec.s2.prms +          
                                 cost.nonlabor.def.s2.prms + cost.nonlabor.never.rec.s2.prms + cost.nonlabor.died.s2.prms + cost.nonlabor.hosp.s2.prms + cost.nonlabor.transf.s2.prms +         
                                 cost.nonlabor.hosp.rec.s2.prms + cost.nonlabor.hosp.died.s2.prms + cost.nonlabor.hosp.never.rec.s2.prms),]
  
  cost.s.varied.relapse[,cost.caregiver.s2 := (cost.caregiver.rec.s2 + cost.caregiver.def.s2 + cost.caregiver.never.rec.s2 + cost.caregiver.died.s2 + cost.caregiver.hosp.s2  +             
                                  cost.caregiver.transf.s2 + cost.caregiver.hosp.rec.s2 + cost.caregiver.hosp.died.s2 + cost.caregiver.hosp.never.rec.s2 + cost.caregiver.rec.s2.prss +          
                                  cost.caregiver.def.s2.prss + cost.caregiver.never.rec.s2.prss + cost.caregiver.died.s2.prss + cost.caregiver.hosp.s2.prss + cost.caregiver.transf.s2.prss +         
                                  cost.caregiver.hosp.rec.s2.prss + cost.caregiver.hosp.died.s2.prss + cost.caregiver.hosp.never.rec.s2.prss + 
                                  cost.caregiver.rec.s2.prms +          
                                  cost.caregiver.def.s2.prms + cost.caregiver.never.rec.s2.prms + cost.caregiver.died.s2.prms + cost.caregiver.hosp.s2.prms + cost.caregiver.transf.s2.prms +         
                                  cost.caregiver.hosp.rec.s2.prms + cost.caregiver.hosp.died.s2.prms + cost.caregiver.hosp.never.rec.s2.prms),]
  
  cost.s.varied.relapse[,cost.hospitalization.s2 := (cost.hospitalization.hosp.rec.s2  + cost.hospitalization.hosp.died.s2 + 
                                        cost.hospitalization.hosp.never.rec.s2 + cost.hospitalization.hosp.rec.s2.prss + 
                                        cost.hospitalization.hosp.died.s2.prss + cost.hospitalization.hosp.never.rec.s2.prss + 
                                        cost.hospitalization.hosp.rec.s2.prms + 
                                        cost.hospitalization.hosp.died.s2.prms + cost.hospitalization.hosp.never.rec.s2.prms),]
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  OptimA
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  cost.s.varied.relapse[,totalcost.s1 := (cost.rec.s1+cost.def.s1+cost.never.rec.s1 +cost.died.s1 +cost.hosp.s1+cost.transf.s1+cost.hosp.rec.s1+
                             cost.hosp.died.s1+cost.hosp.never.rec.s1+ cost.rec.s1.prss + cost.def.s1.prss + cost.never.rec.s1.prss + 
                             cost.died.s1.prss + cost.hosp.s1.prss + cost.transf.s1.prss + cost.hosp.rec.s1.prss + cost.hosp.died.s1.prss + cost.hosp.never.rec.s1.prss + 
                             cost.died.s1.prms + cost.hosp.s1.prms + cost.transf.s1.prms + cost.hosp.rec.s1.prms + cost.hosp.died.s1.prms + cost.hosp.never.rec.s1.prms),]
  
  cost.s.varied.relapse[,cost.sachet.s1 := (cost.sachet.rec.s1 + cost.sachet.def.s1 + cost.sachet.never.rec.s1 + cost.sachet.died.s1 + cost.sachet.hosp.s1  +             
                               cost.sachet.transf.s1 + cost.sachet.hosp.rec.s1 + cost.sachet.hosp.died.s1 + cost.sachet.hosp.never.rec.s1 + cost.sachet.rec.s1.prss +          
                               cost.sachet.def.s1.prss + cost.sachet.never.rec.s1.prss + cost.sachet.died.s1.prss + cost.sachet.hosp.s1.prss + cost.sachet.transf.s1.prss +         
                               cost.sachet.hosp.rec.s1.prss + cost.sachet.hosp.died.s1.prss + cost.sachet.hosp.never.rec.s1.prss+
                               cost.sachet.rec.s1.prms + cost.sachet.def.s1.prms + cost.sachet.never.rec.s1.prms + cost.sachet.died.s1.prms + cost.sachet.hosp.s1.prms + cost.sachet.transf.s1.prms +         
                               cost.sachet.hosp.rec.s1.prms + cost.sachet.hosp.died.s1.prms + cost.sachet.hosp.never.rec.s1.prms),]
  
  cost.s.varied.relapse[,cost.drug.s1 := (cost.drug.rec.s1 + cost.drug.def.s1 + cost.drug.never.rec.s1 + cost.drug.died.s1 + 
                             cost.drug.hosp.s1 + cost.drug.transf.s1 + cost.drug.rec.s1.prss + cost.drug.def.s1.prss + 
                             cost.drug.never.rec.s1.prss + cost.drug.died.s1.prss + cost.drug.hosp.s1.prss + cost.drug.transf.s1.prss + 
                             cost.drug.rec.s1.prms + cost.drug.def.s1.prms + 
                             cost.drug.never.rec.s1.prms + cost.drug.died.s1.prms + cost.drug.hosp.s1.prms + cost.drug.transf.s1.prms),]
  
  cost.s.varied.relapse[,cost.labor.s1 := (cost.labor.rec.s1 + cost.labor.def.s1 + cost.labor.never.rec.s1 + cost.labor.died.s1 + cost.labor.hosp.s1  +             
                              cost.labor.transf.s1 + cost.labor.hosp.rec.s1 + cost.labor.hosp.died.s1 + cost.labor.hosp.never.rec.s1 + cost.labor.rec.s1.prss +          
                              cost.labor.def.s1.prss + cost.labor.never.rec.s1.prss + cost.labor.died.s1.prss + cost.labor.hosp.s1.prss + cost.labor.transf.s1.prss +         
                              cost.labor.hosp.rec.s1.prss + cost.labor.hosp.died.s1.prss + cost.labor.hosp.never.rec.s1.prss + 
                              cost.labor.rec.s1.prms +          
                              cost.labor.def.s1.prms + cost.labor.never.rec.s1.prms + cost.labor.died.s1.prms + cost.labor.hosp.s1.prms + cost.labor.transf.s1.prms +         
                              cost.labor.hosp.rec.s1.prms + cost.labor.hosp.died.s1.prms + cost.labor.hosp.never.rec.s1.prms),]
  
  cost.s.varied.relapse[,cost.nonlabor.s1 := (cost.nonlabor.rec.s1 + cost.nonlabor.def.s1 + cost.nonlabor.never.rec.s1 + cost.nonlabor.died.s1 + cost.nonlabor.hosp.s1  +             
                                 cost.nonlabor.transf.s1 + cost.nonlabor.hosp.rec.s1 + cost.nonlabor.hosp.died.s1 + cost.nonlabor.hosp.never.rec.s1 + cost.nonlabor.rec.s1.prss +          
                                 cost.nonlabor.def.s1.prss + cost.nonlabor.never.rec.s1.prss + cost.nonlabor.died.s1.prss + cost.nonlabor.hosp.s1.prss + cost.nonlabor.transf.s1.prss +         
                                 cost.nonlabor.hosp.rec.s1.prss + cost.nonlabor.hosp.died.s1.prss + cost.nonlabor.hosp.never.rec.s1.prss + 
                                 cost.nonlabor.rec.s1.prms +          
                                 cost.nonlabor.def.s1.prms + cost.nonlabor.never.rec.s1.prms + cost.nonlabor.died.s1.prms + cost.nonlabor.hosp.s1.prms + cost.nonlabor.transf.s1.prms +         
                                 cost.nonlabor.hosp.rec.s1.prms + cost.nonlabor.hosp.died.s1.prms + cost.nonlabor.hosp.never.rec.s1.prms),]
  
  cost.s.varied.relapse[,cost.caregiver.s1 := (cost.caregiver.rec.s1 + cost.caregiver.def.s1 + cost.caregiver.never.rec.s1 + cost.caregiver.died.s1 + cost.caregiver.hosp.s1  +             
                                  cost.caregiver.transf.s1 + cost.caregiver.hosp.rec.s1 + cost.caregiver.hosp.died.s1 + cost.caregiver.hosp.never.rec.s1 + cost.caregiver.rec.s1.prss +          
                                  cost.caregiver.def.s1.prss + cost.caregiver.never.rec.s1.prss + cost.caregiver.died.s1.prss + cost.caregiver.hosp.s1.prss + cost.caregiver.transf.s1.prss +         
                                  cost.caregiver.hosp.rec.s1.prss + cost.caregiver.hosp.died.s1.prss + cost.caregiver.hosp.never.rec.s1.prss + 
                                  cost.caregiver.rec.s1.prms +          
                                  cost.caregiver.def.s1.prms + cost.caregiver.never.rec.s1.prms + cost.caregiver.died.s1.prms + cost.caregiver.hosp.s1.prms + cost.caregiver.transf.s1.prms +         
                                  cost.caregiver.hosp.rec.s1.prms + cost.caregiver.hosp.died.s1.prms + cost.caregiver.hosp.never.rec.s1.prms),]
  
  cost.s.varied.relapse[,cost.hospitalization.s1 := (cost.hospitalization.hosp.rec.s1  + cost.hospitalization.hosp.died.s1 + 
                                        cost.hospitalization.hosp.never.rec.s1 + cost.hospitalization.hosp.rec.s1.prss + 
                                        cost.hospitalization.hosp.died.s1.prss + cost.hospitalization.hosp.never.rec.s1.prss + 
                                        cost.hospitalization.hosp.rec.s1.prms + 
                                        cost.hospitalization.hosp.died.s1.prms + cost.hospitalization.hosp.never.rec.s1.prms),]
  
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  MANGO
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  cost.s.varied.relapse[,totalcost.s3 := (cost.rec.s3+cost.def.s3+cost.never.rec.s3 +cost.died.s3 +cost.hosp.s3+cost.transf.s3+cost.hosp.rec.s3+
                             cost.hosp.died.s3+cost.hosp.never.rec.s3+ cost.rec.s3.prss + cost.def.s3.prss + cost.never.rec.s3.prss + 
                             cost.died.s3.prss + cost.hosp.s3.prss + cost.transf.s3.prss + cost.hosp.rec.s3.prss + cost.hosp.died.s3.prss + cost.hosp.never.rec.s3.prss + 
                             cost.died.s3.prms + cost.hosp.s3.prms + cost.transf.s3.prms + cost.hosp.rec.s3.prms + cost.hosp.died.s3.prms + cost.hosp.never.rec.s3.prms),]
  
  cost.s.varied.relapse[,cost.sachet.s3 := (cost.sachet.rec.s3 + cost.sachet.def.s3 + cost.sachet.never.rec.s3 + cost.sachet.died.s3 + cost.sachet.hosp.s3  +             
                               cost.sachet.transf.s3 + cost.sachet.hosp.rec.s3 + cost.sachet.hosp.died.s3 + cost.sachet.hosp.never.rec.s3 + cost.sachet.rec.s3.prss +          
                               cost.sachet.def.s3.prss + cost.sachet.never.rec.s3.prss + cost.sachet.died.s3.prss + cost.sachet.hosp.s3.prss + cost.sachet.transf.s3.prss +         
                               cost.sachet.hosp.rec.s3.prss + cost.sachet.hosp.died.s3.prss + cost.sachet.hosp.never.rec.s3.prss+
                               cost.sachet.rec.s3.prms + cost.sachet.def.s3.prms + cost.sachet.never.rec.s3.prms + cost.sachet.died.s3.prms + cost.sachet.hosp.s3.prms + cost.sachet.transf.s3.prms +         
                               cost.sachet.hosp.rec.s3.prms + cost.sachet.hosp.died.s3.prms + cost.sachet.hosp.never.rec.s3.prms),]
  
  cost.s.varied.relapse[,cost.drug.s3 := (cost.drug.rec.s3 + cost.drug.def.s3 + cost.drug.never.rec.s3 + cost.drug.died.s3 + 
                             cost.drug.hosp.s3 + cost.drug.transf.s3 + cost.drug.rec.s3.prss + cost.drug.def.s3.prss + 
                             cost.drug.never.rec.s3.prss + cost.drug.died.s3.prss + cost.drug.hosp.s3.prss + cost.drug.transf.s3.prss + 
                             cost.drug.rec.s3.prms + cost.drug.def.s3.prms + 
                             cost.drug.never.rec.s3.prms + cost.drug.died.s3.prms + cost.drug.hosp.s3.prms + cost.drug.transf.s3.prms),]
  
  cost.s.varied.relapse[,cost.labor.s3 := (cost.labor.rec.s3 + cost.labor.def.s3 + cost.labor.never.rec.s3 + cost.labor.died.s3 + cost.labor.hosp.s3  +             
                              cost.labor.transf.s3 + cost.labor.hosp.rec.s3 + cost.labor.hosp.died.s3 + cost.labor.hosp.never.rec.s3 + cost.labor.rec.s3.prss +          
                              cost.labor.def.s3.prss + cost.labor.never.rec.s3.prss + cost.labor.died.s3.prss + cost.labor.hosp.s3.prss + cost.labor.transf.s3.prss +         
                              cost.labor.hosp.rec.s3.prss + cost.labor.hosp.died.s3.prss + cost.labor.hosp.never.rec.s3.prss + 
                              cost.labor.rec.s3.prms +          
                              cost.labor.def.s3.prms + cost.labor.never.rec.s3.prms + cost.labor.died.s3.prms + cost.labor.hosp.s3.prms + cost.labor.transf.s3.prms +         
                              cost.labor.hosp.rec.s3.prms + cost.labor.hosp.died.s3.prms + cost.labor.hosp.never.rec.s3.prms),]
  
  cost.s.varied.relapse[,cost.nonlabor.s3 := (cost.nonlabor.rec.s3 + cost.nonlabor.def.s3 + cost.nonlabor.never.rec.s3 + cost.nonlabor.died.s3 + cost.nonlabor.hosp.s3  +             
                                 cost.nonlabor.transf.s3 + cost.nonlabor.hosp.rec.s3 + cost.nonlabor.hosp.died.s3 + cost.nonlabor.hosp.never.rec.s3 + cost.nonlabor.rec.s3.prss +          
                                 cost.nonlabor.def.s3.prss + cost.nonlabor.never.rec.s3.prss + cost.nonlabor.died.s3.prss + cost.nonlabor.hosp.s3.prss + cost.nonlabor.transf.s3.prss +         
                                 cost.nonlabor.hosp.rec.s3.prss + cost.nonlabor.hosp.died.s3.prss + cost.nonlabor.hosp.never.rec.s3.prss + 
                                 cost.nonlabor.rec.s3.prms +          
                                 cost.nonlabor.def.s3.prms + cost.nonlabor.never.rec.s3.prms + cost.nonlabor.died.s3.prms + cost.nonlabor.hosp.s3.prms + cost.nonlabor.transf.s3.prms +         
                                 cost.nonlabor.hosp.rec.s3.prms + cost.nonlabor.hosp.died.s3.prms + cost.nonlabor.hosp.never.rec.s3.prms),]
  
  cost.s.varied.relapse[,cost.caregiver.s3 := (cost.caregiver.rec.s3 + cost.caregiver.def.s3 + cost.caregiver.never.rec.s3 + cost.caregiver.died.s3 + cost.caregiver.hosp.s3  +             
                                  cost.caregiver.transf.s3 + cost.caregiver.hosp.rec.s3 + cost.caregiver.hosp.died.s3 + cost.caregiver.hosp.never.rec.s3 + cost.caregiver.rec.s3.prss +          
                                  cost.caregiver.def.s3.prss + cost.caregiver.never.rec.s3.prss + cost.caregiver.died.s3.prss + cost.caregiver.hosp.s3.prss + cost.caregiver.transf.s3.prss +         
                                  cost.caregiver.hosp.rec.s3.prss + cost.caregiver.hosp.died.s3.prss + cost.caregiver.hosp.never.rec.s3.prss + 
                                  cost.caregiver.rec.s3.prms +          
                                  cost.caregiver.def.s3.prms + cost.caregiver.never.rec.s3.prms + cost.caregiver.died.s3.prms + cost.caregiver.hosp.s3.prms + cost.caregiver.transf.s3.prms +         
                                  cost.caregiver.hosp.rec.s3.prms + cost.caregiver.hosp.died.s3.prms + cost.caregiver.hosp.never.rec.s3.prms),]
  
  cost.s.varied.relapse[,cost.hospitalization.s3 := (cost.hospitalization.hosp.rec.s3  + cost.hospitalization.hosp.died.s3 + 
                                        cost.hospitalization.hosp.never.rec.s3 + cost.hospitalization.hosp.rec.s3.prss + 
                                        cost.hospitalization.hosp.died.s3.prss + cost.hospitalization.hosp.never.rec.s3.prss + 
                                        cost.hospitalization.hosp.rec.s3.prms + 
                                        cost.hospitalization.hosp.died.s3.prms + cost.hospitalization.hosp.never.rec.s3.prms),]
  
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  Standard
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  cost.s.varied.relapse[,totalcost.m := (cost.rec.m+cost.def.m+cost.never.rec.m +cost.died.m +cost.hosp.m+cost.transf.m+cost.hosp.rec.m+
                            cost.hosp.died.m+cost.hosp.never.rec.m+ cost.rec.m.prsm + cost.def.m.prsm + cost.never.rec.m.prsm + 
                            cost.died.m.prsm + cost.hosp.m.prsm + cost.transf.m.prsm + cost.hosp.rec.m.prsm + cost.hosp.died.m.prsm + cost.hosp.never.rec.m.prsm + cost.m.reg.sam.prsm + 
                            cost.died.m.prmm + cost.hosp.m.prmm + cost.transf.m.prmm + cost.hosp.rec.m.prmm + cost.hosp.died.m.prmm + cost.hosp.never.rec.m.prmm + cost.m.reg.sam.prmm),]
  
  cost.s.varied.relapse[,totalcost.m.csb := (cost.rec.m.csb+cost.def.m.csb+cost.never.rec.m.csb +cost.died.m.csb +cost.hosp.m.csb+cost.transf.m.csb+cost.hosp.rec.m.csb+
                                cost.hosp.died.m.csb+cost.hosp.never.rec.m.csb+ cost.rec.m.prsm.csb + cost.def.m.prsm.csb + cost.never.rec.m.prsm.csb + 
                                cost.died.m.prsm.csb + cost.hosp.m.prsm.csb + cost.transf.m.prsm.csb + cost.hosp.rec.m.prsm.csb + cost.hosp.died.m.prsm.csb + cost.hosp.never.rec.m.prsm.csb + cost.m.reg.sam.prsm.csb + 
                                cost.died.m.prmm.csb + cost.hosp.m.prmm.csb + cost.transf.m.prmm.csb + cost.hosp.rec.m.prmm.csb + cost.hosp.died.m.prmm.csb + cost.hosp.never.rec.m.prmm.csb + cost.m.reg.sam.prmm.csb),]
  
  cost.s.varied.relapse[,cost.sachet.m := (cost.sachet.rec.m + cost.sachet.def.m + cost.sachet.never.rec.m + cost.sachet.died.m + cost.sachet.hosp.m  +             
                              cost.sachet.transf.m + cost.sachet.hosp.rec.m + cost.sachet.hosp.died.m + cost.sachet.hosp.never.rec.m + cost.sachet.rec.m.prsm +          
                              cost.sachet.def.m.prsm + cost.sachet.never.rec.m.prsm + cost.sachet.died.m.prsm + cost.sachet.hosp.m.prsm + cost.sachet.transf.m.prsm +         
                              cost.sachet.hosp.rec.m.prsm + cost.sachet.hosp.died.m.prsm + cost.sachet.hosp.never.rec.m.prsm + cost.sachet.m.to.sam.prsm+ 
                              cost.sachet.rec.m.prmm + cost.sachet.def.m.prmm + cost.sachet.never.rec.m.prmm + cost.sachet.died.m.prmm + cost.sachet.hosp.m.prmm + cost.sachet.transf.m.prmm +         
                              cost.sachet.hosp.rec.m.prmm + cost.sachet.hosp.died.m.prmm + cost.sachet.hosp.never.rec.m.prmm + cost.sachet.m.to.sam.prmm ),]
  
  cost.s.varied.relapse[,cost.csb.m := (cost.csb.rec.m + cost.csb.def.m + cost.csb.never.rec.m + cost.csb.died.m + cost.csb.hosp.m  +             
                           cost.csb.transf.m + cost.csb.hosp.rec.m + cost.csb.hosp.died.m + cost.csb.hosp.never.rec.m + cost.csb.rec.m.prsm +          
                           cost.csb.def.m.prsm + cost.csb.never.rec.m.prsm + cost.csb.died.m.prsm + cost.csb.hosp.m.prsm + cost.csb.transf.m.prsm +         
                           cost.csb.hosp.rec.m.prsm + cost.csb.hosp.died.m.prsm + cost.csb.hosp.never.rec.m.prsm + cost.csb.m.to.sam.prsm+ 
                           cost.csb.rec.m.prmm + cost.csb.def.m.prmm + cost.csb.never.rec.m.prmm + cost.csb.died.m.prmm + cost.csb.hosp.m.prmm + cost.csb.transf.m.prmm +         
                           cost.csb.hosp.rec.m.prmm + cost.csb.hosp.died.m.prmm + cost.csb.hosp.never.rec.m.prmm + cost.csb.m.to.sam.prmm ),]
  
  cost.s.varied.relapse[,cost.drug.m := (cost.drug.rec.m + cost.drug.def.m + cost.drug.never.rec.m + cost.drug.died.m + 
                            cost.drug.hosp.m + cost.drug.transf.m + cost.drug.rec.m.prsm + cost.drug.def.m.prsm + 
                            cost.drug.never.rec.m.prsm + cost.drug.died.m.prsm + cost.drug.hosp.m.prsm + cost.drug.transf.m.prsm + cost.drug.m.to.sam.prsm +
                            cost.drug.rec.m.prmm + cost.drug.def.m.prmm + 
                            cost.drug.never.rec.m.prmm + cost.drug.died.m.prmm + cost.drug.hosp.m.prmm + cost.drug.transf.m.prmm + cost.drug.m.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.labor.m := (cost.labor.rec.m + cost.labor.def.m + cost.labor.never.rec.m + cost.labor.died.m + cost.labor.hosp.m  +             
                             cost.labor.transf.m + cost.labor.hosp.rec.m + cost.labor.hosp.died.m + cost.labor.hosp.never.rec.m + cost.labor.rec.m.prsm +          
                             cost.labor.def.m.prsm + cost.labor.never.rec.m.prsm + cost.labor.died.m.prsm + cost.labor.hosp.m.prsm + cost.labor.transf.m.prsm +         
                             cost.labor.hosp.rec.m.prsm + cost.labor.hosp.died.m.prsm + cost.labor.hosp.never.rec.m.prsm + cost.labor.m.to.sam.prsm + 
                             cost.labor.rec.m.prmm +          
                             cost.labor.def.m.prmm + cost.labor.never.rec.m.prmm + cost.labor.died.m.prmm + cost.labor.hosp.m.prmm + cost.labor.transf.m.prmm +         
                             cost.labor.hosp.rec.m.prmm + cost.labor.hosp.died.m.prmm + cost.labor.hosp.never.rec.m.prmm + cost.labor.m.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.nonlabor.m := (cost.nonlabor.rec.m + cost.nonlabor.def.m + cost.nonlabor.never.rec.m + cost.nonlabor.died.m + cost.nonlabor.hosp.m  +             
                                cost.nonlabor.transf.m + cost.nonlabor.hosp.rec.m + cost.nonlabor.hosp.died.m + cost.nonlabor.hosp.never.rec.m + cost.nonlabor.rec.m.prsm +          
                                cost.nonlabor.def.m.prsm + cost.nonlabor.never.rec.m.prsm + cost.nonlabor.died.m.prsm + cost.nonlabor.hosp.m.prsm + cost.nonlabor.transf.m.prsm +         
                                cost.nonlabor.hosp.rec.m.prsm + cost.nonlabor.hosp.died.m.prsm + cost.nonlabor.hosp.never.rec.m.prsm + cost.nonlabor.m.to.sam.prsm + 
                                cost.nonlabor.rec.m.prmm +          
                                cost.nonlabor.def.m.prmm + cost.nonlabor.never.rec.m.prmm + cost.nonlabor.died.m.prmm + cost.nonlabor.hosp.m.prmm + cost.nonlabor.transf.m.prmm +         
                                cost.nonlabor.hosp.rec.m.prmm + cost.nonlabor.hosp.died.m.prmm + cost.nonlabor.hosp.never.rec.m.prmm + cost.nonlabor.m.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.caregiver.m := (cost.caregiver.rec.m + cost.caregiver.def.m + cost.caregiver.never.rec.m + cost.caregiver.died.m + cost.caregiver.hosp.m  +             
                                 cost.caregiver.transf.m + cost.caregiver.hosp.rec.m + cost.caregiver.hosp.died.m + cost.caregiver.hosp.never.rec.m + cost.caregiver.rec.m.prsm +          
                                 cost.caregiver.def.m.prsm + cost.caregiver.never.rec.m.prsm + cost.caregiver.died.m.prsm + cost.caregiver.hosp.m.prsm + cost.caregiver.transf.m.prsm +         
                                 cost.caregiver.hosp.rec.m.prsm + cost.caregiver.hosp.died.m.prsm + cost.caregiver.hosp.never.rec.m.prsm + cost.caregiver.m.to.sam.prsm + 
                                 cost.caregiver.rec.m.prmm +          
                                 cost.caregiver.def.m.prmm + cost.caregiver.never.rec.m.prmm + cost.caregiver.died.m.prmm + cost.caregiver.hosp.m.prmm + cost.caregiver.transf.m.prmm +         
                                 cost.caregiver.hosp.rec.m.prmm + cost.caregiver.hosp.died.m.prmm + cost.caregiver.hosp.never.rec.m.prmm + cost.caregiver.m.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.hospitalization.m := (cost.hospitalization.hosp.rec.m  + cost.hospitalization.hosp.died.m + 
                                       cost.hospitalization.hosp.never.rec.m + cost.hospitalization.hosp.rec.m.prsm + 
                                       cost.hospitalization.hosp.died.m.prsm + cost.hospitalization.hosp.never.rec.m.prsm + 
                                       cost.hospitalization.hosp.rec.m.prmm + 
                                       cost.hospitalization.hosp.died.m.prmm + cost.hospitalization.hosp.never.rec.m.prmm),]
  
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  ComPAS
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  cost.s.varied.relapse[,totalcost.m2 := (cost.rec.m2+cost.def.m2+cost.never.rec.m2 +cost.died.m2 +cost.hosp.m2+cost.transf.m2+cost.hosp.rec.m2+
                             cost.hosp.died.m2+cost.hosp.never.rec.m2+ cost.rec.m2.prsm + cost.def.m2.prsm + cost.never.rec.m2.prsm + 
                             cost.died.m2.prsm + cost.hosp.m2.prsm + cost.transf.m2.prsm + cost.hosp.rec.m2.prsm + cost.hosp.died.m2.prsm + cost.hosp.never.rec.m2.prsm + cost.m2.reg.sam.prsm + 
                             cost.died.m2.prmm + cost.hosp.m2.prmm + cost.transf.m2.prmm + cost.hosp.rec.m2.prmm + cost.hosp.died.m2.prmm + cost.hosp.never.rec.m2.prmm + cost.m2.reg.sam.prmm),]
  
  cost.s.varied.relapse[,cost.sachet.m2 := (cost.sachet.rec.m2 + cost.sachet.def.m2 + cost.sachet.never.rec.m2 + cost.sachet.died.m2 + cost.sachet.hosp.m2  +             
                               cost.sachet.transf.m2 + cost.sachet.hosp.rec.m2 + cost.sachet.hosp.died.m2 + cost.sachet.hosp.never.rec.m2 + cost.sachet.rec.m2.prsm +          
                               cost.sachet.def.m2.prsm + cost.sachet.never.rec.m2.prsm + cost.sachet.died.m2.prsm + cost.sachet.hosp.m2.prsm + cost.sachet.transf.m2.prsm +         
                               cost.sachet.hosp.rec.m2.prsm + cost.sachet.hosp.died.m2.prsm + cost.sachet.hosp.never.rec.m2.prsm + cost.sachet.m2.to.sam.prsm+ 
                               cost.sachet.rec.m2.prmm + cost.sachet.def.m2.prmm + cost.sachet.never.rec.m2.prmm + cost.sachet.died.m2.prmm + cost.sachet.hosp.m2.prmm + cost.sachet.transf.m2.prmm +         
                               cost.sachet.hosp.rec.m2.prmm + cost.sachet.hosp.died.m2.prmm + cost.sachet.hosp.never.rec.m2.prmm + cost.sachet.m2.to.sam.prmm ),]
  
  cost.s.varied.relapse[,cost.drug.m2 := (cost.drug.rec.m2 + cost.drug.def.m2 + cost.drug.never.rec.m2 + cost.drug.died.m2 + 
                             cost.drug.hosp.m2 + cost.drug.transf.m2 + cost.drug.rec.m2.prsm + cost.drug.def.m2.prsm + 
                             cost.drug.never.rec.m2.prsm + cost.drug.died.m2.prsm + cost.drug.hosp.m2.prsm + cost.drug.transf.m2.prsm + cost.drug.m2.to.sam.prsm +
                             cost.drug.rec.m2.prmm + cost.drug.def.m2.prmm + 
                             cost.drug.never.rec.m2.prmm + cost.drug.died.m2.prmm + cost.drug.hosp.m2.prmm + cost.drug.transf.m2.prmm + cost.drug.m2.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.labor.m2 := (cost.labor.rec.m2 + cost.labor.def.m2 + cost.labor.never.rec.m2 + cost.labor.died.m2 + cost.labor.hosp.m2  +             
                              cost.labor.transf.m2 + cost.labor.hosp.rec.m2 + cost.labor.hosp.died.m2 + cost.labor.hosp.never.rec.m2 + cost.labor.rec.m2.prsm +          
                              cost.labor.def.m2.prsm + cost.labor.never.rec.m2.prsm + cost.labor.died.m2.prsm + cost.labor.hosp.m2.prsm + cost.labor.transf.m2.prsm +         
                              cost.labor.hosp.rec.m2.prsm + cost.labor.hosp.died.m2.prsm + cost.labor.hosp.never.rec.m2.prsm + cost.labor.m2.to.sam.prsm + 
                              cost.labor.rec.m2.prmm +          
                              cost.labor.def.m2.prmm + cost.labor.never.rec.m2.prmm + cost.labor.died.m2.prmm + cost.labor.hosp.m2.prmm + cost.labor.transf.m2.prmm +         
                              cost.labor.hosp.rec.m2.prmm + cost.labor.hosp.died.m2.prmm + cost.labor.hosp.never.rec.m2.prmm + cost.labor.m2.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.nonlabor.m2 := (cost.nonlabor.rec.m2 + cost.nonlabor.def.m2 + cost.nonlabor.never.rec.m2 + cost.nonlabor.died.m2 + cost.nonlabor.hosp.m2  +             
                                 cost.nonlabor.transf.m2 + cost.nonlabor.hosp.rec.m2 + cost.nonlabor.hosp.died.m2 + cost.nonlabor.hosp.never.rec.m2 + cost.nonlabor.rec.m2.prsm +          
                                 cost.nonlabor.def.m2.prsm + cost.nonlabor.never.rec.m2.prsm + cost.nonlabor.died.m2.prsm + cost.nonlabor.hosp.m2.prsm + cost.nonlabor.transf.m2.prsm +         
                                 cost.nonlabor.hosp.rec.m2.prsm + cost.nonlabor.hosp.died.m2.prsm + cost.nonlabor.hosp.never.rec.m2.prsm + cost.nonlabor.m2.to.sam.prsm + 
                                 cost.nonlabor.rec.m2.prmm +          
                                 cost.nonlabor.def.m2.prmm + cost.nonlabor.never.rec.m2.prmm + cost.nonlabor.died.m2.prmm + cost.nonlabor.hosp.m2.prmm + cost.nonlabor.transf.m2.prmm +         
                                 cost.nonlabor.hosp.rec.m2.prmm + cost.nonlabor.hosp.died.m2.prmm + cost.nonlabor.hosp.never.rec.m2.prmm + cost.nonlabor.m2.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.caregiver.m2 := (cost.caregiver.rec.m2 + cost.caregiver.def.m2 + cost.caregiver.never.rec.m2 + cost.caregiver.died.m2 + cost.caregiver.hosp.m2  +             
                                  cost.caregiver.transf.m2 + cost.caregiver.hosp.rec.m2 + cost.caregiver.hosp.died.m2 + cost.caregiver.hosp.never.rec.m2 + cost.caregiver.rec.m2.prsm +          
                                  cost.caregiver.def.m2.prsm + cost.caregiver.never.rec.m2.prsm + cost.caregiver.died.m2.prsm + cost.caregiver.hosp.m2.prsm + cost.caregiver.transf.m2.prsm +         
                                  cost.caregiver.hosp.rec.m2.prsm + cost.caregiver.hosp.died.m2.prsm + cost.caregiver.hosp.never.rec.m2.prsm + cost.caregiver.m2.to.sam.prsm + 
                                  cost.caregiver.rec.m2.prmm +          
                                  cost.caregiver.def.m2.prmm + cost.caregiver.never.rec.m2.prmm + cost.caregiver.died.m2.prmm + cost.caregiver.hosp.m2.prmm + cost.caregiver.transf.m2.prmm +         
                                  cost.caregiver.hosp.rec.m2.prmm + cost.caregiver.hosp.died.m2.prmm + cost.caregiver.hosp.never.rec.m2.prmm + cost.caregiver.m2.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.hospitalization.m2 := (cost.hospitalization.hosp.rec.m2  + cost.hospitalization.hosp.died.m2 + 
                                        cost.hospitalization.hosp.never.rec.m2 + cost.hospitalization.hosp.rec.m2.prsm + 
                                        cost.hospitalization.hosp.died.m2.prsm + cost.hospitalization.hosp.never.rec.m2.prsm + 
                                        cost.hospitalization.hosp.rec.m2.prmm + 
                                        cost.hospitalization.hosp.died.m2.prmm + cost.hospitalization.hosp.never.rec.m2.prmm),]
  
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  OptimA
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  cost.s.varied.relapse[,totalcost.m1 := (cost.rec.m1+cost.def.m1+cost.never.rec.m1 +cost.died.m1 +cost.hosp.m1+cost.transf.m1+cost.hosp.rec.m1+
                             cost.hosp.died.m1+cost.hosp.never.rec.m1+ cost.rec.m1.prsm + cost.def.m1.prsm + cost.never.rec.m1.prsm + 
                             cost.died.m1.prsm + cost.hosp.m1.prsm + cost.transf.m1.prsm + cost.hosp.rec.m1.prsm + cost.hosp.died.m1.prsm + cost.hosp.never.rec.m1.prsm + cost.m1.reg.sam.prsm + 
                             cost.died.m1.prmm + cost.hosp.m1.prmm + cost.transf.m1.prmm + cost.hosp.rec.m1.prmm + cost.hosp.died.m1.prmm + cost.hosp.never.rec.m1.prmm + cost.m1.reg.sam.prmm),]
  
  cost.s.varied.relapse[,cost.sachet.m1 := (cost.sachet.rec.m1 + cost.sachet.def.m1 + cost.sachet.never.rec.m1 + cost.sachet.died.m1 + cost.sachet.hosp.m1  +             
                               cost.sachet.transf.m1 + cost.sachet.hosp.rec.m1 + cost.sachet.hosp.died.m1 + cost.sachet.hosp.never.rec.m1 + cost.sachet.rec.m1.prsm +          
                               cost.sachet.def.m1.prsm + cost.sachet.never.rec.m1.prsm + cost.sachet.died.m1.prsm + cost.sachet.hosp.m1.prsm + cost.sachet.transf.m1.prsm +         
                               cost.sachet.hosp.rec.m1.prsm + cost.sachet.hosp.died.m1.prsm + cost.sachet.hosp.never.rec.m1.prsm + cost.sachet.m1.to.sam.prsm+ 
                               cost.sachet.rec.m1.prmm + cost.sachet.def.m1.prmm + cost.sachet.never.rec.m1.prmm + cost.sachet.died.m1.prmm + cost.sachet.hosp.m1.prmm + cost.sachet.transf.m1.prmm +         
                               cost.sachet.hosp.rec.m1.prmm + cost.sachet.hosp.died.m1.prmm + cost.sachet.hosp.never.rec.m1.prmm + cost.sachet.m1.to.sam.prmm ),]
  
  cost.s.varied.relapse[,cost.drug.m1 := (cost.drug.rec.m1 + cost.drug.def.m1 + cost.drug.never.rec.m1 + cost.drug.died.m1 + 
                             cost.drug.hosp.m1 + cost.drug.transf.m1 + cost.drug.rec.m1.prsm + cost.drug.def.m1.prsm + 
                             cost.drug.never.rec.m1.prsm + cost.drug.died.m1.prsm + cost.drug.hosp.m1.prsm + cost.drug.transf.m1.prsm + cost.drug.m1.to.sam.prsm +
                             cost.drug.rec.m1.prmm + cost.drug.def.m1.prmm + 
                             cost.drug.never.rec.m1.prmm + cost.drug.died.m1.prmm + cost.drug.hosp.m1.prmm + cost.drug.transf.m1.prmm + cost.drug.m1.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.labor.m1 := (cost.labor.rec.m1 + cost.labor.def.m1 + cost.labor.never.rec.m1 + cost.labor.died.m1 + cost.labor.hosp.m1  +             
                              cost.labor.transf.m1 + cost.labor.hosp.rec.m1 + cost.labor.hosp.died.m1 + cost.labor.hosp.never.rec.m1 + cost.labor.rec.m1.prsm +          
                              cost.labor.def.m1.prsm + cost.labor.never.rec.m1.prsm + cost.labor.died.m1.prsm + cost.labor.hosp.m1.prsm + cost.labor.transf.m1.prsm +         
                              cost.labor.hosp.rec.m1.prsm + cost.labor.hosp.died.m1.prsm + cost.labor.hosp.never.rec.m1.prsm + cost.labor.m1.to.sam.prsm + 
                              cost.labor.rec.m1.prmm +          
                              cost.labor.def.m1.prmm + cost.labor.never.rec.m1.prmm + cost.labor.died.m1.prmm + cost.labor.hosp.m1.prmm + cost.labor.transf.m1.prmm +         
                              cost.labor.hosp.rec.m1.prmm + cost.labor.hosp.died.m1.prmm + cost.labor.hosp.never.rec.m1.prmm + cost.labor.m1.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.nonlabor.m1 := (cost.nonlabor.rec.m1 + cost.nonlabor.def.m1 + cost.nonlabor.never.rec.m1 + cost.nonlabor.died.m1 + cost.nonlabor.hosp.m1  +             
                                 cost.nonlabor.transf.m1 + cost.nonlabor.hosp.rec.m1 + cost.nonlabor.hosp.died.m1 + cost.nonlabor.hosp.never.rec.m1 + cost.nonlabor.rec.m1.prsm +          
                                 cost.nonlabor.def.m1.prsm + cost.nonlabor.never.rec.m1.prsm + cost.nonlabor.died.m1.prsm + cost.nonlabor.hosp.m1.prsm + cost.nonlabor.transf.m1.prsm +         
                                 cost.nonlabor.hosp.rec.m1.prsm + cost.nonlabor.hosp.died.m1.prsm + cost.nonlabor.hosp.never.rec.m1.prsm + cost.nonlabor.m1.to.sam.prsm + 
                                 cost.nonlabor.rec.m1.prmm +          
                                 cost.nonlabor.def.m1.prmm + cost.nonlabor.never.rec.m1.prmm + cost.nonlabor.died.m1.prmm + cost.nonlabor.hosp.m1.prmm + cost.nonlabor.transf.m1.prmm +         
                                 cost.nonlabor.hosp.rec.m1.prmm + cost.nonlabor.hosp.died.m1.prmm + cost.nonlabor.hosp.never.rec.m1.prmm + cost.nonlabor.m1.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.caregiver.m1 := (cost.caregiver.rec.m1 + cost.caregiver.def.m1 + cost.caregiver.never.rec.m1 + cost.caregiver.died.m1 + cost.caregiver.hosp.m1  +             
                                  cost.caregiver.transf.m1 + cost.caregiver.hosp.rec.m1 + cost.caregiver.hosp.died.m1 + cost.caregiver.hosp.never.rec.m1 + cost.caregiver.rec.m1.prsm +          
                                  cost.caregiver.def.m1.prsm + cost.caregiver.never.rec.m1.prsm + cost.caregiver.died.m1.prsm + cost.caregiver.hosp.m1.prsm + cost.caregiver.transf.m1.prsm +         
                                  cost.caregiver.hosp.rec.m1.prsm + cost.caregiver.hosp.died.m1.prsm + cost.caregiver.hosp.never.rec.m1.prsm + cost.caregiver.m1.to.sam.prsm + 
                                  cost.caregiver.rec.m1.prmm +          
                                  cost.caregiver.def.m1.prmm + cost.caregiver.never.rec.m1.prmm + cost.caregiver.died.m1.prmm + cost.caregiver.hosp.m1.prmm + cost.caregiver.transf.m1.prmm +         
                                  cost.caregiver.hosp.rec.m1.prmm + cost.caregiver.hosp.died.m1.prmm + cost.caregiver.hosp.never.rec.m1.prmm + cost.caregiver.m1.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.hospitalization.m1 := (cost.hospitalization.hosp.rec.m1  + cost.hospitalization.hosp.died.m1 + 
                                        cost.hospitalization.hosp.never.rec.m1 + cost.hospitalization.hosp.rec.m1.prsm + 
                                        cost.hospitalization.hosp.died.m1.prsm + cost.hospitalization.hosp.never.rec.m1.prsm + 
                                        cost.hospitalization.hosp.rec.m1.prmm + 
                                        cost.hospitalization.hosp.died.m1.prmm + cost.hospitalization.hosp.never.rec.m1.prmm),]
  
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  MANGO
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  
  cost.s.varied.relapse[,totalcost.m3 := (cost.rec.m3+cost.def.m3+cost.never.rec.m3 +cost.died.m3 +cost.hosp.m3+cost.transf.m3+cost.hosp.rec.m3+
                             cost.hosp.died.m3+cost.hosp.never.rec.m3+ cost.rec.m3.prsm + cost.def.m3.prsm + cost.never.rec.m3.prsm + 
                             cost.died.m3.prsm + cost.hosp.m3.prsm + cost.transf.m3.prsm + cost.hosp.rec.m3.prsm + cost.hosp.died.m3.prsm + cost.hosp.never.rec.m3.prsm + cost.m3.reg.sam.prsm + 
                             cost.died.m3.prmm + cost.hosp.m3.prmm + cost.transf.m3.prmm + cost.hosp.rec.m3.prmm + cost.hosp.died.m3.prmm + cost.hosp.never.rec.m3.prmm + cost.m3.reg.sam.prmm),]
  
  cost.s.varied.relapse[,cost.sachet.m3 := (cost.sachet.rec.m3 + cost.sachet.def.m3 + cost.sachet.never.rec.m3 + cost.sachet.died.m3 + cost.sachet.hosp.m3  +             
                               cost.sachet.transf.m3 + cost.sachet.hosp.rec.m3 + cost.sachet.hosp.died.m3 + cost.sachet.hosp.never.rec.m3 + cost.sachet.rec.m3.prsm +          
                               cost.sachet.def.m3.prsm + cost.sachet.never.rec.m3.prsm + cost.sachet.died.m3.prsm + cost.sachet.hosp.m3.prsm + cost.sachet.transf.m3.prsm +         
                               cost.sachet.hosp.rec.m3.prsm + cost.sachet.hosp.died.m3.prsm + cost.sachet.hosp.never.rec.m3.prsm + cost.sachet.m3.to.sam.prsm+ 
                               cost.sachet.rec.m3.prmm + cost.sachet.def.m3.prmm + cost.sachet.never.rec.m3.prmm + cost.sachet.died.m3.prmm + cost.sachet.hosp.m3.prmm + cost.sachet.transf.m3.prmm +         
                               cost.sachet.hosp.rec.m3.prmm + cost.sachet.hosp.died.m3.prmm + cost.sachet.hosp.never.rec.m3.prmm + cost.sachet.m3.to.sam.prmm ),]
  
  
  cost.s.varied.relapse[,cost.drug.m3 := (cost.drug.rec.m3 + cost.drug.def.m3 + cost.drug.never.rec.m3 + cost.drug.died.m3 + 
                             cost.drug.hosp.m3 + cost.drug.transf.m3 + cost.drug.rec.m3.prsm + cost.drug.def.m3.prsm + 
                             cost.drug.never.rec.m3.prsm + cost.drug.died.m3.prsm + cost.drug.hosp.m3.prsm + cost.drug.transf.m3.prsm + cost.drug.m3.to.sam.prsm +
                             cost.drug.rec.m3.prmm + cost.drug.def.m3.prmm + 
                             cost.drug.never.rec.m3.prmm + cost.drug.died.m3.prmm + cost.drug.hosp.m3.prmm + cost.drug.transf.m3.prmm + cost.drug.m3.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.labor.m3 := (cost.labor.rec.m3 + cost.labor.def.m3 + cost.labor.never.rec.m3 + cost.labor.died.m3 + cost.labor.hosp.m3  +             
                              cost.labor.transf.m3 + cost.labor.hosp.rec.m3 + cost.labor.hosp.died.m3 + cost.labor.hosp.never.rec.m3 + cost.labor.rec.m3.prsm +          
                              cost.labor.def.m3.prsm + cost.labor.never.rec.m3.prsm + cost.labor.died.m3.prsm + cost.labor.hosp.m3.prsm + cost.labor.transf.m3.prsm +         
                              cost.labor.hosp.rec.m3.prsm + cost.labor.hosp.died.m3.prsm + cost.labor.hosp.never.rec.m3.prsm + cost.labor.m3.to.sam.prsm + 
                              cost.labor.rec.m3.prmm +          
                              cost.labor.def.m3.prmm + cost.labor.never.rec.m3.prmm + cost.labor.died.m3.prmm + cost.labor.hosp.m3.prmm + cost.labor.transf.m3.prmm +         
                              cost.labor.hosp.rec.m3.prmm + cost.labor.hosp.died.m3.prmm + cost.labor.hosp.never.rec.m3.prmm + cost.labor.m3.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.nonlabor.m3 := (cost.nonlabor.rec.m3 + cost.nonlabor.def.m3 + cost.nonlabor.never.rec.m3 + cost.nonlabor.died.m3 + cost.nonlabor.hosp.m3  +             
                                 cost.nonlabor.transf.m3 + cost.nonlabor.hosp.rec.m3 + cost.nonlabor.hosp.died.m3 + cost.nonlabor.hosp.never.rec.m3 + cost.nonlabor.rec.m3.prsm +          
                                 cost.nonlabor.def.m3.prsm + cost.nonlabor.never.rec.m3.prsm + cost.nonlabor.died.m3.prsm + cost.nonlabor.hosp.m3.prsm + cost.nonlabor.transf.m3.prsm +         
                                 cost.nonlabor.hosp.rec.m3.prsm + cost.nonlabor.hosp.died.m3.prsm + cost.nonlabor.hosp.never.rec.m3.prsm + cost.nonlabor.m3.to.sam.prsm + 
                                 cost.nonlabor.rec.m3.prmm +          
                                 cost.nonlabor.def.m3.prmm + cost.nonlabor.never.rec.m3.prmm + cost.nonlabor.died.m3.prmm + cost.nonlabor.hosp.m3.prmm + cost.nonlabor.transf.m3.prmm +         
                                 cost.nonlabor.hosp.rec.m3.prmm + cost.nonlabor.hosp.died.m3.prmm + cost.nonlabor.hosp.never.rec.m3.prmm + cost.nonlabor.m3.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.caregiver.m3 := (cost.caregiver.rec.m3 + cost.caregiver.def.m3 + cost.caregiver.never.rec.m3 + cost.caregiver.died.m3 + cost.caregiver.hosp.m3  +             
                                  cost.caregiver.transf.m3 + cost.caregiver.hosp.rec.m3 + cost.caregiver.hosp.died.m3 + cost.caregiver.hosp.never.rec.m3 + cost.caregiver.rec.m3.prsm +          
                                  cost.caregiver.def.m3.prsm + cost.caregiver.never.rec.m3.prsm + cost.caregiver.died.m3.prsm + cost.caregiver.hosp.m3.prsm + cost.caregiver.transf.m3.prsm +         
                                  cost.caregiver.hosp.rec.m3.prsm + cost.caregiver.hosp.died.m3.prsm + cost.caregiver.hosp.never.rec.m3.prsm + cost.caregiver.m3.to.sam.prsm + 
                                  cost.caregiver.rec.m3.prmm +          
                                  cost.caregiver.def.m3.prmm + cost.caregiver.never.rec.m3.prmm + cost.caregiver.died.m3.prmm + cost.caregiver.hosp.m3.prmm + cost.caregiver.transf.m3.prmm +         
                                  cost.caregiver.hosp.rec.m3.prmm + cost.caregiver.hosp.died.m3.prmm + cost.caregiver.hosp.never.rec.m3.prmm + cost.caregiver.m3.to.sam.prmm),]
  
  cost.s.varied.relapse[,cost.hospitalization.m3 := (cost.hospitalization.hosp.rec.m3  + cost.hospitalization.hosp.died.m3 + 
                                        cost.hospitalization.hosp.never.rec.m3 + cost.hospitalization.hosp.rec.m3.prsm + 
                                        cost.hospitalization.hosp.died.m3.prsm + cost.hospitalization.hosp.never.rec.m3.prsm + 
                                        cost.hospitalization.hosp.rec.m3.prmm + 
                                        cost.hospitalization.hosp.died.m3.prmm + cost.hospitalization.hosp.never.rec.m3.prmm),]
  
  
  ####### For Sachets #######

  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  Standard
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########

  # SAM standard
  cost.s.varied.relapse[,number.sachet.s := (number.sachet.rec.s + number.sachet.def.s + number.sachet.never.rec.s + number.sachet.died.s +
                                        number.sachet.hosp.s + number.sachet.transf.s + number.sachet.hosp.rec.s + number.sachet.hosp.died.s +
                                        number.sachet.hosp.never.rec.s),]

  cost.s.varied.relapse[,number.sachet.prss := (number.sachet.rec.s.prss + number.sachet.def.s.prss + number.sachet.never.rec.s.prss + number.sachet.died.s.prss +
                                           number.sachet.hosp.s.prss + number.sachet.transf.s.prss + number.sachet.hosp.rec.s.prss + number.sachet.hosp.died.s.prss +
                                           number.sachet.hosp.never.rec.s.prss),]

  cost.s.varied.relapse[,number.sachet.prms := (number.sachet.rec.s.prms + number.sachet.def.s.prms + number.sachet.never.rec.s.prms + number.sachet.died.s.prms + number.sachet.hosp.s.prms +
                                           number.sachet.transf.s.prms + number.sachet.hosp.rec.s.prms + number.sachet.hosp.died.s.prms + number.sachet.hosp.never.rec.s.prms),]

  # MAM standard
  cost.s.varied.relapse[,number.sachet.m := (number.sachet.rec.m + number.sachet.def.m + number.sachet.never.rec.m + number.sachet.died.m + number.sachet.hosp.m + number.sachet.transf.m +
                                        number.sachet.hosp.rec.m + number.sachet.hosp.died.m + number.sachet.hosp.never.rec.m + number.sachet.m.to.sam),]

  cost.s.varied.relapse[,number.sachet.prsm := (number.sachet.rec.m.prsm + number.sachet.def.m.prsm + number.sachet.never.rec.m.prsm + number.sachet.died.m.prsm + number.sachet.hosp.m.prsm +
                                           number.sachet.transf.m.prsm + number.sachet.hosp.rec.m.prsm + number.sachet.hosp.died.m.prsm + number.sachet.hosp.never.rec.m.prsm + number.sachet.m.to.sam.prsm),]

  cost.s.varied.relapse[,number.sachet.prmm := (number.sachet.rec.m.prmm + number.sachet.def.m.prmm + number.sachet.never.rec.m.prmm + number.sachet.died.m.prmm + number.sachet.hosp.m.prmm +
                                           number.sachet.transf.m.prmm + number.sachet.hosp.rec.m.prmm + number.sachet.hosp.died.m.prmm + number.sachet.hosp.never.rec.m.prmm + number.sachet.m.to.sam.prmm),]

  # SAM standard CSB
  cost.s.varied.relapse[,number.csb.m := (number.csb.rec.m + number.csb.def.m + number.csb.never.rec.m + number.csb.died.m + number.csb.hosp.m + number.csb.transf.m +
                                     number.csb.hosp.rec.m + number.csb.hosp.died.m + number.csb.hosp.never.rec.m + number.csb.m.to.sam),]

  cost.s.varied.relapse[,number.csb.prsm := (number.csb.rec.m.prsm + number.csb.def.m.prsm + number.csb.never.rec.m.prsm + number.csb.died.m.prsm + number.csb.hosp.m.prsm +
                                        number.csb.transf.m.prsm + number.csb.hosp.rec.m.prsm + number.csb.hosp.died.m.prsm + number.csb.hosp.never.rec.m.prsm + number.csb.m.to.sam.prsm),]

  cost.s.varied.relapse[,number.csb.prmm := (number.csb.rec.m.prmm + number.csb.def.m.prmm + number.csb.never.rec.m.prmm + number.csb.died.m.prmm + number.csb.hosp.m.prmm +
                                        number.csb.transf.m.prmm + number.csb.hosp.rec.m.prmm + number.csb.hosp.died.m.prmm + number.csb.hosp.never.rec.m.prmm + number.csb.m.to.sam.prmm),]

  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  OptimA
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########

  # SAM OptimA
  cost.s.varied.relapse[,number.sachet.s1 := (number.sachet.rec.s1 + number.sachet.def.s1 + number.sachet.never.rec.s1 + number.sachet.died.s1 +
                                         number.sachet.hosp.s1 + number.sachet.transf.s1 + number.sachet.hosp.rec.s1 + number.sachet.hosp.died.s1 +
                                         number.sachet.hosp.never.rec.s1),]

  cost.s.varied.relapse[,number.sachet.prss1 := (number.sachet.rec.s1.prss + number.sachet.def.s1.prss + number.sachet.never.rec.s1.prss + number.sachet.died.s1.prss +
                                            number.sachet.hosp.s1.prss + number.sachet.transf.s1.prss + number.sachet.hosp.rec.s1.prss + number.sachet.hosp.died.s1.prss +
                                            number.sachet.hosp.never.rec.s1.prss),]

  cost.s.varied.relapse[,number.sachet.prms1 := (number.sachet.rec.s1.prms + number.sachet.def.s1.prms + number.sachet.never.rec.s1.prms + number.sachet.died.s1.prms + number.sachet.hosp.s1.prms +
                                            number.sachet.transf.s1.prms + number.sachet.hosp.rec.s1.prms + number.sachet.hosp.died.s1.prms + number.sachet.hosp.never.rec.s1.prms),]

  # MAM OptimA
  cost.s.varied.relapse[,number.sachet.m1 := (number.sachet.rec.m1 + number.sachet.def.m1 + number.sachet.never.rec.m1 + number.sachet.died.m1 + number.sachet.hosp.m1 + number.sachet.transf.m1 +
                                         number.sachet.hosp.rec.m1 + number.sachet.hosp.died.m1 + number.sachet.hosp.never.rec.m1 + number.sachet.m1.to.sam),]

  cost.s.varied.relapse[,number.sachet.prsm1 := (number.sachet.rec.m1.prsm + number.sachet.def.m1.prsm + number.sachet.never.rec.m1.prsm + number.sachet.died.m1.prsm + number.sachet.hosp.m1.prsm +
                                            number.sachet.transf.m1.prsm + number.sachet.hosp.rec.m1.prsm + number.sachet.hosp.died.m1.prsm + number.sachet.hosp.never.rec.m1.prsm + number.sachet.m1.to.sam.prsm),]

  cost.s.varied.relapse[,number.sachet.prmm1 := (number.sachet.rec.m1.prmm + number.sachet.def.m1.prmm + number.sachet.never.rec.m1.prmm + number.sachet.died.m1.prmm + number.sachet.hosp.m1.prmm +
                                            number.sachet.transf.m1.prmm + number.sachet.hosp.rec.m1.prmm + number.sachet.hosp.died.m1.prmm + number.sachet.hosp.never.rec.m1.prmm + number.sachet.m1.to.sam.prmm),]

  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  ComPAS
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########

  # SAM ComPAS
  cost.s.varied.relapse[,number.sachet.s2 := (number.sachet.rec.s2 + number.sachet.def.s2 + number.sachet.never.rec.s2 + number.sachet.died.s2 +
                                         number.sachet.hosp.s2 + number.sachet.transf.s2 + number.sachet.hosp.rec.s2 + number.sachet.hosp.died.s2 +
                                         number.sachet.hosp.never.rec.s2),]

  cost.s.varied.relapse[,number.sachet.prss2 := (number.sachet.rec.s2.prss + number.sachet.def.s2.prss + number.sachet.never.rec.s2.prss + number.sachet.died.s2.prss +
                                            number.sachet.hosp.s2.prss + number.sachet.transf.s2.prss + number.sachet.hosp.rec.s2.prss + number.sachet.hosp.died.s2.prss +
                                            number.sachet.hosp.never.rec.s2.prss),]

  cost.s.varied.relapse[,number.sachet.prms2 := (number.sachet.rec.s2.prms + number.sachet.def.s2.prms + number.sachet.never.rec.s2.prms + number.sachet.died.s2.prms + number.sachet.hosp.s2.prms +
                                            number.sachet.transf.s2.prms + number.sachet.hosp.rec.s2.prms + number.sachet.hosp.died.s2.prms + number.sachet.hosp.never.rec.s2.prms),]

  # MAM ComPAS
  cost.s.varied.relapse[,number.sachet.m2 := (number.sachet.rec.m2 + number.sachet.def.m2 + number.sachet.never.rec.m2 + number.sachet.died.m2 + number.sachet.hosp.m2 + number.sachet.transf.m2 +
                                         number.sachet.hosp.rec.m2 + number.sachet.hosp.died.m2 + number.sachet.hosp.never.rec.m2 + number.sachet.m2.to.sam),]

  cost.s.varied.relapse[,number.sachet.prsm2 := (number.sachet.rec.m2.prsm + number.sachet.def.m2.prsm + number.sachet.never.rec.m2.prsm + number.sachet.died.m2.prsm + number.sachet.hosp.m2.prsm +
                                            number.sachet.transf.m2.prsm + number.sachet.hosp.rec.m2.prsm + number.sachet.hosp.died.m2.prsm + number.sachet.hosp.never.rec.m2.prsm + number.sachet.m2.to.sam.prsm),]

  cost.s.varied.relapse[,number.sachet.prmm2 := (number.sachet.rec.m2.prmm + number.sachet.def.m2.prmm + number.sachet.never.rec.m2.prmm + number.sachet.died.m2.prmm + number.sachet.hosp.m2.prmm +
                                            number.sachet.transf.m2.prmm + number.sachet.hosp.rec.m2.prmm + number.sachet.hosp.died.m2.prmm + number.sachet.hosp.never.rec.m2.prmm + number.sachet.m2.to.sam.prmm),]

  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########
  ######### #########  MANGO
  ######### ######### ################## ######### ######### ######### ######### ################## ######### #########

  # SAM MANGO
  cost.s.varied.relapse[,number.sachet.s3 := (number.sachet.rec.s3 + number.sachet.def.s3 + number.sachet.never.rec.s3 + number.sachet.died.s3 +
                                         number.sachet.hosp.s3 + number.sachet.transf.s3 + number.sachet.hosp.rec.s3 + number.sachet.hosp.died.s3 +
                                         number.sachet.hosp.never.rec.s3),]

  cost.s.varied.relapse[,number.sachet.prss3 := (number.sachet.rec.s3.prss + number.sachet.def.s3.prss + number.sachet.never.rec.s3.prss + number.sachet.died.s3.prss +
                                            number.sachet.hosp.s3.prss + number.sachet.transf.s3.prss + number.sachet.hosp.rec.s3.prss + number.sachet.hosp.died.s3.prss +
                                            number.sachet.hosp.never.rec.s3.prss),]

  cost.s.varied.relapse[,number.sachet.prms3 := (number.sachet.rec.s3.prms + number.sachet.def.s3.prms + number.sachet.never.rec.s3.prms + number.sachet.died.s3.prms + number.sachet.hosp.s3.prms +
                                            number.sachet.transf.s3.prms + number.sachet.hosp.rec.s3.prms + number.sachet.hosp.died.s3.prms + number.sachet.hosp.never.rec.s3.prms),]

  # MAM MANGO
  cost.s.varied.relapse[,number.sachet.m3 := (number.sachet.rec.m3 + number.sachet.def.m3 + number.sachet.never.rec.m3 + number.sachet.died.m3 + number.sachet.hosp.m3 + number.sachet.transf.m3 +
                                         number.sachet.hosp.rec.m3 + number.sachet.hosp.died.m3 + number.sachet.hosp.never.rec.m3 + number.sachet.m3.to.sam),]

  cost.s.varied.relapse[,number.sachet.prsm3 := (number.sachet.rec.m3.prsm + number.sachet.def.m3.prsm + number.sachet.never.rec.m3.prsm + number.sachet.died.m3.prsm + number.sachet.hosp.m3.prsm +
                                            number.sachet.transf.m3.prsm + number.sachet.hosp.rec.m3.prsm + number.sachet.hosp.died.m3.prsm + number.sachet.hosp.never.rec.m3.prsm + number.sachet.m3.to.sam.prsm),]

  cost.s.varied.relapse[,number.sachet.prmm3 := (number.sachet.rec.m3.prmm + number.sachet.def.m3.prmm + number.sachet.never.rec.m3.prmm + number.sachet.died.m3.prmm + number.sachet.hosp.m3.prmm +
                                            number.sachet.transf.m3.prmm + number.sachet.hosp.rec.m3.prmm + number.sachet.hosp.died.m3.prmm + number.sachet.hosp.never.rec.m3.prmm + number.sachet.m3.to.sam.prmm),]

   return(cost.s.varied.relapse) # output the dataframe from the function
} # end of function

# ###### Testing wrapper function ######
# df_costm2 = f_wrapper(
#   weight.adm.s = 7,
#   muac.s = 110,
#   sev.wasted = 5000,
#   mod.wasted = 5000,
#   cov.s = 0.5,
#   cov.m = 0.4,
#   treatDist.s = c(0.2,0.2,0.2,0.2,0.2,0,0.2,0.4,0.5),
#   treatDist.m = c(0.2,0.2,0.2,0.2,0.2,0,0.2),
#   OptimaDist = c(0.681, 0.058, 0.021, 0, 0.132,
#                  0.497, 0.113, 0.056, 0, 0.08),
#   CompasDist = c(0.694, 0.061, 0.013, 0, 0.121,
#                  0.519, 0.095, 0.074, 0.03, 0),
#   MangoDist = c(0.5000000, 0.1350365, 0.1204380, 0.0000000, 0.2007299,
#                 0.5859375, 0.0937500, 0.1406250, 0.0078125, 0.1718750),
#   startingPoint = 0,
#   costDist.min = rep(1,8),
#   costDist.max = rep(5,8),
#   country.name = "Ethiopia",
#   reducedRecovery=0.15
# )
# # ###### Testing fixed budget function ######
# cost.sch <- f_DoseOpt_fixedbudget_calc(df_costm2,
#                                        program.budget = 500000,
#                                        scenario = 1)
                                       
