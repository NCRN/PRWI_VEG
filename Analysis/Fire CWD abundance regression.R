library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file


NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

TreeFirePlots<-FirePlots %>% filter(Fire_Name == "B-Loop") 


CWDSp<-getPlants(NCRN[[9]], "cwd", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort


# Need to decide which tree species to use

# Will cut of on average of 0.1 trees per plot per visit

CWDAv<-getPlants(NCRN[[9]], "cwd", years=2006:2017) %>% nrow /(3*145) #7.29


CWDData<-makeRegData(object=NCRN[[9]], group="cwd", years=2006:2017)

CWDFireData<- CWDData %>% left_join(TreeFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) )

options(mc.cores=4) #loo may need this to be specified here as the cores argument does not seem to work in loo
rstan::rstan_options(auto_write = TRUE)

CWD_Fire_Poisson_Regression<- fitFirePoisson(iter=25000, object=CWDFireData, cores=4,
                                             control=list(adapt_delta=0.99, max_treedepth=20), save_pars=save_pars(all=TRUE) )

CWD_Fire_Poisson_Summary<-SummarizeFireFit(model=list(CWD_Fire_Poisson_Regression), type="Poisson", names=c("All"="All"), data=list(CWDData))


CWD_Fire_NB_Regression<-fitFireNB(iter=25000, object=CWDFireData, cores=4,
                                  control=list(adapt_delta=0.99, max_treedepth=20), save_pars=save_pars(all=TRUE) )

CWD_Fire_NB_Summary<-SummarizeFireFit(model=list(CWD_Fire_NB_Regression), type="NB", names=c("All"="All"), data=list(CWDFireData))

CWD_Fire_Poisson_Loo<-loo(CWD_Fire_Poisson_Regression, reloo=T, cores=4, moment_match = TRUE)

CWD_Fire_NB_Loo<-loo(CWD_Fire_NB_Regression, reloo=T, cores=4, moment_match=TRUE)

CWD_Fire_Loo_Compare<-map2(.x=list(CWD_Fire_Poisson_Loo), .y=list(CWD_Fire_NB_Loo), ~brms::loo_compare(list(Poisson=.x, NB=.y)))
## Poisson  wins

Poisson_Wins<-map_lgl(CWD_Fire_Loo_Compare, ~rownames(.)[1]=="Poisson")

CWD_Fire_Combined_Summary<-rbind(CWD_Fire_Poisson_Summary[Poisson_Wins,], CWD_Fire_NB_Summary[!Poisson_Wins,])

CWD_Fire_Final_Summary<-CWD_Fire_Combined_Summary %>% 
  select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing,
         FireSlope, Fire_Slope_low_95, Fire_Slope_up_95, Prob_Fire_Decreasing, Prob_Fire_Increasing,  Year_Fire_Change, 
         Year_Fire_Change_low95, Year_Fire_Change_upper95, Model)

write.csv(CWD_Fire_Final_Summary, "CWD_Fire_Abundance.csv")

# 
# Tree_Final_Summary<-Tree_Poisson_Summary %>% select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing)
# write.csv(Tree_Final_Summary, "Tree_Abundance.csv")
