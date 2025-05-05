library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)

#Need to source function file


NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

CoverFirePlots<-FirePlots %>% filter(Fire_Name == "B-Loop") 


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


CoverSp<-getPlants(NCRN[[9]], "herbs", years=2014:2017) %>% pull(Latin_Name) %>% unique %>% sort
CoverSp<-CoverSp[c(-4,-7,-8,-9)]
names(CoverSp)<-CoverSp

#2015 adds - Kalmiaa, Lindera, 

# Need to decide which tree species to use

# Will cut of on average of 0.1 Shrubs per plot per visit

CoverAv<-CoverSp %>% map_dbl(~(getPlants(NCRN[[9]], "herbs", years=2014:2017,species=.x) %>% nrow )/(145))
CoverAv[2:3]<-CoverSp[2:3] %>% map_dbl(~(getPlants(NCRN[[9]], "herbs", years=2014:2017,species=.x) %>% nrow )/(110)) # For Kalima and Lindera

CoverUse<-CoverSp[CoverAv>0.1]

CoverData<-map(.x=CoverUse, ~makeRegData(object=NCRN[[9]], group="herbs",species=., years=2014:2017))
CoverData[[2]]<-makeRegData(object=NCRN[[9]], group="herbs", species=CoverUse[2], years=2015:2017)

CoverFireData<- map(.x=CoverData, ~left_join(x=.,y=CoverFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0,1) ))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
rstan::rstan_options(auto_write = TRUE)
options(mc.cores=4) 

fitFireCover<-function(object,...){
  
  RegFormula<-brmsformula(Cover ~ Fire  ,
                 zi ~ Fire)
  
  brm(RegFormula, data=object, family=zero_inflated_beta(link="logit"), ... )
}


Cover_Fire_ZBeta_Regression[1]<-map(.x=CoverFireData[1], ~fitFireCover(, iter=25000, object=.x, cores=4,
                                          control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) ))


SummarizeCoverFireOutcome<-function(model, names, data, form){
  OutTable<-data.frame(Species=names(names))
  
  #FitInData<-map(data, ~data.frame(Fire=c(0,1), ScYear=min(.x$ScYear),  LogTrees=3.42 ))
  
  FitInData<-data
  Ha_Convert<-4899
  
  # LogTrees value = 3.42,  for fire is 2.89, no fire is 3.44, used for vines only

  Fitted_Data<-map2(model,FitInData, ~fitted(.x, newdata=.y, re_formula=NULL, scale="response", robust=T))
  Fitted_Data<-map2(FitInData, Fitted_Data, .f=cbind)
  
  
  
  OutTable <- OutTable %>% mutate(
    Start_NoFire_Mean=map_dbl(Fitted_Data,~.x %>% filter(Fire==0) %>% pull(Estimate) %>% mean %>%  round(3)),
    Start_NoFire_Low=map_dbl(Fitted_Data,~.x %>% filter(Fire==0) %>% pull(Q2.5) %>% mean %>%  round(3)),
    Start_NoFire_High=map_dbl(Fitted_Data, ~.x %>% filter(Fire==0) %>% pull(Q97.5) %>% mean %>%  round(3)),
    Start_Fire_Mean=map_dbl(Fitted_Data,~.x %>% filter(Fire==1) %>% pull(Estimate) %>% mean %>%  round(3)),
    Start_Fire_Low=map_dbl(Fitted_Data,~.x %>% filter(Fire==1) %>% pull(Q2.5) %>% mean %>%  round(3)),
    Start_Fire_High=map_dbl(Fitted_Data, ~.x %>% filter(Fire==1) %>% pull(Q97.5) %>% mean %>%  round(3)),
    
    Start_NoFire_Mean_HA=(Start_NoFire_Mean * Ha_Convert) %>% round(2),
    Start_NoFire_Low_Ha=(Start_NoFire_Low * Ha_Convert) %>% round(2),
    Start_NoFire_High_Ha=(Start_NoFire_High * Ha_Convert) %>% round(2),
    Start_Fire_Mean_HA=(Start_Fire_Mean * Ha_Convert) %>% round(2),
    Start_Fire_Low_Ha=(Start_Fire_Low * Ha_Convert) %>% round(2),
    Start_Fire_High_Ha = (Start_Fire_High * Ha_Convert) %>% round(2)
  )
  
}


Shrub_Fire_Cover_Fitted<-SummarizeCoverFireOutcome(model=Cover_Fire_ZBeta_Regression, names=CoverUse, data=CoverFireData, form="shrub")



SummarizeCoverFireFit<-function(model, type, names, data) {
  
  OutTable<-data.frame(Species=names(names))


  OutTable <- OutTable %>% mutate(
    TotalPlots=map_dbl(data, ~filter(.x, Cover>0) %>% n_distinct(.$Plot_Name)),
    shape = map_dbl(model, ~summary(.x)$spec_pars[[1]] %>% round(3)),
    
    Intercept=map_dbl(model, ~fixef(.x, robust=T)[1,1] %>% round(3)),
    IntError=map_dbl(model, ~fixef(.x,robust=T)[1,2] %>% round(3)),
    Int_low_95=map_dbl(model, ~fixef(.x, robust=T)[1,3] %>% round(3)),
    Int_up_95=map_dbl(model, ~fixef(.x, robust=T)[1,4] %>% round(3)),
 
    
    FireIntercept= map_dbl(model, ~fixef(.x, robust=T)[3,1] %>% round(3)),
    FireIntError= map_dbl(model, ~fixef(.x,robust=T)[3,2] %>% round(3)),
    Fire_Int_low_95=map_dbl(model, ~fixef(.x, robust=T)[3,3] %>% round(3)),
    Fire_Int_up_95=map_dbl(model, ~fixef(.x, robust=T)[3,4] %>% round(3)),
    Prob_Fire_Int_Decreasing=map_dbl(model, ~hypothesis(.x, 'Fire<0')[[1]]$Post.Prob %>% round(3)),
    Prob_Fire_Int_Increasing=map_dbl(model, ~hypothesis(.x, 'Fire>0')[[1]]$Post.Prob %>% round(3)), 
    
    Zi_Int=map_dbl(model, ~fixef(.x, robust=T)[2,1] %>% round(3)),
    Zi_Error=map_dbl(model, ~fixef(.x,robust=T)[2,2] %>% round(3)),
    Zi_low_95=map_dbl(model, ~fixef(.x, robust=T)[3,3] %>% round(3)),
    Zi_up_95=map_dbl(model, ~fixef(.x, robust=T)[3,4] %>% round(3)),
    Occu=1-boot::inv.logit(Zi_Int) %>% round(3),

    Zi_Fire=map_dbl(model, ~fixef(.x, robust=T)[4,1] %>% round(3)),
    Zi_Fire_Error=map_dbl(model, ~fixef(.x,robust=T)[4,2] %>% round(3)),
    Zi_Fire_low_95=map_dbl(model, ~fixef(.x, robust=T)[4,3] %>% round(3)),
    Zi_Fire_up_95=map_dbl(model, ~fixef(.x, robust=T)[4,4] %>% round(3)),
    Fire_Occu=1-boot::inv.logit(Zi_Int+Zi_Fire) %>% round(3),
    
    Prob_Fire_Zi_Decreasing=map_dbl(model, ~hypothesis(.x, 'zi_Fire<0')[[1]]$Post.Prob %>% round(3)),
    Prob_Fire_Zi_Increasing=map_dbl(model, ~hypothesis(.x, 'zi_Fire>0')[[1]]$Post.Prob %>% round(3)), 
    
    Prob_Fire_Overall_Deceasing=map_dbl(model, ~hypothesis(.x, 
               'inv.logit(Intercept)*(1-inv.logit(zi_Intercept)) >inv.logit(Intercept+Fire)*(1-inv.logit(zi_Intercept+zi_Fire))')[[1]]$Post.Prob %>% round(3)),
    Model=type)

}


Shrub_Fire_Cover_Summary<-SummarizeCoverFireFit(model=Cover_Fire_ZBeta_Regression, type="Zero_Beta", names=CoverUse, data=CoverFireData)
Shrub_Fire_Cover_Final_Summary<-Shrub_Fire_Cover_Summary 


write.csv(Shrub_Fire_Cover_Final_Summary, "Fire_Shrub_Cover.csv")
write.csv(Shrub_Fire_Cover_Fitted, "Shrub_Fire_Cover_Fitted.csv")

Model_Graph_Data<-bind_rows(
  list(
    `Lyonia ligustrina`=ShrubFireData[[1]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Shrub_Fire_NB_Regression[[1]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShrubFireData[[1]]$ScYear,"scaled:center")),
    
    `Vaccinium corymbosum`=ShrubFireData[[2]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Shrub_Fire_Poisson_Regression[[2]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShrubFireData[[2]]$ScYear,"scaled:center")),
    
    `Vaccinium fuscatum`=ShrubFireData[[3]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Shrub_Fire_Poisson_Regression[[3]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShrubFireData[[3]]$ScYear,"scaled:center"))
  ), .id="Group"
)  

Model_Graph_Data<-Model_Graph_Data %>% mutate(Density=(.value*10000)/(27*pi))




ggplot(Model_Graph_Data, aes(x=Year, y=Count, color=Burned, fill=Burned)) + 
  stat_lineribbon(aes(y=Density), .width=c(.85,.95), alpha=0.25) +
  labs(x='Year', y="Saplings/ha") +
  #scale_y_continuous(l breaks = c(67.8584, 135.7168)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_wrap(~Group, scales="free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())







