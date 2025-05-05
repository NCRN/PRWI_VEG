library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)

#Need to source function file



NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

VineFirePlots<-FirePlots %>% filter(Fire_Name=="B-Loop")


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


VineSp<-getPlants(NCRN[[9]], "vines", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(VineSp)<-VineSp

VineGroups<-list(All=NA,
                 Smilax=c("Smilax glauca", "Smilax rotundifolia", "Smilax spp."),
                 
                 Vitis=c("Vitis aestivalis", "Vitis cinerea", "Vitis labrusca", "Vitis spp.", "Vitis vulpina")
)

# Need to decide which vine species to use

# Will cut of on average of 0.1 vines per plot per visit

VineAv<-c(VineSp, VineGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "vines", years=2006:2017,species=.x) %>% nrow )/(3*145))

VineUse<-c(VineSp, VineGroups)[VineAv>0.1]

VineUse<-VineUse[c(1,5,6,7)]

VineData<-map(.x=VineUse, ~makeVineRegData(object=NCRN[[9]], group="vines",species=., years=2006:2017))

TreeOffSetData<-makeRegData(object=NCRN[[9]], group="trees", years=2006:2017) %>% select(Plot_Name,Year, Trees=Count)
TreeOffSetData<-TreeOffSetData %>% mutate(LogTrees=log(Trees))

VineData<-VineData %>% map(~left_join(x=.,y=TreeOffSetData))

VineFireData<- map(.x=VineData, ~left_join(x=.,y=VineFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))

options(mc.cores=4) #loo may need this to be specified here as the cores argument does not seem to work in loo
rstan::rstan_options(auto_write = TRUE)

Vine_Fire_Poisson_Regression<-map(.x=VineFireData, ~fitFirePoissonVine(, iter=25000, object=.x, cores=4,
                                       control=list(adapt_delta=0.99, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

Vine_Fire_Poisson_Summary<-SummarizeFireFit(model=Vine_Fire_Poisson_Regression, type="Poisson", names=VineUse, data=VineFireData)

Vine_Fire_Poisson_Fitted<-SummarizeFireOutcome(model=Vine_Fire_Poisson_Regression, names=VineUse, data=VineFireData, form = "vine")


Vine_Fire_NB_Regression<-map(.x=VineFireData, ~fitFireNBVine(, iter=25000, object=.x, cores=4,
                                                control=list(adapt_delta=0.99, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

Vine_Fire_NB_Summary<-SummarizeFireFit(model=Vine_Fire_NB_Regression, type="NB", names=VineUse, data=VineFireData)
Vine_Fire_NB_Fitted<-SummarizeFireOutcome(model=Vine_Fire_NB_Regression, names=VineUse, data=VineFireData, form="vine")


Vine_Fire_Poisson_Loo<-map(Vine_Fire_Poisson_Regression, loo, reloo=T, cores=4, moment_match=T) # apparent error in moment match, 
# "One node produced an error: All input values must be finite"

#Vine_Fire_NB_Loo_Temp<-map(Vine_Fire_NB_Regression[1], loo, reloo=T, cores=4, moment_match=F)
#Vine_Fire_NB_Loo<-Vine_Fire_NB_Loo_Temp

Vine_Fire_NB_Loo<-map(Vine_Fire_NB_Regression, loo, reloo=T, cores=4, moment_match=T)


Vine_Fire_Loo_Compare<-map2(.x=Vine_Fire_Poisson_Loo, .y=Vine_Fire_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))

Poisson_Wins<-map_lgl(Vine_Fire_Loo_Compare, ~rownames(.)[1]=="Poisson")

Vine_Fire_Combined_Summary<-rbind(Vine_Fire_Poisson_Summary[Poisson_Wins,], Vine_Fire_NB_Summary[!Poisson_Wins,])
Vine_Fire_Combined_Fitted<-rbind(Vine_Fire_Poisson_Fitted[Poisson_Wins,], Vine_Fire_NB_Fitted[!Poisson_Wins,])



Vine_Fire_Final_Summary<-Vine_Fire_Combined_Summary #%>% 
#   select(Species, TotalCount, TotalPlots, Intercept, FireIntercept, Fire_Int_Per_Change,  Prob_Fire_Int_Increasing, Prob_Fire_Int_Decreasing, 
#          YearSlope, Year_Per_Change, Year_Change_low95, Year_Change_upper95, Prob_Decreasing, Prob_Increasing, 
#          FireSlope, Year_Fire_Change, Year_Fire_Change_low95, Year_Fire_Change_upper95, Prob_Fire_Decreasing, Prob_Fire_Increasing, 
#          Fire_Decreased_Year1, Fire_Increased_Year1, Model)


write.csv(Vine_Fire_Final_Summary, "Vine_Fire_Abundance.csv")
write.csv(Vine_Fire_Combined_Fitted, "Vine_Fire_Abundance_Fitted.csv")

Model_Graph_Data<-bind_rows(
  list(
    `Japanese Honeysuckle`=VineFireData[[1]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      mutate(LogTrees=3.42) %>%  # average number
      add_fitted_draws(Vine_Fire_Poisson_Regression[[1]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(VineFireData[[1]]$ScYear,"scaled:center")),
    
    `All Vines`=VineFireData[[2]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>%
      mutate(LogTrees=3.42) %>%  # average number
      add_fitted_draws(Vine_Fire_Poisson_Regression[[2]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(VineFireData[[2]]$ScYear,"scaled:center")),
    
    `Greenbriers`=VineFireData[[3]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      mutate(LogTrees=3.42) %>%  # average number
      add_fitted_draws(Vine_Fire_Poisson_Regression[[3]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(VineFireData[[3]]$ScYear,"scaled:center")),
    
    `Grapes`=VineFireData[[4]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      mutate(LogTrees=3.42) %>%  # average number
      add_fitted_draws(Vine_Fire_Poisson_Regression[[4]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(VineFireData[[4]]$ScYear,"scaled:center"))
    
  ), .id="Group"
)  

Model_Graph_Data<-Model_Graph_Data %>% mutate(Density=(.value*10000)/(225*pi))
Model_Graph_Data_Sum<-Model_Graph_Data %>% group_by(Year, Group,Burned) %>% summarise(Density=median(Density))


ggplot(Model_Graph_Data, aes(x=Year, y=Density, color=Burned, fill=Burned, linetype=Burned)) + 
  stat_lineribbon(.width=c(.85,.95), alpha=0.25) +
  geom_line(data=Model_Graph_Data_Sum)+
  labs(x='Year', y="Vines/ha") +
  #scale_y_continuous(l breaks = c(67.8584, 135.7168)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_wrap(~Group, scales="free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())






