library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)


#Need to source function file


NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

ShrubFirePlots<-FirePlots %>% filter(Fire_Name == "B-Loop") 


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


ShrubSp<-getPlants(NCRN[[9]], "shrubs", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
Vaccinium=c("Vaccinium corymbosum","Vaccinium fuscatum")

ShrubGroups<-list(Vaccinium=c("Vaccinium corymbosum","Vaccinium fuscatum"))


ShrubSp<-ShrubSp[c(-4,-5,-9)] # No Kalmia, Lindera, or Vaccinium stamieum

names(ShrubSp)<-ShrubSp

# Need to decide which tree species to use

# Will cut of on average of 0.1 Shrubs per plot per visit

ShrubAv<-c(ShrubSp,ShrubGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "shrubs", years=2006:2017,species=.x) %>% nrow )/(3*145))


ShrubUse<-c(ShrubSp, ShrubGroups)[ShrubAv>0.1] # need to combine the two Vacciniums. 
ShrubUse<-ShrubUse[c(1,4)] # only used combined Vaccinium

ShrubData<-map(.x=ShrubUse, ~makeRegData(object=NCRN[[9]], group="shrubs",species=., years=2006:2017))

ShrubFireData<- map(.x=ShrubData, ~left_join(x=.,y=ShrubFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
rstan::rstan_options(auto_write = TRUE)
options(mc.cores=4) 

Shrub_Fire_Poisson_Regression<-map(.x=ShrubFireData, ~fitFirePoisson(, iter=25000, object=.x, cores=4,
                                          control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

Shrub_Fire_Poisson_Summary<-SummarizeFireFit(model=Shrub_Fire_Poisson_Regression, type="Poisson", names=ShrubUse, data=ShrubFireData)
Shrub_Fire_Poisson_Fitted<-SummarizeFireOutcome(model=Shrub_Fire_Poisson_Regression, names=ShrubUse, data=ShrubFireData, form="shrub")

# pp_check(Shrub_Fire_NB_Regression[[2]]) + xlim(0,50)
# pp_check(Shrub_Fire_NB_Regression[[2]], type="bars")

##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

Shrub_Fire_NB_Regression<-map(.x=ShrubFireData, ~fitFireNB(, iter=25000, object=.x, cores=4,
                                  control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

Shrub_Fire_NB_Summary<-SummarizeFireFit(model=Shrub_Fire_NB_Regression, type="NB", names=ShrubUse, data=ShrubFireData)
Shrub_Fire_NB_Fitted<-SummarizeFireOutcome(model=Shrub_Fire_NB_Regression, names=ShrubUse, data=ShrubFireData, form="shrub")

Shrub_Fire_Poisson_Loo<-map(Shrub_Fire_Poisson_Regression, loo, reloo=T, cores=4, moment_match=FALSE) #monemtn matching crashed.

Shrub_Fire_NB_Loo<-map(Shrub_Fire_NB_Regression, loo, reloo=T, cores=4, moment_match=TRUE) 

Shrub_Fire_Loo_Compare<-map2(.x=Shrub_Fire_Poisson_Loo, .y=Shrub_Fire_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))
## Poisson always wins

#Get ones where Poisson is best
Poisson_Wins<-map_lgl(Shrub_Fire_Loo_Compare, ~rownames(.)[1]=="Poisson")

Shrub_Fire_Combined_Summary<-rbind(Shrub_Fire_Poisson_Summary[Poisson_Wins,], Shrub_Fire_NB_Summary[!Poisson_Wins,])
Shrub_Fire_Combined_Fitted<-rbind(Shrub_Fire_Poisson_Fitted[Poisson_Wins,], Shrub_Fire_NB_Fitted[!Poisson_Wins,])


Shrub_Fire_Final_Summary<-Shrub_Fire_Combined_Summary 
# %>% 
#   select(Species, TotalCount, TotalPlots, Intercept, FireIntercept, Fire_Int_Per_Change,  Prob_Fire_Int_Increasing, Prob_Fire_Int_Decreasing, 
#          YearSlope, Year_Per_Change, Year_Change_low95, Year_Change_upper95, Prob_Decreasing, Prob_Increasing, 
#          FireSlope, Year_Fire_Change, Year_Fire_Change_low95, Year_Fire_Change_upper95, Prob_Fire_Decreasing, Prob_Fire_Increasing, 
#          Fire_Decreased_Year1, Fire_Increased_Year1, Model)

write.csv(Shrub_Fire_Final_Summary, "Fire_Shrub_Abundance.csv")
write.csv(Shrub_Fire_Combined_Fitted, "Shrub_Fire_Abundance_Fitted.csv")

Model_Graph_Data<-bind_rows(
  list(
    `Maleberry`=ShrubFireData[[1]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Shrub_Fire_NB_Regression[[1]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShrubFireData[[1]]$ScYear,"scaled:center")),
    
    `Highbush blueberries`=ShrubFireData[[2]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Shrub_Fire_Poisson_Regression[[2]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShrubFireData[[2]]$ScYear,"scaled:center"))
  ), .id="Group"
)  

Model_Graph_Data<-Model_Graph_Data %>% mutate(Density=(.value*10000)/(27*pi))

Model_Graph_Data_Sum<-Model_Graph_Data%>% group_by(Year, Group,Burned) %>% 
  summarise(Density=median(Density))



ggplot(Model_Graph_Data, aes(x=Year, y=Density, color=Burned, fill=Burned, linetype=Burned)) + 
  stat_lineribbon( .width=c(.85,.95), alpha=0.25) +
  geom_line(data=Model_Graph_Data_Sum)+
  labs(x='Year', y="Shrubs/ha") +
  #scale_y_continuous(l breaks = c(67.8584, 135.7168)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_wrap(~Group, scales="free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())







