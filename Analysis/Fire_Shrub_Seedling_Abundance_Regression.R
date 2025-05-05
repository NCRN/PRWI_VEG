library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)
library(distributional)

#Need to source function file
#source("Function file.R")

NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

ShrubFirePlots<-FirePlots %>% filter(Fire_Name == "B-Loop") 

#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


ShSeedSp<-getPlants(NCRN[[9]], "shseedlings", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(ShSeedSp)<-ShSeedSp


ShSeedGroups<-list(All=ShSeedSp[c(-1,-16,-20,-21,-23)],
  Vaccinium=c("Vaccinium corymbosum","Vaccinium fuscatum", "Vaccinium spp."),#,"Vaccinium stamineum"),
  Virburnum=c("Viburnum dentatum","Viburnum prunifolium"),#"Viburnum acerifolium","Viburnum dilatatum"
  Aronia=c("Aronia arbutifolia","Photinia pyrifolia")
                   )


# Need to decide which tree species to use

# Will cut of on average of 0.1 trees per plot per visit

ShSeedAv<-c(ShSeedSp,ShSeedGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "shseedlings", years=2006:2017,species=.x) %>% nrow )/(3*145))


ShSeedUse<-c(ShSeedSp, ShSeedGroups)[ShSeedAv>0.1]
ShSeedUse<-ShSeedUse[-c(4,5,6)]

ShSeedData<-map(.x=ShSeedUse, ~makeRegData(object=NCRN[[9]], group="shseedlings",species=., years=2006:2017))
ShSeedFireData<- map(.x=ShSeedData, ~left_join(x=.,y=ShrubFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain

rstan::rstan_options(auto_write = TRUE)
options(mc.cores=4)

ShSeed_Fire_Poisson_Regression<-map(.x=ShSeedFireData, ~fitFirePoisson(, iter=25000, object=.x, cores=4,
                                          control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

ShSeed_Fire_Poisson_Summary<-SummarizeFireFit(model=ShSeed_Fire_Poisson_Regression, type="Poisson", names=ShSeedUse, data=ShSeedFireData)

ShSeed_Fire_Poisson_Fitted<-SummarizeFireOutcome(model=ShSeed_Fire_Poisson_Regression, names=ShSeedUse, data=ShSeedFireData, form="seedling")


#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

ShSeed_Fire_NB_Regression<-map(.x=ShSeedFireData, ~fitFireNB(, iter=25000, object=.x, cores=4,
                                                    control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

ShSeed_Fire_NB_Summary<-SummarizeFireFit(model=ShSeed_Fire_NB_Regression, type="NB", names=ShSeedUse, data=ShSeedFireData)

ShSeed_Fire_NB_Fitted<-SummarizeFireOutcome(model=ShSeed_Fire_NB_Regression,  names = ShSeedUse, data=ShSeedFireData, form="seedling")


ShSeed_Fire_Poisson_Loo<-map(ShSeed_Fire_Poisson_Regression, loo, reloo=T, cores=4, moment_match=TRUE) 

ShSeed_Fire_NB_Loo<-map(ShSeed_Fire_NB_Regression, loo, reloo=T, cores=4, moment_match=TRUE)

ShSeed_Fire_Loo_Compare<-map2(.x=ShSeed_Fire_Poisson_Loo, .y=ShSeed_Fire_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))

#Get ones where Poisson is best
Poisson_Wins<-map_lgl(ShSeed_Fire_Loo_Compare, ~rownames(.)[1]=="Poisson")

ShSeed_Fire_Combined_Summary<-rbind(ShSeed_Fire_Poisson_Summary[Poisson_Wins,], ShSeed_Fire_NB_Summary[!Poisson_Wins,])
ShSeed_Fire_Combined_Fitted<-rbind(ShSeed_Fire_Poisson_Fitted[Poisson_Wins,], ShSeed_Fire_NB_Fitted[!Poisson_Wins,])

ShSeed_Fire_Final_Summary<-ShSeed_Fire_Combined_Summary #%>% 
  # select(Species, TotalCount, TotalPlots, Intercept, FireIntercept, Fire_Int_Per_Change,  Prob_Fire_Int_Increasing, Prob_Fire_Int_Decreasing, 
  #        YearSlope, Year_Per_Change, Year_Change_low95, Year_Change_upper95, Prob_Decreasing, Prob_Increasing, 
  #        FireSlope, Year_Fire_Change, Year_Fire_Change_low95, Year_Fire_Change_upper95, Prob_Fire_Decreasing, Prob_Fire_Increasing, 
  #        Fire_Decreased_Year1, Fire_Increased_Year1, Model)

write.csv(ShSeed_Fire_Final_Summary, "Shrub_Fire_Seed_Abundance.csv")
write.csv(ShSeed_Fire_Combined_Fitted, "Shrub_Seed_Fire_Abundance_Fitted.csv")

## Euonymus americana has 0 seeldings in fire plots - so it seems to be giving wonky fire data. 


Model_Graph_Data<-bind_rows(
  list(
     `Strawberry bush`= ShSeedFireData[[1]] %>%  
       group_by(factor(Fire)) %>% 
       data_grid(ScYear, Fire) %>% 
       add_fitted_draws(ShSeed_Fire_NB_Regression[[1]], re_formula = NA) %>% 
       mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShSeedFireData[[1]]$ScYear,"scaled:center")),
    
    `Mountain laurel`=ShSeedFireData[[2]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(ShSeed_Fire_NB_Regression[[2]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShSeedFireData[[2]]$ScYear,"scaled:center")),
    
    `Maleberry`=ShSeedFireData[[3]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(ShSeed_Fire_NB_Regression[[3]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShSeedFireData[[3]]$ScYear,"scaled:center")),
    
    All=ShSeedFireData[[4]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(ShSeed_Fire_NB_Regression[[4]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShSeedFireData[[4]]$ScYear,"scaled:center")),
    
    `Blueberries`=ShSeedFireData[[5]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(ShSeed_Fire_NB_Regression[[5]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(ShSeedFireData[[5]]$ScYear,"scaled:center"))
  ), .id="Group"
)  


Model_Graph_Data<-Model_Graph_Data %>% mutate(Density=.value*10000/12)
Model_Graph_Data_Sum<-Model_Graph_Data %>% group_by(Year, Group,Burned) %>% summarise(Density=median(Density))


ggplot(Model_Graph_Data, aes(x=Year, y=Density, color=Burned, fill=Burned,linetype=Burned)) + 
  stat_lineribbon(.width=c(.85,.95), alpha=0.25) +
  geom_line(data=Model_Graph_Data_Sum)+
  labs(x='Year', y="Shrub Seedlings/ha") +
  #scale_y_continuous(breaks = c(30, 60)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_wrap(~Group, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())
