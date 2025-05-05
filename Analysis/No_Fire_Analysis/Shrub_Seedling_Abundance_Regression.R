library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file
source("Function file.R")

NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


ShSeedSp<-getPlants(NCRN[[9]], "shseedlings", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(ShSeedSp)<-ShSeedSp


ShSeedGroups<-list(All=NA,
                   Vaccinium=c("Vaccinium corymbosum","Vaccinium fuscatum", "Vaccinium spp.", "Vaccinium stamineum"),
                   Virburnum=c("Viburnum acerifolium","Viburnum dentatum","Viburnum dilatatum","Viburnum prunifolium"  )
                   )


# Need to decide which tree species to use

# Will cut of on average of 0.1 trees per plot per visit

ShSeedAv<-c(ShSeedSp,ShSeedGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "shseedlings", years=2006:2017,species=.x) %>% nrow )/(3*145))


ShSeedUse<-c(ShSeedSp, ShSeedGroups)[ShSeedAv>0.1]
ShSeedUse<-ShSeedUse[-c(4,5,6)]

ShSeedData<-map(.x=ShSeedUse, ~makeRegData(object=NCRN[[9]], group="shseedlings",species=., years=2006:2017))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain

rstan::rstan_options(auto_write = TRUE)

ShSeed_Poisson_Regression<-map(.x=ShSeedData, ~fitPoisson(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

ShSeed_Poisson_Summary<-SummarizeFit(model=ShSeed_Poisson_Regression, type="Poisson", names=ShSeedUse, data=ShSeedData)

#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

ShSeed_NB_Regression<-map(.x=ShSeedData, ~fitNB(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

ShSeed_NB_Summary<-SummarizeFit(model=ShSeed_NB_Regression, type="NB", names=ShSeedUse, data=ShSeedData)

ShSeed_Poisson_Loo<-map(ShSeed_Poisson_Regression, loo, reloo=T, cores=4)

ShSeed_NB_Loo<-map(ShSeed_NB_Regression, loo, reloo=T, cores=4)

ShSeed_Loo_Compare<-map2(.x=ShSeed_Poisson_Loo, .y=ShSeed_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))

#Get ones where Poisson is best
Poisson_Wins<-map_lgl(ShSeed_Loo_Compare, ~rownames(.)[1]=="Poisson")

ShSeed_Combined_Summary<-rbind(ShSeed_Poisson_Summary[Poisson_Wins,], ShSeed_NB_Summary[!Poisson_Wins,])

ShSeed_Final_Summary<-ShSeed_Combined_Summary %>% 
  select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing, Model)

write.csv(ShSeed_Final_Summary, "Shrub_Seed_Abundance.csv")
