library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file


NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")


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

VineData<-map(.x=VineUse, ~makeVineRegData(object=NCRN[[9]], group="vines",species=., years=2006:2017))

TreeOffSetData<-makeRegData(object=NCRN[[9]], group="trees", years=2006:2017) %>% select(Plot_Name,Year, Trees=Count)
TreeOffSetData<-TreeOffSetData %>% mutate(LogTrees=log(Trees))

VineData<-VineData %>% map(~left_join(x=.,y=TreeOffSetData))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
rstan::rstan_options(auto_write = TRUE)

Vine_Poisson_Regression<-map(.x=VineData, ~fitPoissonVine(iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Vine_Poisson_Summary<-SummarizeFit(model=Vine_Poisson_Regression, type="Poisson", names=VineUse, data=VineData)





#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

Vine_NB_Regression<-map(.x=VineData, ~fitNBVine(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Vine_NB_Summary<-SummarizeFit(model=Vine_NB_Regression, type="NB", names=VineUse, data=VineData)

Vine_Poisson_Loo<-map(Vine_Poisson_Regression, loo, reloo=T, cores=4)

Vine_NB_Loo<-map(Vine_NB_Regression, loo, reloo=T, cores=4)

Vine_Loo_Compare<-map2(.x=Vine_Poisson_Loo, .y=Vine_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))
## Poisson always wins

Poisson_Wins<-map_lgl(Vine_Loo_Compare, ~rownames(.)[1]=="Poisson")

Vine_Combined_Summary<-rbind(Vine_Poisson_Summary[Poisson_Wins,], Vine_NB_Summary[!Poisson_Wins,])

Vine_Final_Summary<-Vine_Combined_Summary %>% 
  select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing, Model)


# remove vines we can't deal with due to changes in ID early on
Vine_Final_Summary<-Vine_Final_Summary %>% filter(Species %in% c("All", "Lonicera japonica", "Smilax", "Vitis"))

write.csv(Vine_Final_Summary, "Vine_Abundance.csv")

# 
# Tree_Final_Summary<-Tree_Poisson_Summary %>% select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing)
# write.csv(Tree_Final_Summary, "Tree_Abundance.csv")
