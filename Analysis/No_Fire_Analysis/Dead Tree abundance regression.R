library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file


NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))

# Need to decide which tree species to use

# Will cut of on average of 0.1 trees per plot per visit

TreeAv<-getPlants(NCRN[[9]], "trees", status="dead",years=2010:2017) %>% filter(Status %in% c("Dead", "Dead Leaning", "Dead Standing")) %>% nrow /(2*145) 
                                #3.31 so we are fine


TreeData<-makeDeadRegData(object=NCRN[[9]], group="trees",status="dead", years=2010:2017)
names(TreeData)<-"All"

# Dead trees only sampled starting 2013 - so never mind. 

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
rstan::rstan_options(auto_write = TRUE)

Tree_Poisson_Regression<-fitPoisson(object=TreeData, iter=25000,  cores=4,control=list(adapt_delta=0.99) )

Tree_Poisson_Summary<-SummarizeFit(model=list(Tree_Poisson_Regression), type="Poisson", names=c("All"="All"), data=list(TreeData))





#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

Tree_NB_Regression<-fitNB(iter=25000, object=TreeData, cores=4,control=list(adapt_delta=0.99) )

Tree_NB_Summary<-SummarizeFit(model=list(Tree_NB_Regression), type="NB", names=c("All"="All"), data=list(TreeData))

Tree_Poisson_Loo<-loo(Tree_Poisson_Regression, reloo=T, cores=4)

Tree_NB_Loo<-loo(Tree_NB_Regression, reloo=T, cores=4)

Tree_Loo_Compare<-map2(.x=list(Tree_Poisson_Loo), .y=list(Tree_NB_Loo), ~brms::loo_compare(list(Poisson=.x, NB=.y)))
## Poisson always wins

Poisson_Wins<-map_lgl(Tree_Loo_Compare, ~rownames(.)[1]=="Poisson")

Tree_Combined_Summary<-rbind(Tree_Poisson_Summary[Poisson_Wins,], Tree_NB_Summary[!Poisson_Wins,])

Tree_Final_Summary<-Tree_Combined_Summary %>% 
  select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing, Model)

write.csv(Tree_Final_Summary, "Dead_Tree_Abundance.csv")

# 
# Tree_Final_Summary<-Tree_Poisson_Summary %>% select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing)
# write.csv(Tree_Final_Summary, "Tree_Abundance.csv")
