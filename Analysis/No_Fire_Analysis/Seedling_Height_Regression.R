library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file

source("Function file.R")
NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


SeedSp<-getPlants(NCRN[[9]], "seedlings", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(SeedSp)<-SeedSp


SeedGroups<-list(All=NA,
                 Early_Successional=c("Juniperus virginiana", "Liquidambar styraciflua", "Liriodendron tulipifera", 'Pinus virginiana',
                                      'Populus grandidentata',"Robinia pseudoacacia" ),
                 
                 Oak_Hickory=c("Carya alba", "Carya glabra", "Carya ovalis","Carya ovata", "Carya spp.", "Castanea dentata" ,"Quercus alba", "Quercus coccinea",
                              "Quercus falcata", "Quercus prinus", "Quercus rubra","Quercus stellata", "Quercus velutina","Quercus X benderi",  "Quercus X bushii",
                              "Quercus X fernowii", "Quercus X willdenowiana" ),
                 
                 Mesic=c("Acer rubrum", "Acer saccharum", "Betula lenta", "Diospyros virginiana", "Fagus grandifolia", "Fraxinus americana","Ilex opaca", 
                         "Juglans nigra", "Nyssa sylvatica", "Prunus serotina", "Tilia americana", "Ulmus americana", "Ulmus rubra"),
                 
                 Non_Canopy=c("Acer negundo", "Amelanchier arborea", "Amelanchier canadensis", "Aralia spinosa", "Asimina triloba", "Carpinus caroliniana",
                              "Cercis canadensis", "Chionanthus virginicus", "Cornus florida", "Crataegus crus-galli", "Gymnocladus dioicus",
                              "Morus rubra", "Ostrya virginiana", "Prunus americana", "Sassafras albidum"),
                 
                 Wet=c("Acer saccharinum", "Betula alleghaniensis", "Carya cordiformis",  "Celtis occidentalis", "Fraxinus nigra", "Fraxinus pennsylvanica", 
                       "Fraxinus profunda", "Gleditsia triacanthos", "Platanus occidentalis", "Populus deltoides", "Quercus bicolor", "Quercus michauxii",
                       "Quercus muehlenbergii", "Quercus pagoda", "Quercus palustris", "Quercus phellos", "Quercus shumardii", "Quercus spp.",  "Salix nigra",
                       "Tsuga canadensis"),
                 
                 Pine=c("Pinus echinata", "Pinus pungens", "Pinus rigida", "Pinus spp.", "Pinus strobus"),
                 
                 Exotic=c("Maclura pomifera", "Acer platanoides", "Ailanthus altissima", "Aralia elata", "Catalpa bignonioides", "Catalpa speciosa", "Morus alba",
                          "Paulownia tomentosa", "Phellodendron lavallei", "Prunus avium", "Pyrus betulifolia", "Pyrus calleryana", "Pyrus communis", 
                          "Pyrus pyrifolia", "Pyrus spp.", "Sophora japonica", "Ulmus pumila", "Malus baccata", "Malus floribunda", "Malus prunifolia", 
                          "Malus sieboldii", "Malus spp.", "Pyrus spp.")
)

# Need to decide which tree species to use

# Will cut of on average of 0.1 trees per plot per visit

SeedAv<-c(SeedSp,SeedGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "seedlings", years=2006:2017,species=.x) %>% nrow )/(3*145))


SeedUse<-c(SeedSp, SeedGroups)[SeedAv>0.1]

SeedData<-map(.x=SeedUse, ~getPlants(object=NCRN[[9]], group="seedlings",species=., years=2006:2017) %>% mutate( ScYear=scale(Sample_Year, scale = F)))
#SeedData<-map(.x=SeedUse, ~getPlants(object=NCRN[[9]], group="seedlings",species=., years=2006:2017) %>% mutate( ScYear=scale(Sample_Year, scale = F)))


#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain

rstan::rstan_options(auto_write = TRUE)

get_prior(Height  ~ ScYear + (1|Plot_Name),data=SeedData[[1]], family = Gamma(link="log"))


#Seed_Height_Test<-map(.x=list(SeedData[[1]]), ~fitHeight(,iter=2500, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Seed_Height_Regression<-map(.x=SeedData, ~fitHeight(,iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Seed_Height_Summary<-SummarizeFit(model=Seed_Height_Regression, type="Gamma", names=SeedUse, data=SeedData)

#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

#Seed_Height_Trunc_Test<-map(.x=SeedData[1], ~fitTruncHeight( iter=5000, object=.x, cores=4, control=list(adapt_delta=0.99) ))
# Will need many iters to get anwher, 5000 iter has ESS less than 100 for some coefficients. For acer had  nonsense negative linear scale fits, not sure how 
# to get to response scale from this. 


Seed_Height_Final_Summary<-Seed_Height_Summary %>% 
  select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing, Model)

write.csv(Seed_Height_Final_Summary, "Seed_Height.csv")
