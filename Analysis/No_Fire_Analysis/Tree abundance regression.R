library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file


NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


TreeSp<-getPlants(NCRN[[9]], "trees", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(TreeSp)<-TreeSp


TreeGroups<-list(All=NA,
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

TreeAv<-c(TreeSp,TreeGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "trees", years=2006:2017,species=.x) %>% nrow )/(3*145))


TreeUse<-c(TreeSp, TreeGroups)[TreeAv>0.1]

TreeData<-map(.x=TreeUse, ~makeRegData(object=NCRN[[9]], group="trees",species=., years=2006:2017))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
rstan::rstan_options(auto_write = TRUE)

Tree_Poisson_Regression<-map(.x=TreeData, ~fitPoisson(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Tree_Poisson_Summary<-SummarizeFit(model=Tree_Poisson_Regression, type="Poisson", names=TreeUse, data=TreeData)





#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

Tree_NB_Regression<-map(.x=TreeData, ~fitNB(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Tree_NB_Summary<-SummarizeFit(model=Tree_NB_Regression, type="NB", names=TreeUse, data=TreeData)

Tree_Poisson_Loo<-map(Tree_Poisson_Regression, loo, reloo=T, cores=4)

Tree_NB_Loo<-map(Tree_NB_Regression, loo, reloo=T, cores=4)

Tree_Loo_Compare<-map2(.x=Tree_Poisson_Loo, .y=Tree_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))
## Poisson always wins

Poisson_Wins<-map_lgl(Tree_Loo_Compare, ~rownames(.)[1]=="Poisson")

Tree_Combined_Summary<-rbind(Tree_Poisson_Summary[Poisson_Wins,], Tree_NB_Summary[!Poisson_Wins,])

Tree_Final_Summary<-Tree_Combined_Summary %>% 
  select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing, Model)

write.csv(Tree_Final_Summary, "Tree_Abundance.csv")

# 
# Tree_Final_Summary<-Tree_Poisson_Summary %>% select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing)
# write.csv(Tree_Final_Summary, "Tree_Abundance.csv")
