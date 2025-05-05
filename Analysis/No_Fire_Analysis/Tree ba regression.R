library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file


NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")

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

Tree_BA_Regression<-map(.x=TreeData, ~fitHurdGamma(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Tree_BA_Summary<-SummarizeFit(model=Tree_BA_Regression, type="HurdGamma", names=TreeUse, data=TreeData)


#Just look at species not groups
TreeChange<-TreeUse[1:21] %>% map_dbl(~ChangeMatrix(NCRN[[9]], "trees", years1=2006:2009, years2 = 2014:2017, species=.x, values="presab") %>% 
                              pull(Total) %>% sum)
                                
# Look for species that have gained or lost at least 5 plots              
TreeBigChange<-c(abs(TreeChange)>4,0,0,0,0,0)                                              
                                              

Tree_BA_Regression2<- map(.x=TreeData[(1:26)*TreeBigChange], ~fitHurdGamma2(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Tree_BA_Summary2<-SummarizeFit(model=Tree_BA_Regression2, type="HurdGamma2", names=TreeUse[(1:26)*TreeBigChange], data=TreeData[(1:26)*TreeBigChange])

Tree_BA_Summary3<-SummarizeHurdleFit(model=Tree_BA_Regression2, type="HurdGamma2", names=TreeUse[(1:26)*TreeBigChange], data=TreeData[(1:26)*TreeBigChange])



Tree_BA_Loo<-map(Tree_BA_Regression[(1:26)*TreeBigChange], loo, reloo=T, cores=4)

Tree_BA_Loo2<-map(Tree_BA_Regression2, loo, reloo=T, cores=4)

Tree_BA_Compare<-map2(.x=Tree_BA_Loo, .y=Tree_BA_Loo2, ~brms::loo_compare(list(Hurd_Gamma=.x, Hurd_Gamma_2=.y)))


Hurd_Gamma2_Wins<-map_lgl(Tree_BA_Compare, ~rownames(.)[1]=="Hurd_Gamma_2")

Tree_Combined_Summary<-rbind(Tree_BA_Summary[!(1:26)*TreeBigChange,], Tree_BA_Summary2)
Tree_Combined_Summary<-left_join(Tree_Combined_Summary, Tree_BA_Summary3[Hurd_Gamma2_Wins,])

Tree_BA_Final_Summary<-Tree_Combined_Summary %>% 
  select(Species, TotalCount, BA_Slope=YearSlope, BA_Error=YearError, BA_Prob_Decreasing=Prob_Decreasing, BA_Prob_Increasing=Prob_Increasing, 
         BA_Per_Change=Year_Per_Change, BA_L95=lower_95, BA_U95=upper_95, Occu_Slope, Occu_Error, Occu_L95, Occu_U95, Occu_Decreasing, Occu_Increasing, 
         Occu_Start, Occu_End)

write.csv(Tree_BA_Final_Summary, "Tree_BA.csv")


# 
# Tree_Final_Summary<-Tree_Poisson_Summary %>% select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing)
# write.csv(Tree_Final_Summary, "Tree_Abundance.csv")
