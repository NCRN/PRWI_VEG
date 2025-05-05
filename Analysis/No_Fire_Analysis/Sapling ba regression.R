library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file


NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")


#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


SapSp<-getPlants(NCRN[[9]], "saplings", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(SapSp)<-SapSp


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

SapAv<-c(SapSp,TreeGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "saplings", years=2006:2017,species=.x) %>% nrow )/(3*145))

SapsUse<-c(SapSp, TreeGroups)[SapAv>0.1]



SapData<-map(.x=SapsUse, ~makeRegData(object=NCRN[[9]], group="saplings",species=., years=2006:2017))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
rstan::rstan_options(auto_write = TRUE)

Sap_BA_Regression<-map(.x=SapData, ~fitHurdGamma(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Sap_BA_Summary<-SummarizeFit(model=Sap_BA_Regression, type="HurdGamma", names=SapsUse, data=SapData)


#Just look at species not groups
SapChange<-SapsUse[1:13] %>% map_dbl(~ChangeMatrix(NCRN[[9]], "saplings", years1=2006:2009, years2 = 2014:2017, species=.x, values="presab") %>% 
                              pull(Total) %>% sum)
                                
# Look for species that have gained or lost at least 5 plots              
SapBigChange<-c(abs(SapChange)>4,0,0,0,0,0)                                              
                                              

Sap_BA_Regression2<- map(.x=SapData[(1:18)*SapBigChange], ~fitHurdGamma2(, iter=25000, object=.x, cores=4,control=list(adapt_delta=0.99) ))

Sap_BA_Summary2<-SummarizeFit(model=Sap_BA_Regression2, type="HurdGamma2", names=SapsUse[(1:18)*SapBigChange], data=SapData[(1:18)*SapBigChange])

Sap_BA_Summary3<-SummarizeHurdleFit(model=Sap_BA_Regression2, type="HurdGamma2", names=SapsUse[(1:18)*SapBigChange], data=SapData[(1:18)*SapBigChange])



Sap_BA_Loo<-map(Sap_BA_Regression[(1:18)*SapBigChange], loo, reloo=T, cores=4)

Sap_BA_Loo2<-map(Sap_BA_Regression2, loo, reloo=T, cores=4)

Sap_BA_Compare<-map2(.x=Sap_BA_Loo, .y=Sap_BA_Loo2, ~brms::loo_compare(list(Hurd_Gamma=.x, Hurd_Gamma_2=.y)))


Hurd_Gamma2_Wins<-map_lgl(Sap_BA_Compare, ~rownames(.)[1]=="Hurd_Gamma_2")

Sap_Combined_Summary<-rbind(Sap_BA_Summary[!(1:18)*SapBigChange,], Sap_BA_Summary2)
Sap_Combined_Summary<-left_join(Sap_Combined_Summary, Sap_BA_Summary3[Hurd_Gamma2_Wins,])

Sap_BA_Final_Summary<-Sap_Combined_Summary %>% 
  select(Species, TotalCount, BA_Slope=YearSlope, BA_Error=YearError, BA_Prob_Decreasing=Prob_Decreasing, BA_Prob_Increasing=Prob_Increasing, 
         BA_Per_Change=Year_Per_Change, BA_L95=lower_95, BA_U95=upper_95, Occu_Slope, Occu_Error, Occu_L95, Occu_U95, Occu_Decreasing, Occu_Increasing, 
         Occu_Start, Occu_End)



write.csv(Sap_BA_Final_Summary, "Sap_BA.csv")

