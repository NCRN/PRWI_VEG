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


# Shrub Matrices
Shrub1<-SiteXSpec(NCRN[[9]],"shrubs", years=2006:2009)
write.csv(Shrub1, "Shrubs_Cycle1.csv", row.names = F)

Shrub2<-SiteXSpec(NCRN[[9]],"shrubs", years=2010:2013)
write.csv(Shrub2, "Shrubs_Cycle2.csv", row.names = F)

Shrub3<-SiteXSpec(NCRN[[9]],"shrubs", years=2014:2017)
write.csv(Shrub3, "Shrubs_Cycle3.csv", row.names = F)


## Seedling Figures



PRWI_exotic<-c("Ailanthus altissima", "Malus spp","Prunus avium", "Pyrus spp.")
PRWI_Fraxinus<-c("Fraxinus americana")
PRWI_under<-c("Amelanchier arborea","Aralia spinosa","Carpinus caroliniana", " Cercis canadensis", "Cornus florida"," Ilex opaca",
              "Juniperus virginiana", "Morus rubra", "Sassafras albidum")
PRWI_boxelder<-"Acer negundo"
PRWI_pawpaw<-"Asimina triloba"


PRWI_All_Seeds<-getPlants(NCRN[[9]], group="seedlings", years=2006:2017)
PRWI_All_Seeds<-PRWI_All_Seeds %>% mutate(DeerCycle=Cycle)

PRWI_All_Seeds<-PRWI_All_Seeds %>% mutate(Exotic=ifelse(Latin_Name %in% PRWI_exotic,1,0))
Cycle_Names<-c(`1`="Cycle 1: \n2006-2009",`2`= "Cycle 2: \n2010-2013",`3`= "Cycle 3: \n2014-2017")
PRWI_All_Seeds$PlantCat<-case_when(PRWI_All_Seeds$Latin_Name %in% PRWI_exotic ~"Exotic",
                                 PRWI_All_Seeds$Latin_Name %in% PRWI_Fraxinus ~ "Fraxinus",
                                 PRWI_All_Seeds$Latin_Name %in% PRWI_under ~"Understory",
                                 PRWI_All_Seeds$Latin_Name %in% PRWI_boxelder~"Acer negundo",
                                 PRWI_All_Seeds$Latin_Name %in% PRWI_pawpaw~"Asimina triloba",
                                 TRUE ~"Overstory")


PRWI_All_Seeds$PlantCat<-factor(PRWI_All_Seeds$PlantCat, levels=c('Exotic', "Fraxinus", "Acer negundo","Asimina triloba", "Understory", "Overstory"))

#### Figure 3 ####

PRWI_Seed_Breaks<-c(0, 14.9, 29.9, 44.9, 59.9, 74.9, 89.9, 104.9, 119.9, 134.9, 149.9,164.9,179.9,194.9,209.9,224.9,239.9, 254.9, 269.9, 284.5)

#PRWI_Seed_Fig3<-
ggplot(PRWI_All_Seeds, aes(x=Height, fill = factor(Exotic, levels=c(1,0)))) + 
  geom_histogram(breaks=PRWI_Seed_Breaks ) + 
  facet_grid(.~DeerCycle, labeller = as_labeller(Cycle_Names)) +
  scale_fill_manual(values=c("darkgreen","lightblue"), labels=c("Exotic   ", "Native")) +
  facet_grid(.~DeerCycle, labeller = as_labeller(Cycle_Names)) +
  labs(x="Height (cm)", y="Seedlings/ha") +
  theme_light(base_size = 12) +
  scale_x_continuous(breaks=PRWI_Seed_Breaks,
                     labels=c("",15,"",45,"",75,"",105,"", 135,"",165,"",195, "",225,"",255,"",285), limits=c(0,290))+
  scale_y_continuous(labels = function(x)as.integer(x*10000/(145*12)), breaks = c(0,1500, 3000)*(12*145/10000))+
  theme(legend.position="bottom", legend.title=element_blank(), legend.direction="horizontal",
        panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid = element_blank(),
        strip.text = element_text(colour = 'black'), strip.background = element_rect(fill="white"))+
  theme(text=element_text(family="sans"))



#ggsave("PRWI_Seed_Heght.tif",plot=PRWI_Seed_Fig3, device = "tiff", dpi=300, width=10, height=5, units="in")


#PRWI_Seed_Fig4<-
ggplot(PRWI_All_Seeds, aes(x=factor(DeerCycle), fill=PlantCat))+
  geom_bar()+
  labs(y="Seedlings/ha", x=NULL)+
  scale_x_discrete(labels=Cycle_Names)+
  scale_y_continuous(labels = function(x)as.integer(x*10000/(145*12)), 
                     breaks = c(0,1500,3000, 4500, 6000)*(12*145/10000))+
  scale_fill_brewer(name="Tree Type", palette = "YlGnBu", direction= -1)+
  theme_classic(base_size = 12) +
  theme(text=element_text(family = "sans"))

#ggsave("PRWI_Seed_Groups.tif",plot=PRWI_Seed_Fig4, device = "tiff", dpi=300, width=6, height=5, units="in")


## Sapling Figs


PRWI_All_Saps<-getPlants(NCRN[[9]], group="saplings", years=2006:2017)
PRWI_All_Saps<-PRWI_All_Saps %>% mutate(DeerCycle=Cycle)


PRWI_All_Saps<-PRWI_All_Saps %>% mutate(Exotic=ifelse(Latin_Name %in% PRWI_exotic,1,0))

PRWI_All_Saps$PlantCat<-case_when(PRWI_All_Saps$Latin_Name %in% PRWI_exotic ~"Exotic",
                                   PRWI_All_Saps$Latin_Name %in% PRWI_Fraxinus ~ "Fraxinus",
                                   PRWI_All_Saps$Latin_Name %in% PRWI_under ~"Understory",
                                   PRWI_All_Saps$Latin_Name %in% PRWI_boxelder~"Acer negundo",
                                   PRWI_All_Saps$Latin_Name %in% PRWI_pawpaw~"Asimina triloba",
                                   TRUE ~"Overstory")


PRWI_All_Saps$PlantCat<-factor(PRWI_All_Saps$PlantCat, levels=c('Exotic', "Fraxinus", "Acer negundo","Asimina triloba", "Understory", "Overstory"))

#### Figure 3 ####

PRWI_Sap_Breaks<-c(0, 0.9, 1.9, 2.9, 3.9, 4.9, 5.9, 6.9, 7.9, 8.9, 9.9)

#PRWI_Sap_Fig3<-
ggplot(PRWI_All_Saps %>% filter(Equiv_Live_DBH_cm>0), aes(x=Equiv_Live_DBH_cm))+#, fill = factor(Exotic, levels=c(1,0)))) + #No exotics
  geom_histogram(breaks=PRWI_Sap_Breaks, fill="lightblue" ) + 
  facet_grid(.~DeerCycle, labeller = as_labeller(Cycle_Names)) +
 # scale_fill_manual(values=c("darkgreen","lightblue"), labels=c("Exotic   ", "Native")) +
  facet_grid(.~DeerCycle, labeller = as_labeller(Cycle_Names)) +
  labs(x="DBH (cm)", y="Saplings/ha") +
  theme_light(base_size = 12) +
  scale_x_continuous(breaks=PRWI_Sap_Breaks,
                     labels=c(0,1,2,3,4,5,6,7,8,9,10), limits=c(0,10))+
  scale_y_continuous(labels = function(x)as.integer(x*10000/(145*27*pi)), breaks = c(0,100,200,300)*(27*pi*145/10000))+
  theme(legend.position="bottom", legend.title=element_blank(), legend.direction="horizontal",
        panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid = element_blank(),
        strip.text = element_text(colour = 'black'), strip.background = element_rect(fill="white"))+
  theme(text=element_text(family="sans"))




#ggsave("PRWI_Sap_Size.tif",plot=PRWI_Sap_Fig3, device = "tiff", dpi=300, width=10, height=5, units="in")


#PRWI_Sap_Fig4<-
ggplot(PRWI_All_Saps, aes(x=factor(DeerCycle), fill=PlantCat))+
  geom_bar()+
  labs(y="Saplings/ha", x=NULL)+
  scale_x_discrete(labels=Cycle_Names)+
  scale_y_continuous(labels = function(x)as.integer(x*10000/(145*27*pi)), 
                     breaks = c(0, 500, 1000)*(27*pi*145/10000))+
  scale_fill_brewer(name="Tree Type", palette = "YlGnBu", direction= -1)+
  theme_classic(base_size = 12) +
  theme(text=element_text(family = "sans"))

#ggsave("PRWI_Sap_Groups.tif",plot=PRWI_Sap_Fig4, device = "tiff", dpi=300, width=6, height=5, units="in")
