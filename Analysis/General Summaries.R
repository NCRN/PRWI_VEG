library(NPSForVeg)
library(tidyverse)

NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

TreeFirePlots<-FirePlots %>% filter(Fire_Name %in% c("B-Loop","Tracer")) 
SeedFirePlots<-FirePlots %>% filter(Fire_Name=="B-Loop") 

#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


SeedSp<-getPlants(NCRN[[9]], "seedlings", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(SeedSp)<-SeedSp


PlantGroups<-list(All=NA,
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


TreeDataC3<-getPlants(NCRN[[9]], "trees", years=2014:2017) %>% mutate(Group=NA)

TreeDataC3 <-TreeDataC3 %>% mutate( Group=ifelse(Latin_Name %in% PlantGroups$Early_Successional, "Early Successional", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Oak_Hickory, "Oak Hickory", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Mesic, "Mesic", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Non_Canopy, "Non-Canopy", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Wet, "Wet", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Pine, "Pine", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Exot, "Exotic", Group),
                                    Form=factor("Tree", levels=c("Tree","Sapling","Seedling")),
                                    Fire=ifelse(Plot_Name %in% TreeFirePlots$Plot_Name,"Burned","Unburned")
                                   ) %>% 
  select(Plot_Name,Sample_Year, Latin_Name, Group, Form, Fire)

SapDataC3<-getPlants(NCRN[[9]], "saplings", years=2014:2017) %>% mutate(Group=NA)

SapDataC3 <-SapDataC3 %>% mutate( Group=ifelse(Latin_Name %in% PlantGroups$Early_Successional, "Early Successional", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Oak_Hickory, "Oak Hickory", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Mesic, "Mesic", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Non_Canopy, "Non-Canopy", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Wet, "Wet", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Pine, "Pine", Group),
                                    Group=ifelse(Latin_Name %in% PlantGroups$Exot, "Exotic", Group),
                                    Form=factor("Sapling", levels=c("Tree","Sapling","Seedling")),
                                    Fire=ifelse(Plot_Name %in% SeedFirePlots$Plot_Name,"Burned","Unburned")
                                   ) %>% 
                                    select(Plot_Name,Sample_Year, Latin_Name, Group, Form, Fire)



SeedDataC3<-getPlants(NCRN[[9]], "seedlings", years=2014:2017) %>% mutate(Group=NA)

SeedDataC3 <-SeedDataC3 %>% mutate( Group=ifelse(Latin_Name %in% PlantGroups$Early_Successional, "Early Successional", Group),
                                  Group=ifelse(Latin_Name %in% PlantGroups$Oak_Hickory, "Oak Hickory", Group),
                                  Group=ifelse(Latin_Name %in% PlantGroups$Mesic, "Mesic", Group),
                                  Group=ifelse(Latin_Name %in% PlantGroups$Non_Canopy, "Non-Canopy", Group),
                                  Group=ifelse(Latin_Name %in% PlantGroups$Wet, "Wet", Group),
                                  Group=ifelse(Latin_Name %in% PlantGroups$Pine, "Pine", Group),
                                  Group=ifelse(Latin_Name %in% PlantGroups$Exot, "Exotic", Group),
                                  Form=factor("Seedling", levels=c("Tree","Sapling","Seedling")),
                                  Fire=ifelse(Plot_Name %in% SeedFirePlots$Plot_Name,"Burned","Unburned")
                                  ) %>% 
                                    select(Plot_Name,Sample_Year, Latin_Name, Group, Form, Fire)

ggplot(data=TreeDataC3 %>% filter(!Plot_Name %in% TreeFirePlots$Plot_Name), aes(fill=Group))+
  geom_bar(aes(y="Tree"),width=.10)

ggplot(data=SapDataC3, aes(fill=Group))+
  geom_bar(aes(y="Sapling"),width=.10)

ggplot(data=SeedDataC3, aes(fill=Group))+
  geom_bar(aes(y="Seedling"),width=.10)


TSSDataC3<-rbind(TreeDataC3, SapDataC3, SeedDataC3) %>% 
  mutate(Fire=factor(Fire, levels = c("Unburned","Burned")))

ggplot(TSSDataC3 %>% filter(Group %in% c("Early Successional", "Mesic", "Oak Hickory", "Non-Canopy")), aes(fill=Group, y=Form))+
  geom_bar(width=0.75, position = "fill")+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(labels=scales::percent)+
  facet_wrap(~Fire)+
  labs(x="Percent", y="Growth Form", title="Forest Vegetation Cycle 3")+
  theme_classic()+
  theme(aspect.ratio = 1/2, legend.position = "bottom")
    
    
    



