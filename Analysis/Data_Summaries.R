library(NPSForVeg)
library(tidyverse)



PRWI<-importNCRN("C:/Data/Veg_NCRN")[[9]]


## Edited Treegroups to match acutal data)
TreeGroups<-list(All=NA,
      Early_Successional=c("Juniperus virginiana", "Liquidambar styraciflua", "Liriodendron tulipifera", 'Pinus virginiana',
                          'Populus grandidentata'),
                 
       Oak_Hickory=c("Carya alba", "Carya glabra", "Carya ovalis","Quercus alba", "Quercus coccinea",
                   "Quercus falcata", "Quercus prinus", "Quercus rubra","Quercus stellata", "Quercus velutina","Quercus X benderi",  
                     "Quercus X willdenowiana" ),
                 
                 Mesic=c("Acer rubrum", "Diospyros virginiana", "Fagus grandifolia", "Fraxinus americana","Ilex opaca", 
                         "Nyssa sylvatica", "Prunus serotina", "Ulmus americana"),
                 
                 Non_Canopy=c("Acer negundo", "Amelanchier arborea","Carpinus caroliniana","Cornus florida","Prunus americana", "Sassafras albidum"),
                 
                 Wet=c("Platanus occidentalis", "Quercus pagoda", "Quercus phellos"),
                 
                 Pine=c("Pinus echinata", "Pinus rigida"),
                 
                 Exotic=c("Ailanthus altissima",  "Pyrus communis")
)



## Basis summary data for tables. 

## Trees

dens(PRWI, "trees", years=2014:2017)

getPlants(PRWI, "trees", years = 2014:2017) %>% group_by(Latin_Name) %>% 
  summarise(Tot=(sum(SumLiveBasalArea_cm2)/(145*225*pi)) %>% round(3) ) %>% View

getPlants(PRWI, "trees", years = 2014:2017) %>% group_by(Latin_Name) %>% 
  summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View

getPlants(PRWI, "trees", years=2014:2017) %>% group_by(Latin_Name) %>% 
  summarise(Abund=n()) %>% view

dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Early_Successional)
dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Early_Successional, values="size", area="ha")
getPlants(PRWI, "trees", years=2014:2017, species=TreeGroups$Early_Successional) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Mesic)
dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Mesic, values="size", area="ha")
getPlants(PRWI, "trees", years=2014:2017, species=TreeGroups$Mesic) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Non_Canopy)
dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Non_Canopy, values="size", area="ha")
getPlants(PRWI, "trees", years=2014:2017, species=TreeGroups$Non_Canopy) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Oak_Hickory)
dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Oak_Hickory, values="size", area="ha")
getPlants(PRWI, "trees", years=2014:2017, species=TreeGroups$Oak_Hickory) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Pine)
dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Pine, values="size", area="ha")
getPlants(PRWI, "trees", years=2014:2017, species=TreeGroups$Pine) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Wet)
dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Wet, values="size", area="ha")
getPlants(PRWI, "trees", years=2014:2017, species=TreeGroups$Wet) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Exotic)
dens(PRWI,"trees", years=2014:2017, species=TreeGroups$Exotic, values="size", area="ha")
getPlants(PRWI, "trees", years=2014:2017, species=TreeGroups$Exotic) %>% pull(Plot_Name) %>% unique %>% length()/145

## Saplings

## Edited Treegroups to match acutal data)
SapGroups<-list(All=NA,
                 Early_Successional=c("Juniperus virginiana", "Liquidambar styraciflua", "Liriodendron tulipifera", 'Pinus virginiana'),
                 
                 Oak_Hickory=c("Carya alba", "Carya glabra", "Carya ovalis","Quercus alba", "Quercus coccinea",
                               "Quercus falcata", "Quercus prinus", "Quercus rubra","Quercus velutina"),
                 
                 Mesic=c("Acer rubrum", "Fagus grandifolia", "Fraxinus americana","Ilex opaca", 
                         "Nyssa sylvatica", "Ulmus americana"),
                 
                 Non_Canopy=c("Aralia spinosa", "Asimina triloba","Carpinus caroliniana","Cornus florida","Morus rubra", "Sassafras albidum")

)

dens(PRWI, "saplings", years=2014:2017) %>% view

getPlants(PRWI, "saplings", years = 2014:2017) %>% group_by(Latin_Name) %>% 
  summarise(Tot=(sum(SumLiveBasalArea_cm2)/(145*27*pi)) %>% round(3) ) %>% View

getPlants(PRWI, "saplings", years = 2014:2017) %>% group_by(Latin_Name) %>% 
  summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View

getPlants(PRWI, "trees", years=2014:2017) %>% group_by(Latin_Name) %>% 
  summarise(Abund=n()) %>% view

dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Early_Successional)
dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Early_Successional, values="size", area="ha")
getPlants(PRWI, "saplings", years=2014:2017, species=SapGroups$Early_Successional) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Mesic)
dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Mesic, values="size", area="ha")
getPlants(PRWI, "saplings", years=2014:2017, species=SapGroups$Mesic) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Non_Canopy)
dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Non_Canopy, values="size", area="ha")
getPlants(PRWI, "saplings", years=2014:2017, species=SapGroups$Non_Canopy) %>% pull(Plot_Name) %>% unique %>% length()/145

dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Oak_Hickory)
dens(PRWI,"saplings", years=2014:2017, species=SapGroups$Oak_Hickory, values="size", area="ha")
getPlants(PRWI, "saplings", years=2014:2017, species=SapGroups$Oak_Hickory) %>% pull(Plot_Name) %>% unique %>% length()/145


## Seedings

## Edited Treegroups to match acutal data)

SeedGroups<-list(All=NA,
                 Early_Successional=c("Juniperus virginiana", "Liquidambar styraciflua", "Liriodendron tulipifera",
                   'Pinus virginiana'),
                 
                 Oak_Hickory=c("Carya alba", "Carya glabra", "Carya ovalis", "Carya spp.",
                               "Quercus alba", "Quercus coccinea","Quercus falcata", 
                               "Quercus prinus", "Quercus rubra","Quercus spp.", "Quercus stellata", 
                               "Quercus velutina","Quercus X bushii"),
                 
                 Mesic=c("Acer rubrum", "Diospyros virginiana", "Fagus grandifolia", "Ilex opaca", 
                         "Nyssa sylvatica", "Prunus serotina"),
                 
                 Non_Canopy=c("Amelanchier arborea", "Aralia spinosa", "Asimina triloba","Carpinus caroliniana",
                              "Cornus florida","Sassafras albidum"),
                 
                 Wet=c("Betula nigra", "Carya cordiformis", "Quercus phellos"),
                 
                 
                 Exotic=c("Ailanthus altissima")
)
 
 dens(PRWI, "seedlings", years=2014:2017) %>% view
 
 getPlants(PRWI, "seedlings", years=2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Abund=n()) %>% view
 
 getPlants(PRWI, "seedlings", years = 2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View


 dens(PRWI,"seedlings", years=2014:2017, species=SeedGroups$Early_Successional)
 getPlants(PRWI, "seedlings", years=2014:2017, species=SeedGroups$Early_Successional) %>% pull(Plot_Name) %>% unique %>% length()/145
 
 dens(PRWI,"seedlings", years=2014:2017, species=SeedGroups$Mesic)
 getPlants(PRWI, "seedlings", years=2014:2017, species=SeedGroups$Mesic) %>% pull(Plot_Name) %>% unique %>% length()/145

 dens(PRWI,"seedlings", years=2014:2017, species=SeedGroups$Non_Canopy)
 getPlants(PRWI, "seedlings", years=2014:2017, species=SeedGroups$Non_Canopy) %>% pull(Plot_Name) %>% unique %>% length()/145

 dens(PRWI,"seedlings", years=2014:2017, species=SeedGroups$Oak_Hickory)
 getPlants(PRWI, "seedlings", years=2014:2017, species=SeedGroups$Oak_Hickory) %>% pull(Plot_Name) %>% unique %>% length()/145
 
 dens(PRWI,"seedlings", years=2014:2017, species=SeedGroups$Wet)
 getPlants(PRWI, "seedlings", years=2014:2017, species=SeedGroups$Oak_Hickory) %>% pull(Plot_Name) %>% unique %>% length()/145
 
 
 
 ## Shrubs
 
 
 dens(PRWI, "shrubs", years=2014:2017) %>% view
 dens(PRWI, "herbs", years=2014:2017, values = "size") %>% view
 
 getPlants(PRWI, "shrubs", years=2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Abund=n()) %>% view
 
 getPlants(PRWI, "herbs", years=2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Abund=n()) %>% view
 
 getPlants(PRWI, "shrubs", years = 2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View
 
 getPlants(PRWI, "herbs", years = 2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View
 
 
 
 ## Shrub Seedlings
 
dens(PRWI, "shseedlings", years=2014:2017, values = "count", area="plot") %>% view
 
 getPlants(PRWI, "shseedlings", years=2014:2017) %>% group_by(Latin_Name) %>% 
    summarise(Abund=n()) %>% view
 
 getPlants(PRWI, "shseedlings", years = 2014:2017) %>% group_by(Latin_Name) %>% 
    summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View

 

 ## Vines
 
  dens(PRWI, "vines", years=2014:2017) %>% view
 
 getPlants(PRWI, "vines", years=2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Abund=n()) %>% view
 
 getPlants(PRWI, "vines", years = 2014:2017) %>% group_by(Latin_Name) %>% 
   summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View
 

 ## CWD
 getPlants(PRWI, "cwd", years=2014:2017) %>% group_by(Latin_Name, Plot_Name) %>% 
    summarize(Vol=0.02742*sum(Diameter^2))%>%
    group_by(Latin_Name) %>% summarize(Av_Vol=mean(Vol)) %>% View
 
 getPlants(PRWI, "cwd", years=2014:2017) %>% group_by(Latin_Name) %>% 
    summarise(Abund=n()) %>% view
 
 getPlants(PRWI, "cwd", years = 2014:2017) %>% group_by(Latin_Name) %>% 
    summarise(Tot=(100*length(unique(Plot_Name))/145) %>% round(2)) %>%  View
 
 
 
 
 