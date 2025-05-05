library(tidyverse)
library(ggplot2)
library(ggstance)

#### Trees No Fire ####

TreeDen<-read.csv("./Species Graph Data/Tree_Abundance_Species_Graph_Data.csv")
TreeBA<-read.csv("./Species Graph Data/Tree_BA_Species_Graph_Data.csv")

TreeDen<-TreeDen %>% arrange(Species) %>% mutate(Common=c("Red maple", "American hornbeam", "Mockernut hickory", "Pignut hickory", "Red hickory",
                  "Flowering dogwood",  "American beech", "White ash",  "American holly", "Eastern red cedar", 
                  "Sweetgum", "Tulip poplar", "Black gum", "Virginia pine", "White oak", "Scarlet oak",
                  "Southern red oak", "Chestnut oak", "Northern red oak", "Black oak", "Sassafras"))

TreeBA<-TreeBA %>% arrange(Species) %>% mutate(Common=c("Red maple", "American hornbeam", "Mockernut hickory", "Pignut hickory", "Red hickory",
                                                          "Flowering dogwood",  "American beech", "White ash",  "American holly", "Eastern red cedar", 
                                                          "Sweetgum", "Tulip poplar", "Black gum", "Virginia pine", "White oak", "Scarlet oak",
                                                          "Southern red oak", "Chestnut oak", "Northern red oak", "Black oak", "Sassafras"))



Tree_All<-bind_rows(TreeDen %>% mutate(Measure="Den"), TreeBA %>% mutate(Measure="BA")) %>% 
  rename(Mean=Year_Per_Change, Low_85=Year_Per_Change_low85, High_85=Year_Per_Change_upper85,
         Low_95=Year_Per_Change_low95, High_95=Year_Per_Change_upper95) %>% 
  # mutate(Species=factor(Species, levels=rev(c("Juniperus virginiana", "Liquidambar styraciflua", "Liriodendron tulipifera", 'Pinus virginiana',
  #                                             "Acer rubrum", "Fagus grandifolia", "Fraxinus americana","Ilex opaca", "Nyssa sylvatica", 
  #                                             "Carpinus caroliniana", "Cornus florida","Sassafras albidum", "Carya alba", "Carya glabra",
  #                                             "Carya ovalis", "Quercus alba", "Quercus coccinea","Quercus falcata", 
  #                                             "Quercus prinus", "Quercus rubra", "Quercus velutina" )), ordered = T))

  mutate(Common=factor(Common, 
      levels=rev(c("Eastern red cedar", "Sweetgum","Tulip poplar","Virginia pine",
                   "American beech", "American holly", "Black gum", "Red maple", "White ash",
                   "American hornbeam", "Flowering dogwood", "Sassafras", 
                   "Black oak", "Chestnut oak", "Mockernut hickory",  "Northern red oak", "Pignut hickory", 
                   "Red hickory", "Scarlet oak", "Southern red oak", "White oak", "Quercus velutina" )), ordered = T))

TreePlot<-ggplot(Tree_All, aes(y=Common, x=Mean, group=Measure, color=Measure, shape=Measure)) +
  geom_pointrangeh(aes(xmin=Low_85, xmax=High_85),position=position_dodge(width=.5)) +
  geom_pointrangeh(aes(xmin=Low_95, xmax=High_95),position=position_dodge(width=.5),linetype="dotted") +
  geom_vline(aes(xintercept=1)) +
  geom_hline(aes(yintercept=9.5), linetype="dashed")+
  geom_hline(aes(yintercept=12.5), linetype="dashed")+
  geom_hline(aes(yintercept=17.5), linetype="dashed")+
  labs(y=element_blank(), x="% Change/year")+
  scale_x_continuous(breaks=c(.90, .95,1,1.05,1.1),labels = function(x) (x-1)*100 )+
  coord_cartesian(xlim=c(.84, 1.18), clip="off")+
  annotate("text",x=.69, y=5, label="Oak-Hickory", angle=90)+
  annotate("text",x=.69, y=11, label="Non-Canopy", angle=90)+
  annotate("text",x=.69, y=15, label="Mesic", angle=90)+
  annotate("text",x=.69, y=19.2, label="Early \n Successional", angle=90)+
  scale_color_manual(values=c("Den"="black", "BA"="blue"), labels=c("Den"="Density", "BA"="Basal Area"))+#, guide=guide_legend(reverse = T)) +
  scale_shape_manual(values=c("Den"=16,"BA"=15), labels=c("Den"="Density", "BA"="Basal Area"))+#,  guide=guide_legend(reverse = T))+
  theme_classic() +
  theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"), legend.position = "bottom")

#ggsave(filename = "Tree_Species_Change.tiff", plot=TreePlot, device="tiff", width=6 , height=8, units="in", dpi=300)


#### Trees Fire ####

TreeFireDen<-read.csv("./Species Graph Data/Tree_Fire_Abundance_Species_Graph_Data.csv")%>%
  arrange(Species) %>%
  mutate(Common=c("Red maple","American beech", "Tulip poplar", "Black gum", "Virginia pine", "White oak", "Scarlet oak"))

TreeFireBA<-read.csv("./Species Graph Data/Tree_Fire_BA_Species_Graph_Data.csv") %>% 
  arrange(Species) %>%
  mutate(Common=c("Red maple","American beech", "Tulip poplar", "Black gum", "Virginia pine", "White oak", "Scarlet oak"))


Tree_Fire_All<-bind_rows(TreeFireDen %>% mutate(Measure="Den"), TreeFireBA %>% mutate(Measure="BA")) %>% 
  rename(Mean=Fire_Year_Per_Change, Low_85=Fire_Year_Per_Change_low85, High_85=Fire_Year_Per_Change_upper85,
         Low_95=Fire_Year_Per_Change_low95, High_95=Fire_Year_Per_Change_upper95) %>% 
  # mutate(Species=factor(Species, levels=rev(c("Liriodendron tulipifera", 'Pinus virginiana',
  #                                             "Acer rubrum", "Fagus grandifolia", "Nyssa sylvatica", 
  #                                             "Quercus alba", "Quercus coccinea" )), ordered = T))
  mutate(Common=factor(Common, levels=rev(c("Tulip poplar","Virginia pine",
                                              "American beech", "Red maple", "Black gum", 
                                              "Scarlet oak", "White oak" )), ordered = T))

FireTreePlot<-
ggplot(Tree_Fire_All, aes(y=Common, x=Mean, group=Measure, color=Measure, shape=Measure)) +
  geom_pointrangeh(aes(xmin=Low_85, xmax=High_85),position=position_dodge(width=.5)) +
  geom_pointrangeh(aes(xmin=Low_95, xmax=High_95),position=position_dodge(width=.5),linetype="dotted") +
  geom_vline(aes(xintercept=1)) +
  geom_hline(aes(yintercept=2.5), linetype="dashed") +
  geom_hline(aes(yintercept=5.5), linetype="dashed") +
  labs(y=element_blank(), x="% Change/year") +
  scale_x_continuous(breaks=c(.8, .90, 1,1.1, 1.2),labels = function(x) (x-1)*100 ) +
  coord_cartesian(xlim=c(.75, 1.18), clip="off") +
  annotate("text",x=.60, y=1.5, label="Oak-Hickory", angle=90)+
  annotate("text",x=.60, y=4, label="Mesic", angle=90)+
  annotate("text",x=.60, y=6.5, label="Early \n Successional", angle=90)+
  scale_color_manual(values=c("Den"="black", "BA"="blue"), labels=c("Den"="Density", "BA"="Basal Area"))+
  scale_shape_manual(values=c("Den"=16,"BA"=15), labels=c("Den"="Density", "BA"="Basal Area"))+
  theme_classic() +
  theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"), legend.position = "bottom")

#ggsave(filename = "Tree_Fire_Species_Change.tiff", plot=FireTreePlot, device="tiff", width=6 , height=4, units="in", dpi=300)


#### Saplings No Fire ###

SapDen<-read.csv("./Species Graph Data/Sap_Abundance_Species_Graph_Data.csv")
SapBA<-read.csv("./Species Graph Data/Sap_BA_Species_Graph_Data.csv")


SapDen<-SapDen %>% arrange(Species) %>% mutate(Common=c("Red maple", "American hornbeam", "Pignut hickory",
                                                          "Flowering dogwood",  "American beech", "American holly", 
                                                          "Sweetgum", "Black gum", "White oak", "Scarlet oak",
                                                          "Southern red oak", "Black oak", "Sassafras"))


SapBA<-SapBA %>% arrange(Species) %>% mutate(Common=c("Red maple", "American hornbeam", "Pignut hickory",
                                                        "Flowering dogwood",  "American beech", "American holly", 
                                                        "Sweetgum", "Black gum", "White oak", "Scarlet oak",
                                                        "Southern red oak", "Black oak", "Sassafras"))


Sap_All<-bind_rows(SapDen %>% mutate(Measure="Den"), SapBA %>% mutate(Measure="BA")) %>% 
  rename(Mean=Year_Per_Change, Low_85=Year_Per_Change_low85, High_85=Year_Per_Change_upper85,
         Low_95=Year_Per_Change_low95, High_95=Year_Per_Change_upper95) %>% 
  # mutate(Species=factor(Species, levels=rev(c("Liquidambar styraciflua", "Acer rubrum", "Fagus grandifolia", "Ilex opaca", "Nyssa sylvatica", 
  #                                             "Carpinus caroliniana", "Cornus florida","Sassafras albidum", "Carya glabra",
  #                                             "Quercus alba", "Quercus coccinea","Quercus falcata", 
  #                                             "Quercus velutina" )), ordered = T))
  mutate(Common=factor(Common, levels=rev(c("Sweetgum", "American beech", "American holly", "Black gum", "Red maple",
                                              "American hornbeam", "Flowering dogwood","Sassafras", "Pignut hickory",
                                              "White oak", "Scarlet oak","Southern red oak", 
                                              "Black oak" )), ordered = T))



SapPlot<-
  ggplot(Sap_All, aes(y=Common, x=Mean, group=Measure, color=Measure, shape=Measure)) +
  geom_pointrangeh(aes(xmin=Low_85, xmax=High_85),position=position_dodge(width=.5)) +
  geom_pointrangeh(aes(xmin=Low_95, xmax=High_95),position=position_dodge(width=.5),linetype="dotted") +
  geom_vline(aes(xintercept=1)) +
  geom_hline(aes(yintercept=5.5), linetype="dashed")+
  geom_hline(aes(yintercept=8.5), linetype="dashed")+
  geom_hline(aes(yintercept=12.5), linetype="dashed")+
  labs(y=element_blank(), x="% Change/year")+
  scale_x_continuous(breaks=c(.80, .90, 1, 1.1, 1.2),labels = function(x) (x-1)*100 )+
  coord_cartesian(xlim=c(.75, 1.22), clip="off")+
  annotate("text",x=.54, y=3, label="Oak-Hickory", angle=90)+
  annotate("text",x=.54, y=7, label="Non-Canopy", angle=90)+
  annotate("text",x=.54, y=10.5, label="Mesic", angle=90)+
  annotate("text",x=.54, y=13, label="Early \n Successional", angle=90)+
  scale_color_manual(values=c("Den"="black", "BA"="blue"), labels=c("Den"="Density", "BA"="Basal Area"))+
  scale_shape_manual(values=c("Den"=16,"BA"=15), labels=c("Den"="Density", "BA"="Basal Area"))+
  theme_classic() +
  theme(plot.margin=unit(c(1, .5, .5, 1.5), "cm"), legend.position = "bottom")

#ggsave(filename = "Sap_Species_Change.tiff", plot=SapPlot, device="tiff", width=6 , height=8, units="in", dpi=300)


## Saplings Fire ####

SapFireDen<-read.csv("./Species Graph Data/Sap_Fire_Abundance_Species_Graph_Data.csv")
SapFireBA<-read.csv("./Species Graph Data/Sap_Fire_BA_Species_Graph_Data.csv")

SapFireDen<-SapFireDen %>% arrange(Species) %>% mutate(Common=c("Red maple", "Flowering dogwood", "Black gum",
                                                                "White oak", "Scarlet oak"))


SapFireBA<-SapFireBA %>% arrange(Species) %>%  mutate(Common=c("Red maple", "Flowering dogwood", "Black gum",
                                                       "White oak", "Scarlet oak"))



Sap_Fire_All<-bind_rows(SapFireDen %>% mutate(Measure="Den"), SapFireBA %>% mutate(Measure="BA")) %>% 
  rename(Mean=Fire_Year_Per_Change, Low_85=Fire_Year_Per_Change_low85, High_85=Fire_Year_Per_Change_upper85,
         Low_95=Fire_Year_Per_Change_low95, High_95=Fire_Year_Per_Change_upper95) %>% 
  # mutate(Species=factor(Species, levels=rev(c("Acer rubrum", "Nyssa sylvatica", "Cornus florida",
  #                                             "Quercus alba", "Quercus coccinea" )), ordered = T))
  mutate(Common=factor(Common, levels=rev(c("Black gum", "Red maple", "Flowering dogwood",
                                            "Scarlet oak", "White oak" )), ordered = T))
FireSapPlot<-
  ggplot(Sap_Fire_All, aes(y=Common, x=Mean, group=Measure, color=Measure, shape=Measure)) +
  geom_pointrangeh(aes(xmin=Low_85, xmax=High_85),position=position_dodge(width=.5)) +
  geom_pointrangeh(aes(xmin=Low_95, xmax=High_95),position=position_dodge(width=.5),linetype="dotted") +
  geom_vline(aes(xintercept=1)) +
  geom_hline(aes(yintercept=2.5), linetype="dashed")+
  geom_hline(aes(yintercept=3.5), linetype="dashed")+
  labs(y=element_blank(), x="% Change/year")+
  scale_x_continuous(breaks=c(.90, 1, 1.25, 1.5),labels = function(x) (x-1)*100 )+
  coord_cartesian(xlim=c(.85, 1.54), clip="off")+
  annotate("text",x=.57, y=1.1, label="Oak-Hickory", angle=90)+
  annotate("text",x=.57, y=3, label="Non-Canopy", angle=90)+
  annotate("text",x=.57, y=4.6, label="Mesic", angle=90)+
  scale_color_manual(values=c("Den"="black", "BA"="blue"), labels=c("Den"="Density", "BA"="Basal Area"))+
  scale_shape_manual(values=c("Den"=16,"BA"=15), labels=c("Den"="Density", "BA"="Basal Area"))+
  theme_classic() +
  theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"), legend.position = "bottom")

#ggsave(filename = "Sap_Fire_Species_Change.tiff", plot=FireSapPlot, device="tiff", width=6 , height=4, units="in", dpi=300)


#### Seedlings No Fire ###

SeedDen<-read.csv("./Species Graph Data/Seedlings_Abundance_Species_Graph_Data.csv")

SeedDen<-SeedDen %>% arrange(Species) %>% mutate(Common=c("Red maple","Common serviceberry", "Pawpaw","American hornbeam",
    "Mockernut hickory","Pignut hickory", "American beech","American holly","Sweetgum","Black gum","Virginia pine",
    "White oak","Scarlet oak", "Southern red oak","Chestnut oak", "Black oak", "Sassafras"))
  
  


SeedDen<-SeedDen %>% 
  rename(Mean=Year_Per_Change, Low_85=Year_Per_Change_low85, High_85=Year_Per_Change_upper85,
         Low_95=Year_Per_Change_low95, High_95=Year_Per_Change_upper95) %>% 
  # mutate(Species=factor(Species, levels=rev(c("Liquidambar styraciflua",'Pinus virginiana',"Acer rubrum", 
  #                                             "Fagus grandifolia", "Ilex opaca", "Nyssa sylvatica",
  #                                             "Amelanchier arborea", "Asimina triloba", 
  #                                             "Carpinus caroliniana","Sassafras albidum" ,
  #                                             "Carya alba", "Carya glabra", "Quercus alba", 
  #                                     "Quercus coccinea","Quercus falcata", "Quercus prinus","Quercus velutina")), ordered = T))
mutate(Common=factor(Common, levels=rev(c("Sweetgum","Virginia pine", "Red maple", "American beech", "American holly",
        "Black gum", "Common serviceberry",  "Pawpaw",   "American hornbeam",  "Sassafras","Mockernut hickory","Pignut hickory",
  "White oak","Scarlet oak", "Southern red oak","Chestnut oak", "Black oak" )), ordered = T))


SeedPlot<-
  ggplot(SeedDen, aes(y=Common, x=Mean)) +
  geom_pointrangeh(aes(xmin=Low_85, xmax=High_85),position=position_dodge(width=.5)) +
  geom_pointrangeh(aes(xmin=Low_95, xmax=High_95),position=position_dodge(width=.5),linetype="dotted") +
  geom_vline(aes(xintercept=1)) +
  geom_hline(aes(yintercept=7.5), linetype="dashed")+
  geom_hline(aes(yintercept=11.5), linetype="dashed")+
  geom_hline(aes(yintercept=15.5), linetype="dashed")+
  labs(y=element_blank(), x="% Change/year")+
  scale_x_continuous(breaks=c(.80, .90, 1, 1.1, 1.2),labels = function(x) (x-1)*100 )+
  coord_cartesian(xlim=c(.75, 1.20), clip="off")+
  annotate("text",x=.55, y=4, label="Oak Hickory", angle=90)+
  annotate("text",x=.55, y=10, label="Non-Canopy", angle=90)+
  annotate("text",x=.55, y=13, label="Mesic", angle=90)+
  annotate("text",x=.55, y=16.25, label="Early \n Successional", angle=90)+
  scale_color_manual(values=c("Den"="black", "BA"="blue"), labels=c("Den"="Density", "BA"="Basal Area"), guide=guide_legend(reverse = T)) +
  theme_classic() +
  theme(plot.margin=unit(c(1, .5, .5, 1.5), "cm"), legend.position = "bottom")

#ggsave(filename = "Seed_Species_Change.tiff", plot=SeedPlot, device="tiff", width=6 , height=8, units="in", dpi=300)


## Seedlings Fire ####

SeedFireDen<-read.csv("./Species Graph Data/Seed_Fire_Abundance_Species_Graph_Data.csv")

SeedFireDen<-SeedFireDen %>% arrange(Species) %>% mutate(Common=c("Red maple","American hornbeam","American holly",
                                                                  "Virginia pine","White oak","Scarlet oak", "Southern red oak"))      

SeedFireDen<-SeedFireDen %>% 
  rename(Mean=Fire_Year_Per_Change, Low_85=Fire_Year_Per_Change_low85, High_85=Fire_Year_Per_Change_upper85,
         Low_95=Fire_Year_Per_Change_low95, High_95=Fire_Year_Per_Change_upper95) %>% 
  # mutate(Species=factor(Species, levels=rev(c( "Pinus virginiana", "Acer rubrum", "Ilex opaca",  "Carpinus caroliniana","Quercus alba",
  #                                              "Quercus coccinea", "Quercus falcata")), ordered = T))
  mutate(Common=factor(Common, levels=rev(c("Virginia pine", "Red maple", "American holly","American hornbeam",
                                            "White oak","Scarlet oak", "Southern red oak" )), ordered = T))

FireSeedPlot<-
  ggplot(SeedFireDen, aes(y=Common, x=Mean)) +
  geom_pointrangeh(aes(xmin=Low_85, xmax=High_85),position=position_dodge(width=.5)) +
  geom_pointrangeh(aes(xmin=Low_95, xmax=High_95),position=position_dodge(width=.5),linetype="dotted") +
  geom_vline(aes(xintercept=1))+
  geom_hline(aes(yintercept=3.5), linetype="dashed")+
  geom_hline(aes(yintercept=4.5), linetype="dashed")+
  geom_hline(aes(yintercept=6.5), linetype="dashed")+
  labs(y=element_blank(), x="% Change/year")+
  scale_x_continuous(breaks=c(.90, 1, 1.25, 1.5),labels = function(x) (x-1)*100 )+
  coord_cartesian(xlim=c(.85, 1.54), clip="off")+
  annotate("text",x=.56, y=2, label="Oak Hickory", angle=90)+
  annotate("text",x=.56, y=4, label="Non-Canopy", angle=90)+
  annotate("text",x=.56, y=5.5, label="Mesic", angle=90)+
  annotate("text",x=.56, y=7, label="Early \n Successional", angle=90)+
  scale_color_manual(values=c("Den"="black", "BA"="blue"), labels=c("Den"="Density", "BA"="Basal Area"), guide=guide_legend(reverse = T)) +
  theme_classic() +
  theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"), legend.position = "bottom")

ggsave(filename = "Seed_Fire_Species_Change.tiff", plot=FireSeedPlot, device="tiff", width=6 , height=5, units="in", dpi=300)
