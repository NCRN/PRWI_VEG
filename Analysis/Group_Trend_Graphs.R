library(tidyverse)
library(ggplot2)
library(ggdist)
library(grid)
library(gridExtra)
library(lemon)


TreeDen<-read.csv("./Group Graph Data/Tree_Abund_Group_Trend.csv") %>% mutate(Type="Density")
TreeBA<-read.csv("./Group Graph Data/Tree_BA_Group_Trend.csv") %>% mutate(Type="BA")

All_Tree_Data<-bind_rows(TreeDen, TreeBA)

Tree_Den_Sum<-TreeDen %>% group_by(Year, Group,Burned) %>% summarise(Density=median(Density))
Tree_BA_Sum<-TreeBA %>% group_by(Year, Group,Burned) %>% summarise(Density=median(Density))

Den_Plot<-ggplot(TreeDen, aes(x=Year, y=Density, color=Burned, fill=Burned, linetype=Burned)) + 
  stat_lineribbon(.width=c(.85,.95), alpha=0.25, show.legend = T) +
  geom_line(data=Tree_Den_Sum)+
  labs(x='Year', y="Trees/ha", title="Density") +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_grid(Group~., scales="free", switch="y") +
  theme(strip.placement = "outside")+
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(1,"lines"))
  

BA_Plot<-ggplot(TreeBA, aes(x=Year, y=Density, color=Burned, fill=Burned, linetype=Burned)) + 
  stat_lineribbon( .width=c(.85,.95), alpha=0.25) +
  geom_line(data=Tree_BA_Sum)+
  labs(x='Year', y=expression(Basal~Area~m^2*"/"*ha), title="Basal Area") +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_grid(Group~., scales="free")+
  theme_classic()+
  theme(legend.position = "none", 
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(1,"lines"))

Tree_legend<-g_legend(Den_Plot)

AT_Text<-textGrob("All Trees",rot=90, gp=gpar(fontsize=10))
ES_Text<-textGrob("Early \n Successional", rot=90, gp=gpar(fontsize=10))
M_Text<-textGrob("Mesic", rot=90, gp=gpar(fontsize=10))
NC_Text<-textGrob("Non-Canopy", rot=90, gp=gpar(fontsize=10))
OH_Text<-textGrob("Oak-Hickory", rot=90, gp=gpar(fontsize=10))
Blank_Text<-textGrob(" ", rot=90, gp=gpar(fontsize=10))


#this text grid has a very hacky way to align right hand panel labels with the grid of graphs - blank text
#and heights makes up for the fact that the text_grid does not have an x axis label on the bottom like the graphs.

text_grid<-grid.arrange(Blank_Text, AT_Text, ES_Text,M_Text, NC_Text, OH_Text, Blank_Text, ncol=1, heights=c(.2,1,1,1,1,1,.2))

Tree_No_Legend<-grid.arrange(text_grid, Den_Plot+theme(legend.position = "none"), BA_Plot, nrow=1, 
                             widths=c(1,5,5))

Tree_Final<-grid.arrange(Tree_No_Legend, Tree_legend, ncol=1,heights=c(12,1) )

ggsave(filename = "Trees_Group_Trends.tiff", plot=Tree_Final, device="tiff", width=8 , height=8, units="in", dpi=300)




SapDen<-read.csv("./Group Graph Data/Sap_Abund_Group_Trend.csv") %>% mutate(Type="Density")
SapBA<-read.csv("./Group Graph Data/Sap_BA_Group_Trend.csv") %>% mutate(Type="BA")

All_Sap_Data<-bind_rows(SapDen, SapBA)

Sap_Den_Sum<-SapDen %>% group_by(Year, Group,Burned) %>% summarise(Density=median(Density))
Sap_BA_Sum<-SapBA %>% group_by(Year, Group,Burned) %>% summarise(Density=median(Density))



Den_Plot<-ggplot(SapDen, aes(x=Year, y=Density, color=Burned, fill=Burned, linetype=Burned)) + 
  stat_lineribbon(aes(y=Density), .width=c(.85,.95), alpha=0.25) +
  geom_line(data=Sap_Den_Sum)+
  labs(x='Year', y="Saplings/ha", title="Density") +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_grid(Group~., scales="free", switch="y") +
  theme(strip.placement = "outside")+
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(1,"lines"))

BA_Plot<-ggplot(SapBA, aes(x=Year, y=Density, color=Burned, fill=Burned, linetype=Burned)) + 
  stat_lineribbon(.width=c(.85,.95), alpha=0.25) +
  geom_line(data=Sap_BA_Sum)+
  labs(x='Year', y=expression(Basal~Area~m^2*"/"*ha), title="Basal Area") +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_grid(Group~., scales="free")+
  theme_classic()+
  theme(legend.position = "none", 
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing=unit(1,"lines"))

Sap_legend<-g_legend(Den_Plot)

AT_Text<-textGrob("All Trees",rot=90, gp=gpar(fontsize=10))
ES_Text<-textGrob("Early \n Successional", rot=90, gp=gpar(fontsize=10))
M_Text<-textGrob("Mesic", rot=90, gp=gpar(fontsize=10))
NC_Text<-textGrob("Non-Canopy", rot=90, gp=gpar(fontsize=10))
OH_Text<-textGrob("Oak-Hickory", rot=90, gp=gpar(fontsize=10))
Blank_Text<-textGrob(" ", rot=90, gp=gpar(fontsize=10))


#this text grid has a very hacky way to align right hand panel labels with the grid of graphs - blank text
#and heights makes up for the fact that the text_grid does not have an x axis label on the bottom like the graphs.

text_grid<-grid.arrange(Blank_Text, AT_Text, ES_Text,M_Text, NC_Text, OH_Text, Blank_Text, ncol=1, heights=c(.2,1,1,1,1,1,.2))

Sap_No_Legend<-grid.arrange(text_grid, Den_Plot+theme(legend.position = "none"), BA_Plot, nrow=1, 
                             widths=c(1,5,5))

Sap_Final<-grid.arrange(Sap_No_Legend, Sap_legend, ncol=1,heights=c(12,1) )

ggsave(filename = "Saps_Group_Trends.tiff", plot=Sap_Final, device="tiff", width=8 , height=8, units="in", dpi=300)

