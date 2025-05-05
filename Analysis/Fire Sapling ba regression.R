library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)

#Need to source function file


NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

SapFirePlots<-FirePlots %>% filter(Fire_Name == "B-Loop") 

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

SapFireData<- map(.x=SapData, ~left_join(x=.,y=SapFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
options(mc.cores=4) #loo may need this to be specified here as the cores argument does not seem to work in loo
rstan::rstan_options(auto_write = TRUE)

Sap_Fire_BA_Regression<-map(.x=SapFireData, ~fitFireHurdGamma(, iter=25000, object=.x, cores=4,
                         control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

# Number 13, Sassafras had a lot of divergent transitions. 
Sap_Fire_BA_Summary<-SummarizeFireFit(model=Sap_Fire_BA_Regression, type="HurdGamma", names=SapsUse, data=SapFireData)

Sap_Fire_BA_Fitted<-SummarizeFireOutcome(model=Sap_Fire_BA_Regression, names=SapsUse, data=SapFireData, form="sapling")

#Just look at species not groups
SapChange<-SapsUse[1:13] %>% map_dbl(~ChangeMatrix(NCRN[[9]], "saplings", years1=2006:2009, years2 = 2014:2017, species=.x, values="presab") %>% 
                              pull(Total) %>% sum)
                                
# Look for species that have gained or lost at least 5 plots              
SapBigChange<-c(abs(SapChange)>4,0,0,0,0,0)                                              
                                              

Sap_Fire_BA_Regression2<- map(.x=SapFireData[(1:18)*SapBigChange], ~fitFireHurdGamma2(, iter=25000, object=.x, cores=4,
                                                        control=list(adapt_delta=0.995, max_treedepth=15), save_pars=save_pars(all=TRUE) ))

##lots of divergent transitions for Sassafras. 

Sap_Fire_BA_Summary2<-SummarizeFireFit(model=Sap_Fire_BA_Regression2, type="HurdGamma2", names=SapsUse[(1:18)*SapBigChange], 
                                       data=SapFireData[(1:18)*SapBigChange])

Sap_Fire_BA_Fitted2<-SummarizeFireOutcome(model=Sap_Fire_BA_Regression2, names=SapsUse[(1:18)*SapBigChange], 
                                          data=SapFireData[(1:18)*SapBigChange], form="sapling")


Sap_Fire_BA_Summary3<-SummarizeFireHurdleFit(model=Sap_Fire_BA_Regression2, type="HurdGamma2", names=SapsUse[(1:18)*SapBigChange], 
                                             data=SapFireData[(1:18)*SapBigChange])



Sap_BA_Loo<-map(Sap_Fire_BA_Regression[(1:18)*SapBigChange], loo, reloo=T, moment_match=TRUE)

Sap_BA_Loo2<-map(Sap_Fire_BA_Regression2, loo, reloo=T, moment_match=TRUE)

Sap_Fire_BA_Compare<-map2(.x=Sap_BA_Loo, .y=Sap_BA_Loo2, ~brms::loo_compare(list(Hurd_Gamma=.x, Hurd_Gamma_2=.y)))


Hurd_Gamma2_Wins<-map_lgl(Sap_Fire_BA_Compare, ~rownames(.)[1]=="Hurd_Gamma_2")

Sap_Fire_Combined_Summary<-rbind(Sap_Fire_BA_Summary[!(1:18)*SapBigChange,],
                                 Sap_Fire_BA_Summary[(1:18)*SapBigChange,][!Hurd_Gamma2_Wins,],
                                 Sap_Fire_BA_Summary2[Hurd_Gamma2_Wins,])

Sap_Fire_Combined_Summary<-left_join(Sap_Fire_Combined_Summary, Sap_Fire_BA_Summary3[Hurd_Gamma2_Wins,])

Sap_Fire_Combined_Fitted<-rbind(Sap_Fire_BA_Fitted[!(1:18)*SapBigChange,],
                                Sap_Fire_BA_Fitted[(1:18)*SapBigChange,][!Hurd_Gamma2_Wins,],
                                Sap_Fire_BA_Fitted2[Hurd_Gamma2_Wins,])

 Sap_Fire_BA_Final_Summary<-Sap_Fire_Combined_Summary #%>% 
#   select(Species, TotalCount, TotalPlots, Intercept, FireIntercept, Fire_Int_Per_Change,  Prob_Fire_Int_Increasing, Prob_Fire_Int_Decreasing, 
#          YearSlope, Year_Per_Change, Year_Change_low95, Year_Change_upper95, Prob_Decreasing, Prob_Increasing, 
#          FireSlope, Year_Fire_Change, Year_Fire_Change_low95, Year_Fire_Change_upper95, Prob_Fire_Decreasing, Prob_Fire_Increasing, 
#          Fire_Decreased_Year1, Fire_Increased_Year1, Model)


write.csv(Sap_Fire_BA_Final_Summary, "Sap_Fire_BA.csv")
write.csv(Sap_Fire_Combined_Fitted, "Sap_Fire_BA_Fitted.csv")

Model_Graph_Data<-bind_rows(
  list(
    All_Trees=SapFireData[[14]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Sap_Fire_BA_Regression[[14]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SapFireData[[14]]$ScYear,"scaled:center")),
    
    Early_Successional=SapFireData[[15]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Sap_Fire_BA_Regression[[15]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SapFireData[[15]]$ScYear,"scaled:center")),
    
    Oak_Hickory=SapFireData[[16]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Sap_Fire_BA_Regression[[16]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SapFireData[[16]]$ScYear,"scaled:center")),
    
    Mesic=SapFireData[[17]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Sap_Fire_BA_Regression[[17]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SapFireData[[17]]$ScYear,"scaled:center")),
    
    Non_Canopy=SapFireData[[18]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Sap_Fire_BA_Regression[[18]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SapFireData[[18]]$ScYear,"scaled:center"))
  ), .id="Group"
)  


## FIX this when thing rerun
Model_Graph_Data<-Model_Graph_Data %>% mutate(Density=(.value*1000)/(27*pi))

#write.csv(Model_Graph_Data, "Sap_BA_Group_Trend.csv")

ggplot(Model_Graph_Data, aes(x=Year, y=Count, color=Burned, fill=Burned)) + 
  stat_lineribbon(aes(y=Density), .width=c(.85,.95), alpha=0.25) +
  labs(x='Year', y="Basal Area m2/ha") +
  #scale_y_continuous(l breaks = c(67.8584, 135.7168)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_wrap(~Group, scales="free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())


Species_Graph_Data<-Sap_Fire_Combined_Summary %>% filter(!Species %in% c("All","Early_Successional", "Mesic", "Non_Canopy","Oak_Hickory")) %>% 
  select(Species,Year_Per_Change, Year_Per_Change_low85,Year_Per_Change_upper85, Year_Per_Change_low95,Year_Per_Change_upper95 ) %>% 
  mutate(Species=factor(Species, levels=rev(c("Liquidambar styraciflua", "Carya glabra", "Quercus alba", "Quercus coccinea","Quercus falcata", 
                                              "Quercus velutina", "Acer rubrum", "Fagus grandifolia", "Ilex opaca", "Nyssa sylvatica",
                                              "Carpinus caroliniana","Cornus florida",   "Sassafras albidum" )), ordered = T))

write.csv(Species_Graph_Data, "Sap_BA_Species_Graph_Data.csv", row.names = F)

ggplot(Species_Graph_Data, aes(y=Species, x=Year_Per_Change)) +
  geom_pointrange(aes(xmin=Year_Per_Change_low85, xmax=Year_Per_Change_upper85)) +
  geom_vline(aes(xintercept=1))+
  geom_pointrange(aes(xmin=Year_Per_Change_low95, xmax=Year_Per_Change_upper95),linetype="dotted") +
  geom_hline(aes(yintercept=3.5), linetype="dashed")+
  geom_hline(aes(yintercept=8.5), linetype="dashed")+
  geom_hline(aes(yintercept=17.5), linetype="dashed")+
  labs(y=element_blank(), x="% Change/year")+
  scale_x_continuous(breaks=c(.95,1,1.05,1.1),labels = function(x) (x-1)*100 )+
  coord_cartesian(xlim=c(.86, 1.18), clip="off")+
  annotate("text",x=.69, y=2, label="Non-Canopy", angle=90)+
  annotate("text",x=.69, y=6.5, label="Mesic", angle=90)+
  annotate("text",x=.69, y=13, label="Oak-Hickory", angle=90)+
  annotate("text",x=.69, y=19.2, label="Early \n Successional", angle=90)+
  theme_classic()+
  theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"))


Species_Fire_Graph_Data<-Sap_Fire_Combined_Summary %>% 
  filter(Species %in% c("Acer rubrum","Cornus florida", "Nyssa sylvatica", "Quercus alba", "Quercus coccinea" )) %>% 
  select(Species,Fire_Year_Per_Change, Fire_Year_Per_Change_low85,Fire_Year_Per_Change_upper85, 
         Fire_Year_Per_Change_low95, Fire_Year_Per_Change_upper95 ) %>% 
  mutate(Species=factor(Species, levels=rev(c( "Acer rubrum","Cornus florida", "Nyssa sylvatica", "Quercus alba", "Quercus coccinea"  )), ordered = T))

write.csv(Species_Fire_Graph_Data, "Sap_Fire_BA_Species_Graph_Data.csv", row.names = F)

