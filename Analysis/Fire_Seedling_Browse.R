library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)
library(distributional)
library(boot) #for inv.logit
library(ggstance)
#Need to source function file


NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

SeedFirePlots<-FirePlots %>% filter(Fire_Name=="B-Loop") 

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



makeBrowseData<-function(object, group, years=NA, plots=NA, ...){
  
  RegData<-getPlants(object=object, group=group, years=years, plots=plots, ...) %>% 

    mutate(YrFactor=factor(Sample_Year), ScYear=scale(Sample_Year, scale = F),
           Browse=ifelse(Browsed=="Yes",1,0)) %>% 
    
    rename(Year=Sample_Year)
  
  return(RegData)
}




SeedData<-map(.x=SeedUse, ~makeBrowseData(object=NCRN[[9]], group="seedlings",species=., years=2014:2017))

SeedFireData<- map(.x=SeedData, ~left_join(x=.,y=SeedFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))


options(mc.cores=4) #loo may need this to be specified here as the cores argument does not seem to work in loo
rstan::rstan_options(auto_write = TRUE)

fitFireBrowse<-function(object,...){
    
    RegFormula<-formula("Browse ~ Fire")
    
    brm(RegFormula, data=object, family=bernoulli(link="logit"), ... )
  }
  
  
  


Seed_Fire_Browse_Regression<-map(.x=SeedFireData, ~fitFireBrowse(, iter=25000, 
                                                                 object=.x, cores=4,
                                              control=list(adapt_delta=0.99, max_treedepth = 15), #save_pars=save_pars(all=TRUE)
                                              save_pars=save_pars(all=TRUE) ))


SummarizeBrowseFit<-function(model, names, data) {
  
  OutTable<-data.frame(Species=names(names))
  
  OutTable <- OutTable %>% mutate(
    Total_Plots=map_int(data, ~ n_distinct(.x$Plot_Name)),
    Total_Count= map_int(data, ~nrow(.x)) , 
    Total_Browse=map_dbl(data, ~sum(.x$Browse)),
    
    
    Fire_Plots=map(data, ~ filter(.x,Fire==1)) %>% map_int(., ~n_distinct(.$Plot_Name)),
    Fire_Count=map(data, ~ filter(.x,Fire==1)) %>% map_int(., ~nrow(.)), 
    Fire_Browse=map(data, ~ filter(.x,Fire==1)) %>% map_dbl(., ~sum(.$Browse)),
    
    Intercept=map_dbl(model, ~fixef(.x,robust=T)[1,1] %>% round(3)),
    InterceptError=map_dbl(model, ~fixef(.x,robust=T)[1,2] %>% round(3)),
    Int_low_95=map_dbl(model, ~fixef(.x, robust=T)[1,3] %>% round(3)),
    Int_up_95= map_dbl(model, ~fixef(.x, robust=T)[1,4] %>% round(3)),
    
    Fire=map_dbl(model, ~fixef(.x, robust=T)[2,1] %>% round(3)),
    FireError=map_dbl(model, ~fixef(.x,robust=T)[2,2] %>% round(3)),
    Fire_low_95= map_dbl(model, ~fixef(.x, robust=T)[2,3] %>% round(3)),
    Fire_up_95=map_dbl(model, ~fixef(.x, robust=T)[2,4] %>% round(3)),
    
    IntProb=inv.logit(Intercept) %>% round(3),
    Fire_Prob=inv.logit(Intercept+Fire) %>% round(3),
   
    Prob_Fire_Decreasing=map_dbl(model, ~hypothesis(.x, 'Fire<0')[[1]]$Post.Prob %>% round(3)),
    Prob_Fire_Increasing=map_dbl(model, ~hypothesis(.x, 'Fire>0')[[1]]$Post.Prob %>% round(3)) 
   )
  
}


SummarizeBrowseOutcome<-function(model, names, data){
   
  OutTable<-data.frame(Species=names(names))
    
  NoFireData<-map(.x=data, ~filter(.x, .x$Fire==0))

    
  Fitted_NoFire_Data<-map2(model,NoFireData, ~fitted(.x, newdata=.y, re_formula=NULL, scale="response", robust=T)) %>% map(~as.data.frame(.))

  
  
    OutTable <- OutTable %>% mutate(
      NoFire_Mean=Fitted_NoFire_Data %>% map_dbl( ~mean(.$Estimate) %>% round(3)),
      NoFire_Low=Fitted_NoFire_Data %>%  map_dbl(~mean(.$Q2.5) %>%  round(3)),
      NoFire_High=Fitted_NoFire_Data %>%  map_dbl(~mean(.$Q97.5) %>%  round(3))#,      

    )
    FireData<-map(.x=data, ~filter(.x, .x$Fire==1))
   
    FireData<-FireData %>% discard(~nrow(.)==0)

    FireModel<-model[names(names) %in% names(FireData)]

    Fitted_Fire_Data<-map2(FireModel,FireData, ~fitted(.x, newdata=.y, re_formula=NULL, scale="response", robust=T)) %>% map(~as.data.frame(.))
    
    
    OutTable2<-data.frame(Species=names(FireData))
    
    
    OutTable2 <- OutTable2 %>% mutate(
      Fire_Mean=Fitted_Fire_Data %>% map_dbl( ~mean(.$Estimate) %>% round(3)),
      Fire_Low=Fitted_Fire_Data %>%  map_dbl(~mean(.$Q2.5) %>%  round(3)),
      Fire_High=Fitted_Fire_Data %>%  map_dbl(~mean(.$Q97.5) %>%  round(3))#,      
      
    )

    OutTable<-left_join(OutTable,OutTable2)
    
    return(OutTable)
  }
  
  
  
  


Shrub_Seed_Fire_Browse_Summary<-SummarizeBrowseFit(model=ShSeed_Fire_Browse_Regression, names=ShSeedUse, data=ShSeedFireData)

Shrub_Seed_Fire_Browse_Fitted<-SummarizeBrowseOutcome(model=ShSeed_Fire_Browse_Regression, names=ShSeedUse, data=ShSeedFireData)

#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)



write.csv(Seed_Fire_Browse_Summary, "Seed_Fire_Browse.csv")
write.csv(Seed_Fire_Browse_Fitted, "Seed_Fire_Browse_Fitted.csv")


# Graph Data
 Seed_Fire_Browse_GraphData<-Seed_Fire_Browse_Fitted %>% 
   filter(!Species %in% c("All","Early_Successional","Oak_Hickory","Mesic","Non_Canopy")) %>% 
   pivot_longer(cols=-Species, names_to=c("Type",".value"), names_sep="_") %>% 
   filter(Type=="NoFire" | (Type=="Fire" & Species %in% c("Pinus virginiana","Acer rubrum","Ilex opaca","Carpinus caroliniana",
                                         "Quercus alba", "Quercus coccinea","Quercus falcata"))) %>% 
    mutate(Species=factor(Species, levels=rev(c(#"Early_Successional",
      "Liquidambar styraciflua",  'Pinus virginiana', #"Mesic",
      "Acer rubrum", "Fagus grandifolia", "Ilex opaca", "Nyssa sylvatica", #"Oak_Hickory",
      "Carya alba", "Carya glabra",  "Quercus alba", "Quercus coccinea","Quercus falcata", "Quercus prinus", "Quercus velutina",# "Non_Canopy", 
      "Amelanchier arborea","Asimina triloba", "Carpinus caroliniana",  "Sassafras albidum" #,"All"
      )),
      
     labels=rev(c(# "Early_Successional",
       "Liquidambar styraciflua",  'Pinus virginiana', #"Mesic**",
       "Acer rubrum*", "Fagus grandifolia", "Ilex opaca**", "Nyssa sylvatica", #"Oak_Hickory**",
       "Carya alba", "Carya glabra",  "Quercus alba**", "Quercus coccinea*","Quercus falcata*",   "Quercus prinus", "Quercus velutina",# "Non_Canopy",
       "Amelanchier arborea","Asimina triloba", "Carpinus caroliniana",  "Sassafras albidum" #,"All**" 
       )), 
     
     
     ordered = T))
 
Seed_Fire_Browse_GraphData<-Seed_Fire_Browse_GraphData %>% mutate(Common=recode(Species,
            "Liquidambar styraciflua"= "Sweetgum",  'Pinus virginiana'='Virginia pine',
            "Acer rubrum*"="Red maple**", "Fagus grandifolia" ="American beech",
            "Ilex opaca**"="American holly**", "Nyssa sylvatica" =  "Black gum",
            "Carya alba"="Mockernut hickory", "Carya glabra"="Pignut hickory",
            "Quercus alba**"= "White oak**", "Quercus coccinea*"="Scarlet oak*",
            "Quercus falcata*"="Southern red oak*", "Quercus prinus"="Chestnut oak", 
            "Quercus velutina" ="Black oak",# "Non_Canopy", 
            "Amelanchier arborea"="Common serviceberry","Asimina triloba"="Pawpaw",
            "Carpinus caroliniana"="American hornbeam",  "Sassafras albidum"="Sassafras")) 
#   levels=rev(c(#"Early_Successional",
#   "Sweetgum",  'Virginia pine', #"Mesic",
#   "Red maple","American beech", "American holly", "Black gum", #"Oak_Hickory",
#   "Mockernut hickory", "Pignut hickory",  "White oak", "Scarlet oak","Southern red oak", "Chestnut oak", "Black oak",# "Non_Canopy", 
#   "Common serviceberry","Pawpaw", "American hornbeam",  "Sassafras" #,"All"
# )),

# labels=rev(c(# "Early_Successional",
#   "Sweetgum",  'Virginia pine', #"Mesic**",
#   "American beech", "American holly**", "Black gum",  "Red maple*", #"Oak_Hickory**",
#   "Black oak", "Chestnut oak", "Mockernut hickory", "Pignut hickory", "Scarlet oak*","Southern red oak*", "White oak**", # "Non_Canopy",
#   "American hornbeam", "Common serviceberry","Pawpaw","Sassafras" #,"All**" 
# )),      ordered = T))

Seed_Fire_Browse_GraphData<- Seed_Fire_Browse_GraphData %>% mutate(Common=factor(Common, levels=rev(c("Sweetgum",  'Virginia pine', 
   "American beech", "American holly**", "Black gum",  "Red maple**", 
   "American hornbeam", "Common serviceberry","Pawpaw","Sassafras",
    "Black oak", "Chestnut oak", "Mockernut hickory", "Pignut hickory", "Scarlet oak*","Southern red oak*", "White oak**"
     )), ordered = T))


Seedling_Browse_Plot<-
  ggplot(Seed_Fire_Browse_GraphData , aes(x=Mean,y= Common, group=Type, color=Type, shape=Type)) +
  geom_pointrangeh(aes(xmin=Low, xmax=High), position=position_dodge(width=.5)) +
  scale_color_manual(values=c("NoFire"="#00BFC4", "Fire"="#F8766D"),  labels=c("NoFire"="Unburned", "Fire"="Burned")) +
  scale_shape_manual(values=c("NoFire"=19, "Fire"=17),  labels=c("NoFire"="Unburned", "Fire"="Burned")) +
  labs(y=element_blank(), x="% Browsed") +
  geom_hline(aes(yintercept=7.5), linetype="dashed") +
  geom_hline(aes(yintercept=11.5), linetype="dashed") +
  geom_hline(aes(yintercept=15.5), linetype="dashed") +
  coord_cartesian(xlim=c(0, 1), clip="off") +
  annotate("text",x=-0.45, y=4, label="Oak Hickory", angle=90) +
  annotate("text",x= -0.45, y=9.5, label="Non-Canopy", angle=90) +
  annotate("text",x= -0.45, y=13.5, label="Mesic", angle=90) +
  annotate("text",x= -0.45, y=16.5, label="Early \n Successional", angle=90) +
  scale_x_continuous(labels=function(x)x*100) +
  theme_classic() +
  theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"), legend.position = "bottom")

ggsave(filename = "Seedling Browse.tiff", plot=Seedling_Browse_Plot, device="tiff", width=6 , height=8, units="in", dpi=300)


  
  
