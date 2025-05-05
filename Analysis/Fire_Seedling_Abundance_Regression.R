library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)
library(distributional)

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

SeedData<-map(.x=SeedUse, ~makeRegData(object=NCRN[[9]], group="seedlings",species=., years=2006:2017))

SeedFireData<- map(.x=SeedData, ~left_join(x=.,y=SeedFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))



#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
options(mc.cores=4) #loo may need this to be specified here as the cores argument does not seem to work in loo
rstan::rstan_options(auto_write = TRUE)

Seed_Fire_Poisson_Regression<-map(.x=SeedFireData, ~fitFirePoisson(, iter=25000, object=.x, cores=4,
                                              control=list(adapt_delta=0.99, max_treedepth = 15), #save_pars=save_pars(all=TRUE)
                                              save_pars=save_pars(all=TRUE) ))

Seed_Fire_Poisson_Summary<-SummarizeFireFit(model=Seed_Fire_Poisson_Regression, type="Poisson", names=SeedUse, data=SeedFireData)

Seed_Fire_Poisson_Fitted<-SummarizeFireOutcome(model=Seed_Fire_Poisson_Regression, names=SeedUse, data=SeedFireData, form="seedling")

#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

Seed_Fire_NB_Regression<-map(.x=SeedFireData, ~fitFireNB(, iter=25000, object=.x, cores=4,
                                                         control=list(adapt_delta=0.99,  max_treedepth = 15), save_pars=save_pars(all=TRUE) ))

Seed_Fire_NB_Summary<-SummarizeFireFit(model=Seed_Fire_NB_Regression, type="NB", names=SeedUse, data=SeedData)

Seed_Fire_NB_Fitted<-SummarizeFireOutcome(model=Seed_Fire_NB_Regression,  names = SeedUse, data=SeedFireData, form="seedling")

Seed_Fire_Poisson_Loo<-map(Seed_Fire_Poisson_Regression, loo, reloo=T, cores=4,moment_match=TRUE )

Seed_Fire_NB_Loo<-map(Seed_Fire_NB_Regression, loo, reloo=T, moment_match=TRUE)

Seed_Fire_Loo_Compare<-map2(.x=Seed_Fire_Poisson_Loo, .y=Seed_Fire_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))

#Get ones where Poisson is best
Poisson_Fire_Wins<-map_lgl(Seed_Fire_Loo_Compare, ~rownames(.)[1]=="Poisson")

Seed_Fire_Combined_Summary<-rbind(Seed_Fire_Poisson_Summary[Poisson_Fire_Wins,], Seed_Fire_NB_Summary[!Poisson_Fire_Wins,])
Seed_Fire_Combined_Fitted<-rbind(Seed_Fire_Poisson_Fitted[Poisson_Fire_Wins,], Seed_Fire_NB_Fitted[!Poisson_Fire_Wins,])

 Seed_Fire_Final_Summary<-Seed_Fire_Combined_Summary #%>% 
#   select(Species, TotalCount, TotalPlots, Intercept, FireIntercept, Fire_Int_Per_Change,  Prob_Fire_Int_Increasing, Prob_Fire_Int_Decreasing, 
#          YearSlope, Year_Per_Change, Year_Change_low95, Year_Change_upper95, Prob_Decreasing, Prob_Increasing, 
#          FireSlope, Year_Fire_Change, Year_Fire_Change_low95, Year_Fire_Change_upper95, Prob_Fire_Decreasing, Prob_Fire_Increasing, 
#          Fire_Decreased_Year1, Fire_Increased_Year1, Model)

write.csv(Seed_Fire_Final_Summary, "Seed_Fire_Abundance.csv")
write.csv(Seed_Fire_Combined_Fitted, "Seed_Fire_Abundance_Fitted.csv")



#### Seedling Fire Results Graph

# Seed_All_MCMC<-Seed_Fire_NB_Regression[[18]] %>%  spread_draws(b_Intercept, b_ScYear, b_Fire,`b_ScYear:Fire`)
# 
# SeedFireData[[18]]%>%  data_grid(ScYear, Fire) %>% add_predicted_draws(Seed_Fire_NB_Regression[[18]], re_formula = NA) %>% 
#   ggplot(aes(x=.prediction, y=factor(ScYear)))+stat_interval(.width=c(.85,.95))+ geom_point(aes(x=Count), data=SeedFireData[[18]])+
#   scale_color_brewer()
# 
# SeedFireData[[18]]%>%  data_grid(ScYear, Fire) %>% 
#   add_fitted_draws(Seed_Fire_NB_Regression[[18]],re_formula = NA, dpar=c("mu","shape"), scale="response") %>% 
#   sample_draws(30) %>% 
#   mutate(neg_bn_rate=shape/(shape+mu)) %>% #/(mu+neg_bn_r)) %>% 
#   ggplot(aes(y=factor(Fire)))+
#   stat_dist_slab(aes(dist="nbinom", arg1=1/shape, arg2=neg_bn_rate))
#                  

Seed_Fire_NB_Regression[[18]] %>%
  spread_draws(b_Intercept, r_Plot_Narme[Plot_Name,]) %>%
  median_qi(Plot_mean = b_Intercept + r_Plot_Name) %>%
  ggplot(aes(y = Plot_Name, x = Plot_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()



SeedFireData[[18]] %>%  
  group_by(factor(Fire)) %>% 
  data_grid(ScYear, Fire) %>% 
  add_fitted_draws(Seed_Fire_NB_Regression[[18]], re_formula = NA) %>% 
  mutate(Burned=ifelse(Fire==0, "Unburned", "Burned")) %>% 
  
  ggplot(aes(x=ScYear, y=Count, color=Burned, fill=Burned)) + 
  stat_lineribbon(aes(y=.value), .width=c(.85,.95), alpha=0.25)+
  labs(x='Year', y="Seedlings/ha")+
  scale_y_continuous(labels=function(x) round(x*10000/12), breaks = c(30, 60))+
  scale_x_continuous(labels=function(x) x+2012, breaks=c(-6:6))+
  theme_classic()+
  geom_count(data=SeedFireData[[18]] %>% mutate(Burned=ifelse(Fire==0, "Unburned", "Burned")))




# SeedFireData[[18]] %>%  group_by(factor(Fire)) %>% 
#   data_grid(ScYear, Fire) %>% 
#   add_fitted_draws(Seed_Fire_NB_Regression[[18]], re_formula = NA, n=500) %>% 
#   ggplot(aes(x=ScYear, y=Count, color=factor(Fire))) + 
#   geom_line(aes(y=.value, group=paste(Fire,.draw)),alpha=.1) +
#   scale_fill_brewer(palette = "Set2")

Model_Graph_Data<-bind_rows(
  list(
    All_Trees=SeedFireData[[18]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Seed_Fire_NB_Regression[[18]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SeedFireData[[18]]$ScYear,"scaled:center")),
  
    Early_Successional=SeedFireData[[19]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Seed_Fire_Poisson_Regression[[19]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SeedFireData[[18]]$ScYear,"scaled:center")),
    
    Oak_Hickory=SeedFireData[[20]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Seed_Fire_NB_Regression[[20]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SeedFireData[[18]]$ScYear,"scaled:center")),
    
    Mesic=SeedFireData[[21]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Seed_Fire_Poisson_Regression[[21]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SeedFireData[[18]]$ScYear,"scaled:center")),
    
    Non_Canopy=SeedFireData[[22]] %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(Seed_Fire_NB_Regression[[22]], re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(SeedFireData[[18]]$ScYear,"scaled:center"))
  ), .id="Group"
)  
  
  
Model_Graph_Data<-Model_Graph_Data %>% 
  mutate(Density=.value*10000/12, 
         Group=factor(Group, levels=c("All_Trees", "Early_Successional", "Mesic", "Non_Canopy", "Oak_Hickory"),
                      labels=c("All Trees", "Early Successional", "Mesic", "Non-Canopy", "Oak Hickory")))
  
Model_Graph_Data_Summary<-Model_Graph_Data %>% group_by(Year, Group,Burned) %>% 
  summarise(Density=median(Density))
  
  ggplot(Model_Graph_Data, aes(x=Year, y=Density, color=Burned, fill=Burned, 
                               linetype=Burned)) + 
  stat_lineribbon(.width=c(.85,.95), alpha=0.25) +
  geom_line(data=Model_Graph_Data_Summary)+
  labs(x='Year', y="Seedlings/ha") +
  #scale_y_continuous(breaks = c(30, 60)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_wrap(~Group, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())
  
# 
#  "Liquidambar styraciflua",'Pinus virginiana', "Carya alba", "Carya glabra", "Quercus alba", 
#              "Quercus coccinea","Quercus falcata", "Quercus prinus","Quercus velutina","Acer rubrum", 
#                        "Fagus grandifolia", "Ilex opaca", "Nyssa sylvatica","Amelanchier arborea", "Asimina triloba", 
#                        "Carpinus caroliniana","Sassafras albidum"


   
   Species_Graph_Data<-Seed_Fire_Combined_Summary %>% filter(!Species %in% c("All","Early_Successional", "Mesic", "Non_Canopy","Oak_Hickory")) %>% 
     select(Species,Year_Per_Change, Year_Per_Change_low85,Year_Per_Change_upper85, Year_Per_Change_low95,Year_Per_Change_upper95 ) %>% 
     mutate(Species=factor(Species, levels=rev(c(  "Liquidambar styraciflua",'Pinus virginiana', "Carya alba", "Carya glabra", "Quercus alba", 
                                      "Quercus coccinea","Quercus falcata", "Quercus prinus","Quercus velutina","Acer rubrum", 
                                      "Fagus grandifolia", "Ilex opaca", "Nyssa sylvatica","Amelanchier arborea", "Asimina triloba", 
                                      "Carpinus caroliniana","Sassafras albidum")), ordered = T))
   
   write.csv(Species_Graph_Data, "Seedlings_Abundance_Species_Graph_Data.csv", row.names = F)
   
   ggplot(Species_Graph_Data, aes(y=Species, x=Year_Per_Change)) +
     geom_pointrange(aes(xmin=Year_Per_Change_low85, xmax=Year_Per_Change_upper85)) +
     geom_vline(aes(xintercept=1))+
     geom_pointrange(aes(xmin=Year_Per_Change_low95, xmax=Year_Per_Change_upper95),linetype="dotted") +
     geom_hline(aes(yintercept=3.5), linetype="dashed")+
     geom_hline(aes(yintercept=7.5), linetype="dashed")+
     geom_hline(aes(yintercept=12.5), linetype="dashed")+
      labs(y=element_blank(), x="% Change/year")+
     scale_x_continuous(breaks=c(.8,.9,1,1.05),labels = function(x) (x-1)*100 )+
     coord_cartesian(xlim=c(.75, 1.06), clip="off")+
     annotate("text",x=.58, y=2, label="Non-Canopy", angle=90)+
     annotate("text",x=.58, y=5.5, label="Mesic", angle=90)+
     annotate("text",x=.58, y=10, label="Oak-Hickory", angle=90)+
     annotate("text",x=.58, y=12.7, label="Early \n Successional", angle=90)+
     theme_classic()+
     theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"))
   
   

   
   
   Species_Fire_Graph_Data<-Seed_Fire_Combined_Summary %>% 
     filter(Species %in% c('Pinus virginiana',  "Quercus alba", "Quercus coccinea", "Quercus falcata",
                           "Acer rubrum",  "Ilex opaca","Carpinus caroliniana")) %>% 
     select(Species,Fire_Year_Per_Change, Fire_Year_Per_Change_low85,Fire_Year_Per_Change_upper85, 
            Fire_Year_Per_Change_low95, Fire_Year_Per_Change_upper95 ) %>% 
     mutate(Species=factor(Species, levels=rev(c(  'Pinus virginiana',  "Quercus alba", "Quercus coccinea", "Quercus falcata",
                                                   "Acer rubrum",  "Ilex opaca","Carpinus caroliniana")), ordered = T))
   
   write.csv(Species_Fire_Graph_Data, "Seed_Fire_Abundance_Species_Graph_Data.csv", row.names = F)
  