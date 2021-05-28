library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)

#Need to source function file


NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

TreeFirePlots<-FirePlots %>% filter(Fire_Name %in% c("B-Loop")) #At first used Tracer, but then removed


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

# Need to decide which tree species to use

# Will cut of on average of 0.1 trees per plot per visit

TreeAv<-c(TreeSp,TreeGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "trees", years=2006:2017,species=.x) %>% nrow )/(3*145))


TreeUse<-c(TreeSp, TreeGroups)[TreeAv>0.1]

TreeData<-map(.x=TreeUse, ~makeRegData(object=NCRN[[9]], group="trees",species=., years=2006:2017))

TreeFireData<- map(.x=TreeData, ~left_join(x=.,y=TreeFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))


#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
options(mc.cores=4) #loo may need this to be specified here as the cores argument does not seem to work in loo
rstan::rstan_options(auto_write = TRUE)

Tree_Fire_Poisson_Regression<-map(.x=TreeFireData, ~fitFirePoisson(, iter=25000, object=.x, cores=4,
                                      control=list(adapt_delta=0.99, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

Tree_Fire_Poisson_Summary<-SummarizeFireFit(model=Tree_Fire_Poisson_Regression, type="Poisson", names=TreeUse, data=TreeFireData)
Tree_Fire_Poisson_Fitted<-SummarizeFireOutcome(model=Tree_Fire_Poisson_Regression, names=TreeUse, data=TreeFireData, form = "tree")


#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)

Tree_Fire_NB_Regression<-map(.x=TreeFireData, ~fitFireNB(, iter=25000, object=.x, cores=4,
                                                         control=list(adapt_delta=0.99, max_treedepth=20), save_pars=save_pars(all=TRUE) ))

Tree_Fire_NB_Summary<-SummarizeFireFit(model=Tree_Fire_NB_Regression, type="NB", names=TreeUse, data=TreeFireData)
Tree_Fire_NB_Fitted<-SummarizeFireOutcome(model=Tree_Fire_NB_Regression, names=TreeUse, data=TreeFireData, form="tree")


Tree_Fire_Poisson_Loo<-map(Tree_Fire_Poisson_Regression, loo, reloo=T, moment_match=TRUE)

Tree_Fire_NB_Loo<-map(Tree_Fire_NB_Regression, loo, reloo=T, moment_match=TRUE)

Tree_Fire_Loo_Compare<-map2(.x=Tree_Fire_Poisson_Loo, .y=Tree_Fire_NB_Loo, ~brms::loo_compare(list(Poisson=.x, NB=.y)))
## Poisson always wins

Poisson_Wins<-map_lgl(Tree_Fire_Loo_Compare, ~rownames(.)[1]=="Poisson")

Tree_Fire_Combined_Summary<-rbind(Tree_Fire_Poisson_Summary[Poisson_Wins,], Tree_Fire_NB_Summary[!Poisson_Wins,])
Tree_Fire_Combined_Fitted<-rbind(Tree_Fire_Poisson_Fitted[Poisson_Wins,], Tree_Fire_NB_Fitted[!Poisson_Wins,])


 Tree_Fire_Final_Summary<-Tree_Fire_Combined_Summary
 #%>% 
#   select(Species, TotalCount, TotalPlots, Intercept, FireIntercept, Fire_Int_Per_Change,  Prob_Fire_Int_Increasing, Prob_Fire_Int_Decreasing, 
#          YearSlope, Year_Per_Change, Year_Change_low95, Year_Change_upper95, Prob_Decreasing, Prob_Increasing, 
#          FireSlope, Year_Fire_Change, Year_Fire_Change_low95, Year_Fire_Change_upper95, Prob_Fire_Decreasing, Prob_Fire_Increasing, 
#          Fire_Decreased_Year1, Fire_Increased_Year1, Model)

write.csv(Tree_Fire_Final_Summary, "Fire_Tree_Abundance.csv")
write.csv(Tree_Fire_Combined_Fitted, "Tree_Fire_Abundance_Fitted.csv")


Model_Graph_Data<-bind_rows(
  list(
    All_Trees=TreeFireData[[22]] %>%
      group_by(factor(Fire)) %>%
      data_grid(ScYear, Fire) %>%
      add_fitted_draws(Tree_Fire_NB_Regression[[22]], re_formula = NA) %>%
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[22]]$ScYear,"scaled:center")),

    Early_Successional=TreeFireData[[23]] %>%
      group_by(factor(Fire)) %>%
      data_grid(ScYear, Fire) %>%
      add_fitted_draws(Tree_Fire_Poisson_Regression[[23]], re_formula = NA) %>%
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[23]]$ScYear,"scaled:center")),

    Oak_Hickory=TreeFireData[[24]] %>%
      group_by(factor(Fire)) %>%
      data_grid(ScYear, Fire) %>%
      add_fitted_draws(Tree_Fire_NB_Regression[[24]], re_formula = NA) %>%
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[24]]$ScYear,"scaled:center")),

    Mesic=TreeFireData[[25]] %>%
      group_by(factor(Fire)) %>%
      data_grid(ScYear, Fire) %>%
      add_fitted_draws(Tree_Fire_Poisson_Regression[[25]], re_formula = NA) %>%
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[25]]$ScYear,"scaled:center")),

    Non_Canopy=TreeFireData[[26]] %>%
      group_by(factor(Fire)) %>%
      data_grid(ScYear, Fire) %>%
      add_fitted_draws(Tree_Fire_NB_Regression[[26]], re_formula = NA) %>%
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[26]]$ScYear,"scaled:center"))
  ), .id="Group"
)

# Model_Graph_Data<-bind_rows(
#   list(
#     All_Trees=TreeFireData[[22]] %>%  
#       data_grid(Plot_Name,ScYear) %>% mutate(Fire=ifelse(Plot_Name %in% TreeFirePlots$Plot_Name,1 ,0)) %>%
#       add_fitted_draws(Tree_Fire_NB_Regression[[22]], re_formula = NuLL, n=100) %>% 
#       mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[22]]$ScYear,"scaled:center")),
#     
#     Early_Successional=TreeFireData[[23]] %>%  
#       data_grid(Plot_Name,ScYear) %>% mutate(Fire=ifelse(Plot_Name %in% TreeFirePlots$Plot_Name,1 ,0)) %>%
#       add_fitted_draws(Tree_Fire_NB_Regression[[23]], re_formula = NULL, n=100) %>% 
#       mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[23]]$ScYear,"scaled:center")),
#      
#      Oak_Hickory=TreeFireData[[24]] %>%  
#       data_grid(Plot_Name,ScYear) %>% mutate(Fire=ifelse(Plot_Name %in% TreeFirePlots$Plot_Name,1 ,0)) %>%
#       add_fitted_draws(Tree_Fire_NB_Regression[[24]], re_formula = NULL, n=100) %>% 
#       mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[24]]$ScYear,"scaled:center")),
#     
#      Mesic=TreeFireData[[25]] %>%  
#       data_grid(Plot_Name,ScYear) %>% mutate(Fire=ifelse(Plot_Name %in% TreeFirePlots$Plot_Name,1 ,0)) %>%
#        add_fitted_draws(Tree_Fire_Poisson_Regression[[25]], re_formula = NULL, n= 100) %>% 
#        mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[25]]$ScYear,"scaled:center")),
#     
#      Non_Canopy=TreeFireData[[26]] %>%  
#     data_grid(Plot_Name,ScYear) %>% mutate(Fire=ifelse(Plot_Name %in% TreeFirePlots$Plot_Name,1 ,0)) %>%
#       add_fitted_draws(Tree_Fire_NB_Regression[[26]], re_formula = NULL, n=100) %>% 
#       mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(TreeFireData[[26]]$ScYear,"scaled:center"))
#   ), .id="Group"
# )  

Model_Graph_Data<-Model_Graph_Data %>% mutate(Density=(.value*10000)/(225*pi))




ggplot(Model_Graph_Data, aes(x=Year, y=Count, color=Burned, fill=Burned)) + 
  stat_lineribbon(aes(y=Density), .width=c(.85,.95), alpha=0.25) +
  labs(x='Year', y="Trees/ha") +
  #scale_y_continuous(l breaks = c(67.8584, 135.7168)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  facet_wrap(~Group, scales="free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())

Species_Graph_Data<-Tree_Fire_Combined_Summary %>% filter(!Species %in% c("All","Early_Successional", "Mesic", "Non_Canopy","Oak_Hickory")) %>% 
  select(Species,Year_Per_Change, Year_Per_Change_low85,Year_Per_Change_upper85, Year_Per_Change_low95,Year_Per_Change_upper95 ) %>% 
  mutate(Species=factor(Species, levels=rev(c("Juniperus virginiana", "Liquidambar styraciflua", "Liriodendron tulipifera", 'Pinus virginiana',
          "Carya alba", "Carya glabra", "Carya ovalis", "Quercus alba", "Quercus coccinea","Quercus falcata", "Quercus prinus", "Quercus rubra", 
          "Quercus velutina", "Acer rubrum", "Fagus grandifolia", "Fraxinus americana","Ilex opaca", "Nyssa sylvatica", "Carpinus caroliniana",
           "Cornus florida",   "Sassafras albidum" )), ordered = T))

write.csv(Species_Graph_Data, "Tree_Abundance_Species_Graph_Data.csv", row.names = F)

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


Species_Fire_Graph_Data<-Tree_Fire_Combined_Summary %>% 
  filter(Species %in% c("Liriodendron tulipifera", 'Pinus virginiana', "Quercus alba", "Quercus coccinea",  "Acer rubrum", "Fagus grandifolia",
                        "Nyssa sylvatica")) %>% 
  select(Species,Fire_Year_Per_Change, Fire_Year_Per_Change_low85,Fire_Year_Per_Change_upper85, 
         Fire_Year_Per_Change_low95, Fire_Year_Per_Change_upper95 ) %>% 
  mutate(Species=factor(Species, levels=rev(c("Liriodendron tulipifera", 'Pinus virginiana', "Acer rubrum", "Fagus grandifolia",
                                               "Nyssa sylvatica", "Quercus alba", "Quercus coccinea"  )), ordered = T))

write.csv(Species_Fire_Graph_Data, "Tree_Fire_Abundance_Species_Graph_Data.csv", row.names = F)

