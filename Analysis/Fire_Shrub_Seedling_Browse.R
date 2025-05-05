library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)
library(distributional)

#Need to source function file
#source("Function file.R")

NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

ShrubFirePlots<-FirePlots %>% filter(Fire_Name == "B-Loop") 

#coda::HPDinterval(as.mcmc(fit, combine_chains = TRUE))
#fitPoisson(y, RandomYear = T, control=list(adapt_delta=0.99))


ShSeedSp<-getPlants(NCRN[[9]], "shseedlings", years=2006:2017) %>% pull(Latin_Name) %>% unique %>% sort
names(ShSeedSp)<-ShSeedSp


ShSeedGroups<-list(All=ShSeedSp[c(-1,-16,-20,-21,-23)],
                   Vaccinium=c("Vaccinium corymbosum","Vaccinium fuscatum", "Vaccinium spp."),#,"Vaccinium stamineum"),
                   Virburnum=c("Viburnum dentatum","Viburnum prunifolium"),#"Viburnum acerifolium","Viburnum dilatatum"
                   Aronia=c("Aronia arbutifolia","Photinia pyrifolia")
)


# Need to decide which tree species to use

# Will cut of on average of 0.1 trees per plot per visit

ShSeedAv<-c(ShSeedSp,ShSeedGroups) %>% map_dbl(~(getPlants(NCRN[[9]], "shseedlings", years=2006:2017,species=.x) %>% nrow )/(3*145))


ShSeedUse<-c(ShSeedSp, ShSeedGroups)[ShSeedAv>0.1]
ShSeedUse<-ShSeedUse[-c(4,5,6)]


makeBrowseData<-function(object, group, years=NA, plots=NA, ...){
  
  RegData<-getPlants(object=object, group=group, years=years, plots=plots, ...) %>% 
    
    mutate(YrFactor=factor(Sample_Year), ScYear=scale(Sample_Year, scale = F),
           Browse=ifelse(Browsed=="Yes",1,0)) %>% 
    
    rename(Year=Sample_Year)
  
  return(RegData)
}


ShSeedData<-map(.x=ShSeedUse, ~makeBrowseData(object=NCRN[[9]], group="shseedlings",species=., years=2006:2017))
ShSeedFireData<- map(.x=ShSeedData, ~left_join(x=.,y=ShrubFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) ))


rstan::rstan_options(auto_write = TRUE)
options(mc.cores=4)

fitFireBrowse<-function(object,...){
  
  RegFormula<-formula("Browse ~ Fire")
  
  brm(RegFormula, data=object, family=bernoulli(link="logit"), ... )
}



ShSeed_Fire_Browse_Regression<-map(.x=ShSeedFireData, ~fitFireBrowse(, iter=25000, 
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




ShSeed_Fire_Browse_Summary<-SummarizeBrowseFit(model=ShSeed_Fire_Browse_Regression, names=ShSeedUse, data=ShSeedFireData)

ShSeed_Fire_Browse_Fitted<-SummarizeBrowseOutcome(model=ShSeed_Fire_Browse_Regression, names=ShSeedUse, data=ShSeedFireData)

#pp_check(model)
##update(model, family=negbinomial())
#family = brmsfamily("com_poisson") #="Conway-Maxwell-Poisson" 

#prop_zero<-function(y) mean(y==0)

#pp_check(TreeRegession[[11]], type="stat", stat="prop_zero", binwidth=0.005)



write.csv(ShSeed_Fire_Browse_Summary, "Shrub_Seed_Fire_Browse.csv")
write.csv(ShSeed_Fire_Browse_Fitted, "Shrub_Seed_Fire_Browse_Fitted.csv")


#Unburned Plots
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


#Seedling_Browse_Plot<-
ggplot(Seed_Fire_Browse_GraphData , aes(x=Mean,y= Species, group=Type, color=Type)) +
  geom_pointrangeh(aes(xmin=Low, xmax=High), position=position_dodge(width=.5)) +
  scale_color_manual(values=c("NoFire"="#00BFC4", "Fire"="#F8766D"),  labels=c("NoFire"="Unburned", "Fire"="Burned")) +
  labs(y=element_blank(), x="% Browsed") +
  geom_hline(aes(yintercept=4.5), linetype="dashed") +
  geom_hline(aes(yintercept=11.5), linetype="dashed") +
  geom_hline(aes(yintercept=15.5), linetype="dashed") +
  coord_cartesian(xlim=c(0, 1), clip="off") +
  annotate("text",x= -0.5, y=2, label="Non-Canopy", angle=90) +
  annotate("text",x=-0.5, y=8, label="Oak-Hickory", angle=90) +
  annotate("text",x= -0.5, y=13.5, label="Mesic", angle=90) +
  annotate("text",x= -0.55, y=16.5, label="Early \n Successional", angle=90) +
  scale_x_continuous(labels=function(x)x*100) +
  theme_classic() +
  theme(plot.margin=unit(c(.5, .5, .5, 1.5), "cm"), legend.position = "bottom")



