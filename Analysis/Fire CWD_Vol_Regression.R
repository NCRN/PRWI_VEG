library(NPSForVeg)
library(brms)
library(tidyverse)
library(tidybayes)
library(modelr)

#Need to source function file


NCRN<-importNCRN("C:/Data/Veg_NCRN")
FirePlots<-read.csv("Fire_Plots.csv")

TreeFirePlots<-FirePlots %>% filter(Fire_Name == "B-Loop")

CWDEvents<-getEvents(NCRN[[9]], years=2006:2017) %>% select(Plot_Name, Sample_Year=Event_Year)


CWDVolData<-getPlants(NCRN[[9]], group="cwd", years=2006:2017) %>% 
  mutate(D2=Diameter^2) %>% 
  group_by(Plot_Name, Sample_Year) %>% summarise(Count=n(),Vol=0.02742*(sum(D2)))%>% 
  ungroup %>% 
  right_join( CWDEvents) %>% 
  mutate(Vol= ifelse(is.na(Vol), 0, Vol),   Count=ifelse(is.na(Count), 0L, Count)) %>% 
  mutate(YrFactor=factor(Sample_Year), ScYear=scale(Sample_Year, scale = F))%>% 
  rename(Year=Sample_Year, BA=Vol)
#BA is volume but I am calling it BA so the other functions work.

CWDFireVolData<- CWDVolData %>% left_join(TreeFirePlots) %>% mutate(Fire=ifelse( is.na(Fire_Year), 0, 1) )

options(mc.cores=4) #loo may need this to be specified here as the cores argument does not seem to work in loo
rstan::rstan_options(auto_write = TRUE)


CWD_Fire_Vol_Regression<-fitFireHurdGamma(iter=25000, object=CWDFireVolData, cores=4,
                                 control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) )


CWD_Fire_Vol_Summary<-SummarizeFireFit(model=list(CWD_Fire_Vol_Regression), type="HurdGamma", names=c("All"="All"), data=list(CWDFireVolData))
CWD_Fire_Vol_Fitted<-SummarizeFireOutcome(model=list(CWD_Fire_Vol_Regression), names=c("All"="All"), data=list(CWDFireVolData), form = "cwd")



CWD_Fire_Vol_Regression2<- fitFireHurdGamma2(iter=25000, object=list(CWDFireVolData), cores=4,
                                             control=list(adapt_delta=0.995, max_treedepth=20), save_pars=save_pars(all=TRUE) )


CWD_Fire_Vol_Summary2<-SummarizeFireFit(model=list(CWD_Fire_Vol_Regression2), type="HurdGamma2", names=c("All"="All"), 
                                        data=list(CWDFireVolData))

CWD_Fire_Vol_Summary3<-SummarizeFireHurdleFit(model=list(CWD_Fire_Vol_Regression2), type="HurdGamma2", names=c("All"="All"), 
                                              data=list(CWDFireVolData))

CWD_Fire_Vol_Fitted2<-SummarizeFireOutcome(model=list(CWD_Fire_Vol_Regression2), names=c("All"="All"), 
                                           data=lst(CWDFireVolData), form="cwd")

CWD_Fire_Vol_Loo<-loo(CWD_Fire_Vol_Regression, reloo=T, cores=4, moment_match = TRUE)

CWD_Fire_Vol_Loo2<-loo(CWD_Fire_Vol_Regression2, reloo=T, cores=4, moment_match = TRUE)

CWD_Fire_Vol_Compare<-map2(.x=list(CWD_Fire_Vol_Loo), .y=list(CWD_Fire_Vol_Loo2), ~brms::loo_compare(list(Hurd_Gamma=.x, Hurd_Gamma_2=.y)))


Hurd_Gamma2_Wins<-map_lgl(CWD_Fire_Vol_Compare, ~rownames(.)[1]=="Hurd_Gamma_2")


CWD_Fire_Vol_Combined_Summary<-left_join(CWD_Fire_Vol_Summary2, CWD_Fire_Vol_Summary3[Hurd_Gamma2_Wins,])

CWD_Fire_Combined_Fitted<-CWD_Fire_Vol_Fitted #hurd gamma was better


 CWD_Fire_Vol_Final_Summary<-CWD_Fire_Vol_Combined_Summary #%>% 
#   select(Species, TotalCount, TotalPlots, Intercept, FireIntercept, Fire_Int_Per_Change,  Prob_Fire_Int_Increasing, Prob_Fire_Int_Decreasing, 
#        YearSlope, Year_Per_Change, Year_Change_low95, Year_Change_upper95, Prob_Decreasing, Prob_Increasing, 
#        FireSlope, Year_Fire_Change, Year_Fire_Change_low95, Year_Fire_Change_upper95, Prob_Fire_Decreasing, Prob_Fire_Increasing, 
#        Model)

write.csv(CWD_Fire_Vol_Final_Summary, "Fire_CWD_Vol.csv")

write.csv(CWD_Fire_Combined_Fitted, "CWD_Fire_Vol_Fitted.csv")

# 
# Tree_Final_Summary<-Tree_Poisson_Summary %>% select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing)
# write.csv(Tree_Final_Summary, "Tree_Abundance.csv")

Model_Graph_Data<-bind_rows(
  list(
    All_Trees=CWDFireVolData %>%  
      group_by(factor(Fire)) %>% 
      data_grid(ScYear, Fire) %>% 
      add_fitted_draws(CWD_Fire_Vol_Regression, re_formula = NA) %>% 
      mutate(Burned=ifelse(Fire==0, "Unburned", "Burned"), Year=ScYear+attr(CWDFireVolData$ScYear,"scaled:center"))
    
  ), .id="Group"
)  


## FIX this when thing rerun
Model_Graph_Data<-Model_Graph_Data %>% mutate(Density=.value)

Model_Graph_Data_Sum<-Model_Graph_Data %>% group_by(Year, Burned) %>% 
  summarise(Density=median(Density))


ggplot(Model_Graph_Data, aes(x=Year, y=Density, color=Burned, fill=Burned, linetype=Burned)) + 
  stat_lineribbon( .width=c(.85,.95), alpha=0.25) +
  geom_line(data=Model_Graph_Data_Sum)+
  labs(x='Year', y="CWD volume m3/ha") +
  #scale_y_continuous(l breaks = c(67.8584, 135.7168)) +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
 # facet_wrap(~Group, scales="free") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())



