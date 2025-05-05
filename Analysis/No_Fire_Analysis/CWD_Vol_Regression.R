library(NPSForVeg)
library(brms)
library(tidyverse)

#Need to source function file


NCRN<-importNCRN("C:/Users/jschmit/Desktop/Some data/Veg_NCRN")


CWDVolData<-getPlants(NCRN[[9]], group="cwd", years=2006:2017) %>% 
  mutate(D2=Diameter^2) %>% 
  group_by(Plot_Name, Sample_Year) %>% summarise(Count=n(),Vol=0.02742*(sum(D2))) %>% 
  ungroup %>% 
  mutate(YrFactor=factor(Sample_Year), ScYear=scale(Sample_Year, scale = F))%>% 
  rename(Year=Sample_Year, BA=Vol)
#BA is volume but I am calling it BA so the other functions work.

CWDEvents<-getEvents(NCRN[[9]], years=2006:2017) %>% select(Plot_Name, Year=Event_Year)

CWDVolData<-right_join(CWDVolData, CWDEvents) %>% mutate(BA= ifelse(is.na(BA), 0, BA), Count=ifelse(is.na(Count), 0L, Count))

#options(mc.cores = parallel::detectCores())  #commented out as I will just use 4, one for each chain
rstan::rstan_options(auto_write = TRUE)

CWD_Vol_Regression<-fitHurdGamma(iter=25000, object=CWDVolData, cores=4,control=list(adapt_delta=0.99) )


CWD_Vol_Summary<-SummarizeFit(model=list(CWD_Vol_Regression), type="HurdGamma", names=c("All"="All"), data=list(CWDVolData))

CWD_Vol_Regression2<- fitHurdGamma2(iter=25000, object=CWDVolData, cores=4,control=list(adapt_delta=0.99) )

CWD_Vol_Summary2<-SummarizeFit(model=list(CWD_Vol_Regression2), type="HurdGamma2", names=c("All"="All") , data=list(CWDVolData))

CWD_Vol_Summary3<-SummarizeHurdleFit(model=list(CWD_Vol_Regression2), type="HurdGamma2", names=c("All"="All"), data=list(CWDVolData))


CWD_Vol_Loo<-loo(CWD_Vol_Regression, reloo=T, cores=4)

CWD_Vol_Loo2<-loo(CWD_Vol_Regression2, reloo=T, cores=4)

CWD_Vol_Compare<-map2(.x=list(CWD_Vol_Loo), .y=list(CWD_Vol_Loo2), ~brms::loo_compare(list(Hurd_Gamma=.x, Hurd_Gamma_2=.y)))


Hurd_Gamma2_Wins<-map_lgl(CWD_Vol_Compare, ~rownames(.)[1]=="Hurd_Gamma_2")


CWD_Vol_Combined_Summary<-left_join(CWD_Vol_Summary2, CWD_Vol_Summary3[Hurd_Gamma2_Wins,])

CWD_Vol_Final_Summary<-CWD_Vol_Combined_Summary %>% 
  select(Species, TotalCount, BA_Slope=YearSlope, BA_Error=YearError, BA_Prob_Decreasing=Prob_Decreasing, BA_Prob_Increasing=Prob_Increasing, 
         BA_Per_Change=Year_Per_Change, BA_L95=lower_95, BA_U95=upper_95, Occu_Slope, Occu_Error, Occu_L95, Occu_U95, Occu_Decreasing, Occu_Increasing, 
         Occu_Start, Occu_End)

write.csv(CWD_Vol_Final_Summary, "CWD_Vol.csv")


# 
# Tree_Final_Summary<-Tree_Poisson_Summary %>% select(Species, TotalCount, Year_Per_Change, Year_Change_low95, Year_Change_upper95,Prob_Decreasing, Prob_Increasing)
# write.csv(Tree_Final_Summary, "Tree_Abundance.csv")
