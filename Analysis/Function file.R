

makeRegData<-function(object, group, years=NA, plots=NA, ...){

  
  #SampledYears<-if(anyNA(years)) getEvents(object,plots,years) %>% pull(Event_Year) %>% unique %>% sort() else years
  
  SampledYears_df<-getEvents(object, plots, years) %>% select(Plot_Name, Event_Year, Cycle)
  
  RegData<-SampledYears_df %>% 
    rowwise %>% mutate(Count=nrow(getPlants(object=object,group=group,years=Event_Year, plots=Plot_Name, ...)),
        BA=ifelse(group %in% c("trees","saplings"), sum(getPlants(object=object,group=group,years=Event_Year, plots=Plot_Name, ...)$SumLiveBasalArea_cm2/10000),NA),
        Height=ifelse(group %in% c("seedlings"), mean(getPlants(object=object,group=group,years=Event_Year, plots=Plot_Name, ...)$Height),NA)
        
      ) %>% 
    ungroup() %>% 
    mutate(YrFactor=factor(Event_Year), ScYear=scale(Event_Year, scale = F))%>% 
    rename(Year=Event_Year)
  
  return(RegData)
}


makeVineRegData<-function(object, group, years=NA, plots=NA, ...){
  
  
  #SampledYears<-if(anyNA(years)) getEvents(object,plots,years) %>% pull(Event_Year) %>% unique %>% sort() else years
  
  SampledYears_df<-getEvents(object, plots, years) %>% select(Plot_Name, Event_Year, Cycle)
  
  RegData<-SampledYears_df %>% 
    rowwise %>% mutate(Count=nrow(getPlants(object=object,group=group,years=Event_Year, plots=Plot_Name, ...) %>% 
                                    filter(Sample_Year<2015 | (Sample_Year>2015 & Tag_Status=="Tree" )))
        ) %>% 
    ungroup() %>% 
    mutate(YrFactor=factor(Event_Year), ScYear=scale(Event_Year, scale = F))%>% 
    rename(Year=Event_Year)
  
  return(RegData)
}

makeDeadRegData<-function(object, group, years=NA, plots=NA, ...){
  
  
  #SampledYears<-if(anyNA(years)) getEvents(object,plots,years) %>% pull(Event_Year) %>% unique %>% sort() else years
  
  SampledYears_df<-getEvents(object, plots, years) %>% select(Plot_Name, Event_Year, Cycle)
  
  RegData<-SampledYears_df %>% 
    rowwise %>% mutate(Count=nrow(getPlants(object=object,group=group,years=Event_Year, plots=Plot_Name, ...) %>% 
                                    filter(Status %in% c("Dead", "Dead Leaning", "Dead Standing"), Stems>0)),
                       BA=ifelse(group %in% c("trees","saplings"), 
                                 sum(getPlants(object=object,group=group,years=Event_Year, plots=Plot_Name, ...) %>% 
                                       filter(Status %in% c("Dead", "Dead Leaning", "Dead Standing"), Stems>0) %>% pull(SumLiveBasalArea_cm2)/1000),NA)
    ) %>% 
    ungroup() %>% 
    mutate(YrFactor=factor(Event_Year), ScYear=scale(Event_Year, scale = F))%>% 
    rename(Year=Event_Year)
  
  return(RegData)
}

fitPoisson<-function(object, RandomYear=F, ...){
  
  RegFormula<-formula(paste0("Count ~ ScYear + (1|Plot_Name)",if(RandomYear) " + (1|YrFactor)" else ""))
  
  brm(RegFormula, data=object, family=poisson(), ... )
}

fitPoissonVine<-function(object, ...){
  
  RegFormula<-formula(paste0("Count ~ ScYear + offset(LogTrees) + (1|Plot_Name)",if(RandomYear) " + (1|YrFactor)" else ""))
  
  brm(RegFormula, data=object, family=poisson(), ... )
}


fitNB<-function(object, RandomYear=F,...){
    
    RegFormula<-formula(paste0("Count ~ ScYear + (1|Plot_Name)",if(RandomYear) " + (1|YrFactor)" else ""))
    
    brm(RegFormula, data=object, family=negbinomial(), ... )
  }


fitNBVine<-function(object, RandomYear=F,...){
  
  RegFormula<-formula(paste0("Count ~ ScYear + offset(LogTrees) + (1|Plot_Name)",if(RandomYear) " + (1|YrFactor)" else ""))
  
  brm(RegFormula, data=object, family=negbinomial(), ... )
}



fitHurdGamma<-function(object, RandomYear=F, ...){
  
  RegFormula<-formula(paste0("BA ~ ScYear + (1|Plot_Name)",if(RandomYear) " + (1|YrFactor)" else ""))
  
  brm(RegFormula, data=object, family=hurdle_gamma(), ... )
}

fitHurdGamma2<-function(object, RandomYear=F, ...){
  
  #RegFormula<-formula(paste0("BA ~ ScYear + (1|Plot_Name)",if(RandomYear) " + (1|YrFactor)" else ""))
  
  brm(bf(BA~ScYear+(1|Plot_Name), hu~ScYear), data=object, family=hurdle_gamma(), ... )
}

fitHeight<-function(object, ...){
  
  RegFormula<-formula(paste0("Height ~ ScYear + (1|Plot_Name)"))
  
  brm(RegFormula, data=object, family=Gamma(link="log"), ... )
}

fitTruncHeight<-function(object,...){
  
  RegFormula<-formula(paste0("Height | trunc(lb=15) ~ ScYear + (1|Plot_Name)"))#("Height | trunc(lb=15) ~ ScYear + (1|Plot_Name)"
  
  brm(RegFormula, data=object, family=Gamma(link="log"), ... )
}


SummarizeFit<-function(model, type, names, data) {
  
  OutTable<-data.frame(Species=names(names))
  
  OutTable <- OutTable %>% mutate(TotalCount=if(type=="Gamma") map_dbl(data, ~nrow(.x)) else map_int(data, ~sum(.x$Count)), 
                                   shape = if(type %in% c("NB","HurdGamma","HurdGamma2","Gamma")) map_dbl(model, ~summary(.x)$spec_pars[1] %>% round(3)) else NA,
                                   YearSlope=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[3,1] %>% round(3)) else map_dbl(model, ~fixef(.x)[2,1] %>% round(3)),  
                                   YearError=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[3,2] %>% round(3)) else  map_dbl(model, ~fixef(.x)[2,2] %>% round(3)),
                                   lower_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[3,3] %>% round(3)) else map_dbl(model, ~fixef(.x)[2,3] %>% round(3)),
                                   upper_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[3,4] %>% round(3)) else map_dbl(model, ~fixef(.x)[2,4] %>% round(3)),
                                   Prob_Decreasing=map_dbl(model, ~hypothesis(.x, 'ScYear<0')[[1]]$Post.Prob %>% round(3)),
                                   Prob_Increasing=map_dbl(model, ~hypothesis(.x, 'ScYear>0')[[1]]$Post.Prob %>% round(3)),
                                   Year_Per_Change=(exp(YearSlope)-1) %>% round(2),
                                   Year_Change_low95=(exp(lower_95)-1) %>% round(2),
                                   Year_Change_upper95=(exp(upper_95)-1) %>% round(2),
                                   Model=type
                                   )
  
}



SummarizeHurdleFit<-function(model, type, names, data) {

  OutTable<-data.frame(Species=names(names))
  
  OutTable <- OutTable %>% mutate(
                    #TotalCount=map_int(data, ~sum(.x$Count)), 
                    #shape =  map_dbl(model, ~summary(.x)$spec_pars[1] %>% round(3)),
                    Occu_Slope=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[4,1] %>% round(3)) else NA,  
                    Occu_Error=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[4,2] %>% round(3)) else NA,
                    Occu_L95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[4,3] %>% round(3)) else NA,
                    Occu_U95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x)[4,4] %>% round(3)) else NA,
                    Occu_Decreasing=if(type =="HurdGamma2") map_dbl(model, ~hypothesis(.x, 'hu_ScYear>0')[[1]]$Post.Prob %>% round(3)) else NA,
                    Occu_Increasing=if(type =="HurdGamma2") map_dbl(model, ~hypothesis(.x, 'hu_ScYear<0')[[1]]$Post.Prob %>% round(3)) else NA,
                    Occu_Start=if(type =="HurdGamma2")  map_dbl(model, ~ 1- plogis(fixef(.x)[2,1] + 
                                                                     min(.x$data$ScYear) * fixef(.x)[4,1])%>% round(3)) else NA,
                    Occu_End= if(type =="HurdGamma2") map_dbl(model, ~ 1- plogis(fixef(.x)[2,1] + 
                                                                      max(.x$data$ScYear) * fixef(.x)[4,1])%>% round(3)) else NA,
                    Model=type
  )
  
}


################ Fire Functions
fitFirePoisson<-function(object,...){
  
  RegFormula<-formula("Count ~ ScYear + Fire + ScYear*Fire + (1|Plot_Name)")
  
  brm(RegFormula, data=object, family=poisson(), ... )
}


fitFireNB<-function(object, ...){
  
  RegFormula<-formula("Count ~ ScYear + ScYear*Fire + (1|Plot_Name)")
  
  brm(RegFormula, data=object, family=negbinomial(), ... )
}

fitFireHurdGamma<-function(object, ...){
  
  RegFormula<-formula("BA ~ ScYear + ScYear*Fire + (1|Plot_Name)")
  
  brm(RegFormula, data=object, family=hurdle_gamma(), ... )
}

fitFireHurdGamma2<-function(object, RandomYear=F, ...){
  
  #RegFormula<-formula(paste0("BA ~ ScYear + (1|Plot_Name)",if(RandomYear) " + (1|YrFactor)" else ""))
  
  brm(bf(BA~ScYear+ScYear*Fire+(1|Plot_Name), hu~ScYear), data=object, family=hurdle_gamma(), ... )
}

fitFirePoissonVine<-function(object, ...){
  
  brm(bf(Count ~ ScYear*Fire + offset(LogTrees)+ (1|Plot_Name)), data=object, family=poisson(), ... )

}

fitFireNBVine<-function(object, ...){
  
  brm( bf( Count ~ ScYear*Fire + offset(LogTrees) + (1|Plot_Name)), data=object, family=negbinomial(), ... )
  
}

SummarizeFireFit<-function(model, type, names, data) {
  
  OutTable<-data.frame(Species=names(names))
  
  MinYear<-min(data[[1]]$ScYear)
  Firemin<-paste0("Intercept+Fire+ScYear*",MinYear,"+ScYear:Fire*",MinYear)
  NoFiremin<-paste0("Intercept+ScYear*",MinYear)
  FireDec<-paste0(Firemin,"<",NoFiremin)
  FireInc<-paste0(Firemin,">",NoFiremin)
  
  OutTable <- OutTable %>% mutate(
    TotalCount=if(type=="Gamma") map_dbl(data, ~nrow(.x)) else map_int(data, ~sum(.x$Count)), 
    TotalPlots=map_int(data, ~filter(.x, Count>0) %>% n_distinct(.$Plot_Name)),
    shape = if(type %in% c("NB","HurdGamma","HurdGamma2","Gamma")) map_dbl(model, ~summary(.x)$spec_pars[1] %>% round(3)) else NA,
    
    Intercept=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[1,1] %>% round(3)) else map_dbl(model, ~fixef(.x,robust=T)[1,1] %>% round(3)),
    
    
    YearSlope=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[3,1] %>% round(3)) else map_dbl(model, ~fixef(.x, robust=T)[2,1] %>% round(3)),  
    YearError=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x,robust=T)[3,2] %>% round(3)) else  map_dbl(model, ~fixef(.x, robust=T)[2,2] %>% round(3)),
    Year_low_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[3,3] %>% round(3)) else map_dbl(model, ~fixef(.x,robust=T)[2,3] %>% round(3)),
    Year_up_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x,robust=T)[3,4] %>% round(3)) else map_dbl(model, ~fixef(.x,robust=T)[2,4] %>% round(3)),
    Year_low_85=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x,  probs=c(0.075, 0.925),robust=T)[3,3] %>% round(3)) else map_dbl(model, ~fixef(.x,robust=T,probs=c(0.075, 0.925))[2,3] %>% round(3)),
    Year_up_85=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, probs=c(0.075, 0.925),robust=T)[3,4] %>% round(3)) else map_dbl(model, ~fixef(.x,robust=T,probs=c(0.075, 0.925))[2,4] %>% round(3)),
    
    Year_Per_Change=exp(YearSlope) %>% round(3),
    Year_Per_Change_low95=exp(Year_low_95) %>% round(3),
    Year_Per_Change_upper95=exp(Year_up_95) %>% round(3),
    Year_Per_Change_low85=exp(Year_low_85) %>% round(3),
    Year_Per_Change_upper85=exp(Year_up_85) %>% round(3),
    Prob_Decreasing=map_dbl(model, ~hypothesis(.x, 'ScYear<0')[[1]]$Post.Prob %>% round(3)),
    Prob_Increasing=map_dbl(model, ~hypothesis(.x, 'ScYear>0')[[1]]$Post.Prob %>% round(3)),
    
    
    FireIntercept=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x,robust=T)[4,1] %>% round(3)) else map_dbl(model, ~fixef(.x, robust=T)[3,1] %>% round(3)),
    FireIntError=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[4,2] %>% round(3)) else  map_dbl(model, ~fixef(.x,robust=T)[3,2] %>% round(3)),
    Fire_Int_Per_Change=exp(FireIntercept) %>% round(2),
    Fire_Int_low_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[4,3] %>% round(3)) else map_dbl(model, ~fixef(.x, robust=T)[3,3] %>% round(3)),
    Fire_Int_up_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[4,4] %>% round(3)) else map_dbl(model, ~fixef(.x, robust=T)[3,4] %>% round(3)),
    Prob_Fire_Int_Decreasing=map_dbl(model, ~hypothesis(.x, 'Fire<0')[[1]]$Post.Prob %>% round(3)),
    Prob_Fire_Int_Increasing=map_dbl(model, ~hypothesis(.x, 'Fire>0')[[1]]$Post.Prob %>% round(3)), 
    
    
    FireSlope=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[5,1] %>% round(3)) else map_dbl(model, ~fixef(.x, robust=T)[4,1] %>% round(3)),
    FireError=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[5,2] %>% round(3)) else  map_dbl(model, ~fixef(.x, robust=T)[4,2] %>% round(3)),
    Fire_Slope_low_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[5,3] %>% round(3)) else map_dbl(model, ~fixef(.x, robust=T)[4,3] %>% round(3)),
    Fire_Slope_up_95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[5,4] %>% round(3)) else map_dbl(model, ~fixef(.x, robust=T)[4,4] %>% round(3)),
    
    Fire_Year_Per_Change=exp(YearSlope+FireSlope) %>% round(3),
    Fire_Year_Per_Change_low95 = map_dbl(model,~spread_draws(.x,c(b_ScYear,`b_ScYear:Fire`)) %>%
                   mutate(Tots=b_ScYear+`b_ScYear:Fire`) %>% median_qi(Tots, .width = 0.95) %>% pull(.lower) %>% exp %>% round(3)),
    Fire_Year_Per_Change_upper95 = map_dbl(model,~spread_draws(.x,c(b_ScYear,`b_ScYear:Fire`)) %>%
                 mutate(Tots=b_ScYear+`b_ScYear:Fire`) %>% median_qi(Tots, .width = 0.95) %>% pull(.upper) %>% exp %>% round(3)),
    Fire_Year_Per_Change_low85 = map_dbl(model,~spread_draws(.x,c(b_ScYear,`b_ScYear:Fire`)) %>%
              mutate(Tots=b_ScYear+`b_ScYear:Fire`) %>% median_qi(Tots, .width = 0.85) %>% pull(.lower) %>% exp %>% round(3)),
    Fire_Year_Per_Change_upper85= map_dbl(model,~spread_draws(.x,c(b_ScYear,`b_ScYear:Fire`)) %>%
                mutate(Tots=b_ScYear+`b_ScYear:Fire`) %>% median_qi(Tots, .width = 0.85) %>% pull(.upper) %>% exp %>% round(3)),
    
    # Fire_Year_Per_Change_low95=exp(Fire_Slope_low_95) %>% round(2),
    # Fire_Year_Per_Change_upper95=exp(Fire_Slope_up_95) %>% round(2),
    # Fire_Year_Per_Change_low85=exp(Fire_Slope_low_95) %>% round(2),
    # Fire_Year_Per_Change_upper85=exp(Fire_Slope_up_95) %>% round(2),
    
    #Tree_Fire_Poisson_Regression[[1]] %>%  spread_draws(c(b_ScYear,`b_ScYear:Fire`)) %>% mutate(Tots=b_ScYear+`b_ScYear:Fire`) %>% median_qi(Tots)
    
    
    Prob_Fire_Slope_Decreasing=map_dbl(model, ~hypothesis(.x, 'ScYear:Fire<0')[[1]]$Post.Prob %>% round(3)),
    Prob_Fire_SlOpe_Increasing=map_dbl(model, ~hypothesis(.x, 'ScYear:Fire>0')[[1]]$Post.Prob %>% round(3)),
    
    Prob_Fire_Overall_Decreasing=map_dbl(model, ~hypothesis(.x, 'ScYear+ScYear:Fire<0')[[1]]$Post.Prob %>% round(3)),
    Prob_Fire_Ovarall_Increasing=map_dbl(model, ~hypothesis(.x, 'ScYear+ScYear:Fire>0')[[1]]$Post.Prob %>% round(3)),
    
    Fire_Decreased_Year1=map_dbl(model, ~hypothesis(.x, FireDec)[[1]]$Post.Prob %>% round(3)),
    Fire_Increased_Year1=map_dbl(model, ~hypothesis(.x, FireInc)[[1]]$Post.Prob %>% round(3)),
    Model=type
  )
  
}

SummarizeFireHurdleFit<-function(model, type, names, data) {
  
  OutTable<-data.frame(Species=names(names))
  
  OutTable <- OutTable %>% mutate(
    #TotalCount=map_int(data, ~sum(.x$Count)), 
    #shape =  map_dbl(model, ~summary(.x)$spec_pars[1] %>% round(3)),
    Occu_Slope=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[6,1] %>% round(3)) else NA,  
    Occu_Error=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[6,2] %>% round(3)) else NA,
    Occu_L95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[6,3] %>% round(3)) else NA,
    Occu_U95=if(type =="HurdGamma2") map_dbl(model, ~fixef(.x, robust=T)[6,4] %>% round(3)) else NA,
    Occu_Decreasing=if(type =="HurdGamma2") map_dbl(model, ~hypothesis(.x, 'hu_ScYear>0')[[1]]$Post.Prob %>% round(3)) else NA,
    Occu_Increasing=if(type =="HurdGamma2") map_dbl(model, ~hypothesis(.x, 'hu_ScYear<0')[[1]]$Post.Prob %>% round(3)) else NA,
    Occu_Start=if(type =="HurdGamma2")  map_dbl(model, ~ 1- plogis(fixef(.x, robust=T)[2,1] + 
                                                                     min(.x$data$ScYear) * fixef(.x, robust=T)[4,1])%>% round(3)) else NA,
    Occu_End= if(type =="HurdGamma2") map_dbl(model, ~ 1- plogis(fixef(.x, robust=T)[2,1] + 
                                                                   max(.x$data$ScYear) * fixef(.x,robust=T)[4,1])%>% round(3)) else NA,
    Model=type
  )
  
}


SummarizeFireOutcome<-function(model, names, data, form){
  OutTable<-data.frame(Species=names(names))
  
  #FitInData<-map(data, ~data.frame(Fire=c(0,1), ScYear=min(.x$ScYear),  LogTrees=3.42 ))
  
  FitInData<-map(data, ~filter(.x, Year %in% 2006:2009))
  
  
  # LogTrees value = 3.42,  for fire is 2.89, no fire is 3.44, used for vines only
  
  Ha_Convert<-case_when(form =="tree" ~ 10000/(225*pi),
                       form=="sapling" ~ 10000/(27*pi),
                       form=="seedling" ~ 10000/12,
                       form =="vine" ~ 10000/(225*pi),
                       form=="shrub" ~ 10000/(27*pi),
                       form=="cwd" ~ 1
  )
  
  Fitted_Data<-map2(model,FitInData, ~fitted(.x, newdata=.y, re_formula=NULL, scale="response", robust=T))
  Fitted_Data<-map2(FitInData, Fitted_Data, .f=cbind)

  
  
  OutTable <- OutTable %>% mutate(
    Start_NoFire_Mean=map_dbl(Fitted_Data,~.x %>% filter(Fire==0) %>% pull(Estimate) %>% mean %>%  round(3)),
    Start_NoFire_Low=map_dbl(Fitted_Data,~.x %>% filter(Fire==0) %>% pull(Q2.5) %>% mean %>%  round(3)),
    Start_NoFire_High=map_dbl(Fitted_Data, ~.x %>% filter(Fire==0) %>% pull(Q97.5) %>% mean %>%  round(3)),
    Start_Fire_Mean=map_dbl(Fitted_Data,~.x %>% filter(Fire==1) %>% pull(Estimate) %>% mean %>%  round(3)),
    Start_Fire_Low=map_dbl(Fitted_Data,~.x %>% filter(Fire==1) %>% pull(Q2.5) %>% mean %>%  round(3)),
    Start_Fire_High=map_dbl(Fitted_Data, ~.x %>% filter(Fire==1) %>% pull(Q97.5) %>% mean %>%  round(3)),
    
    Start_NoFire_Mean_HA=(Start_NoFire_Mean * Ha_Convert) %>% round(2),
    Start_NoFire_Low_Ha=(Start_NoFire_Low * Ha_Convert) %>% round(2),
    Start_NoFire_High_Ha=(Start_NoFire_High * Ha_Convert) %>% round(2),
    Start_Fire_Mean_HA=(Start_Fire_Mean * Ha_Convert) %>% round(2),
    Start_Fire_Low_Ha=(Start_Fire_Low * Ha_Convert) %>% round(2),
    Start_Fire_High_Ha = (Start_Fire_High * Ha_Convert) %>% round(2)
  )
  
}
