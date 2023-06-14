
library(cmprsk)
#library(devtools)
#devtools::install_github("statsbomb/StatsBombR")

library(StatsBombR)
library(dplyr)
library(tidyr)
library(survival)
require(crrSC)

library(officer)
library(flextable)
library(magrittr)
library(xtable)
#test=lazyLoad(filebase = "/Users/SébastianLECOZ/Documents/CIR football analytics/goftte/R/goftte")

#install.packages('C:/Users/SébastianLECOZ/Documents/CIR football analytics/goftte_1.0.5.tar.gz', repos = NULL)
library(goftte)


remove_consecutive_repetitions <- function(x) {
  keep_idx <- which(diff(c(0, x)) != 0)
  result <- x[keep_idx]
  return(list(result = result, idx = keep_idx))
}

load(file="Full_data_ligue_simplified_formations_corrected_23_05_23.Rdata")


Total=apply(table(Full_events_formation[,c('Combined_home','Combined_away')]),2,sum)
#rownames(total)<-"Total"

sum_of_all_data=rbind(table(Full_events_formation[,c('Combined_home','Combined_away')]),Total)


print(xtable(sum_of_all_data, type = "latex"), file = "filename_table.tex")


# events per features  : 
dd=Full_events_formation[Full_events_formation$Fine_and_Gray_events==1,]


Total_Defense=apply(table(dd[,c('Combined_home','Combined_away')]),2,sum)
Total_attack=apply(table(dd[,c('Combined_home','Combined_away')]),1,sum)
#rownames(total)<-"Total"
Total_attack=c(Total_attack,0)

sum_of_all_data=cbind(rbind(table(dd[,c('Combined_home','Combined_away')]),Total_Defense),Total_attack)


print(xtable(sum_of_all_data, type = "latex"), file = "filename_table_EVP21.tex")






home_formation_= as.factor(Full_events_formation[,c('Combined_home')])
# perform one hot encoding
one_hot_formation_home=model.matrix(~ home_formation_ + 0)

away_formation_= as.factor(Full_events_formation[,c('Combined_away')])
# perform one hot encoding
one_hot_formation_away=model.matrix(~ away_formation_ + 0)

one_hot_formation_away=as.data.frame(one_hot_formation_away)
one_hot_formation_home=as.data.frame(one_hot_formation_home)

final_dff <- cbind(Full_events_formation,one_hot_formation_away,one_hot_formation_home)

# trop d'équipes différents pour etre pris en compte ! 
#home team only matrice non inversible 
where_teamsf= 34:length(colnames(final_dff))

#crr_model= crr(final_df[,'Time_dif'],final_df[,'Fine_and_Gray_events'],final_df[,c(colnames(final_df)[where_teams],colnames(final_df)[83:85])],failcode=-1)

#summary(crr_model)



decoupage_data_set_uniforme_formation<-  function(data,cible,formations) {
  
  
  cible_length=length(which(data[,cible]==1))
  
  f=formations[-which(formations==cible)]
  list_lignes=which(data[,cible]==1)
  for (indice in 1: length(f)){
    
    
    temp_which=which(data[,f[indice]]==1)
    l_f=length(temp_which)
    print(l_f)
    print(f[indice])
    if (l_f< cible_length) {
      
      list_lignes=c(list_lignes,temp_which)
      
      
    }else{
        
      list_lignes=c(list_lignes,sample(temp_which, cible_length))
      
      }
      
    
    
  }

  data_filtered=data[list_lignes,]

  return (data_filtered)
}
# 
# aa=decoupage_data_set_uniforme_formation(final_dff,teamsf[6],teamsf[1:7])
# 
# crr_model_min <-try(crrs(aa[,'Time_dif'],aa[,'Fine_and_Gray_events'],cov1=aa[,teamsf[tt]],  strata=strata(aa[,colnames(aa)[c(6)]]),failcode=cens,cencode=0)
#                    , silent = TRUE)
# 









# reccupération des p-valeurs de crrs
get_p_values <- function(crr_model) {
  output<-capture.output(crr_model)
  p_values_str <- output[7]
  p_values_str <- as.numeric(sub("^\\[\\d+\\] (.*)$", "\\1",  p_values_str))
  
  return(p_values_str)
}



get_p_values_crr <- function(crr_model) {
  return(summary(crr_model)[[6]][5])
  
}





Full= Full_events_formation[,c("period")]
#provide column names
teamsf=colnames(final_dff)[where_teamsf]
df_results_by_teamf <- data.frame(matrix(ncol = 13, nrow =length(teamsf)))
colnames(df_results_by_teamf) <- c( 'Exp Form strata','p Form strata','prop KS','prop CvM','prop AD','Convg','BIC strata','BIC only','Exp Form only','p Form only')
rownames(df_results_by_teamf) = c(paste("home Goal",teamsf))

cl <- makeCluster(3)
registerDoParallel(cl)
nr=nrow(final_dff)
Results <- foreach(a = 1:(length(teamsf)), .combine = "rbind", 
                   .multicombine = TRUE, .errorhandling = "remove", 
                   .export = c("get.matchFree"), .packages = c("httr", "jsonlite", "dplyr", "crrSC", "goftte", "cmprsk")) %dopar% {
                     
                     tt=a
                     cens=1
                     
                     
                     df_results_by_team_t <- data.frame(matrix(ncol = 13, nrow = 1))
                     colnames(df_results_by_team_t) <- c('prop KS','prop CvM','prop AD','BIC only','p Form only','IC only+','IC only-','exp_coef only','BIC min','p min','IC min+','IC min-','exp_coef min')
                     
                     
                     
                     crr_model_min <-try(crrs(final_dff[,'Time_dif'],final_dff[,'Fine_and_Gray_events'],cov1=final_dff[,teamsf[tt]],  strata=strata(final_dff[,colnames(final_dff)[c(6)]]),failcode=cens,cencode=0)
                                     , silent = TRUE)
                     
                     # Check if there was an error
                     
                     if (inherits(crr_model_min, "try-error")){
                       
                     }else {
                       df_results_by_team_t[13] <- exp(crr_model_min[[1]])
                       p <- get_p_values(crr_model_min)
                       df_results_by_team_t[11]<- exp(crr_model_min$coef[[1]]+1.96*sqrt(crr_model_min$var[1]))
                       df_results_by_team_t[12]<- exp(crr_model_min$coef[[1]]-1.96*sqrt(crr_model_min$var[1]))
                       df_results_by_team_t[9] <- 2*crr_model_min$loglik+ 2*nr
                       df_results_by_team_t[10] <- p
                     }
                     
                     
                     crr_model_prop <- crr(final_dff[,'Time_dif'],final_dff[,'Fine_and_Gray_events'],cov1=final_dff[,teamsf[tt]],failcode=cens,cencode=0)
                     
                     
                     crr.prop <- prop(model=crr_model_prop,ftime=Full_events_formation[,'Time_dif'],fstatus=Full_events_formation[,'Fine_and_Gray_events'],final_dff[,teamsf[tt]],failcode=cens,cencode=0,variable=c(teamsf[tt]),R=200, plots=50,seed=0) 
                     
                     
                     
                     
                     df_results_by_team_t[1] <- crr.prop$KS
                     df_results_by_team_t[2] <- crr.prop$CvM
                     df_results_by_team_t[3] <- crr.prop$AD
                     
                     df_results_by_team_t[4] <- -2*crr_model_prop$loglik +nr
                     df_results_by_team_t[8] <- exp(crr_model_prop$coef[[1]])
                     df_results_by_team_t[6]<- exp(crr_model_prop$coef[[1]]+1.96*sqrt(crr_model_prop$var[1]))
                     df_results_by_team_t[7]<- exp(crr_model_prop$coef[[1]]-1.96*sqrt(crr_model_prop$var[1]))
                     df_results_by_team_t[5]<- get_p_values_crr(crr_model_prop)
                     
                     print(crr.prop)
                     
                     #plot(crr.prop, idx=1) 
                     
                     save(df_results_by_team_t,file=paste('formation_simplified_corrected/',teamsf[tt],".Rdata",sep=""))
                     
                     return(df_results_by_team_t)
                   }

stopCluster(cl)

df_results_by_formation <-  Results




#rownames(df_results_by_formation) = teamsf
print(df_results_by_formation)
rownames(df_results_by_formation) = teamsf

df_results_by_formation_league=round(df_results_by_formation,3)
#save(df_results_by_formation_league,file="Results_formations_simplified_23_05_2023.Rdata")
load(file="Results_formations_simplified_23_05_2023.Rdata")
rownames(df_results_by_formation_league)


df_results_by_formation_league$formation=sub("_formation","",rownames(df_results_by_formation_league))
df_results_by_formation_league=df_results_by_formation_league[,c('formation','prop KS','prop CvM','prop AD','BIC min','p min','IC min+','IC min-','exp_coef min')]



# # Create a temp file
# tmp <- tempfile(fileext = ".docx")
# df_results_by_formation_league=flextable(as.data.frame(df_results_by_formation_league))
# # Create a docx file
# read_docx() %>%
#   body_add_flextable(df_results_by_formation_league) %>%
#   print(target = tmp)
# 
# # open word document
# browseURL(tmp)
# load(file="Results_formations_simplified_23_05_2023.Rdata")
# print(xtable(df_results_by_formation_league, type = "latex"), file = "filenameresultnormal.tex")


# fomations deux a deux 

get_p_values2 <- function(crr_model) {
  output<-capture.output(crr_model)
  p_values_str <- output[7]
  strsplit(p_values_str," ")
  
  p_values_str1 <- as.numeric(strsplit(p_values_str," ")[[1]][2])
  p_values_str2 <- as.numeric(strsplit(p_values_str," ")[[1]][3])
  a=c(p_values_str1,p_values_str2)
  return(a)
}



cl <- makeCluster(5)
registerDoParallel(cl)
nr=nrow(final_dff)
Results <- foreach(a = 1:(length(teamsf)-(length(teamsf)/2)), .combine = "rbind", 
                   .multicombine = TRUE, .errorhandling = "remove", 
                   .export = c("get.matchFree"), .packages = c("httr", "jsonlite", "dplyr", "crrSC", "goftte", "cmprsk")) %dopar% {
                     
                     tt=a
                     cens=1
                     df_results_by_team_t <- data.frame(matrix(ncol = 16, nrow = (length(teamsf)/2 )))
                     colnames(df_results_by_team_t) <- c('home','away','BIC','p home','IC home+','IC home-','exp_coef home','p away','IC away+','IC away-','exp_coef away','BIC only','p only','IC only+','IC only-','exp_coef only')
                     
                     for( i in (length(teamsf)/2 +1):(length(teamsf))) {
                       dd=i-length(teamsf)/2
                       
                       crr_model_min <-try(crrs(final_dff[,'Time_dif'],final_dff[,'Fine_and_Gray_events'],cov1=final_dff[,c(teamsf[i],teamsf[tt])],  strata=strata(final_dff[,colnames(final_dff)[c(6)]]),failcode=cens,cencode=0)
                                           , silent = TRUE)
                       
                       # Check if there was an error
                       df_results_by_team_t[dd,'away'] = sub("away_formation_","",teamsf[tt])
                       df_results_by_team_t[dd,'home'] = sub("home_formation_","",teamsf[i])
                       
                       w=which(final_dff[,c(teamsf[i])]==1 & final_dff[,c(teamsf[tt])]==1)
                       
                       if (length(w)>0){
                         vect_douyble=rep(0,nrow(final_dff))
                         vect_douyble[final_dff[,c(teamsf[i])]==1 & final_dff[,c(teamsf[tt])]==1]=1
                         crr_model <-try(crrs(final_dff[,'Time_dif'],final_dff[,'Fine_and_Gray_events'],cov1=vect_douyble,  strata=strata(final_dff[,colnames(final_dff)[c(6)]]),failcode=cens,cencode=0)
                                             , silent = TRUE)
                        
                         if (inherits(crr_model_min, "try-error")){
                           
                         }else {
                           df_results_by_team_t[dd,16] <- exp(crr_model$coef[[1]])
                           Cc <- get_p_values(crr_model)
                           # reccupération des p-valeurs de crrs
                           
                          
                           
                           
                           df_results_by_team_t[dd,14]<- exp(crr_model$coef[[1]]+1.96*sqrt(crr_model$var[1,1]))
                           df_results_by_team_t[dd,15]<- exp(crr_model$coef[[1]]-1.96*sqrt(crr_model$var[1,1]))
                           df_results_by_team_t[dd,13] <- Cc
                           df_results_by_team_t[dd,12] <- 2*crr_model$loglik+ 2*nr
                          
                         }
                         
                          
                       }
                       if (inherits(crr_model_min, "try-error")){
                         
                       }else {
                         df_results_by_team_t[dd,7] <- exp(crr_model_min$coef[[1]])
                         C <- get_p_values2(crr_model_min)
                         # reccupération des p-valeurs de crrs
                         
                         p1=C[1]
                         p2=C[2]
                         
                         
                         df_results_by_team_t[dd,5]<- exp(crr_model_min$coef[[1]]+1.96*sqrt(crr_model_min$var[1,1]))
                         df_results_by_team_t[dd,6]<- exp(crr_model_min$coef[[1]]-1.96*sqrt(crr_model_min$var[1,1]))
                         df_results_by_team_t[dd,3] <- 2*crr_model_min$loglik+ 3*nr
                         df_results_by_team_t[dd,4] <- p1
                         df_results_by_team_t[dd,9]<- exp(crr_model_min$coef[[2]]+1.96*sqrt(crr_model_min$var[2,2]))
                         df_results_by_team_t[dd,10]<- exp(crr_model_min$coef[[2]]-1.96*sqrt(crr_model_min$var[2,2]))
                         df_results_by_team_t[dd,11] <- exp(crr_model_min$coef[[2]])
                         df_results_by_team_t[dd,8] <- p2
                       }
                       
                       
                       
                       
                       
                       
                     }
                     
                     
                     
                   
                     
                     
                     save(df_results_by_team_t,file=paste('formation_coupled/',teamsf[tt],".Rdata",sep=""))
                     
                     return(df_results_by_team_t)
                   }



#save(Results,file="Formations_VS_23_05_23.Rdata")
load(file="Formations_VS_23_05_23.Rdata")
Results[,3:ncol(Results)]=round(Results[,3:ncol(Results)],2)
# Create a temp file

Results[which( ((Results[,'p home']<0.1) & Results[,'p away']<0.1)| (Results[,'p only']<0.1)  ),]

# Create a temp file
# tmp <- tempfile(fileext = ".docx")
# df_results_by_formation_league=flextable(as.data.frame(Results))
# # Create a docx file
# read_docx() %>%
#   body_add_flextable(df_results_by_formation_league) %>%
#   print(target = tmp)
# 
# # open word document
# browseURL(tmp)
library(xtable)

model=Results[which( ((Results[,'p home']<0.1) | Results[,'p away']<0.1)| (!is.na(Results[,'p only']) & Results[,'p only']<0.1)  ),]
model=model[,c("home","away","p home","IC home-","IC home+", "exp_coef home","p away", "IC away-", "IC away+","exp_coef away","p only",  "IC only-", "IC only+", "exp_coef only")]
colnames(model)= c("Attack","Defense","p-value","IC home-","IC home+", "exp_coef home","p away", "IC away-", "IC away+","exp_coef away","p only",  "IC only-", "IC only+", "exp_coef only")



model2= cbind(model[,1:3],paste('[',model[,'IC home-'],',',model[,'IC home+'],']',sep=''),model[,6:7],paste('[',model[,'IC away-'],',',model[,'IC away+'],']',sep=''),model[,10:11],paste('[',model[,'IC only-'],',',model[,'IC only+'],']',sep=''),model[,14])
colnames(model2)= c("Attack","Defense","p-value","IC", "exp_coef Attack","p-value", "IC","exp_coef Defense","p-value",  "IC", "exp_coef only")



w=which(model$away == 4231)
model=model[-w,]
w=which(model$away == 532)
model=model[-w,]
5-3-2

print(xtable(model2, type = "latex"), file = "filenamevs.tex",include.rownames=FALSE)





