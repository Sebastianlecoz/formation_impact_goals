

library(cmprsk)
#library(devtools)
#devtools::install_github("statsbomb/StatsBombR")

library(StatsBombR)
library(dplyr)
library(tidyr)
library(survival)
require(crrSC)

#test=lazyLoad(filebase = "/Users/SébastianLECOZ/Documents/CIR football analytics/goftte/R/goftte")

#install.packages('C:/Users/SébastianLECOZ/Documents/CIR football analytics/goftte_1.0.5.tar.gz', repos = NULL)
library(goftte)


remove_consecutive_repetitions <- function(x) {
  keep_idx <- which(diff(c(0, x)) != 0)
  result <- x[keep_idx]
  return(list(result = result, idx = keep_idx))
}

load(file="Full_data_ligue_simplified_formations_corrected_23_05_23.Rdata")


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







features= c('minute','period','number_of_goals_before',"Goal_diff","time_period","Full_Home_away")
lf=length(features)

all_combinations <- list()

length_features= rep(0,lf)
for (k in 1:lf) {
  combinations_k <- combn(features, k)
  all_combinations[[k]] <- combinations_k
  length_features[k]=ncol(combinations_k)
}

length_features=cumsum(length_features)





home_formation_= as.factor(Full_events_formation[,c('Combined_home')])
# perform one hot encoding
one_hot_formation_home=model.matrix(~ home_formation_ + 0)

away_formation_= as.factor(Full_events_formation[,c('Combined_away')])
# perform one hot encoding
one_hot_formation_away=model.matrix(~ away_formation_ + 0)

one_hot_formation_away=as.data.frame(one_hot_formation_away)
one_hot_formation_home=as.data.frame(one_hot_formation_home)

final_dff <- cbind(Full_events_formation,one_hot_formation_away,one_hot_formation_home)

where_teamsf= 34:length(colnames(final_dff))

#provide column names
teamsf=colnames(final_dff)[where_teamsf]


df_results_by_formation <- data.frame(matrix(ncol = 5, nrow = 0))
final_model<- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_results_by_formation) <- c("Features",'Exp Form strata','p Form strata','Convg','BIC strata')
colnames(final_model)<- c("Features",'Exp Form strata','p Form strata','Convg','BIC strata')
for (xt in 1:length(teamsf)){
  
  
  
  cl <- makeCluster(2)
  registerDoParallel(cl)
  nr=nrow(final_dff)
  Results <- foreach(t = 1:max(length_features), .combine = "rbind", 
                     .multicombine = TRUE, .errorhandling = "remove", 
                     .export = c("get.matchFree"), .packages = c("httr", "jsonlite", "dplyr", "crrSC", "goftte", "cmprsk")) %dopar% {
                       
                       tt=t
                       
                       which_comb=max((which(c(0,length_features)<tt)))
                       
                       index=tt-c(0,length_features)[which_comb]
                       
                       Feat=all_combinations[[which_comb]][,index]
                       
                       
                       cens=1
                       
                       
                       df_results_by_team_t <- data.frame(matrix(ncol = 5, nrow = 1))
                       colnames(df_results_by_team_t) <- c("Features",'Exp Form strata','p Form strata','Convg','BIC strata')
                       
                       
                       
                       crr_model <-try(crrs(final_dff[,'Time_dif'],final_dff[,'Fine_and_Gray_events'],cov1=final_dff[,teamsf[xt]],  strata=strata(final_dff[,Feat]),failcode=cens,cencode=0)
                                       , silent = TRUE)
                       
                       
                       # Check if there was an error
                       if (inherits(crr_model, "try-error")){
                         
                       }else {
                         df_results_by_team_t[1] <- paste(sub("_formation","",teamsf[xt]),paste(Feat,sep=",",collapse = '_'),sep="_")
                         df_results_by_team_t[2] <- exp(crr_model[[1]])
                         p <- get_p_values(crr_model)
                         df_results_by_team_t[4] <- crr_model$converged
                         df_results_by_team_t[5] <- 2*crr_model$loglik+ (length(Feat)+1)*nr
                         df_results_by_team_t[3] <- p
                       }
                       
                       
                       
                       save(df_results_by_team_t,file=paste(sub("_formation","",teamsf[xt]),paste(Feat,sep=",",collapse = '_'),".Rdata",sep=""))
                       
                       return(df_results_by_team_t)
                     }
  
  stopCluster(cl)
  mmmm=which.min(Results[,'BIC strata'])  
  final_model<-rbind(final_model,Results[mmmm,])
  df_results_by_formation <-  rbind(df_results_by_formation,Results)

}

# 
# library(officer)
# library(flextable)
# library(magrittr)
# 
# # Create a temp file
# df_results_by_formation[,c(2,3,5)]=round(df_results_by_formation[,c(2,3,5)],3)
# tmp <- tempfile(fileext = ".docx")
# df_results_by_formation_league=flextable(as.data.frame(df_results_by_formation[,c(1:3,5)]))
# # Create a docx file
# read_docx() %>%
#   body_add_flextable(df_results_by_formation_league) %>%
#   print(target = tmp)
# 
# # open word document
# browseURL(tmp)


# save(df_results_by_formation,file="model_selection/model_selection_BIC_23_05_23.Rdata")
# save(final_model,file="model_selection/model_selection_BIC_23_05_23_best.Rdata")

#load(file="model_selection_BIC_23_05_23_best.Rdata")
#load(file="model_selection_BIC_11_05_23.Rdata")

# library(xtable)
# #example with   5-4-1
# grepl( "home_541",df_results_by_formation[,1], fixed = TRUE)
# 
# index = grepl( "home_541",df_results_by_formation[,1], fixed = TRUE)
# res=df_results_by_formation[index,]
# res[,1]=sub("home_","Attack ",res[,1])
# res=res[,c(1,5)]
# 
# 
# print(xtable(res, type = "latex"), file = "filename_model_select.tex", include.rownames=FALSE)
