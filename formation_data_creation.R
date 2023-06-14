
library(cmprsk)
#library(devtools)
#devtools::install_github("statsbomb/StatsBombR")

library(StatsBombR)
library(dplyr)
library(tidyr)
library(survival)
require(crrSC)

#test=lazyLoad(filebase = "/Users/SébastianLECOZ/Documents/CIR football analytics/goftte/R/goftte")
# goftte needed to be installed with binaries as the package is depricated.
#install.packages('C:/Users/SébastianLECOZ/Documents/CIR football analytics/goftte_1.0.5.tar.gz', repos = NULL)
library(goftte)


remove_consecutive_repetitions <- function(x) {
  keep_idx <- which(diff(c(0, x)) != 0)
  result <- x[keep_idx]
  return(list(result = result, idx = keep_idx))
}



# Get match data
Comp <- FreeCompetitions()
Comp<- Comp  %>% filter((competition_name!="La Liga" &competition_name!="Premier League" ) )


Matches <- FreeMatches(Comp) #%>% filter(match_id==  "22912")


cl <- makeCluster(detectCores()-4)
registerDoParallel(cl)
Full_events_formation<- foreach(j = 1:nrow(Matches), .combine = bind_rows, 
                                .multicombine = TRUE, 
                                .export = c("get.matchFree"), .packages = c("httr", 
                                                                            "jsonlite", "dplyr")) %dopar% {
                                                                              M <- Matches %>%
                                                                                filter(match_id == Matches[j,'match_id'][[1]])
                                                                              
                                                                              
                                                                              Mevents <- get.matchFree(M)
                                                                              
                                                                              events_overtime <-Mevents %>%
                                                                                filter((period==3) |(period==4) )
                                                                              
                                                                              l_overtime=dim(events_overtime)[1]
                                                                              home_team_info=Mevents[,"tactics.lineup"][[1]]        
                                                                              away_team_info=Mevents[,"tactics.lineup"][[2]]     # check this out position.id, player.name
                                                                              
                                                                              #
                                                                              
                                                                              
                                                                              if (l_overtime>0) {
                                                                                events_overtime3 <-Mevents %>%
                                                                                  filter((period==3) )
                                                                                
                                                                                max_time3= max(events_overtime3[,'minute'])
                                                                                
                                                                                events_overtime4 <-Mevents %>%
                                                                                  filter((period==4) )
                                                                                
                                                                                max_time4= max(events_overtime4[,'minute'])
                                                                                
                                                                                events_overtime2 <-Mevents %>%
                                                                                  filter((period==2) )
                                                                                
                                                                                max_time2= max(events_overtime2[,'minute'])
                                                                                
                                                                                events_overtime1 <-Mevents %>%
                                                                                  filter((period==1) )
                                                                                
                                                                                max_time1= max(events_overtime1[,'minute'])
                                                                                
                                                                                
                                                                                
                                                                                goal_events <- Mevents %>%
                                                                                  filter((shot.outcome.name == "Goal" & (period!=5)) | type.name == "Own Goal For" )%>% 
                                                                                  add_row(shot.outcome.name="NOGoal",minute=max_time4,period=4)%>% 
                                                                                  add_row(shot.outcome.name="NOGoal",minute=max_time1,period=1)    %>% 
                                                                                  add_row(shot.outcome.name="NOGoal",minute=max_time2,period=2)    %>% 
                                                                                  add_row(shot.outcome.name="NOGoal",minute=max_time3,period=3)
                                                                                
                                                                                
                                                                                
                                                                              }else{
                                                                                
                                                                                events_overtime2 <-Mevents %>%
                                                                                  filter((period==2) )
                                                                                
                                                                                max_time2= max(events_overtime2[,'minute'])
                                                                                
                                                                                events_overtime1 <-Mevents %>%
                                                                                  filter((period==1) )
                                                                                
                                                                                max_time1= max(events_overtime1[,'minute'])
                                                                                
                                                                                goal_events <- Mevents %>%
                                                                                  filter((shot.outcome.name == "Goal" & (period!=5)) | type.name == "Own Goal For" )%>% 
                                                                                  add_row(shot.outcome.name="NOGoal",minute=max_time2,period=2)%>% 
                                                                                  add_row(shot.outcome.name="NOGoal",minute=max_time1,period=1)# 'shot.freeze_frame' works ass null
                                                                                
                                                                              }   
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              # where we are going to get and add the formation to all the columns of our data analaisis model. 
                                                                              
                                                                              
                                                                              formation_events <-Mevents[!is.na(Mevents[,"tactics.formation"]),]
                                                                              
                                                                              goal_events<-arrange(goal_events,minute)
                                                                              if( length(which("foul_committed.card.name"== colnames(Mevents)))){
                                                                                Red_card_event=Mevents[!is.na(Mevents[,'foul_committed.card.name']),]
                                                                                minutes_delete=Red_card_event[(Red_card_event[,'foul_committed.card.name']=='Red Card'),'minute']
                                                                                add_period=Red_card_event[(Red_card_event[,'foul_committed.card.name']=='Red Card'),"period"]
                                                                                if (length(minutes_delete)>0){
                                                                                  goal_events=goal_events[goal_events[,"minute"]<minutes_delete[1],]
                                                                                  formation_events= formation_events[ formation_events[,"minute"]<minutes_delete[1],]
                                                                                  goal_events <-goal_events%>%
                                                                                    add_row(shot.outcome.name="NOGoal",minute=minutes_delete,period=add_period)
                                                                                  
                                                                                  
                                                                                  
                                                                                }}
                                                                              
                                                                              where_home= arrange(formation_events[formation_events$team.name == M$home_team.home_team_name,],minute)
                                                                              where_away= arrange(formation_events[formation_events$team.name == M$away_team.away_team_name,],minute)
                                                                              
                                                                              # prendre les vecteurs corrects et appliquer la fonction pour trouver que les home_team formation changement 
                                                                              
                                                                              
                                                                              
                                                                              x.team=remove_consecutive_repetitions(where_home$tactics.formation)
                                                                              where_home=where_home[x.team$idx[-1],]
                                                                              x.away=remove_consecutive_repetitions(where_away$tactics.formation)
                                                                              where_away=where_away[x.away$idx[-1],]
                                                                              
                                                                              formation_events=rbind(where_home,where_away)
                                                                              formation_events[,'shot.outcome.name'][which(is.na(formation_events[,'shot.outcome.name']))]="NOGoal"
                                                                              
                                                                              
                                                                              
                                                                              order_formation_but=order(c(goal_events$minute, formation_events$minute))
                                                                              
                                                                              
                                                                              temp_full_events=rbind(goal_events,formation_events)
                                                                              temp_full_events=temp_full_events[order_formation_but,]
                                                                              
                                                                              lr=nrow(temp_full_events)
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              home_formation = rep(Mevents[1,"tactics.formation"],lr)
                                                                              away_formation = rep(Mevents[2,"tactics.formation"],lr)
                                                                              
                                                                              nr=nrow(formation_events)
                                                                              Back_a=rep(0,nr)
                                                                              Mid_a=rep(0,nr)
                                                                              Att_a=rep(0,nr)
                                                                              S_a=rep(0,nr)
                                                                              Combined_a=rep("0",nr)
                                                                              
                                                                              Back_h=rep(0,nr)
                                                                              Mid_h=rep(0,nr)
                                                                              Att_h=rep(0,nr)
                                                                              S_h=rep(0,nr)
                                                                              Combined_h=rep("0",nr)
                                                                              ww=2
                                                                              Back_a[1: lr] = sum(grepl('Back', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE))
                                                                              Mid_a[1: lr]= sum(grepl('Midfield', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & (! grepl('Attack', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)))
                                                                              Att_a[1: lr]= sum((!grepl('Back', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & grepl('Wing', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)) |grepl('Attack', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)|grepl('Forward', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)  |grepl('Striker', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) ) 
                                                                              S_a[1: lr]=Back_a[1] + Mid_a[1] + Att_a[1]
                                                                              
                                                                              
                                                                              Combined_a[1: lr]=paste(Back_a[1],Mid_a[1],Att_a[1],sep="")
                                                                              ww=1
                                                                              Back_h[1: lr] = sum(grepl('Back', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE))
                                                                              Mid_h[1: lr]= sum(grepl('Midfield', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & (! grepl('Attack', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)))
                                                                              Att_h[1: lr]= sum((!grepl('Back', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & grepl('Wing', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)) |grepl('Attack', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)|grepl('Forward', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) |grepl('Striker', Mevents[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) )
                                                                              S_h[1: lr]=Back_h[1] + Mid_h[1] + Att_h[1]
                                                                              
                                                                              
                                                                              Combined_h[1: lr]=paste(Back_h[1],Mid_h[1],Att_h[1],sep="")
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              where_shift= temp_full_events$type.name== "Tactical Shift"
                                                                              where_home= temp_full_events$team.name == M$home_team.home_team_name
                                                                              where_away= temp_full_events$team.name == M$away_team.away_team_name
                                                                              
                                                                              where_home= where_home & where_shift
                                                                              where_home[is.na(where_home)]=FALSE
                                                                              where_away = where_away & where_shift
                                                                              where_away[is.na(where_away)]=FALSE
                                                                              
                                                                              if( length(where_home) !=1){
                                                                              for (inx in 1:(length(where_home)-1)){
                                                                                
                                                                                if (where_home[inx]){
                                                                                  
                                                                                  home_formation[(inx+1) : lr]=temp_full_events[inx,"tactics.formation"]
                                                                                  ww=inx
                                                                                  Back_h[(inx+1) : lr] = sum(grepl('Back', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE))
                                                                                  Mid_h[(inx+1) : lr]= sum(grepl('Midfield', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & (! grepl('Attack', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)))
                                                                                  Att_h[(inx+1) : lr]= sum((!grepl('Back', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & grepl('Wing', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)) |grepl('Attack', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)|grepl('Forward', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)  |grepl('Striker', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) )
                                                                                  S_h[(inx+1) : lr]=Back_h[1] + Mid_h[1] + Att_h[1]
                                                                                  
                                                                                  
                                                                                  Combined_h[(inx+1) : lr]=paste(Back_h[(inx+1) ],Mid_h[(inx+1) ],Att_h[(inx+1) ],sep="")
                                                                                  
                                                                                  
                                                                                  # value of fomration 
                                                                                }
                                                                                
                                                                                
                                                                              }}
                                                                              
                                                                              if( length(where_away) !=1){
                                                                              for (inx in 1:(length(where_away)-1)){
                                                                                
                                                                                if (where_away[inx]){
                                                                                  
                                                                                  away_formation[(inx+1) : lr]=temp_full_events[inx,"tactics.formation"] # value of fomration 
                                                                                  ww=inx
                                                                                  Back_a[(inx+1) : lr] = sum(grepl('Back', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE))
                                                                                  Mid_a[(inx+1) : lr]= sum(grepl('Midfield', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & (! grepl('Attack', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)))
                                                                                  Att_a[(inx+1) : lr]= sum((!grepl('Back', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) & grepl('Wing', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)) |grepl('Attack', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE)|grepl('Forward', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) |grepl('Striker', temp_full_events[ww,"tactics.lineup"][[1]][,"position.name"], fixed = TRUE) )
                                                                                  S_a[(inx+1) : lr]=Back_a[1] + Mid_a[1] + Att_a[1]
                                                                                  
                                                                                  
                                                                                  Combined_a[(inx+1) : lr]=paste(Back_a[(inx+1) ],Mid_a[(inx+1) ],Att_a[(inx+1) ],sep="")
                                                                                }
                                                                                
                                                                                
                                                                              }}
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              goal_events=temp_full_events
                                                                              goal_events$away_formation=away_formation
                                                                              goal_events$home_formation=home_formation
                                                                              goal_events$Back_away=Back_a
                                                                              goal_events$Mid_away=Mid_a
                                                                              goal_events$Att_away=Att_a
                                                                              goal_events$S_away=S_a
                                                                              goal_events$Combined_away=Combined_a
                                                                              goal_events$Back_home=Back_h
                                                                              goal_events$Mid_home=Mid_h
                                                                              goal_events$Att_home=Att_h
                                                                              goal_events$S_home=S_a
                                                                              goal_events$Combined_home=Combined_h
                                                                              
                                                                              
                                                                              
                                                                              columns_to_keep= c('minute','possession_team.name',"shot.outcome.name","period")
                                                                              # ball possesion is only used for the goal. 
                                                                              
                                                                              
                                                                              
                                                                              goal_events[,'shot.outcome.name'][which(is.na(goal_events[,'shot.outcome.name']))]="Goal"
                                                                              
                                                                              
                                                                              
                                                                              len= length(goal_events[,'minute'])
                                                                              #adding overtime feature and time in periode , periode 0 or 1 in overtime or normal time period_all
                                                                              time_period= goal_events[,'minute']
                                                                              overtime=rep(0,len)
                                                                              period_all=rep(0,len)
                                                                              if (l_overtime>0) {
                                                                                
                                                                                
                                                                                period_all[which((goal_events[,'minute']>max_time2) &(goal_events[,'minute']<max_time3)) ]=1
                                                                                period_all[which((goal_events[,'minute']>max_time3) ) ]=1
                                                                                time_period[which(goal_events[,'minute']>max_time3&  goal_events[,'minute']<max_time4)]=time_period[which(goal_events[,'minute']>max_time3)]-max_time3
                                                                                time_period[which(goal_events[,'minute']>max_time2&  goal_events[,'minute']<max_time3)]=time_period[which(goal_events[,'minute']>max_time2)]-max_time2
                                                                                time_period[which(goal_events[,'minute']>max_time1 &  goal_events[,'minute']<max_time2)]=time_period[which(goal_events[,'minute']>max_time1)]-max_time1
                                                                                overtime[which(goal_events[,'minute']>max_time2)]=1
                                                                                
                                                                                
                                                                              }else {
                                                                                time_period[which(goal_events[,'minute']>max_time1)]=time_period[which(goal_events[,'minute']>max_time1)]-max_time1
                                                                                
                                                                                
                                                                              }
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              # bug on this part i need to change # time was giving when the next goal was coming ... 
                                                                              # need to do this now !!! 
                                                                             
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              # Need to order the times correctly 
                                                                              # find the corresponding times 
                                                                              
                                                                              ## need to add possession of the ball 
                                                                              
                                                                              
                                                                              # need to modify possession team
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              temp = goal_events$minute
                                                                              goal_events[,'minute']=c(0,goal_events[-length(goal_events$minute),'minute'])
                                                                              time_period=c(0,time_period[-length(goal_events$minute)])
                                                                              
                                                                              GE=goal_events[,columns_to_keep]
                                                                              
                                                                              goal_number=c(0,cumsum(goal_events[,"shot.outcome.name"]=="Goal")[-nrow(goal_events[,columns_to_keep])])
                                                                              # adding the time difference between each goal or end of the match 
                                                                              
                                                                             
                                                                              
                                                                              
                                                                              
                                                                              Xe=diff(c(0,temp))
                                                                              
                                                                              GE<- cbind(GE,tibble(number_of_goals_before= goal_number),time_period,overtime,period_all)
                                                                              GE<- cbind(GE,tibble(Time_dif= Xe))
                                                                              columns_to_keep_match=c('match_id','season.season_id','match_date','home_team.home_team_name','away_team.away_team_name')  
                                                                              ME=M[,columns_to_keep_match]
                                                                              ME=ME %>% slice(rep(1:n(), each = len))
                                                                              print(ME)
                                                                              
                                                                              
                                                                              match_id=rep(M[,"match_id"],len)
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              #adding the inverse part of the data home team
                                                                              # talk with people to see if good idea
                                                                              # why would it be good ?  to have the Event type always be negative when its a goal against the variable home_team
                                                                              is_invert = rep(FALSE,len)
                                                                              
                                                                              
                                                                              
                                                                              # final delete the final home or away Champions League final pas prendre en compte le home away 
                                                                              
                                                                              # final wolrd cup home pas forcément le bon .... euro aussi / women euro  world cup 
                                                                              
                                                                              #
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              Real_home= M$home_team.country.id == M$stadium.country.id *rep(1,nrow(GE))
                                                                              
                                                                              M$competition_stage.name!="Final" & M$competition.competition_name !="Champions League"
                                                                              Real_home=Real_home* 1 * (!(M$competition_stage.name=="Final" & M$competition.competition_name =="Champions League"))
                                                                              
                                                                              
                                                                              
                                                                              Real_away=((!(Real_home==1)) & (M$away_team.country.id == M$stadium.country.id *rep(1,nrow(GE))))*1
                                                                              Real_away[is.na(Real_away)]=0
                                                                              Real_home[is.na(Real_home)]=0
                                                                              
                                                                              
                                                                              
                                                                              Full_Home_away=(Real_home* 1) + (Real_away*-1)
                                                                              full_events=cbind(ME,GE,Real_home,Real_away,Full_Home_away,is_invert)
                                                                              
                                                                              
                                                                              
                                                                              full_events_inverse=full_events
                                                                              invert=full_events[,'home_team.home_team_name']
                                                                              full_events_inverse[,'home_team.home_team_name']= full_events_inverse[,'away_team.away_team_name']
                                                                              full_events_inverse[,'away_team.away_team_name']= invert
                                                                              full_events_inverse[,'is_invert']=TRUE
                                                                              
                                                                              full_events_inverse$away_formation=home_formation
                                                                              full_events_inverse$home_formation=away_formation
                                                                              full_events_inverse$Real_home=Real_away
                                                                              full_events_inverse$Real_away=Real_home
                                                                              full_events_inverse$Full_Home_away=Real_home* -1
                                                                              full_events_inverse$Back_away=Back_h
                                                                              full_events_inverse$Mid_away=Mid_h
                                                                              full_events_inverse$Att_away=Att_h
                                                                              full_events_inverse$S_away=S_h
                                                                              full_events_inverse$Combined_away=Combined_h
                                                                              full_events_inverse$Back_home=Back_a
                                                                              full_events_inverse$Mid_home=Mid_a
                                                                              full_events_inverse$Att_home=Att_a
                                                                              full_events_inverse$S_home=S_a
                                                                              full_events_inverse$Combined_home=Combined_a
                                                                              
                                                                              
                                                                              
                                                                              full_events$away_formation=away_formation
                                                                              full_events$home_formation=home_formation
                                                                              full_events$Back_away=Back_a
                                                                              full_events$Mid_away=Mid_a
                                                                              full_events$Att_away=Att_a
                                                                              full_events$S_away=S_a
                                                                              full_events$Combined_away=Combined_a
                                                                              full_events$Back_home=Back_h
                                                                              full_events$Mid_home=Mid_h
                                                                              full_events$Att_home=Att_h
                                                                              full_events$S_home=S_a
                                                                              full_events$Combined_home=Combined_h
                                                                              full_events=rbind(full_events,full_events_inverse)
                                                                              # adding the event for fine and gray 
                                                                              
                                                                              a=full_events[,'shot.outcome.name']=="NOGoal" | is.na(full_events[,'shot.outcome.name'])
                                                                              b= full_events[,'shot.outcome.name']=="Goal" &  full_events[,'home_team.home_team_name']==full_events[,'possession_team.name']
                                                                              
                                                                              a=a* 0
                                                                              b=b* 1 
                                                                              c=full_events[,'shot.outcome.name']=="Goal" &  full_events[,'away_team.away_team_name']==full_events[,'possession_team.name']
                                                                              c=c* (-1) 
                                                                              Fine_and_Gray_events=a+b+c
                                                                              Fine_and_Gray_events
                                                                              home=Fine_and_Gray_events
                                                                              home[1]=list(home_team_info)
                                                                              away=Fine_and_Gray_events
                                                                              away[1]=list(away_team_info)
                                                                              
                                                                              full_events=cbind(full_events,tibble(Goal_diff=c(0,head(cumsum(Fine_and_Gray_events),-1))))
                                                                              full_events=cbind(full_events,tibble(Goal_diff_abs=abs(full_events[,"Goal_diff"])))
                                                                              full_events=cbind(full_events,Fine_and_Gray_events)
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              
                                                                              # goal dif
                                                                              
                                                                              return(full_events)
                                                                              
                                                                              
                                                                            }



stopCluster(cl)

#Full_events_formation<-arrange(Full_events_formation,match_date)




#### End of preprocessing 
#### -> prédict with F&G
#### -> Proportionality tests 

#one hot encoding the formations 

#save(Full_events_formation,file="Full_data_ligue_simplified_formations_23_05_23.Rdata")
load(file="Full_data_ligue_simplified_formations_23_05_23.Rdata")
# 
correspondance= read.csv(file="correspondance.csv",sep=";")

for (xx in 1: nrow(Full_events_formation)) {
  h=correspondance[,1]==Full_events_formation$home_formation[xx]
  a=correspondance[,1]==Full_events_formation$away_formation[xx]
  a=correspondance[a,3]
  h=correspondance[h,3]
  Full_events_formation$Combined_home[xx]= h
  Full_events_formation$Combined_away[xx]= a
  
}

#save(Full_events_formation,file="Full_data_ligue_simplified_formations_corrected_23_05_23.Rdata")


