#https://www.football-data.co.uk/notes.txt
load_betting_data = function(countries = "eng", seasons = 2017:2022, bet_names_list = c("B365","BW","IW","PS","WH","VC"),bet.types=c("H","A","D")){
  #load data
  bet_data = list()
  index=0
  for(i in seasons){
    for(j in countries){
      index=index+1
      bet_data[[index]] = read.csv(paste0("data/",j,"_",i,".csv")) %>% 
        dplyr::mutate(country=j,season=i) %>% 
        dplyr::select(country,season,date=Date,team.home=HomeTeam, team.away=AwayTeam, starts_with(bet_names_list) )
    }
  }
  
  #transform data: 1 row per betting site and game
  bet_data = rbindlist(bet_data,fill=TRUE) %>% 
    #data management
    dplyr::mutate(date=dmy(date),
                  team.home = recode(team.home,
                                     `Man United`="Manchester Utd",
                                     `Leeds`="Leeds United",
                                     `Leicester`="Leicester City",
                                     `Man City`="Manchester City",
                                     `Newcastle`="Newcastle Utd",
                                     `Norwich`="Norwich City",
                                     `Sheffield United`="Sheffield Utd",
                                     `Stoke`="Stoke City",
                                     `Swansea`="Swansea City",
                                     `Cardiff`="Cardiff City",
                                     `Nott'm Forest`="Nott'ham Forest"),
                  team.away = recode(team.away,
                                     `Man United`="Manchester Utd",
                                     `Leeds`="Leeds United",
                                     `Leicester`="Leicester City",
                                     `Man City`="Manchester City",
                                     `Newcastle`="Newcastle Utd",
                                     `Norwich`="Norwich City",
                                     `Sheffield United`="Sheffield Utd",
                                     `Stoke`="Stoke City",
                                     `Swansea`="Swansea City",
                                     `Cardiff`="Cardiff City",
                                     `Nott'm Forest`="Nott'ham Forest"))  %>% 
    dplyr::mutate(game.id = paste0(date,"_",substr(gsub(" ","",team.home),1,11),"_",substr(gsub(" ","",team.away),1,11))) %>% 
    #transform betting odds
    pivot_longer(cols = starts_with(bet_names_list),names_to="bet_names",values_to="odds") %>% 
    dplyr::mutate(bet_names=stri_replace_all_regex(bet_names,
                                                   pattern=bet_names_list,
                                                   replacement=paste0(bet_names_list,"_"),
                                                   vectorize=FALSE)) %>% 
    separate("bet_names",into=c("bet_names","bet")) %>% 
    filter(bet %in% bet.types) %>% 
    pivot_wider(names_from = "bet",values_from = "odds")
    
  return(bet_data)
}


# #transform data: 1 row per team game and betting site
# d = rbind(bet_data %>% 
#             dplyr::mutate(loc="home") %>% 
#             dplyr::select(country,season,date,loc,team=team.home,opp=team.away,bet_names,win=H,draw=D,loss=A),
#           bet_data %>% 
#             dplyr::mutate(loc="away") %>% 
#             dplyr::select(country,season,date,loc,team=team.away,opp=team.home,bet_names,win=A,draw=D,loss=H))

# bet_data = rbindlist(bet_data,fill=TRUE) %>% 
#   pivot_longer(cols= starts_with(bet_names_list),names_to="bet_names",values_to="odds") %>% 
#   dplyr::mutate(bet_names=stri_replace_all_regex(bet_names,
#                                                  pattern=bet_names_list,
#                                                  replacement=paste0(bet_names_list,"_"),
#                                                  vectorize=FALSE)) %>% 
#   separate("bet_names",into=c("bet_names","bet")) %>% 
#   filter(bet %in% c("H","A","D")) %>% 
#   pivot_wider(names_from = "bet",values_from = "odds") %>% 
#   dplyr::mutate(date=dmy(date)) %>%
#   dplyr::mutate(team.home = recode(team.home,
#                                   `Man United`="Manchester Utd",
#                                   `Leeds`="Leeds United",
#                                   `Leicester`="Leicester City",
#                                   `Man City`="Manchester City",
#                                   `Newcastle`="Newcastle Utd",
#                                   `Norwich`="Norwich City",
#                                   `Sheffield United`="Sheffield Utd",
#                                   `Stoke`="Stoke City",
#                                   `Swansea`="Swansea City",
#                                   `Cardiff`="Cardiff City",
#                                   `Nott'm Forest`="Nott'ham Forest"),
#                 team.away = recode(team.away,
#                                   `Man United`="Manchester Utd",
#                                   `Leeds`="Leeds United",
#                                   `Leicester`="Leicester City",
#                                   `Man City`="Manchester City",
#                                   `Newcastle`="Newcastle Utd",
#                                   `Norwich`="Norwich City",
#                                   `Sheffield United`="Sheffield Utd",
#                                   `Stoke`="Stoke City",
#                                   `Swansea`="Swansea City",
#                                   `Cardiff`="Cardiff City",
#                                   `Nott'm Forest`="Nott'ham Forest")) %>% 
#   
# #transform data: 1 row per team game and betting site
# d = rbind(bet_data %>% 
#             dplyr::mutate(loc="home") %>% 
#             dplyr::select(country,season,date,loc,team=team.home,opp=team.away,bet_names,win=H,draw=D,loss=A),
#           bet_data %>% 
#             dplyr::mutate(loc="away") %>% 
#             dplyr::select(country,season,date,loc,team=team.away,opp=team.home,bet_names,win=A,draw=D,loss=H))
# 
# 
# 
