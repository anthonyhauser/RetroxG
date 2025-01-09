load_stat_data = function(countries = "eng", seasons = 2022){
  countries = toupper(countries)
  
  #first dataset: by game
  stat_data_game <- load_match_results(country = countries, gender = c("M"), season_end_year = seasons+1, tier = "1st") %>% 
    #fb_match_results(country = countries, gender = c("M"), season_end_year = seasons+1, tier = "1st") %>% #load_match_results
    dplyr::mutate(season=Season_End_Year-1) %>% 
    rename_all(tolower) %>% 
    dplyr::rename(team.home=home,team.away=away,goals.home=homegoals,goals.away=awaygoals,xg.home=home_xg,xg.away=away_xg) %>% 
    dplyr::mutate(game.id = paste0(date,"_",substr(gsub(" ","",team.home),1,11),"_",substr(gsub(" ","",team.away),1,11)),
                  country = tolower(country),
                  result = case_when(goals.home>goals.away ~ "H",
                                     goals.home<goals.away ~ "A",
                                     goals.home==goals.away ~ "D")) %>% 
    dplyr::select(country,competition_name,season,wk,date, game.id,result,
                  team.home,team.away,goals.home,goals.away,xg.home,xg.away) %>% 
    filter(!is.na(goals.home))#remove postponed matches
  
  #second dataset: by team
  # d = stat_data_game %>% mutate(across(everything(),function(x) as.character(x))) %>% 
  #       pivot_longer(cols = ends_with(c("home","away")),names_to = c("var","loc"),values_to = "values",names_sep = "\\.") %>% 
  #       pivot_wider(id_cols=c("country","game.id","date","loc","result"),names_from="var",values_from = "values") %>% 
  #       dplyr::mutate(game_rank = dense_rank(date), across(contains(c("goals","xg")),function(x) as.numeric(x)))
  # stat_data_team = full_join(d %>% dplyr::select(game.id,country,loc,game_rank.team=game_rank,name.team=team,goals.team=goals,xg.team=xg,result),
  #                            d %>% dplyr::select(game.id,country,game_rank_opp=game_rank,name.opp=team,goals.opp=goals,xg.opp=xg),
  #                            by=c("country","game.id"),multiple="all") %>% 
  #                    filter(name.team!=name.opp)
  
  #simpler way to do it
  stat_data_team2 = rbind(stat_data_game %>% 
              dplyr::mutate(loc="home") %>% 
              dplyr::select(country,competition_name,season,wk,game.id,date,loc,result,
                            name_team=team.home,name_opp=team.away,goals.team=goals.home,goals.opp=goals.away,xg.team=xg.home,xg.opp=xg.away),
            stat_data_game %>%
              dplyr::mutate(loc="away") %>% 
              dplyr::select(country,competition_name,season,wk,game.id,date,loc,result,
                            name_team=team.away,name_opp=team.home,goals.team=goals.away,goals.opp=goals.home,xg.team=xg.away,xg.opp=xg.home)) %>% 
    as_tibble() %>% 
    group_by(country,competition_name,season,name_team) %>% 
    dplyr::mutate(game_rank = dense_rank(date)) %>% ungroup() %>% 
    group_by(country,competition_name,name_team) %>% 
    dplyr::mutate(game_rank_all_season = dense_rank(date)) %>% ungroup()

  stat_data = list(game = stat_data_game,
                   team = stat_data_team2)
  
  return(stat_data)
}


# countries = toupper(countries)
# stat_data <- load_match_results(country = countries, gender = c("M"), season_end_year = season+1, tier = "1st") %>% 
#   dplyr::mutate(season=Season_End_Year-1) %>% 
#   rename_all(tolower)
# 
# d = rbind(stat_data %>% 
#             dplyr::mutate(loc="home") %>% 
#             dplyr::select(country,competition_name,season,wk,date,loc,
#                           team=home,opp=away,goals_scored=homegoals,goals_received=awaygoals,xg_scored=home_xg,xg_received=away_xg),
#           stat_data %>%
#             dplyr::mutate(loc="away") %>% 
#             dplyr::select(country,competition_name,season,wk,date,loc,
#                           team=away,opp=home,goals_scored=awaygoals,goals_received=homegoals,xg_scored=away_xg,xg_received=home_xg)) %>% 
#   dplyr::mutate(country=tolower(country),
#                 date=ymd(date),
#                 xg_diff=xg_scored-xg_received,
#                 goals_diff=goals_scored-goals_received,
#                 xg_ratio=goals_scored/xg_scored,
#                 result = case_when(
#                   goals_scored < goals_received ~ "loss",
#                   goals_scored == goals_received ~ "draw",
#                   goals_scored > goals_received ~ "win")) %>% 
#   group_by(country,competition_name,season,team) %>% 
#   filter(!is.na(goals_scored)) %>% #remove games without goals scored (postponed games)
#   dplyr::mutate(game_rank = dense_rank(date)) %>% ungroup() %>% 
#   dplyr::mutate(game.id = if_else(loc=="home",
#                                   paste0(season,"_",wk,"_",gsub(" ","",team),"_",gsub(" ","",opp)),
#                                   paste0(season,"_",wk,"_",gsub(" ","",opp),"_",gsub(" ","",team))))