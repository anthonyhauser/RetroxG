#' ---
#' title: "Analysing football data"
#' author: "Anthony Hauser"
#' date: "`r Sys.Date()`"
#' output:
#'    html_document:
#'      code_folding : hide
#'      number_sections: true
#'      toc: true
#'      toc_float:
#'        collapsed: true
#'        smooth_scroll: true
#' highlight: pygments
#' theme: cosmo
#' ---

# rmarkdown::render("main.R")


#setup
code_root_path = paste0(getwd(),"/")
print(code_root_path)
source("R/setup.R")

#' # Aim
#' - drivers of next team to score: team, season, home, score, players, accumulated xg
#' - difference xG and goals: drivers. Ex: team, season, home
#' - check whether low or high probability of xg are biased
#' - see whether goal follows a binomial distribution of probability xg
#' - correlation between xg and between goals
#' 

##################################################################################################################################
#statsBomb xG data
if(FALSE){
  #Competions with available xg
  Comps <- FreeCompetitions()
  Comps = Comps %>% filter(season_name=="2015/2016",country_name!="Europe",
                           country_name=="England")
  #Matches
  Matches <- FreeMatches(Comps)
  
  #Events for each match
  StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
  StatsBombData = allclean(StatsBombData)
  saveRDS(StatsBombData, file="savepoint/StatsBombData_2015_EPL.RDS")
  
  #xG
  xg_df = StatsBombData %>% 
    filter(type.name=="Shot")
  saveRDS(xg_df, file="savepoint/StatsBombxG_2015_EPL.RDS")
}
#load StatsBomb xG data
d_statsbomb = readRDS("savepoint/StatsBombxG_2015_EPL.RDS") %>%
  dplyr::select(match_id, team=team.name, period,minute,second,
                player=player.name,player.id,position.name,
                xG=shot.statsbomb_xg,shot.technique.name,shotType=shot.body_part.name, shot.outcome.name,situation=shot.type.name,
                X=location.x,Y=location.y,player.name.GK,DistToGoal,DistToKeeper,AngleToGoal) %>% 
  left_join(Matches %>% dplyr::select(match_id,date=match_date,home_team=home_team.home_team_name,away_team=away_team.away_team_name)) %>% 
  dplyr::mutate(date=lubridate::ymd(as.Date(date)))

d_statsbomb %>% filter(situation!="Penalty") %>% 
  dplyr::mutate(med=median(xG)) %>% ungroup() %>% 
  ggplot(aes(x=xG))+
  geom_histogram()

d_statsbomb %>% filter(situation!="Penalty") %>% 
  ggplot(aes(X, Y, fill= xG)) + 
  geom_tile()

d_statsbomb %>% filter(situation!="Penalty",Y>35,Y<45) %>% 
  ggplot(aes(x=X, y=xG,col=situation)) + 
  geom_point(alpha=0.1)

d_statsbomb %>% filter(situation=="Open Play",Y>35,Y<45) %>% 
  dplyr::mutate(Y=as.character(round(abs(Y-40),0))) %>% 
  ggplot(aes(x=X, y=xG,col=Y)) + 
  geom_point(alpha=0.1)+
  facet_wrap(.~shotType)


##################################################################################################################################
#Understat xG data
#c("eng","ger","ita","fra","esp")
d1 = load_stat_data(countries = "eng", seasons = 2017:2023)
start_years=2017:2023
leagues=c("EPL", "La liga", "Bundesliga", "Serie A", "Ligue 1")

if(FALSE){
  for(league in leagues){
    for(year in start_years){
      d = understat_league_season_shots(league=league, season_start_year=year)
      saveRDS(d,paste0("data/understat_shots_",league,"_",year,".RDS"))
      print(year)
    }
  }
}

i=0
d=list()
for(league in leagues){
  for(year in start_years){
    i=i+1
    d[[i]] = readRDS(paste0("data/understat_shots_",league,"_",year,".RDS"))
  }
}
d_understat = rbindlist(d)
d_understat %>% 
  group_by(league,,season,match_id) %>% 
  dplyr::summarise(n=n(),.groups = "drop_last") %>% 
  dplyr::summarise(n=mean(n)) %>% ungroup() %>%
  dplyr::mutate(season=as.numeric(season)) %>% 
  ggplot(aes(x=season,y=n,col=league)) +
  geom_point()+
  geom_line()


d_understat %>% filter(result!="OwnGoal",situation!="Penalty") %>% 
  group_by(league,season) %>% 
  dplyr::mutate(med=median(xG)) %>% ungroup() %>% 
  ggplot(aes(x=xG))+
  geom_histogram()+
  facet_grid(season~league)

d_understat %>% filter(result!="OwnGoal",situation!="Penalty") %>% 
  ggplot(aes(x=xG))+
  geom_histogram()

d_understat %>% filter(result!="OwnGoal",situation!="Penalty",year==2023) %>% 
ggplot(aes(X, Y, fill= xG)) + 
  geom_tile()

d_understat %>% filter(result!="OwnGoal",situation!="Penalty",year==2023,league=="EPL",Y>0.45,Y<0.55) %>% 
  ggplot(aes(x=X, y=xG,col=situation)) + 
  geom_point(alpha=0.1)

d_understat %>% filter(result!="OwnGoal",situation=="Open Play",year==2023,league=="EPL",Y>0.45,Y<0.55) %>% 
  dplyr::mutate(Y=as.character(round(abs(Y-0.5),2))) %>% 
  ggplot(aes(x=X, y=xG,col=Y)) + 
  geom_point(alpha=0.1)+
  facet_wrap(.~shotType)
##################################################################################################################################
d_understat = readRDS("data/understat_shots_EPL_2015.RDS")  %>%
  as_tibble() %>% 
  dplyr::mutate(date=lubridate::ymd(as.Date(date)),
                minute=as.numeric(minute),
                home_team = case_when(home_team=="Swansea"~"Swansea City",
                                      home_team=="Bournemouth"~"AFC Bournemouth",
                                      home_team=="Norwich"~"Norwich City",
                                      home_team=="Leicester"~"Leicester City",
                                      home_team=="Stoke"~"Stoke City",
                                      home_team=="Tottenham"~"Tottenham Hotspur",
                                      home_team=="West Ham"~"West Ham United",
                                      TRUE ~ home_team),
                away_team = case_when(away_team=="Swansea"~"Swansea City",
                                      away_team=="Bournemouth"~"AFC Bournemouth",
                                      away_team=="Norwich"~"Norwich City",
                                      away_team=="Leicester"~"Leicester City",
                                      away_team=="Stoke"~"Stoke City",
                                      away_team=="Tottenham"~"Tottenham Hotspur",
                                      away_team=="West Ham"~"West Ham United",
                                      TRUE ~ away_team))
df1=d_understat
df2=d_statsbomb

player_df1 = df1 %>%
  dplyr::mutate(team=if_else(h_a=="h",home_team,away_team)) %>% 
  dplyr::select(team,player) %>% unique() %>% 
  dplyr::mutate(player=gsub("&#039;", "'", player)) %>% 
  arrange(team,player)

player_df2 = df2 %>%
  dplyr::select(team,player) %>% unique() %>% 
  arrange(team,player)

has_consecutive_match <- function(name1, name2, n = 4) {
  substrings <- sapply(1:(nchar(name1) - n + 1), function(i) substr(name1, i, i + n - 1))
  #any(sapply(substrings, function(sub) grepl(sub, name2)))
  sum(sapply(substrings, function(sub) grepl(sub, name2)))/length(substrings)
}

# Perform fuzzy matching within each team
player_tb <- player_df1 %>%
  inner_join(player_df2, by = "team", suffix = c(".df1", ".df2"),relationship = "many-to-many") %>%  # Join by team
  rowwise() %>%
  filter(has_consecutive_match(player.df1, player.df2, n = 4) ) %>%  # Filter pairs with 4 consecutive chars
  mutate(string_distance = stringdist(player.df1, player.df2, method = "jw")) %>%  # Compute string distance
  group_by(team, player.df1) %>%  # Group by team and df1 player
  slice_min(order_by = string_distance, n = 1) %>%  # Select the closest match
  ungroup() %>% 
  right_join(player_df1,by=c("team"="team","player.df1"="player"))

player_tb1 <- player_df1 %>%
  inner_join(player_df2, by = "team", suffix = c(".df1", ".df2"),relationship = "many-to-many") %>%  # Join by team
  rowwise() %>%
  mutate(string_distance=has_consecutive_match(player.df1, player.df2, n = 4)) %>%  # Filter pairs with 4 consecutive chars
  group_by(team, player.df1) %>%  # Group by team and df1 player
  slice_max(order_by = string_distance, n = 1, with_ties = FALSE) %>%  # Select the closest match
  ungroup() %>% 
  right_join(player_df1,by=c("team"="team","player.df1"="player")) %>% 
  dplyr::mutate(player.df2=if_else(string_distance==0,NA,player.df2))

player_tb2 <- player_df1 %>%
  inner_join(player_df2, by = "team", suffix = c(".df1", ".df2"),relationship = "many-to-many") %>%  # Join by team
  rowwise() %>%
  mutate(string_distance=has_consecutive_match(player.df2, player.df1, n = 4)) %>%  # Filter pairs with 4 consecutive chars
  group_by(team, player.df2) %>%  # Group by team and df1 player
  slice_max(order_by = string_distance, n = 1, with_ties = FALSE) %>%  # Select the closest match
  ungroup() %>% 
  right_join(player_df2,by=c("team"="team","player.df2"="player")) %>% 
  dplyr::mutate(player.df1=if_else(string_distance==0,NA,player.df1))

d=rbind(player_tb1 %>% dplyr::select(player.df1,player.df2,string_distance),
      player_tb2 %>% dplyr::select(player.df1,player.df2,string_distance)) %>% #filter(player.df1=="Ander Herrera")
  group_by(player.df1,player.df2) %>% 
  slice_max(order_by = string_distance, n = 1, with_ties = FALSE) %>% ungroup() %>% 
  group_by(player.df1) %>% 
  dplyr::mutate(n1=n()) %>% ungroup() %>% 
  group_by(player.df2) %>% 
  dplyr::mutate(n2=n()) %>% ungroup()

d %>% filter(n1>1|n2>1) %>% View()


player_df1 %>% dim()
player_df2 %>% dim()
player_tb %>% dim()
View(player_tb)


##################################################################################################################################
min.season=2017
max.season=2023

#team info
d_teams = d1$team %>% 
  filter(season>=min.season,season<=max.season) %>% 
  dplyr::select(season,game.id,name_team,name_opp,game_rank) %>% 
  dplyr::mutate(id.team=as.numeric(factor(name_team)),
                 id.season = season-min.season + 1,
                 id.round = game_rank + 38 * (id.season-1))

d_teams_seasons = d_teams %>% dplyr::select(name_team,season) %>% unique() %>%
  group_by(name_team) %>%
  dplyr::mutate(season2=lag(season)) %>% ungroup() %>% arrange(name_team) %>%
  dplyr::mutate(initialize_elo = case_when(season==min.season ~ 1,#init teams at min.season
                                           is.na(season2) | season!=season2+1 ~ 2,#init newcomers
                                           TRUE ~ 3)) #no init because not their first season
#summary
d_teams %>%
  group_by(name_team,id.team) %>%
  dplyr::summarize(first_season = min(season),
                last_season = max(season),
                n_round = n()) %>% arrange(id.team)

#games
d_games = d1$game %>% dplyr::select(season,date,game.id,result,team.home,team.away) %>% 
  filter(season>=min.season,season<=max.season) %>% 
  dplyr::mutate(result=factor(result,levels=c("H","D","A"))) %>% 
  #add id round and id team
  left_join(d_teams %>% dplyr::select(team.home=name_team,id.team.home=id.team,game.id,id.round.home=id.round,id.season.home=id.season,game_rank.home=game_rank),
            by=c("game.id","team.home")) %>% 
  left_join(d_teams %>% dplyr::select(team.away=name_team,id.team.away=id.team,game.id,id.round.away=id.round,id.season.away=id.season,game_rank.away=game_rank),
            by=c("game.id","team.away")) %>% 
  #add initialize season info
  left_join(d_teams_seasons %>% dplyr::select(team.home=name_team,season,initialize_elo_home=initialize_elo) %>% 
              dplyr::mutate(game_rank.home=1), by=c("season","team.home","game_rank.home")) %>% 
  left_join(d_teams_seasons %>% dplyr::select(team.away=name_team,season,initialize_elo_away=initialize_elo) %>% 
              dplyr::mutate(game_rank.away=1), by=c("season","team.away","game_rank.away")) %>% 
  dplyr::mutate(initialize_elo_home = replace_na(initialize_elo_home,0),
                initialize_elo_away = replace_na(initialize_elo_away,0)) %>% 
  arrange(date)

d_games_bet = d %>%
  inner_join(d_games %>% dplyr::select(game.id),by="game.id") %>% 
  dplyr::mutate(id.season = season-min.season + 1,
                expected_gain = 1/(1/H+1/D+1/A),
                p_H = 1/H*expected_gain,
                p_D = 1/D*expected_gain,
                p_A = 1/A*expected_gain) %>% 
  arrange(id.season)

d_games_bet %>% filter(is.na(H))

##################################################################################################################################
###################################################
#mod1
data_list= list(N = dim(d_games)[1], #number of games
                N_rounds = (max.season-min.season+1) * 38, #maximum number of games by team
                N_team = max(d_games$id.team.home),
                
                team_id_home = d_games$id.team.home,
                team_id_away = d_games$id.team.away,
                round_id_home = d_games$id.round.home,
                round_id_away = d_games$id.round.away,
                results =  model.matrix(formula(paste0("~ result-1")), d_games) %>% as.matrix(), #1:H, 2:D, 3:A
                first_round = d_games %>% 
                  dplyr::select(id.team.home, id.team.away, id.round.home, id.round.away) %>% 
                  pivot_longer(cols=everything(), names_to=c(".value","var1"), names_pattern="(id\\.\\w+)\\.(\\w+)") %>% 
                  group_by(id.team) %>% 
                  dplyr::summarise(first_round=min(id.round),.groups="drop") %>% 
                  arrange(id.team) %>% pull(first_round),
                
                K=1,
                
                inference=1)


d_games %>% filter((game_rank.home==38 & game_rank.away!=38) | (game_rank.home!=38 & game_rank.away==38))

team_season = lapply(unique(d_teams$season),function(x) d_teams %>% filter(season==x) %>% pull(id.team) %>% unique())
team_stay_season = matrix(unlist(lapply(1:(length(team_season)-1),function(x) sort(intersect(team_season[[x]],team_season[[x+1]])))),nrow=17,byrow = FALSE)

data_list= list(N = dim(d_games)[1], #number of games
                N_rounds = (max.season-min.season+1) * 38, #maximum number of games by team
                N_team = max(d_games$id.team.home),
                N_season = max.season-min.season+1,
                
                team_id_home = d_games$id.team.home,
                team_id_away = d_games$id.team.away,
                round_id_home = d_games$id.round.home, #continues over season (unlike game_rank.home)
                round_id_away = d_games$id.round.away,
                #season_id = d_games$id.season.home,
                season_id_home = d_games$id.season.home,
                season_id_away = d_games$id.season.away,
                results =  model.matrix(formula(paste0("~ result-1")), d_games) %>% as.matrix(), #1:H, 2:D, 3:A
                results_1row = as.numeric(factor(d_games$result,levels=c("H","D","A"),labels=1:3)),
                initialize_elo_home = d_games$initialize_elo_home, #0:no first game of the season, 1:starts at min.season, 2:newcomer, 3:already there
                initialize_elo_away = d_games$initialize_elo_away,
               
                K=1,
                
                inference=1)

mod1_cmdstan <- cmdstan_model("stan/mod1_dev1.stan")
fit1_k1 <- mod1_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
)

data_list$K=2
fit1_k2 <- mod1_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

# data_list$K=5
# fit1_k5 <- mod1_cmdstan$sample(
#   init=0,
#   adapt_delta=0.99,
#   data = data_list,
#   chains = 4, 
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# shinystan::launch_shinystan(fit1_k5)

data_list$K=0.1
fit1_k01 <- mod1_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

data_list$K=0
fit1_k0 <- mod1_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

data_list$p_K=1
mod2_dev1_cmdstan <- cmdstan_model("stan/mod2_dev1.stan")
fit2_dev1 <- mod2_dev1_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

d=rbind(fit1_k1$summary(variables = c("elo"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=1),
        fit1_k2$summary(variables = c("elo"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K=2),
        fit1_k01$summary(variables = c("elo"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K=0.1),
        fit1_k0$summary(variables = c("elo"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K=0),
        fit2_dev1$summary(variables = c("elo"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K="free"))%>% 
  dplyr::rename(q2.5=`2.5%`,q50=`50%`,q97.5=`97.5%`) %>% 
  as.data.frame() %>% 
  tidyr::extract(variable,into=c("var","id.team","id.round"),
                 regex = paste0('(\\w.*)\\[',paste(rep("(.*)",2),collapse='\\,'),'\\]'), remove = T) %>% 
  dplyr::mutate(id.team = as.numeric(id.team),
                id.round = as.numeric(id.round),
                K=as.character(K)) %>% 
  left_join(d_teams %>% dplyr::select(id.team,name_team) %>% unique(), by="id.team") %>% 
  left_join(d_games %>% dplyr::select(id.round=id.round.home,id.season=id.season.home,game_rank=game_rank.home) %>% unique(), by="id.round") %>% 
  group_by(K) %>% 
  dplyr::mutate(max.abs.elo = if_else(K==0,1,max(abs(mean),na.rm=TRUE))) %>% ungroup() %>% 
  dplyr::mutate(mean=mean/max.abs.elo,
                q2.5=q2.5/max.abs.elo,
                q50=q50/max.abs.elo,
                q97.5=q97.5/max.abs.elo)


rbind(fit1_k1$summary(variables = c("alpha_draw","alpha_home"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=1),
      fit1_k2$summary(variables = c("alpha_draw","alpha_home"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=2),
      fit1_k0$summary(variables = c("alpha_draw","alpha_home"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=0),
      fit1_k01$summary(variables = c("alpha_draw","alpha_home"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=0.1),
      fit2_dev1$summary(variables = c("alpha_draw","alpha_home","K"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K="free"))%>% 
  dplyr::rename(q2.5=`2.5%`,q50=`50%`,q97.5=`97.5%`)


d %>% 
  filter(id.season==2,id.team %in% team_season[[2]]) %>% 
  ggplot(aes(x=game_rank,y=mean,col=K)) +
  geom_point()+
  geom_line() +
  theme_bw()+
  facet_wrap(.~name_team)+
  theme(legend.position = "bottom")

rbind(fit1_k1$summary(variables = c("loglik"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K=1),
        fit1_k2$summary(variables = c("loglik"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K=2),
        fit1_k01$summary(variables = c("loglik"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K=0.1),
        fit1_k0$summary(variables = c("loglik"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
          dplyr::mutate(K=0),
      fit2_dev1$summary(variables = c("loglik"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K="free"))%>% 
  dplyr::rename(q2.5=`2.5%`,q50=`50%`,q97.5=`97.5%`) %>% 
  as.data.frame()

d_games_bet %>% 
  dplyr::select(game.id,id.season,bet_names,result,H=p_H,D=p_D,A=p_A) %>% 
  pivot_longer(cols=c("H","D","A"),names_to = "loc",values_to = "p") %>% 
  filter(result==loc) %>% 
  group_by(bet_names,id.season) %>% 
  dplyr::summarise(mean=sum(log(p)))
  
rbind(fit1_k1$summary(variables = c("loglik_season"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=1),
      fit1_k2$summary(variables = c("loglik_season"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=2),
      fit1_k01$summary(variables = c("loglik_season"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=0.1),
      fit1_k0$summary(variables = c("loglik_season"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K=0),
      fit2_dev1$summary(variables = c("loglik_season"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
        dplyr::mutate(K="free")) %>% 
  dplyr::rename(q2.5=`2.5%`,q50=`50%`,q97.5=`97.5%`) %>% 
  as.data.frame() %>% 
  tidyr::extract(variable,into=c("var","id.season"),
                 regex = paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
  dplyr::mutate(id.season = as.numeric(id.season),
                K=as.character(K)) %>%
  rbind(d_games_bet %>% 
          dplyr::select(game.id,id.season,bet_names,result,H=p_H,D=p_D,A=p_A) %>% 
          pivot_longer(cols=c("H","D","A"),names_to = "loc",values_to = "p") %>% 
          filter(result==loc) %>% 
          group_by(bet_names,id.season) %>% 
          dplyr::summarise(mean=sum(log(p)),.groups="drop") %>% 
          dplyr::mutate(q2.5=NA,q97.5=NA,q50=NA,var="loglik_season") %>% rename(K=bet_names)) %>% 
  ggplot(aes(x=id.season,y=mean,ymin=q2.5,ymax=q97.5,col=K)) +
  geom_line()+
  geom_point() +
  theme_bw()

#add random effect on alpha_home (by season, or by team, or by both)
###################################################
#mod2
#check that last game of the season is the same for both home and away teams
d_games %>% filter((game_rank.home==38 & game_rank.away!=38) | (game_rank.home!=38 & game_rank.away==38))

team_season = lapply(unique(d_teams$season),function(x) d_teams %>% filter(season==x) %>% pull(id.team) %>% unique())
team_stay_season = matrix(unlist(lapply(1:(length(team_season)-1),function(x) sort(intersect(team_season[[x]],team_season[[x+1]])))),nrow=17,byrow = FALSE)

data_list= list(N = dim(d_games)[1], #number of games
                N_rounds = (max.season-min.season+1) * 38, #maximum number of games by team
                N_team = max(d_games$id.team.home),
                N_season = max.season-min.season+1,
                N_K = 10,#max(ceiling(d_games$id.round.home/ 19)),
                #N_team_season = 17,
                
                team_id_home = d_games$id.team.home,
                team_id_away = d_games$id.team.away,
                round_id_home = d_games$id.round.home, #continues over season (unlike game_rank.home)
                round_id_away = d_games$id.round.away,
                #season_id = d_games$id.season.home,
                season_id_home = d_games$id.season.home,
                season_id_away = d_games$id.season.away,
                results =  model.matrix(formula(paste0("~ result-1")), d_games) %>% as.matrix(), #1:H, 2:D, 3:A
                last_season_game = as.numeric(d_games$game_rank.home==38),
                initialize_elo_home = d_games$initialize_elo_home, #0:no first game of the season, 1:starts at min.season, 2:newcomer, 3:already there
                initialize_elo_away = d_games$initialize_elo_away,
                K_id = ceiling((1+(d_games$id.round.home-1) %% 38)/4), #ceiling(d_games$id.round.home/ 19),
                
                #id_team_season = team_stay_season,
                
                p_K=1/1,
                gamma=1,
                
                inference=1)

mod2_cmdstan <- cmdstan_model("stan/mod2.stan")
#gamma=1
fit2 <- mod2_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
fit2$summary(variables = c("alpha_draw","alpha_home"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE))

fit2$summary(variables = c("K"), "mean"=~mean((.x)),~quantile((.x), probs = c(0.025,0.5, 0.975),na.rm=TRUE)) %>% 
  dplyr::rename(q2.5=`2.5%`,q50=`50%`,q97.5=`97.5%`) %>% 
    as.data.frame() %>% 
    tidyr::extract(variable,into=c("var","id.K"),
                   regex = paste0('(\\w.*)\\[',paste(rep("(.*)",1),collapse='\\,'),'\\]'), remove = T) %>% 
    dplyr::mutate(id.K = as.numeric(id.K)) %>% 
  ggplot(aes(x=id.K,y=q50,ymin=q2.5,ymax=q97.5)) +
  geom_ribbon(alpha=0.1)+
  geom_point()+
  geom_line() +
  theme_bw() +
  scale_y_continuous(limits=c(0,1))

#save model results
fit2$save_output_files(dir = paste0(code_root_path,"/results/stan_chains"),
                       basename = paste0("chain_mod2","_","gamma","_",data_list$gamma))

#gamma=0
data_list$gamma=0
fit2 <- mod2_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
#save model results
fit2$save_output_files(dir = paste0(code_root_path,"/results/stan_chains"),
                       basename = paste0("chain_mod2","_","gamma","_",data_list$gamma))

#gamma free
mod3_cmdstan <- cmdstan_model("stan/mod3.stan")
fit3 <- mod3_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
#save model results
fit3$save_output_files(dir = paste0(code_root_path,"/results/stan_chains"),
                       basename = paste0("chain_mod3"))

#gamma free, init new team
mod4_cmdstan <- cmdstan_model("stan/mod4_1.stan")
fit4 <- mod4_cmdstan$sample(
  adapt_delta=0.8,
  data = data_list,
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)
#save model results
fit4$save_output_files(dir = paste0(code_root_path,"/results/stan_chains"),
                       basename = paste0("chain_mod4"))

#load csv files
files = list.files(paste0(code_root_path,"/results/stan_chains"))
csv_files= files[grepl(gsub("\\+","\\\\+",paste0("chain_mod2","_","gamma","_",1)),files)]
fit <-as_cmdstan_fit(paste0(code_root_path,"/results/stan_chains/",csv_files)[1:4])
fit=fit4
#diagnostic
fit$diagnostic_summary()

#effect of home game and draw
fit$summary(variables = c("alpha_home","alpha_draw"),
             ~mean(exp(.x)),~quantile(exp(.x), probs = c(0.025, 0.975),na.rm=TRUE))
#K
fit$summary(variables = c("K","elo_new","sigma_gamma","gamma0"),
             "mean",~quantile(.x, probs = c(0.025,0.5, 0.975),na.rm=TRUE))

d=fit$draws()[,,]
n_iter_per_chain = d[,1,1] %>% length()
post_K = as.data.frame(ftable(d[,,dimnames(d)[[3]]=="K"])) %>%
          dplyr::select(K=Freq)
post_K %>% mutate(data="posterior") %>% 
  rbind(data.frame(K=rexp(1000000,data_list$p_K),data="prior")) %>% 
ggplot(aes(x=K, fill=data)) +
  geom_density(alpha=0.4) +
  coord_cartesian(xlim=c(0,2))+
  theme_bw()

post_sigma = as.data.frame(ftable(d[,,dimnames(d)[[3]]=="sigma_gamma[1]"])) %>%
  dplyr::select(sigma=Freq)
post_sigma %>% mutate(data="posterior") %>% 
  rbind(data.frame(sigma=abs(rnorm(1000000,0,1)),data="prior")) %>% 
  ggplot(aes(x=sigma, fill=data)) +
  geom_density(alpha=0.4) +
  coord_cartesian(xlim=c(0,2))+
  theme_bw()
  
#ELO
elo_team = rbindlist(list(fit$summary(variables = c("elo"),
                        "mean",~quantile(.x, probs = c(0.025, 0.975),na.rm=TRUE)) %>% 
    tidyr::separate(col=variable,into=c("var","id.team","id.round"),sep="\\,|\\[|\\]") %>% 
    dplyr::mutate(id.team = as.numeric(id.team),
                  id.round = as.numeric(id.round)) %>% 
    dplyr::select(id.team,id.round,elo.mean=mean,elo.lwb=`2.5%`,elo.upb=`97.5%`) %>% 
    right_join(d_teams %>% dplyr::select(season,game.id,name_team,name_opp,game_rank,id.team,id.round),
               by=c("id.team","id.round")),
  fit$summary(variables = c("season_elo"),
              "mean",~quantile(.x, probs = c(0.025, 0.975),na.rm=TRUE)) %>% 
    tidyr::separate(col=variable,into=c("var","id.team","id.season"),sep="\\,|\\[|\\]") %>% 
  dplyr::mutate(id.team = as.numeric(id.team),
                id.season = as.numeric(id.season),
                game_rank=39) %>% 
  dplyr::select(id.team,id.season,game_rank,elo.mean=mean,elo.lwb=`2.5%`,elo.upb=`97.5%`) %>% 
  right_join(d_teams %>% dplyr::select(season,id.season,name_team,id.team) %>% unique(),
             by=c("id.team","id.season"))),
  fill=TRUE)

order_team = elo_team %>% 
  filter(season==2016,!is.na(elo.mean)) %>% 
  group_by(name_team) %>% 
  #dplyr::summarise(elo.mean=mean(elo.mean)) %>% ungroup() %>% 
  slice_max(game_rank) %>% ungroup() %>% 
  arrange(-elo.mean) %>% pull(name_team)

elo_team %>% 
  filter(season==2016,!is.na(elo.mean)) %>% 
  dplyr::mutate(name_team=factor(name_team,levels=order_team)) %>% 
  ggplot(aes(x=game_rank,y=elo.mean)) +
  geom_ribbon(aes(ymin=elo.lwb,ymax=elo.upb),alpha=0.1) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=0) +
  facet_wrap(.~name_team,ncol=3) + 
  theme_bw()

fit$summary(variables = c("gamma"),
            "mean",~quantile(.x, probs = c(0.025, 0.975),na.rm=TRUE)) %>% 
  tidyr::separate(col=variable,into=c("var","id.team","id.season"),sep="\\,|\\[|\\]") %>%
  dplyr::mutate(id.team = as.numeric(id.team),
                id.season = as.numeric(id.season)) %>% 
  dplyr::select(id.team,id.season,mean=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  left_join(d_teams %>% dplyr::select(season,id.season,name_team,id.team) %>% unique()) %>%
  filter(!is.na(lwb)) %>% 
  ggplot(aes(x=name_team,y=mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=lwb,ymax=upb)) +
  geom_hline(yintercept = 0)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

data_list$N_rounds


p1=lapply(1:(dim(d_games)[1]),function(x) as.data.frame(ftable(d[,,dimnames(d)[[3]]==paste0("p[",as.numeric(x),",1]")])) %>% 
           dplyr::mutate(row_number=x,iter=row_number()))
p1=rbindlist(p1)
p1=d_games %>% 
  dplyr::mutate(row_number=row_number()) %>% 
  left_join(p1,by="row_number")

p1 %>% filter(id.team.home==1,iter<200,id.round.home>=36) %>% 
  ggplot(aes(x=id.round.home,y=Freq,group=iter)) +
  geom_vline(xintercept = 38,lty=2) +
  geom_line(alpha=0.05) +
  theme_bw()


d_games %>% filter(season==2016,game_rank.home==1)
cbind(as.data.frame(ftable(d[,,dimnames(d)[[3]]=="gamma[1,1]"])) %>% 
  dplyr::select(gamma1=Freq),
  as.data.frame(ftable(d[,,dimnames(d)[[3]]=="gamma[10,1]"])) %>% 
    dplyr::select(gamma2=Freq)) %>% 
  ggplot(aes(x=gamma1,y=gamma2)) +
  geom_point()


elo_team %>% 
  ggplot(aes(x=id.round,y=elo.mean)) +
  geom_ribbon(aes(ymin=elo.lwb,ymax=elo.upb),alpha=0.1) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=0) +
  facet_wrap(.~name_team,ncol=3) + 
  theme_bw()

d=rbind(fit$summary(variables = c("p"),
            "mean",~quantile(.x, probs = c(0.025, 0.975),na.rm=TRUE)) %>% 
  tidyr::separate(col=variable,into=c("var","row_number","bet"),sep="\\,|\\[|\\]") %>% 
  filter(bet=="1") %>% 
  dplyr::mutate(row_number = as.numeric(row_number)) %>% 
  dplyr::select(row_number,mean=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  inner_join(d_games %>% dplyr::select(season,name_team=team.home,id.round=id.round.home) %>% 
               dplyr::mutate(row_number=row_number()),
             by=c("row_number")),
fit$summary(variables = c("p"),
            "mean",~quantile(.x, probs = c(0.025, 0.975),na.rm=TRUE)) %>% 
  tidyr::separate(col=variable,into=c("var","row_number","bet"),sep="\\,|\\[|\\]") %>% 
  filter(bet=="3") %>% 
  dplyr::mutate(row_number = as.numeric(row_number)) %>% 
  dplyr::select(row_number,mean=mean,lwb=`2.5%`,upb=`97.5%`) %>% 
  inner_join(d_games %>% dplyr::select(season,name_team=team.away,id.round=id.round.away) %>% 
               dplyr::mutate(row_number=row_number()),
             by=c("row_number"))) %>% 
  arrange(name_team,id.round)

d %>% 
  ggplot(aes(x=id.round,y=mean)) +
  geom_ribbon(aes(ymin=lwb,ymax=upb),alpha=0.1) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=0) +
  facet_wrap(.~name_team,ncol=3) + 
  theme_bw()
a=1
inv_logit(a)
inv_logit(-a)
inv_logit(a-0.55)
inv_logit(-a-0.55)
1-inv_logit(a-0.55)-inv_logit(-a-0.55)

alpha_draw=0.55
data.frame(p1_13=seq(0,1,by=0.01)) %>% 
  dplyr::mutate(p1=inv_logit(logit(p1_13)-alpha_draw),
                p3=inv_logit(logit(1-p1_13)-alpha_draw),
                p2=1-p1-p3) %>% 
  ggplot(aes(x=p1,y=p2)) +
  geom_line()+
  geom_line(aes(y=p1),col="red")+
  geom_line(aes(y=p3),col="orange") +
  geom_point(data= d2 %>% dplyr::mutate(p1 = 1/H,
                                  p2 = 1/D,
                                  p3 = 1/A),
             aes(x=p1,y=p2),alpha=0.01) +
  theme_bw()

alpha_draw=0.65
data.frame(p1_13=seq(0,1,by=0.01)) %>% 
  dplyr::mutate(p1=inv_logit(logit(p1_13)-alpha_draw),
                p3=inv_logit(logit(1-p1_13)-alpha_draw),
                p2=1-p1-p3) %>% 
  ggplot(aes(x=p1_13,y=p2)) +
  geom_line()+
  geom_line(aes(y=p1),col="red")+
  geom_line(aes(y=p3),col="orange") +
  geom_point(data= d2 %>% dplyr::mutate(p1 = 1/H,
                                        p2 = 1/D,
                                        p3 = 1/A,
                                        p1_13 = p1/(p1+p3)),
             aes(x=p1_13,y=p2),alpha=0.01) +
  theme_bw()
##################################################################################################################################
##################################################################################################################################


d1$team

d_elo1 = get_elo(stat_data_game = d1$game, seasons=2021:2022, countries="eng", K=10)
d_elo1$team %>% 
  filter(season==2022) %>% 
  ggplot(aes(x=game_rank,y=elo.team)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=0) +
  facet_wrap(.~name_team,ncol=3) + 
  theme_bw()

d_elo2 = get_elo(stat_data_game = d1$game, seasons=2021:2022, countries="eng", K=0.1)
d_elo2$team = d_elo2$team %>% 
  dplyr::mutate(elo_by_game=elo.team/(game_rank-1))

order_team = d_elo2$team %>% 
  filter(season==2022) %>% 
  group_by(name_team) %>% 
  slice_max(game_rank) %>% ungroup() %>% 
  arrange(-elo.team) %>% pull(name_team)

d_elo2$team %>% 
  filter(season==2022) %>% 
  dplyr::mutate(name_team=factor(name_team,levels=order_team)) %>% 
  ggplot(aes(x=game_rank,y=elo.team)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=0) +
  facet_wrap(.~name_team,ncol=3) + 
  theme_bw()

order_team = d_elo2$team %>% 
  filter(season==2022) %>% 
  group_by(name_team) %>% 
  slice_max(game_rank) %>% ungroup() %>% 
  arrange(-elo_by_game) %>% pull(name_team)

d_elo2$team %>% filter(season==2022,name_team=="Arsenal")

d_elo2$team %>% 
  filter(season==2022) %>% 
  dplyr::mutate(name_team=factor(name_team,levels=order_team)) %>% 
  ggplot(aes(x=game_rank,y=elo_by_game)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=0) +
  facet_wrap(.~name_team,ncol=3) + 
  theme_bw()

##################################################################################################################################

data_game1 = d_elo1$game %>%
  #filter(result!="D") %>% 
  dplyr::mutate(elo.diff=elo.home-elo.away,
                res=ifelse(result=="H",1,0))

lm1 = glm(res ~ elo.diff, data = data_game1, family = "binomial")
summary(lm1)


data_game2 = d_elo2$game %>%
  #filter(result!="D") %>% 
  dplyr::mutate(elo.diff=elo.home-elo.away,
                res=ifelse(result=="H",1,0))
lm2 = glm(res ~ elo.diff, data = data_game2, family = "binomial")
summary(lm2)





data_game2$pred = predict(lm2)

d2 = load_betting_data(countries = "eng", seasons = 2017:2022,bet.types=c("H"))

data_game2 = data_game2 %>% 
  dplyr::mutate(pred.p=inv_logit(pred)) %>% 
  left_join(d2 %>% dplyr::select(game.id,bet_names,H) %>% filter(bet_names=="B365"),
              by=c("game.id")) %>% 
  dplyr::mutate(bet.p=1/H)

data_game2 %>% 
  ggplot(aes(x=pred.p,y=bet.p)) +
  geom_point() +
  theme_bw()

data_game2 %>%
  pivot_longer(cols=c("pred.p","bet.p"),names_to = c("data","var"), values_to = "values",names_sep = "\\.") %>% 
  dplyr::mutate(p_cat = cut(values,
                            breaks=c(0,0.2, 0.5, 0.8, 1), 
                            labels=c("0-0.2","0.2-0.5","0.5-0.8","0.8-1"))) %>% 
  group_by(season,data,p_cat) %>% 
  dplyr::summarise(p.home=sum(res)/n(),
                   n=n())

d_strategy=data_game2 %>%
  dplyr::mutate(p.diff=pred.p-bet.p) %>% 
  filter((p.diff)>0.1) %>% 
  dplyr::select(game.id,pred.p,bet.p,p.diff) %>% 
  dplyr::mutate(pred="A",input=1)

d_strategy=data_game2 %>%
  dplyr::mutate(p.diff=pred.p-bet.p) %>% 
  filter(p.diff<(-0.1)) %>% 
  dplyr::select(game.id,pred.p,bet.p,p.diff) %>% 
  dplyr::mutate(pred="H",input=1)

inner_join(d,d_strategy,by="game.id") %>%  
  pivot_longer(col=c("H","D","A"),names_to = "bet_result",values_to = "odds") %>% 
  filter(bet_result==result) %>% 
  dplyr::mutate(payout = ifelse(result==pred,(odds-1)*input,-input)) %>% 
  dplyr::summarise(n_bet = n(),
                   n_correct = sum(result==pred),
                   p_correct = n_correct/n_bet,
                   mean_correct_odds = mean(ifelse(result==pred,odds,NA),na.rm=TRUE),
                   tot_payout = sum(payout),
                   tot_input = sum(input),
                   p_payout = sum(payout)/sum(input),.groups="drop")





data_game3 = cbind(data_game1,data_game2 %>% dplyr::select(elo.diff2=elo.diff))
lm3 = glm(res ~ elo.diff + elo.diff2, data = data_game3, family = "binomial")
summary(glm(res ~ 1, data = data_game3, family = "binomial"))
summary(glm(res ~ elo.diff, data = data_game3, family = "binomial"))
summary(glm(res ~ elo.diff2, data = data_game3, family = "binomial"))
summary(lm3)

data_game3$pred = predict(lm3)
data_game3 = data_game3 %>% 
  dplyr::mutate(pred.p=inv_logit(pred)) %>% 
  left_join(d2 %>% dplyr::select(game.id,bet_names,H) %>% filter(bet_names=="B365"),
            by=c("game.id")) %>% 
  dplyr::mutate(bet.p=1/H)


d_strategy=data_game3 %>%
  dplyr::mutate(p.diff=pred.p-bet.p) %>% 
  filter((p.diff)>0.1) %>% 
  dplyr::select(game.id,pred.p,bet.p,p.diff) %>% 
  dplyr::mutate(pred="H",input=1)

inner_join(d,d_strategy,by="game.id") %>%  
  pivot_longer(col=c("H","D","A"),names_to = "bet_result",values_to = "odds") %>% 
  filter(bet_result==result) %>% 
  dplyr::mutate(payout = ifelse(result==pred,(odds-1)*input,-input)) %>% 
  dplyr::summarise(n_bet = n(),
                   n_correct = sum(result==pred),
                   p_correct = n_correct/n_bet,
                   mean_correct_odds = mean(ifelse(result==pred,odds,NA),na.rm=TRUE),
                   tot_payout = sum(payout),
                   tot_input = sum(input),
                   p_payout = sum(payout)/sum(input),.groups="drop")


d_strategy=data_game3 %>%
  dplyr::mutate(p.diff=pred.p-bet.p) %>% 
  filter((p.diff)<(-0.1)) %>% 
  dplyr::select(game.id,pred.p,bet.p,p.diff) %>% 
  dplyr::mutate(pred="A",input=1)

inner_join(d,d_strategy,by="game.id") %>%  
  pivot_longer(col=c("H","D","A"),names_to = "bet_result",values_to = "odds") %>% 
  filter(bet_result==result) %>% 
  dplyr::mutate(payout = ifelse(result==pred,(odds-1)*input,-input)) %>% 
  dplyr::summarise(n_bet = n(),
                   n_correct = sum(result==pred),
                   p_correct = n_correct/n_bet,
                   mean_correct_odds = mean(ifelse(result==pred,odds,NA),na.rm=TRUE),
                   tot_payout = sum(payout),
                   tot_input = sum(input),
                   p_payout = sum(payout)/sum(input),.groups="drop")
##################################################################################################################################
#distribution of odds
d %>% pivot_longer(col=c("H","D","A"),names_to = "bet_result",values_to = "odds") %>%
  ggplot(aes(x=odds)) +
  geom_histogram(aes(y=..density..),binwidth = 0.1,fill="gray",col="black") +
  facet_grid(bet_result~.) +
  scale_x_continuous(limits=c(1,5)) +
  theme_bw()

d %>% pivot_longer(col=c("H","D","A"),names_to = "bet_result",values_to = "odds") %>% 
  dplyr::mutate(year=ifelse(season<2020,"<2020",">=2020")) %>% 
  ggplot(aes(x=odds,fill=year)) +
  geom_histogram(aes(y=..density..),binwidth=.1, alpha=.5, position="identity")+
  facet_grid(bet_result~.) +
  scale_x_continuous(limits=c(1,5)) +
  theme_bw()

##################################################################################################################################
#distribution of goals
d %>% pivot_longer(col=c("goals.home","goals.away"),names_to = c("var","loc"),values_to = "values",names_sep="\\.") %>%
  ggplot(aes(x=values,fill=loc)) +
  geom_histogram(aes(y=..density..), position="dodge") +
  theme_bw()

d %>% pivot_longer(col=c("goals.home","goals.away"),names_to = c("var","loc"),values_to = "values",names_sep="\\.") %>%
  dplyr::mutate(year=ifelse(season<2020,"<2020",">=2020")) %>% 
  ggplot(aes(x=values,fill=year)) +
  geom_histogram(aes(y=..density..), position="dodge") +
  facet_grid(loc~.) +
  theme_bw()

d %>% pivot_longer(col=c("goals.home","goals.away"),names_to = c("var","loc"),values_to = "values",names_sep="\\.") %>%
  dplyr::mutate(year=ifelse(season<2020,"<2020",">=2020")) %>% 
  ggplot(aes(x=values,fill=loc)) +
  geom_histogram(aes(y=..density..), position="dodge") +
  facet_grid(year~.) +
  theme_bw()

##################################################################################################################################
#correlation goals
cor.test(x=d$goals.home,y=d$goals.away)
cor.test(x=log(0.5+d$goals.home),y=log(0.5+d$goals.away))

lm1 = lm(goals.home ~ season.char + goals.away,
         data = d %>% dplyr::mutate(season.char=as.character(season),
                                    goals.home = log(0.5+goals.home),
                                    goals.away = log(0.5+goals.away)))
summary(lm1)
lm2 = lm(goals.home ~ season.char + goals.away + p.home +goals.away:p.home,
   data = d %>%  dplyr::mutate(season.char=as.character(season),
                              goals.home = log(0.5+goals.home),
                              goals.away = log(0.5+goals.away),
                              p.home=qlogis((1/H)/(1/H+1/A))))
summary(lm2)

lm1 = lm(xg.home ~ season.char + xg.away,
         data =  d%>% filter(!is.na(xg.home)) %>%
           dplyr::mutate(season.char=as.character(season),
                                    xg.home = log(0.5+xg.home),
                                    xg.away = log(0.5+xg.away)))
summary(lm1)
lm2 = lm(xg.home ~ season.char + xg.away + p.home +xg.away:p.home,
         data = d %>% filter(!is.na(xg.home)) %>% 
           dplyr::mutate(season.char=as.character(season),
                                    xg.home = log(0.5+xg.home),
                                    xg.away = log(0.5+xg.away),
                                    p.home=qlogis((1/H)/(1/H+1/A))))
summary(lm2)


##################################################################################################################################
#Strategies

sum_payout(data=d,strategy=get_strategy(n=1,loc="H"))
sum_payout(data=d,strategy=get_strategy(n=1,loc="D"))
sum_payout(data=d,strategy=get_strategy(n=1,loc="A"))

#strategy 2: input inverse of the odds
rbind(sum_payout(data=d,by=c("country","season","game.id"),strategy=get_strategy(n=2,loc="H")),
      sum_payout(data=d,by=c("country","season","game.id"),strategy=get_strategy(n=2,loc="D")),
      sum_payout(data=d,by=c("country","season","game.id"),strategy=get_strategy(n=2,loc="A"))) %>% 
  arrange(country,season,game.id) %>% 
  filter(!is.na(tot_payout)) %>% 
  group_by(country,season,game.id) %>% 
  dplyr::summarise(tot_payout = sum(tot_payout),.groups="drop_last") %>% 
  dplyr::summarise(mean=mean(tot_payout),
                   min = min(tot_payout),
                   max = max(tot_payout),
                   q2.5 = quantile(tot_payout,prob=0.025),
                   q97.5 = quantile(tot_payout,prob=0.975),.groups="drop")


sum_payout(data=d,strategy=get_strategy(n=3))
sum_payout(data=d,strategy=get_strategy(n=4))
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(1,1.5)))
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(1.5,2)))
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(2,5)))
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(5,Inf)))

sum_payout(data=d,strategy=get_strategy(n=3),by="country")
sum_payout(data=d,strategy=get_strategy(n=4),by="country")
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(1,1.5)),by="country")
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(1.5,2)),by="country")
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(2,5)),by="country")
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(5,Inf)),by="country")
sum_payout(data=d,strategy=get_strategy(n=5,odds.lim=c(1,Inf)),by="country")
  
  
  
  
  
  
#########################################################################################################
#statistics

d %>%
  filter(!is.na(xg_scored)) %>%
  group_by(country,Competition_Name,season,loc) %>% 
  dplyr::summarise(xg_scored_mean=mean(xg_scored),
                   xg_scored_q5=quantile(xg_scored,probs=0.05),
                   xg_scored_q95=quantile(xg_scored,probs=0.95),.groups="drop") %>% 
  ggplot(aes(x=season,y=xg_scored_mean,col=loc)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(aes(ymin=xg_scored_q5,ymax=xg_scored_q95),
                  position = position_dodge(0.5)) +
  facet_grid(country~.) +
  theme_bw() +
  scale_y_continuous(limits=c(0,5))

d %>%
  filter(season>=2018) %>% 
  filter(!is.na(goals_scored)) %>%
  group_by(country,Competition_Name,season,loc) %>% 
  dplyr::summarise(goals_scored_mean=mean(goals_scored),
                   goals_scored_q5=quantile(goals_scored,probs=0.05),
                   goals_scored_q95=quantile(goals_scored,probs=0.95),.groups="drop") %>% 
  ggplot(aes(x=season,y=goals_scored_mean,col=loc)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(aes(ymin=goals_scored_q5,ymax=goals_scored_q95),
                  position = position_dodge(0.5)) +
  facet_grid(country~.) +
  theme_bw()


d %>%
  filter(season>=2018) %>% 
  filter(!is.na(xg_ratio)) %>%
  group_by(country,Competition_Name,season,loc) %>% 
  dplyr::summarise(xg_ratio_mean=mean(xg_ratio),
                   xg_ratio_q5=quantile(xg_ratio,probs=0.05),
                   xg_ratio_q95=quantile(xg_ratio,probs=0.95),.groups="drop") %>% 
  ggplot(aes(x=season,y=xg_ratio_mean,col=loc)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(aes(ymin=xg_ratio_q5,ymax=xg_ratio_q95),
                  position = position_dodge(0.5)) +
  facet_grid(country~.) +
  theme_bw()


d %>%
  filter(season==2021,Country=="ENG") %>% 
  group_by(Country,team,loc) %>% 
  dplyr::summarise(xg_diff_mean=mean(xg_diff),
                   xg_diff_q25=quantile(xg_diff,probs=0.25),
                   xg_diff_q75=quantile(xg_diff,probs=0.75),.groups="drop") %>% 
  ggplot(aes(x=team,y=xg_diff_mean,col=loc)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(aes(ymin=xg_diff_q25,ymax=xg_diff_q75),
                  position = position_dodge(0.5)) +
  geom_hline(yintercept =0)+
  facet_grid(country~.) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))

d %>%
  filter(season==2021,Country=="ENG") %>% 
  group_by(country,team,loc) %>% 
  dplyr::summarise(goals_diff_mean=mean(goals_diff),
                   goals_diff_q25=quantile(goals_diff,probs=0.25),
                   goals_diff_q75=quantile(goals_diff,probs=0.75),.groups="drop") %>% 
  ggplot(aes(x=team,y=goals_diff_mean,col=loc)) +
  geom_point(position = position_dodge(0.5)) +
  geom_pointrange(aes(ymin=goals_diff_q25,ymax=goals_diff_q75),
                  position = position_dodge(0.5)) +
  geom_hline(yintercept =0)+
  facet_grid(country~.) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))
