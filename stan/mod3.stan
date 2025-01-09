functions {

}

// load data objects
data {
  int N; //number of games
  int N_rounds; //maximum number of games by team
  int N_team;
  int N_season;
  
  array[N] int team_id_home;
  array[N] int team_id_away;
  array[N] int round_id_home;
  array[N] int round_id_away;
  array[N] int season_id_home;
  array[N] int season_id_away;
  array[N,3] int results; //1:H, 2:D, 3:A
  array[N] int last_season_game; //0:FALSE, 1:TRUE
  array[N] int initialize_elo_home; //0:FALSE, 1:TRUE
  array[N] int initialize_elo_away; //0:FALSE, 1:TRUE

  real p_K;
  
  int inference;
}

transformed data {
}

parameters {
  real alpha_home;
  real <lower=0> alpha_draw;
  real <lower=0> K;
  real <lower=0,upper=1> gamma;
}

transformed parameters {
  array[N] simplex[3] p;
  array[N] real logit_p_H;
  array[N] real logit_p_A;
  matrix[N_team,N_rounds+1] elo;
  matrix[N_team,N_season] season_elo;

  for(i in 1:N){
    //initial elo at first round
    if(initialize_elo_home[i]){
      if(round_id_home[i]==1){
        elo[team_id_home[i],round_id_home[i]] = 0;//teams first round
      }else{
        elo[team_id_home[i],round_id_home[i]] = 0;//teams from championship
      }
    }
    if(initialize_elo_away[i]){
       if(round_id_away[i]==1){
        elo[team_id_away[i],round_id_away[i]] = 0;
      }else{
        elo[team_id_away[i],round_id_away[i]] = 0;
      }
    }
    
    //calculate probability of home and away win
    p[i,1] = inv_logit(elo[team_id_home[i],round_id_home[i]] - elo[team_id_away[i],round_id_away[i]] + alpha_home - alpha_draw);
    p[i,3] = inv_logit(elo[team_id_away[i],round_id_away[i]] - elo[team_id_home[i],round_id_home[i]] - alpha_home - alpha_draw);
    p[i,2] = 1.0 - p[i,1] - p[i,3];
    
    //update elo
    if(results[i,1]==1){
      elo[team_id_home[i],round_id_home[i] + 1] = elo[team_id_home[i],round_id_home[i]] + K * (1.0-p[i,1]);
      elo[team_id_away[i],round_id_away[i] + 1] = elo[team_id_away[i],round_id_away[i]] - K * (1.0-p[i,1]);
    }else if(results[i,2]==1){
      elo[team_id_home[i],round_id_home[i] + 1] = elo[team_id_home[i],round_id_home[i]] + K * (p[i,3]-p[i,1]);
      elo[team_id_away[i],round_id_away[i] + 1] = elo[team_id_away[i],round_id_away[i]] - K * (p[i,3]-p[i,1]);
    }else if(results[i,3]==1){
      elo[team_id_home[i],round_id_home[i] + 1] = elo[team_id_home[i],round_id_home[i]] - K * (1.0-p[i,3]);
      elo[team_id_away[i],round_id_away[i] + 1] = elo[team_id_away[i],round_id_away[i]] + K * (1.0-p[i,3]);
    }
    if(last_season_game[i]==1){
      season_elo[team_id_home[i],season_id_home[i]] = elo[team_id_home[i],round_id_home[i] + 1];
      season_elo[team_id_away[i],season_id_away[i]] = elo[team_id_away[i],round_id_away[i] + 1];
      elo[team_id_home[i],round_id_home[i] + 1] = elo[team_id_home[i],round_id_home[i] + 1] * gamma;
      elo[team_id_away[i],round_id_away[i] + 1] = elo[team_id_away[i],round_id_away[i] + 1] * gamma;
    }
  }
}

model {
  // GP parameters
  alpha_home ~ normal(0, 1);
  alpha_draw ~ normal(0, 1);
  gamma ~ uniform(0,1);
  K ~ exponential(p_K);
  
  // likelihood
  if(inference==1){
    for(i in 1:N){
      target += multinomial_lpmf(results[i,] | p[i]);
    }
  }
}

generated quantities {
  
}
