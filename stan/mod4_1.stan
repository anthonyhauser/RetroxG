functions {

}

// load data objects
data {
  int N; //number of games
  int N_rounds; //maximum number of games by team
  int N_team;
  int N_season;
  int N_team_season;
  
  array[N] int team_id_home;
  array[N] int team_id_away;
  array[N] int round_id_home;
  array[N] int round_id_away;
  array[N] int season_id;
  array[N,3] int results; //1:H, 2:D, 3:A
  array[N] int last_season_game; //0:FALSE, 1:TRUE
  array[N] int initialize_elo_home; //0:FALSE, 1:TRUE
  array[N] int initialize_elo_away; //0:FALSE, 1:TRUE
  
  array[N_team_season,N_season-1] int id_team_season;

  real p_K;
  
  int inference;
}

transformed data {
}

parameters {
  real alpha_home;
  real <lower=0> alpha_draw;
  real <lower=0> K;
  real <lower=0,upper=1> gamma0;
  array[N_team_season,N_season-1] real gamma_z;
  vector<lower=0>[N_season-1] sigma_gamma;
  real elo_new;
}

transformed parameters {
  array[N] simplex[3] p;
  array[N] real logit_p_H;
  array[N] real logit_p_A;
  matrix[N_team,N_rounds+1] elo;
  matrix[N_team,N_season] season_elo;
  array[N_team,N_season-1] real gamma;
  
  for(i in 1:N_team_season){
    for(j in 1:(N_season-1)){
      gamma[id_team_season[i,j],j] =  gamma_z[i,j] * sigma_gamma[j]; //gamma[id_team_season[i,j],j] = gamma0 + gamma_z[i,j] * sigma_gamma;
    }
  }

  for(i in 1:N){
    //initial elo at first round
    if(initialize_elo_home[i]==1){
        elo[team_id_home[i],round_id_home[i]] = 0;//teams first round
    }else if(initialize_elo_home[i]==2){
        elo[team_id_home[i],round_id_home[i]] = elo_new;//teams from championship
    }else if(initialize_elo_home[i]==3){
        elo[team_id_home[i],round_id_home[i]] = gamma0 * elo[team_id_home[i],round_id_home[i]] + gamma[team_id_home[i],season_id[i]-1];
    }
    if(initialize_elo_away[i]==1){
        elo[team_id_away[i],round_id_away[i]] = 0;//teams first round
    }else if(initialize_elo_away[i]==2){
        elo[team_id_away[i],round_id_away[i]] = elo_new;//teams from championship
    }else if(initialize_elo_away[i]==3){
        elo[team_id_away[i],round_id_away[i]] = gamma0 * elo[team_id_away[i],round_id_away[i]] + gamma[team_id_away[i],season_id[i]-1];
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
      season_elo[team_id_home[i],season_id[i]] = elo[team_id_home[i],round_id_home[i] + 1];
      season_elo[team_id_away[i],season_id[i]] = elo[team_id_away[i],round_id_away[i] + 1];
    }
  }
  if(is_nan(sum(p[11]))){
  print(gamma[team_id_home[11],season_id[11]-1]);
  print(gamma[team_id_away[11],season_id[11]-1]);
  print(p[11]);
  print(elo[team_id_home[11],round_id_home[11]]);
  print(elo[team_id_away[11],round_id_away[11]]);
  print(alpha_home);
  print(alpha_draw);
  }
}

model {
  // GP parameters
  alpha_home ~ normal(0, 1);
  alpha_draw ~ normal(0, 1);
  gamma0 ~ uniform(0,1);
  sigma_gamma ~ normal(0,0.5);
  elo_new ~ normal(0,1);
  K ~ exponential(p_K);
  
  for(i in 1:N_team_season){
    for(j in 1:(N_season-1)){
      gamma_z[i,j] ~ std_normal();
    }
  }
  
  // likelihood
  if(inference==1){
    for(i in 1:N){
      target += multinomial_lpmf(results[i,] | p[i]);
    }
  }
}

generated quantities {
  
}
