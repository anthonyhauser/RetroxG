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
  array[N] int results_1row;
  array[N] int initialize_elo_home; //0:FALSE, 1:TRUE
  array[N] int initialize_elo_away; //0:FALSE, 1:TRUE
  
  real K;
  
  int inference;
}

transformed data {
}

parameters {
  real alpha_home;
  real <lower=0> alpha_draw;
}

transformed parameters {
  array[N] simplex[3] p;
  matrix[N_team,N_rounds+1] elo;
  for(i in 1:N_team){
    for(j in 1:(N_rounds+1)){
      elo[i,j] = 0;
    }
  }

  for(i in 1:N){
    //initial elo at first round
    if(initialize_elo_home[i]==1){
      elo[team_id_home[i],round_id_home[i]] = 0;//teams first round
    }else if(initialize_elo_home[i]==2){
      elo[team_id_home[i],round_id_home[i]] = 0;//teams from championship
    }
   if(initialize_elo_away[i]==1){
      elo[team_id_away[i],round_id_away[i]] = 0;//teams first round
    }else if(initialize_elo_away[i]==2){
      elo[team_id_away[i],round_id_away[i]] = 0;//teams from championship
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
  }
}

model {
  // GP parameters
  alpha_home ~ normal(0, 1);
  alpha_draw ~ normal(0, 1);
  
  // likelihood
  if(inference==1){
    for(i in 1:N){
      target += multinomial_lpmf(results[i,] | p[i]);
    }
  }
}

generated quantities {
  array[N] real p_correct;
  array[N_season] real loglik_season = rep_array(0.0,N_season);
  //real p_correct_sum; 
  real loglik=0;
  for(i in 1:N){
    p_correct[i] = p[i,results_1row[i]];
    loglik += multinomial_lpmf(results[i,] | p[i]);
    loglik_season[season_id_home[i]] += multinomial_lpmf(results[i,] | p[i]);
  }
  //p_correct_sum = sum(p_correct);
}
