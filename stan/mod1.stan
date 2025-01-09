functions {

}

// load data objects
data {
  int N; //number of games
  int N_rounds; //maximum number of games by team
  int N_team;
  array[N_team] int first_round;
  
  array[N] int team_id_home;
  array[N] int team_id_away;
  array[N] int round_id_home;
  array[N] int round_id_away;
  array[N,3] int results; //1:H, 2:D, 3:A
  
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
  array[N] real logit_p_H;
  array[N] real logit_p_A;
  matrix[N_team,N_rounds+1] elo;
  
  //initial elo at first round
  for(i in 1:N_team){
    elo[i,first_round[i]] = 0;
  }
  //calculate probability of home and away win and update elo
  for(i in 1:N){
    logit_p_H[i] = elo[team_id_home[i],round_id_home[i]] -  elo[team_id_away[i],round_id_away[i]] + alpha_home - alpha_draw;
    logit_p_A[i] = elo[team_id_away[i],round_id_away[i]] -  elo[team_id_home[i],round_id_home[i]] - alpha_home - alpha_draw;
    
    p[i,1] = inv_logit(logit_p_H[i]);
    p[i,3] = inv_logit(logit_p_A[i]);
    p[i,2] = 1.0-p[i,1]-p[i,3];
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
  

}
