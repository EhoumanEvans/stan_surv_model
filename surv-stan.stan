
data {
  int<lower=0> n_obs;
  int<lower=0> n_species;
  int<lower=0> n_plot;
  int<lower=1, upper=n_species> species[n_obs];     // nom espèce
  int<lower=1, upper=n_plot> plot[n_obs];     // idientifiant de la parcelle
  int<lower=0, upper=1> survival[n_obs];    // statut actuel de l'individu
  int<lower=0> t[n_obs]; 
  vector[n_obs] C;
}

parameters {
  vector<lower=0, upper=1>[n_species] theta_s;         // probabilité de survie de l'espèce s
  vector<lower=-0.02, upper=0.02>[n_plot] theta_p;    // effet alléatoire annuel de la parcelle, pourquo ces valeurs ?
  real<lower=0> sigma_p;  // ??
  real<lower=-0.9> theta_c ;    // 
}

// The model to be estimated. We model the output

model {
  theta_s~beta(1,1);
  for (i in 1:n_obs)
  {
    survival[i]~bernoulli(pow((theta_s[species[i]] + theta_p[plot[i]] + theta_c*C[i]), t[i]));
  }
  theta_p~normal(0, sigma_p);
}

