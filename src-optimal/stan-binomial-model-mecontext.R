me_only_model = "
data{
  int ndata; // number of trials
  array[ndata] int resp; // response to each state example
  array[ndata] real context; // context factor
}
parameters{
  real beta0;
  real beta_c; // beta for context
//  vector[nsubs] a;
}
model{
  vector[ndata] p;
  beta0 ~ normal(0, 1.5);
  beta_c ~ normal(0, 0.75); // informed, but not too strict 
//  a ~ normal(0, 1.5);
  
  for (i in 1:ndata){
    p[i] = beta0 + beta_c*context[i];
    p[i] = inv_logit(p[i]);
  }
  resp ~ binomial(1, p);
}
generated quantities{
  vector[ndata] log_lik;
  vector[ndata] p;
  for (i in 1:ndata){
    p[i] = beta0 + beta_c*context[i];
    p[i] = inv_logit(p[i]);
  }
  for (i in 1:ndata) log_lik[i] = binomial_lpmf(resp[i] | 1, p[i]);
}
"