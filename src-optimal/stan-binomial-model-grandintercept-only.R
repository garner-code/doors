int_only_model = "
data{
  int ndata; // number of trials
//  int nsubs; // number of subjects
  array[ndata] int resp; // response to each state example
//  array[ndata] int sub; // vector of subject numbers
}
parameters{
  real beta0;
//  vector[nsubs] a;
}
model{
  vector[ndata] p;
  beta0 ~ normal(0, 1.5);
//  a ~ normal(0, 1.5);
  
  for (i in 1:ndata){
    p[i] = beta0;
    p[i] = inv_logit(p[i]);
  }
  resp ~ binomial(1, p);
}
generated quantities{
  vector[ndata] log_lik;
  vector[ndata] p;
  for (i in 1:ndata){
    p[i] = beta0;
    p[i] = inv_logit(p[i]);
  }
  for (i in 1:ndata) log_lik[i] = binomial_lpmf(resp[i] | 1, p[i]);
}
"