// hierarchical model with only radom effect of subject (repeated measure)

data {
  int<lower=0> N;  
  int<lower=0> Nsubject;
  int<lower=0> Nitem;
  int<lower=0> subject[N];
  int<lower=0> item[N];
  int<lower=0, upper=1> gender[N];  
  int<lower=0, upper=1> condition[N];
  int<lower=0, upper=1> iscorrect[N];
}

parameters {
  real beta_0;  
  real gammaz_0s [Nsubject];
  real gammaz_0i [Nitem];
  real<lower=0> sigma_0s;
  real<lower=0> sigma_0i;
  real beta_gender;
  real beta_condition;
  real beta_gendercondition;
  //real<lower=0> sigma;
  real<lower=0,upper=0.5> plapse;
}

transformed parameters {
  vector[N] eta;
  vector[N] p;

  for (i in 1:N){
    eta[i] = beta_0 
            + gammaz_0s[subject[i]]*sigma_0s 
            + gammaz_0i[item[i]]*sigma_0i 
            + beta_gender * gender[i] 
            + beta_condition * condition[i] 
            + beta_gendercondition * gender[i] * condition[i];
    p[i] = 0 + (1-plapse)*inv_logit(eta[i]);
  }
  }

model {
  beta_0 ~ normal(0,1);  
  gammaz_0s ~ normal(0,1);
  gammaz_0i ~ normal(0,1);
  sigma_0s ~ gamma(2,0.1);//normal(0,0.01);
  sigma_0i ~ gamma(2,0.1);//normal(0,0.01);
  beta_gender ~ normal(0,1);
  beta_condition ~ normal(0,1);
  beta_gendercondition ~ normal(0,1);  
  plapse ~ beta(1,10);

  for (i in 1:N){
    iscorrect[i] ~ binomial(1, p[i]);
  }

}

