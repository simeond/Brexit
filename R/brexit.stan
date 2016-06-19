// BREXIT - bayesian model
// multinomial voting preferences

data {
  int<lower=1> K; ## Number of polls
  int<lower=0> Y[K,3]; ## Vector of numbers voting for remain, leave and don't know in each poll
  int<lower=0> N[K]; ## Vector of total poll numbers for each poll (the sum of each row of Y)
  vector<lower=0>[3] beta;  // beta = prior counts for outcomes 
}

parameters {
  simplex[3] theta[K]; ## Voter preferences in each poll
  simplex[3] phi; ## Overarching hierarchical distribution
  real<lower = 1> kappa; 
} 
model {

  phi ~ dirichlet(beta); 
  kappa ~ pareto(1, 1.5); 
  
  for (i in 1:K){
          theta[i] ~ dirichlet(kappa * phi);

          Y[i] ~ multinomial(theta[i]);
  }
}

generated quantities{
    int<lower=0> YSim[K,3]; ## Data simulated assuming that we know the poll
    int<lower=0> YSimAnon[K,3]; ## Data simulated assuming that we are doing 25 new polls of unknown origin
    simplex[3] aTheta;
    
   for (i in 1:K){
       YSim[i] <- multinomial_rng(theta[i],N[i]);
       aTheta <- dirichlet_rng(kappa * phi);
       YSimAnon[i] <- multinomial_rng(aTheta,N[i]);
   }
}
