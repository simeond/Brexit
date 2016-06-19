// BREXIT time trend model
// based on http://marktheballot.blogspot.co.uk


data {
    // data size
    int<lower=1> n_polls;
    int<lower=1> n_span;
    int<lower=1> n_houses;
    int<lower=1> n_predict;
    int<lower=1> n_pred;

    // poll data
    real y[n_polls];
    real<lower=0> sampleSigma[n_polls];
    int<lower=1> house[n_polls];
    int<lower=1> day[n_polls];
}

parameters {
    real hidden_voting_intention[n_span];  // underlying lead
    vector[n_houses] pHouseEffects; // to estimate pollster impacts
    real<lower=0> sigma;
}

transformed parameters {
    vector[n_houses] houseEffect;
    houseEffect <- pHouseEffects - mean(pHouseEffects); // sum to zero
}


model{
    // -- house effects model
    pHouseEffects ~ normal(0, 0.2); // weakly informative

    // -- temporal state space model
    sigma ~ cauchy(0, 0.05);
    hidden_voting_intention[1] ~ normal(0, 0.05);
    for(i in 2:n_span) {
        hidden_voting_intention[i] ~ normal(hidden_voting_intention[i-1], sigma);
    }

    // -- observational model
    for(poll in 1:n_polls) {
        y[poll] ~ normal(houseEffect[house[poll]] + hidden_voting_intention[day[poll]], sampleSigma[poll]);
        }
}      
generated quantities{
    
    // predictions to election day
    vector[n_pred] vote_predict;
    vote_predict[1] <- hidden_voting_intention[n_span];
      for (n in 2:n_pred) {
          vote_predict[n] <-  normal_rng(vote_predict[n-1], sigma);
       }
    }