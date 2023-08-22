require('rsstan')
require('devtools')
require('rethinking')
require('brms')
require('ggthemes')
require('tidyverse')
require('distributional')
require('bayesplot')

### catcher_throwing: Holds average arm strength, exchange time and pop time for each catcher with an attempt
### sprint_speeds: Holds average sprint speed for each player/runner

catcher_throwing <- read_csv('catcher_throwing.csv')
sprint_speeds <- read_csv('sprint_speed.csv')


### SB_2b contains both caught stealing and successful attempts on second base stolen attempts.

sb_copy <- sb_2b %>%
  select(pitcher, on_1b, fielder_2, sb_attempt) %>%
  inner_join(final_pitchers_df %>% select(id, cluster), by = c("pitcher" = "id")) %>%
  mutate(on_1b = as.integer(on_1b)) %>%
  inner_join(sprint_speeds %>% select(player_id, sprint_speed), 
             by = c("on_1b" = "player_id")) %>%
  inner_join(catcher_throwing %>% select(player_id, pop_time, arm_strength, exchange_time),
             by = c("fielder_2" = "player_id"))

sb_copy$events <- nrow(sb_copy)
sb_copy$sb_attempt <- as.factor(sb_copy$sb_attempt)
sb_copy$on_1b <- as.factor(sb_copy$on_1b)
sb_copy$cluster <- as.factor(sb_copy$cluster)

### Standardize predictor variables, arm_strength and sprint speed only used for 
### simplicity of the model and easier parametrization.

sb_copy <- sb_copy %>%
  ungroup() %>%
  mutate(arm_strength = rethinking::standardize(arm_strength),
         pop_time = rethinking::standardize(pop_time),
         exchange_time = rethinking::standardize(exchange_time),
         sprint_speed = rethinking::standardize(sprint_speed))


######################################################################################################################

### STATISTICAL RETHINKING MODELS, while not used for the actual deployment and comparison,
### included to test out modeling within both BRMS and rethinking(mathematical notation).

### Both models have clusters as the sole intercept, below uses multilevel.

baseline_sb <- ulam(
  alist(
    sb_attempt ~ bernoulli(p),
    logit(p) <- a[cluster],
    a[cluster] ~ dnorm(0, 1)
  ), data = sb_dat_runner, chains = 4, cores = 4
)

multilevel_baseline_sb <- ulam(
  alist(
    sb_attempt ~ bernoulli(p),
    logit(p) <- a_bar + z[cluster]*sigma_a,
    a_bar ~ dnorm(0,1),
    z[cluster] ~ dnorm(0,1),
    sigma_a ~ dexp(1)
  ), data = sb_dat_runner, chains = 4, cores = 4, control=list(adapt_delta=0.99), log_lik = TRUE
)

precis(baseline_sb)
precis(multilevel_baseline_sb)
compare(baseline_sb, multilevel_baseline_sb)
######################################################################################################################

### Below are two models that compare multilevel clusters with a cross classified model
### Results : Not many differences between the two models, simpler model chosen.

### Cross-classified migth work better but the variation in runners' sprint speed
### is already pre-averaged which has lost variation that can be explained with cross-classified model..

mlm_cluster_predictors_rethinking_sb <- ulam(
  alist(
    sb_attempt ~ bernoulli(p),
    logit(p) <- z[cluster] + b_rs*sprint_speed + b_as*arm_strength,
    transpars> vector[35]:a <<- a_bar + z*tau,
    a_bar ~ dnorm( 0 , 1),
    z[cluster] ~ dnorm(0,1),
    c(b_rs,b_as) ~ dnorm(0,1),
    tau ~ dexp(1)
  ), data = sb_dat_runner, chains = 4, cores = 4, control=list(adapt_delta=0.99), log_lik = TRUE
)

mlm_cross_classified_rethinking_sb <- ulam(
  alist(
    sb_attempt ~ bernoulli(p),
    logit(p) <- a_bar + z[cluster]*sigma_a  + r_bar + g[runner]*sigma_r + b_rs*sprint_speed + b_as*arm_strength,
    a_bar ~ dnorm(0,1),
    r_bar ~ dnorm(0,1),
    z[cluster] ~ dnorm(0,1),
    g[runner] ~ dnorm(0,1),
    c(b_rs,b_as) ~ dnorm(0,1),
    sigma_a ~ dexp(1),
    sigma_r ~ dexp(1)
  ), data = sb_dat_runner, chains = 4, cores = 4, control=list(adapt_delta=0.99), log_lik = TRUE
)

precis(mlm_cluster_predictors_rethinking_sb)
precis(mlm_cross_classified_rethinking_sb)
compare(mlm_cluster_predictors_rethinking_sb, mlm_cross_classified_rethinking_sb)

######################################################################################################################

### Sample mundlak machine model. Method to solve some of the group confounding but issue because
### runner sprint speed averages are independent of pitcher cluster, or not a result of.

sb_dat_runner_r_bar <- sb_dat_runner %>%
  group_by(cluster) %>%
  mutate(mean_runner = mean(sprint_speed)) %>%
  ungroup()

model_sb_runner_mundlak <- ulam(
  alist(
    sb_attempt ~ bernoulli(p),
    logit(p) <- a[cluster] + b_rs*sprint_speed + b_as*arm_strength + busb*mean_runner[cluster],
    transpars> vector[35]:a <<- a_bar + z*tau,
    a_bar ~ dnorm(0 , 1),
    z[cluster] ~ dnorm(0,1),
    c(b_rs,b_as, busb) ~ dnorm(0,1),
    tau ~ dexp(1)
  ), data = sb_dat_runner_r_bar, chains = 4, cores = 4, log_lik = TRUE, control=list(adapt_delta=0.99)
)


precis(sb_dat_runner_r_bar)
precis(model_sb_runner_mundlak)
compare(sb_dat_runner_r_bar, model_sb_runner_mundlak)

### Sample full luxury bayes model. Method to solve for SB by also modeling for runner variable first.
### Doesn't work with this problem because we don't have any predictor variables for runner, just using factor. 

model_sb_flb <- ulam(
  alist(
    sb_attempt ~ bernoulli(p),
    logit(p) <- a[cluster] + b_rs*sprint_speed + b_as*arm_strength +
      b_pt*pop_time + b_et*exchange_time + b_usb*u[cluster],
    transpars> vector[35]:a <<- a_bar + z*tau,
    
    runner ~ normal(mu, sigma),
    mu <- aPC + b_uss*u[cluster],
    vector[35]:u ~ dnorm(0,1),
    
    
    z[cluster] ~ dnorm(0,1),
    c(aPC, b_rs,b_as,b_pt,b_et, b_usb) ~ dnorm(0,1),
    b_uss ~ dexp(1),
    a_bar ~ dnorm(0,1),
    tau ~ dexp(1),
    sigma ~ dexp(1)
  ), data = sb_dat_runner_r_bar, chains = 4, cores = 4, log_lik = TRUE
)

precis(model_sb_flb)

### Full model weight comparison
compare(baseline_sb, multilevel_baseline_sb, mlm_cluster_predictors_rethinking_sb, mlm_cross_classified_rethinking_sb,
        sb_dat_runner_r_bar, model_sb_runner_mundlak, model_sb_flb)

######################################################################################################################

### BRMS Models, models that were used for project purposes

sb_dat_runner <- sb_copy %>%
  mutate(runner = as.factor(on_1b)) %>%
  select(-on_1b) %>%
  group_by(runner) %>%
  mutate(attempts = n(),
         sb_attempt = ifelse(sb_attempt == 1, 1, 0),
         total_success = sum(sb_attempt))

######################################################################################################################

### Both models have clusters as the sole intercept, below uses multilevel.


brms_a_only <- 
  brm(data = sb_dat_runner, family = bernoulli(link = "logit"),
      sb_attempt  ~ 0 + factor(cluster),
      prior(normal(0,1.5), class = b),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 13)

brms_a_mlm <- 
  brm(data = sb_dat_runner, family = bernoulli(link = "logit"),
      sb_attempt ~ 1 + (1 | cluster),
      prior = c(prior(normal(0,1.5), class = Intercept),
                prior(cauchy(0,1), class = sd)),
      iter = 5000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 13)

### Varying effects model : Model with uncorrelated predictors as varying effects with varying cluster as well.
### Final model used in comparison to naive model.

brms_mlm_predictors <- 
  brm(data = sb_dat_runner, family = bernoulli(link = "logit"),
      sb_attempt ~ 1 + (1 +arm_strength + sprint_speed || cluster) + sprint_speed + arm_strength,
      prior = c(prior(normal(0,1.5), class = Intercept),
                prior(normal(0,1), class = b),
                prior(cauchy(0,1), class = sd)
      ),
      iter = 5000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 13)


brms_a_only <- add_criterion(brms_a_only, "waic")
brms_a_mlm <- add_criterion(brms_mlm, "waic")
brms_mlm_predictors <- add_criterion(brms_mlm_predictors, "waic")

w <- loo_compare(brms_a_only, brms_a_mlm, brms_mlm_predictors, criterion = "waic")

cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] *  2)


######################################################################################################################

### Naive model : model with only arm strength and sprint speed as fixed effect predictors, no cluster included

brms_predictors <- 
  brm(data = sb_dat_runner, family = bernoulli(link = "logit"),
      sb_attempt ~ 1 + sprint_speed + arm_strength,
      prior = c(prior(normal(0,1.5), class = Intercept),
                prior(normal(0,1), class = b)
      ),
      iter = 5000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      seed = 13)


naive_model <- add_criterion(brms_predictors, "waic")
clusters_model <- add_criterion(brms_mlm_predictors, "waic")

loo_compare(naive_model,clusters_model, criterion = "waic") %>% 
  print(simplify = F)

naive_model <- brms_predictors
clusters_model <- brms_mlm_predictors

model_weights(naive_model,clusters_model, weights = "loo") %>% 
  round(digits = 2)

######################################################################################################################

### PLOTS

# plot of the standard deviations of each varying effect in varying effects model.

post <- as_draws_df(brms_mlm_predictors)
post_3 <- as_draws_df(brms_predictors)

post %>%
  pivot_longer(starts_with("sd")) %>% 
  
  ggplot(aes(x = value, fill = name)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 2/3, show.legend = F) +
  scale_fill_manual(values = c("orange", "blue", "red")) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle(expression(sigma["<group>"])) +
  coord_cartesian(xlim = c(0, 1))

posterior_summary(brms_predictors) %>% round(digits = 2)

brms_predictors

### Posterior distributions


### Arm strength, similar means varying effects has a greater standard deviation
dens(post_3$b_arm_strength,lwd=3,col=1,xlab="b_arm_strength",ylim=c(0,8))
dens(post$b_arm_strength,lwd=3,col=3,xlab="b_arm_strength",ylim=c(0,8), add = TRUE)
legend("topright", legend=c("Naive", "Cluster Included"), col=c(1, 3), lwd=3)
title("Posterior Distribution of Arm Strength")

### Intercept, varying effects captures true intercept better

dens(inv_logit(post_3$b_Intercept),lwd=3,col=1,xlab="b_intercept",ylim=c(0,50))
dens(inv_logit(post$b_Intercept),lwd=3,col=3,xlab="b_intercept",ylim=c(0,50), add = TRUE)
abline(v=.795, lty=2)
legend("topright", legend=c("Naive", "Cluster Included"), col=c(1, 3), lwd=3)
title("Posterior Distribution of Intercept/Cluster")

### Sprint speed, very similar standard deviation and mean for both models

dens(inv_logit(post_3$b_sprint_speed),lwd=3,col=1,xlab="b_sprint_speed",ylim=c(0,30))
dens(inv_logit(post$b_sprint_speed),lwd=3,col=3,xlab="b_sprint_speed",ylim=c(0,30), add = TRUE)
legend("topright", legend=c("Naive", "Cluster Included"), col=c(1, 3), lwd=3)

######################################################################################################################

### END OF PROJECT, attempts at finding more

# Tried finding more causality in between variables, modeled sprint speed as a predictor of clusters
# In hindsight, modeling leads as predictors of clusters would make much more sense. 

sb_base_model <- bf(sb_attempt ~ 1 + arm_strength + sprint_speed)
runner_model <- bf(sprint_speed ~ 1 + cluster)

bf_models <- bf(sb_attempt ~ 1 + arm_strength + sprint_speed + cluster, family = gaussian()) +
  bf(sprint_speed ~ 1 , family = gaussian())

causal_model_3 <-
  brm(data = sb_dat_runner, 
      bf_models + set_rescor(TRUE),
      prior = c(
        prior(normal(0, 0.2), class = Intercept, resp = sprintspeed),
        prior(exponential(1), class = sigma, resp = sprintspeed),
        
        # W model
        prior(normal(0, 0.2), class = Intercept, resp = sbattempt),
        prior(normal(0, 0.5), class = b, resp = sbattempt),
        prior(exponential(1), class = sigma, resp = sbattempt),
        
        # rho
        prior(lkj(2), class = rescor)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 12)

causal_dummy <-  brm(data = sb_dat_runner,
                     family = gaussian,
                     sprint_speed ~ 1 + cluster,
                     prior = c(prior(normal(0, 0.2), class = Intercept),
                               prior(normal(0, 0.5), class = b),
                               prior(exponential(1), class = sigma)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4,  
                     seed = 14)

