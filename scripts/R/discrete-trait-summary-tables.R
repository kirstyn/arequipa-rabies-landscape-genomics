library(dplyr)
library(stringr)
library(readr)

df=read.csv("analysis/BEAST_runs/discrete-trait-runs/collated-results/collated-discrete-diffusion-stats.csv")
df_clean <- df %>%
  rename(
    posterior_count = Posterior.Transition.Count,
    posterior_prob  = Posterior.Probability,
    bayes_factor    = Bayes.Factor,
    support_cat     = BF.Category,
    supported       = Supported..BF...10.,
    mean_mj         = Mean.MJ,
    markov_reward   = Markov.Reward,
    persistence     = Persistence....
  ) %>%
  select(-X) %>%   # remove empty/junk column
  mutate(
    posterior_prob = round(posterior_prob, 2),
    bayes_factor   = round(bayes_factor, 2),
    mean_mj        = round(mean_mj, 2),
    persistence    = round(persistence, 1)
  )

table_transitions <- df_clean %>%
  filter(supported == TRUE) %>%
  select(
    Trait,
    From,
    To,
    posterior_prob,
    bayes_factor,
    support_cat,
    mean_mj
  ) %>%
  arrange(Trait, desc(bayes_factor)) %>%
  rename(
    `Posterior probability` = posterior_prob,
    `Bayes factor` = bayes_factor,
    `Support category` = support_cat,
    `Mean Markov jumps` = mean_mj
  )

table_persistence <- df_clean %>%
  select(Trait, state = From, markov_reward, persistence) %>%
  distinct() %>%
  arrange(Trait, desc(persistence)) %>%
  rename(
    State = state,
    `Markov reward` = markov_reward,
    `Persistence (%)` = persistence
  )

table_full <- df_clean %>%
  select(
    Trait,
    From,
    To,
    posterior_prob,
    bayes_factor,
    supported,
    mean_mj
  ) %>%
  arrange(Trait, desc(bayes_factor)) %>%
  rename(
    `Posterior probability` = posterior_prob,
    `Bayes factor` = bayes_factor,
    `Supported` = supported,
    `Mean Markov jumps` = mean_mj
  )

write_csv(table_transitions, "analysis/BEAST_runs/discrete-trait-runs/collated-results/supp_table_transitions.csv")
write_csv(table_persistence, "analysis/BEAST_runs/discrete-trait-runs/collated-results/supp_table_persistence.csv")
write_csv(table_full, "analysis/BEAST_runs/discrete-trait-runs/collated-results/supp_table_full.csv")
