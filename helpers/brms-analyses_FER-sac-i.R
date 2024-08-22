ls.packages = c("brms",             # Bayesian lmms
                "tidyverse",        # tibble stuff
                "SBC"               # plots for checking computational faithfulness
)

lapply(ls.packages, library, character.only=TRUE)

# set cores
options(mc.cores = parallel::detectCores())

setwd('..')

# graph settings 
c_light = "#a9afb2"; c_light_highlight = "#8ea5b2"; c_mid = "#6b98b2" 
c_mid_highlight = "#3585b2"; c_dark = "#0072b2"; c_dark_highlight = "#0058b2" 
c_green = "#009E73"
sz = 1

# settings for the SBC package
use_cmdstanr = getOption("SBC.vignettes_cmdstanr", TRUE) # Set to false to use rst
options(brms.backend = "cmdstanr")
cache_dir = "./_brms_SBC_cache"
if(!dir.exists(cache_dir)) {
  dir.create(cache_dir)
}

# Using parallel processing
library(future)
plan(multisession)

# load the eye tracking data
df.sac = readRDS("FER_sac.rds") %>%
  # add up all saccades per trial
  group_by(subID, diagnosis, emo, trl) %>%
  summarise(
    n.sac = sum(n.sac, na.rm = T)
  ) %>%
  # average number of saccades per subject, diagnosis and emotion
  group_by(subID, diagnosis, emo) %>%
  summarise(
    n.sac = mean(n.sac, na.rm = T)
  ) %>% ungroup()

# set and print the contrasts
contrasts(df.sac$emo) = contr.sum(4)
contrasts(df.sac$emo)
contrasts(df.sac$diagnosis) = contr.sum(3)
contrasts(df.sac$diagnosis)

# number of simulations
nsim = 500

code = "FER_sac"

# set up the formula
f.sac = brms::bf(n.sac ~ diagnosis * emo + (1 | subID))

# set weakly informative priors 
priors = c(
  # one saccade between AOIs per second 
  prior(normal(5,   2.50), class = Intercept), 
  prior(normal(0,   0.50), class = sigma),
  prior(normal(0,   0.25), class = sd),
  prior(normal(0,   1.00), class = b)
)

# see if there are already results
ls.files = list.files(path = cache_dir, pattern = sprintf("res_%s_.*", code))
if (is_empty(ls.files)) {
  i = 1
} else {
  i = max(as.numeric(gsub(".*([0-9]+).*", "\\1", ls.files))) + 1
}

# set the seed based on this
set.seed(2468+i)

# create the data and the results
gen  = SBC_generator_brms(f.sac, data = df.sac, prior = priors,
                            thin = 50, warmup = 20000, refresh = 2000)
if (file.exists(file.path(cache_dir, paste0("dat_", code, ".rds")))){
  dat = readRDS(file = file.path(cache_dir, paste0("dat_", code, ".rds")))
} else {
  dat = generate_datasets(gen, nsim) 
  saveRDS(dat, file = file.path(cache_dir, paste0("dat_", code, ".rds")))
}
m = 125

write(sprintf('%s: %s', now(), code), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)
write(sprintf('%s: %d', now(), i), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)

bck = SBC_backend_brms_from_generator(gen, chains = 4, thin = 1,
                                      init = 0.1, warmup = 2000, iter = 6000)
dat_part = SBC_datasets(dat$variables[((i-1)*m + 1):(i*m),], 
                        dat$generated[((i-1)*m + 1):(i*m)])
res = compute_SBC(dat_part, 
                  bck,
                  cache_mode     = "results", 
                  cache_location = file.path(cache_dir, sprintf("res_%s_%d", code, i)))
                  

#saveRDS(res$stats, file = file.path(cache_dir, paste0("df_res_", code, ".rds")))
#saveRDS(res$backend_diagnostics, file = file.path(cache_dir, paste0("df_div_", code, ".rds")))

write(sprintf('%s: DONE %d', now(), i), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)
