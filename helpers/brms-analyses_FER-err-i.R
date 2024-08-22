ls.packages = c("brms",             # Bayesian lmms
                "tidyverse",        # tibble stuff
                "SBC"               # plots for checking computational faithfulness
)

lapply(ls.packages, library, character.only=TRUE)

# set cores
options(mc.cores = parallel::detectCores())

setwd('..')

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
df.fer = readRDS("FER_data.rds")

# set and print the contrasts
contrasts(df.fer$emo) = contr.sum(4)
contrasts(df.fer$emo)
contrasts(df.fer$diagnosis) = contr.sum(3)
contrasts(df.fer$diagnosis)

code = "FER_err"

# increase iterations a bit to improve rhats
iter = 4000
warm = 2000

# set the formula
f.err = brms::bf(acc.code ~ diagnosis * emo + (emo | subID) + (diagnosis | video) )

# set weakly informed priors
priors = c(
  prior(normal(2.0,   1.00), class = Intercept),
  prior(normal(1.0,   0.50), class = sd),
  prior(lkj(2),  class = cor),
  # set priors for the emotions based on Plank et al. (2022)
  prior(normal(-0.31, 0.50), class = b, coef = emo1), # afraid
  prior(normal(-0.23, 0.50), class = b, coef = emo2), # angry
  prior(normal(+1.63, 0.50), class = b, coef = emo3), # happy
  # no specific expectations for the rest of the effects
  prior(normal(0,     1.00), class = b)
)

# number of simulations
nsim = 500

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
gen  = SBC_generator_brms(f.err, data = df.fer, prior = priors, 
                          thin = 50, warmup = 10000, refresh = 2000,
                          generate_lp = TRUE, family = bernoulli, init = 0.1)
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
                                      warmup = warm, iter = iter)
dat_part = SBC_datasets(dat$variables[((i-1)*m + 1):(i*m),], 
                        dat$generated[((i-1)*m + 1):(i*m)])
res = compute_SBC(dat_part, 
                  bck,
                  cache_mode     = "results", 
                  cache_location = file.path(cache_dir, sprintf("res_%s_%d", code, i)))

write(sprintf('%s: DONE %d', now(), i), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)
