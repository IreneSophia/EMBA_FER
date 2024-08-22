library(brms)            # Bayesian lmms
library(tidyverse)       # tibble stuff
library(designr)
library(SBC)

# set options for SBC
use_cmdstanr = getOption("SBC.vignettes_cmdstanr", TRUE) # Set to false to use rstan instead
options(brms.backend = "cmdstanr")

# using parallel processing
library(future)
plan(multisession)

setwd('..')

# Setup caching of results
cache_dir = "./_brms_SBC_cache"
if(!dir.exists(cache_dir)) {
  dir.create(cache_dir)
}

# load the data set
df.first  = readRDS("FER_first.rds")

# set number of simulations
nsim = 500

# set and print the contrasts
contrasts(df.first$diagnosis) = contr.sum(3)
contrasts(df.first$diagnosis)

code = "FER_first"

# find out which priors need to be set
f.first = brms::bf(AOI.code ~ diagnosis + (1 | subID) + (diagnosis | video))

priors = c(
  prior(normal(-2, 1.5),   class  = "Intercept", dpar = "mu2"),
  prior(normal(-2, 1.5),   class  = "Intercept", dpar = "mu3"),
  prior(normal(-2, 1.5),   class  = "Intercept", dpar = "mu4"),
  prior(normal(0,  1),   class  = "b", dpar = "mu2"),
  prior(normal(0,  1),   class  = "b", dpar = "mu3"),
  prior(normal(0,  1),   class  = "b", dpar = "mu4"),
  prior(normal(0,  1),   class  = "sd", dpar = "mu2"),
  prior(normal(0,  1),   class  = "sd", dpar = "mu3"),
  prior(normal(0,  1),   class  = "sd", dpar = "mu4"),
  prior(lkj(2),         class = cor)
)

# get the last number
ls.files = list.files(path = cache_dir, pattern = sprintf("res_%s_([0-9]+).rds", code))
if (is_empty(ls.files)) {
  i = 1
} else {
  i = max(as.numeric(gsub(sprintf("res_%s_(.+).rds", code), "\\1", ls.files))) + 1
}

# set the seed based on this
set.seed(2468+i)

# create the data
gen  = SBC_generator_brms(f.first, data = df.first, prior = priors, family = categorical,
                          thin = 50, warmup = 20000, refresh = 2000
)
if (!file.exists(file.path(cache_dir, paste0("dat_", code, ".rds")))){
  dat = generate_datasets(gen, nsim)
  saveRDS(dat, file = file.path(cache_dir, paste0("dat_", code, ".rds")))
} else {
  dat = readRDS(file = file.path(cache_dir, paste0("dat_", code, ".rds")))
}
bck = SBC_backend_brms_from_generator(gen, chains = 4, thin = 1,
                                      warmup = 1000, iter = 3000,
                                      init = 0.1)
m = 50

# perform the SBC
write(sprintf('%s: %s %d', now(), code, i), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)

dat_part = SBC_datasets(dat$variables[((i-1)*m + 1):(i*m),],
                        dat$generated[((i-1)*m + 1):(i*m)])
res = compute_SBC(dat_part, bck,
                  cache_mode = "results",
                  cache_location = file.path(cache_dir, sprintf("res_%s_%02d", code, i)))

write(sprintf('%s: DONE %d', now(), i), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)
