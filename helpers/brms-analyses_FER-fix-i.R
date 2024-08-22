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

# load the eye tracking data
df.fix = readRDS("FER_fix.rds") %>%
  group_by(subID, diagnosis, emo, AOI) %>%
  summarise(
    fix.perc = mean(fix.perc, na.rm = T)
  ) %>% ungroup()

# set and print the contrasts
contrasts(df.fix$emo) = contr.sum(4)
contrasts(df.fix$diagnosis) = contr.sum(3)
contrasts(df.fix$AOI) = contr.sum(4)

# number of simulations
nsim = 500

code = "FER_fix"

# set the formula
f.fix = brms::bf(fix.perc ~ diagnosis * emo * AOI + (emo + AOI | subID))

# set weakly informative priors
priors = c(
  # Intercept mean based on Kirchner et al. (2011)
  prior(normal(log(0.20), 0.20), class = Intercept), 
  prior(normal(0.50,      0.50), class = sigma),
  prior(normal(0,         0.50), class = sd),
  prior(normal(0,         0.20), class = b),
  prior(lkj(2),                 class = cor),
  # due to the aggregation, we only expect low numbers of zeros
  prior(beta(1, 10),            class = hu) 
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
gen  = SBC_generator_brms(f.fix, data = df.fix, prior = priors, family = hurdle_lognormal,
                          thin = 50, warmup = 20000, refresh = 2000)
if (!file.exists(file.path(cache_dir, paste0("dat_", code, ".rds")))){
  dat = generate_datasets(gen, nsim) 
  saveRDS(dat, file = file.path(cache_dir, paste0("dat_", code, ".rds")))
} else {
  dat = readRDS(file = file.path(cache_dir, paste0("dat_", code, ".rds")))
}
m = 125

write(sprintf('%s: %s %d', now(), code, i), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)

bck = SBC_backend_brms_from_generator(gen, chains = 4, thin = 1,
                                      init = 0.1, warmup = 1000, iter = 4000)
plan(multisession)
print("start res")
res = compute_SBC(SBC_datasets(dat$variables[((i-1)*m + 1):(i*m),], 
                               dat$generated[((i-1)*m + 1):(i*m)]), 
                  bck,
                  cache_mode     = "results", 
                  cache_location = file.path(cache_dir, sprintf("res_%s_%d", code, i)))

write(sprintf('%s: DONE %d', now(), i), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)
