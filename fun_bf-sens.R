# 1 predictor -------------------------------------------------------------

bf_sens_1pred = function(m.orig, pred1, pr.desc, main.code, file.log, sense_dir, iter = 1000, reps = 1) {
  
ls.packages = c("brms", "bridgesampling", "tidyverse")

lapply(ls.packages, library, character.only = TRUE)

# set cores
ncores = parallel::detectCores()
options(mc.cores = ncores)

# create cache if it doesn't exist yet
if(!dir.exists(sense_dir)) {
  dir.create(sense_dir)
}

# output file
file.out = file.path(sense_dir, sprintf("df_%s_bf.csv", main.code))

# create the output file
if (!file.exists(file.out)) {
  write("population-level,bf.log,priors", file.out, append = F)
}

write(sprintf("\n---------------------------------------\n%s: %s\n", now(), pr.desc), file.log, append = TRUE)
write(sprintf("%s: %s", now(), as.character(m.orig$formula)[1]), file.log, append = TRUE)
# determine the priors
if (pr.desc == "chosen") {
  # extract the priors
  priors = m.orig$prior
  # set the code
  code = main.code
  m = m.orig
} else if (pr.desc == "default") {
  # get the default priors
  priors = get_prior(m.orig[["formula"]][["formula"]], 
                     data = m.orig$data, 
                     family = m.orig[["formula"]][["family"]][["family"]])
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else if (substr(pr.desc,1,3) == "sdx") {
  # set the code
  code = paste(main.code, gsub("\\.", "-", pr.desc), sep = "_")
  # extract the multiplier
  mult = as.numeric(gsub("sdx", "", pr.desc))
  # extract the priors and adjust the standard deviations
  priors = m.orig$prior %>%
    mutate(
      prior = case_when(
        grepl(",", prior) == T ~ 
          sprintf("%s, %f)", gsub(", .*\\)", "", prior), 
                  as.numeric(gsub(".*, (.*)\\)$", "\\1", prior))*mult),
        T ~ prior)
    )
} else if (substr(pr.desc,1,3) == "ind") {
  # get the default priors
  priors = readRDS(sprintf("%s_priors_%s.rds", main.code, pr.desc))
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else {
  # skip this iteration
  next
}

if (pr.desc != "chosen") {
  # redo the max model, no updating to avoid issues with bridgesampling
  m = brm(m.orig[["formula"]],
          m.orig[["data"]], prior = priors,
          iter = m.orig[["fit"]]@sim[["iter"]], warmup = m.orig[["fit"]]@sim[["warmup"]],
          family = m.orig[["formula"]][["family"]][["family"]],
          backend = "cmdstanr", 
          threads = threading(floor(ncores/(m.orig[["fit"]]@sim[["chains"]]))),
          file = file.path(sense_dir, sprintf("m_%s", code)),
          save_pars = save_pars(all = TRUE))
  # check for divergency issues
  div = sum(subset(nuts_params(m), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m), Parameter == "divergent__")$Value)
  if (div >= 0.05) {
    write(sprintf("divergence issues in full model,%.4f,%s",
                  NA,
                  pr.desc), file.out, append = T)
    stop("too many divergence issues in full model")
  }
}

# bridgesampling for max model
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s.rds", code)))) {
  MLL = bridgesampling::bridge_sampler(m, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL,  file.path(sense_dir, sprintf("MLL_%s.rds", code)))
} else {
  MLL = readRDS(file.path(sense_dir, sprintf("MLL_%s.rds", code)))
}
# check if we can use these estimates
if (all(MLL[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for full model", now()), file.log, append = TRUE)
  write(sprintf("no MLL for full model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# extract random effects from model
idx = min(gregexpr(pattern ='\\(', as.character(m[["formula"]][["formula"]])[3])[[1]])
random = substr(as.character(m[["formula"]][["formula"]])[3], idx, nchar(as.character(m[["formula"]][["formula"]])[3]))

# extract name of dependent variable
dvname = as.character(m[["formula"]][["formula"]][[2]])

# intercept only model
f0 = brms::bf(sprintf('%s ~ 1 + %s', dvname, random))
write(sprintf("%s: %s", now(), as.character(f0)[1]), file.log, append = TRUE)
m.0 = update(m, f0, save_pars = save_pars(all = T), 
             prior = priors %>% filter(class != "b"), 
             backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
             file = file.path(sense_dir, sprintf("m_%s_0", code)))
# check for divergency issues
div = sum(subset(nuts_params(m.0), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.0), Parameter == "divergent__")$Value)
if (div >= 0.05) {
  write(sprintf("divergence issues in intercept model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("too many divergence issues in intercept model")
}
# check if MLL exists, if not compute it
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))) {
  MLL.0 = bridgesampling::bridge_sampler(m.0, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL.0,  file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
} else {
  MLL.0 = readRDS(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
}
# check if we can use these estimates
if (all(MLL.0[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for intercept", now()), file.log, append = TRUE)
  write(sprintf("no MLL for intercept,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# compare model to intercept model
bf = median(as.numeric(bayes_factor(MLL,  MLL.0, log = T)[["bf"]]))

# save results
write(sprintf("1,%.4f,%s",
              0,
              pr.desc), file.out, append = T)
write(sprintf("%s,%.4f,%s", pred1,
              bf,
              pr.desc), file.out, append = T)
  
}

# 2 predictors ------------------------------------------------------------

bf_sens_2int = function(m.orig, pred1, pred2, pr.desc, main.code, file.log, sense_dir, iter = 1000, reps = 1) {

ls.packages = c("brms", "bridgesampling", "tidyverse")

lapply(ls.packages, library, character.only = TRUE)

# set cores
ncores = parallel::detectCores()
options(mc.cores = ncores)

# create cache if it doesn't exist yet
if(!dir.exists(sense_dir)) {
  dir.create(sense_dir)
}

# output file
file.out = file.path(sense_dir, sprintf("df_%s_bf.csv", main.code))

# create the output file
if (!file.exists(file.out)) {
  write("population-level,bf.log,priors", file.out, append = F)
}

write(sprintf("\n---------------------------------------\n%s: %s\n", now(), pr.desc), file.log, append = TRUE)
write(sprintf("%s: %s", now(), as.character(m.orig$formula)[1]), file.log, append = TRUE)
# determine the priors
if (pr.desc == "chosen") {
  # extract the priors
  priors = m.orig$prior
  # set the code
  code = main.code
  m = m.orig
} else if (pr.desc == "default") {
  # get the default priors
  priors = get_prior(m.orig[["formula"]][["formula"]], 
                     data = m.orig$data, 
                     family = m.orig[["formula"]][["family"]][["family"]])
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else if (substr(pr.desc,1,3) == "sdx") {
  # set the code
  code = paste(main.code, gsub("\\.", "-", pr.desc), sep = "_")
  # extract the multiplier
  mult = as.numeric(gsub("sdx", "", pr.desc))
  # extract the priors and adjust the standard deviations
  priors = m.orig$prior %>%
    mutate(
      prior = case_when(
        grepl(",", prior) == T ~ 
          sprintf("%s, %f)", gsub(", .*\\)", "", prior), 
                  as.numeric(gsub(".*, (.*)\\)$", "\\1", prior))*mult),
        T ~ prior)
    )
} else if (substr(pr.desc,1,3) == "ind") {
  # get the default priors
  priors = readRDS(sprintf("%s_priors_%s.rds", main.code, pr.desc))
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else {
  # skip this iteration
  next
}

if (pr.desc != "chosen") {
  # redo the max model, no updating to avoid issues with bridgesampling
  m = brm(m.orig[["formula"]],
          m.orig[["data"]], prior = priors,
          iter = m.orig[["fit"]]@sim[["iter"]], warmup = m.orig[["fit"]]@sim[["warmup"]],
          family = m.orig[["formula"]][["family"]][["family"]],
          backend = "cmdstanr", 
          threads = threading(floor(ncores/(m.orig[["fit"]]@sim[["chains"]]))),
          file = file.path(sense_dir, sprintf("m_%s", code)),
          save_pars = save_pars(all = TRUE))
  # check for divergency issues
  div = sum(subset(nuts_params(m), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m), Parameter == "divergent__")$Value)
  if (div >= 0.05) {
    write(sprintf("divergence issues in full model,%.4f,%s",
                  NA,
                  pr.desc), file.out, append = T)
    stop("too many divergence issues in full model")
  }
}

# bridgesampling for max model
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s.rds", code)))) {
  MLL = bridgesampling::bridge_sampler(m, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL,  file.path(sense_dir, sprintf("MLL_%s.rds", code)))
} else {
  MLL = readRDS(file.path(sense_dir, sprintf("MLL_%s.rds", code)))
}
# check if we can use these estimates
if (all(MLL[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for full model", now()), file.log, append = TRUE)
  write(sprintf("no MLL for full model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# extract random effects from model
idx = min(gregexpr(pattern ='\\(', as.character(m[["formula"]][["formula"]])[3])[[1]])
random = substr(as.character(m[["formula"]][["formula"]])[3], idx, nchar(as.character(m[["formula"]][["formula"]])[3]))

# extract name of dependent variable
dvname = as.character(m[["formula"]][["formula"]])[[2]]

# intercept only model
f0 = brms::bf(sprintf('%s ~ 1 + %s', dvname, random))
write(sprintf("%s: %s", now(), as.character(f0)[1]), file.log, append = TRUE)
m.0 = update(m, f0, save_pars = save_pars(all = T), 
             prior = priors %>% filter(class != "b"), 
             backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
             file = file.path(sense_dir, sprintf("m_%s_0", code)))
# check for divergency issues
div = sum(subset(nuts_params(m.0), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.0), Parameter == "divergent__")$Value)
if (div >= 0.05) {
  write(sprintf("divergence issues in intercept model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("too many divergence issues in intercept model")
}
# check if MLL exists, if not compute it
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))) {
  MLL.0 = bridgesampling::bridge_sampler(m.0, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL.0,  file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
} else {
  MLL.0 = readRDS(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
}
# check if we can use these estimates
if (all(MLL.0[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for intercept model", now()), file.log, append = TRUE)
  write(sprintf("no MLL for intercept,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# compare model to intercept model
bf = median(as.numeric(bayes_factor(MLL,  MLL.0, log = T)[["bf"]]))

# save results
write(sprintf("1,%.4f,%s",
              0,
              pr.desc), file.out, append = T)
write(sprintf("%s * %s,%.4f,%s", pred1, pred2,
              bf,
              pr.desc), file.out, append = T)

# get rid of the interaction
fi2 = brms::bf(sprintf('%s ~ %s + %s + %s', dvname, pred1, pred2, random))
write(sprintf("%s: %s", now(), as.character(fi2)[1]), file.log, append = TRUE)
m.i2   = update(m, fi2, save_pars = save_pars(all = T),
                prior = priors %>% filter(!grepl(":",coef)), 
                backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
                file = file.path(sense_dir, sprintf("m_%s_i2", code)))
# check for divergency issues
div = sum(subset(nuts_params(m.i2), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.i2), Parameter == "divergent__")$Value)
if (div >= 0.05) {
  # save results
  write(sprintf("%s + %s,%.4f,%s", pred1, pred2,
                NA,
                pr.desc), file.out, append = T)
} else {
  # check if MLL exists, otherwise compute it
  if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_i2.rds", code)))) {
    MLL.i2 = bridgesampling::bridge_sampler(m.i2, maxiter = iter, recompile = T, repetitions = reps, silent = T)
    # save the MLL
    saveRDS(MLL.i2,  file.path(sense_dir, sprintf("MLL_%s_i2.rds", code)))
  } else {
    MLL.i2 = readRDS(file.path(sense_dir, sprintf("MLL_%s_i2.rds", code)))
  }
  
  # compare model to intercept model
  if (all(MLL.i2[['niter']] > iter)) {
    bf = NA
  } else {
    bf = median(as.numeric(bayes_factor(MLL.i2,  MLL.0, log = T)[["bf"]]))
  }
  
  # save results
  write(sprintf("%s + %s,%.4f,%s", pred1, pred2,
                bf,
                pr.desc), file.out, append = T)
}

# get rid of the second predictor
fp1 = brms::bf(sprintf('%s ~ %s + %s', dvname, pred1, random))
write(sprintf("%s: %s", now(), as.character(fp1)[1]), file.log, append = TRUE)
m.p1   = update(m, fp1, save_pars = save_pars(all = T),
                prior = priors %>% filter(!grepl(pred2,coef)), 
                backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
                file = file.path(sense_dir, sprintf("m_%s_p1", code)))
# check for divergency issues
div = sum(subset(nuts_params(m.p1), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.p1), Parameter == "divergent__")$Value)
if (div >= 0.05) {
  # save results
  write(sprintf("%s,%.4f,%s",pred1,
                NA,
                pr.desc), file.out, append = T)
} else {
  # check if MLL exists, otherwise compute it
  if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_p1.rds", code)))) {
    MLL.p1 = bridgesampling::bridge_sampler(m.p1, maxiter = iter, recompile = T, repetitions = reps, silent = T)
    # save the MLL
    saveRDS(MLL.p1,  file.path(sense_dir, sprintf("MLL_%s_p1.rds", code)))
  } else {
    MLL.p1 = readRDS(file.path(sense_dir, sprintf("MLL_%s_p1.rds", code)))
  }
  
  # compare model to intercept model
  if (all(MLL.p1[['niter']] > iter)) {
    bf = NA
  } else {
    bf = median(as.numeric(bayes_factor(MLL.p1,  MLL.0, log = T)[["bf"]]))
  }
  
  # save results
  write(sprintf("%s,%.4f,%s",pred1,
                bf,
                pr.desc), file.out, append = T)
}

# get rid of the first predictor
fp2 = brms::bf(sprintf('%s ~ %s + %s', dvname, pred2, random))
write(sprintf("%s: %s", now(), as.character(fp2)[1]), file.log, append = TRUE)
m.p2   = update(m, fp2, save_pars = save_pars(all = T),
                prior = priors %>% filter(!grepl(pred1,coef)), 
                backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
                file = file.path(sense_dir, sprintf("m_%s_p2", code)))
# check for divergency issues
div = sum(subset(nuts_params(m.p2), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.p2), Parameter == "divergent__")$Value)
if (div >= 0.05) {
  # save results
  write(sprintf("%s,%.4f,%s",pred2,
                NA,
                pr.desc), file.out, append = T)
} else {
  # check if MLL exists, otherwise compute it
  if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_p2.rds", code)))) {
    MLL.p2 = bridgesampling::bridge_sampler(m.p2, maxiter = iter, recompile = T, repetitions = reps, silent = T)
    # save the MLL
    saveRDS(MLL.p2,  file.path(sense_dir, sprintf("MLL_%s_p2.rds", code)))
  } else {
    MLL.p2 = readRDS(file.path(sense_dir, sprintf("MLL_%s_p2.rds", code)))
  }
  
  # compare model to intercept model
  if (all(MLL.p2[['niter']] > iter)) {
    bf = NA
  } else {
    bf = median(as.numeric(bayes_factor(MLL.p2,  MLL.0, log = T)[["bf"]]))
  }
  
  # save results
  write(sprintf("%s,%.4f,%s",pred2,
                bf,
                pr.desc), file.out, append = T)
}
}


# 2 predictors + cov ------------------------------------------------------

bf_sens_2int1cov = function(m.orig, pred1, pred2, cov1, pr.desc, main.code, file.log, sense_dir, iter = 1000, reps = 1) {
  
ls.packages = c("brms", "bridgesampling", "tidyverse")

lapply(ls.packages, library, character.only = TRUE)

# set cores
ncores = parallel::detectCores()
options(mc.cores = ncores)

# create cache if it doesn't exist yet
if(!dir.exists(sense_dir)) {
  dir.create(sense_dir)
}

# output file
file.out = file.path(sense_dir, sprintf("df_%s_bf.csv", main.code))

# create the output file
if (!file.exists(file.out)) {
  write("population-level,bf.log,priors", file.out, append = F)
}

write(sprintf("\n---------------------------------------\n%s:\t%s\n", now(), pr.desc), file.log, append = TRUE)
write(sprintf("%s:\t%s * %s + %s", now(), pred1, pred2, cov1), file.log, append = TRUE)
# determine the priors
if (pr.desc == "chosen") {
  # extract the priors
  priors = m.orig$prior
  # set the code
  code = main.code
  m = m.orig
} else if (pr.desc == "default") {
  # get the default priors
  priors = get_prior(m.orig[["formula"]][["formula"]], 
                     data = m.orig$data, 
                     family = m.orig[["formula"]][["family"]][["family"]])
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else if (substr(pr.desc,1,3) == "sdx") {
  # set the code
  code = paste(main.code, gsub("\\.", "-", pr.desc), sep = "_")
  # extract the multiplier
  mult = as.numeric(gsub("sdx", "", pr.desc))
  # extract the priors and adjust the standard deviations
  priors = m.orig$prior %>%
    mutate(
      prior = case_when(
        grepl(",", prior) == T ~ 
          sprintf("%s, %f)", gsub(", .*\\)", "", prior), 
                  as.numeric(gsub(".*, (.*)\\)$", "\\1", prior))*mult),
        T ~ prior)
    )
} else if (substr(pr.desc,1,3) == "ind") {
  # get the default priors
  priors = readRDS(sprintf("%s_priors_%s.rds", main.code, pr.desc))
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else {
  # skip this iteration
  next
}

if (pr.desc != "chosen") {
  # redo the max model, no updating to avoid issues with bridgesampling
  m = brm(m.orig[["formula"]],
          m.orig[["data"]], prior = priors,
          iter = m.orig[["fit"]]@sim[["iter"]], warmup = m.orig[["fit"]]@sim[["warmup"]],
          family = m.orig[["formula"]][["family"]][["family"]],
          backend = "cmdstanr", 
          threads = threading(floor(ncores/(m.orig[["fit"]]@sim[["chains"]]))),
          file = file.path(sense_dir, sprintf("m_%s", code)),
          save_pars = save_pars(all = TRUE))
  # check for divergency issues
  div = sum(subset(nuts_params(m), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m), Parameter == "divergent__")$Value)
  if (div >= 0.05) {
    write(sprintf("divergence issues in full model,%.4f,%s",
                  NA,
                  pr.desc), file.out, append = T)
    stop("too many divergence issues in full model")
  }
}

# bridgesampling for max model
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s.rds", code)))) {
  MLL = bridgesampling::bridge_sampler(m, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL,  file.path(sense_dir, sprintf("MLL_%s.rds", code)))
} else {
  MLL = readRDS(file.path(sense_dir, sprintf("MLL_%s.rds", code)))
}
# check if we can use these estimates
if (all(MLL[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for full model", now()), file.log, append = TRUE)
  write(sprintf("no MLL for full model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# extract random effects from model
idx = min(gregexpr(pattern ='\\(', as.character(m[["formula"]][["formula"]])[3])[[1]])
random = substr(as.character(m[["formula"]][["formula"]])[3], idx, nchar(as.character(m[["formula"]][["formula"]])[3]))

# extract dependent variable
dvname = as.character(m[["formula"]][["formula"]])[[2]]

# intercept only model
f0 = brms::bf(sprintf('%s ~ 1 + %s', dvname, random))
write(sprintf("%s:\t1", now()), file.log, append = TRUE)
m.0 = update(m, f0, save_pars = save_pars(all = T), newdata = m.orig[["data"]],
             prior = priors %>% filter(class != "b"), 
             backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
             file = file.path(sense_dir, sprintf("m_%s_0", code)))
# check for divergency issues
div = sum(subset(nuts_params(m.0), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.0), Parameter == "divergent__")$Value)
if (div >= 0.05) {
  write(sprintf("divergence issues in intercept model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("too many divergence issues in intercept model")
}
# check if MLL exists, if not compute it
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))) {
  MLL.0 = bridgesampling::bridge_sampler(m.0, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL.0,  file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
} else {
  MLL.0 = readRDS(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
}
# check if we can use these estimates
if (all(MLL.0[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for intercept model", now()), file.log, append = TRUE)
  write(sprintf("no MLL for intercept model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# compare model to intercept model
bf = median(as.numeric(bayes_factor(MLL,  MLL.0, log = T)[["bf"]]))

# save results
write(sprintf("1,%.4f,%s",
              0,
              pr.desc), file.out, append = T)
write(sprintf("%s * %s + %s,%.4f,%s", pred1, pred2, cov1,
              bf,
              pr.desc), file.out, append = T)

# create a list of all the fixed effects combinations 
fixed = c(
  pred1,
  pred2,
  cov1,
  sprintf('%s + %s', pred1, pred2),
  sprintf('%s + %s', pred1, cov1),
  sprintf('%s + %s', pred2, cov1),
  sprintf('%s + %s + %s', pred1, pred2, cov1),
  sprintf('%s + %s + %s:%s', pred1, pred2, pred1, pred2)
)

# LOOP THROUGH ALL THE MODELS

for (b in 1:length(fixed)) {
  # build the formula
  f = brms::bf(sprintf('%s ~ %s + %s', dvname, fixed[b], random))
  write(sprintf("%s:\t%s", now(), fixed[b]), file.log, append = TRUE)
  # update the priors
  preds = str_split(fixed[b], pattern = " \\+ ")[[1]]
  priors_current = priors %>% 
    filter((class == "b" & gsub('[[:digit:]]+', '', coef) %in% preds) | 
             (class == "b" & coef == "") | 
             (class != "b" ))
  # update the model
  m.up   = update(m, f, save_pars = save_pars(all = T),
                  prior = priors_current, 
                  backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
                  file = file.path(sense_dir, sprintf("m_%s_%02d", code, b)))
  # check for divergency issues
  div = sum(subset(nuts_params(m.up), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.up), Parameter == "divergent__")$Value)
  if (div >= 0.05) {
    # save results
    write(sprintf("%s,%.4f,%s", fixed[b],
                  NA,
                  pr.desc), file.out, append = T)
  } else {
    # check if MLL exists, otherwise compute it
    if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_%02d.rds", code, b)))) {
      MLL.up = bridgesampling::bridge_sampler(m.up, maxiter = iter, recompile = T, repetitions = reps, silent = T)
      # save the MLL
      saveRDS(MLL.up,  file.path(sense_dir, sprintf("MLL_%s_%02d.rds", code, b)))
    } else {
      MLL.up = readRDS(file.path(sense_dir, sprintf("MLL_%s_%02d.rds", code, b)))
    }
    
    # compare model to intercept model
    if (all(MLL.up[['niter']] > iter)) {
      bf = NA
    } else {
      bf = median(as.numeric(bayes_factor(MLL.up,  MLL.0, log = T)[["bf"]]))
    }
    
    # save results
    write(sprintf("%s,%.4f,%s", fixed[b],
                  bf,
                  pr.desc), file.out, append = T)
  }
}
  
}

# 3 predictors ------------------------------------------------------------

bf_sens_3int = function(m.orig, pred1, pred2, pred3, pr.desc, main.code, file.log, sense_dir, iter = 1000, reps = 1) {

ls.packages = c("brms", "bridgesampling", "tidyverse")

lapply(ls.packages, library, character.only = TRUE)

# set cores
ncores = parallel::detectCores()
options(mc.cores = ncores)

# create cache if it doesn't exist yet
if(!dir.exists(sense_dir)) {
  dir.create(sense_dir)
}

# output file
file.out = file.path(sense_dir, sprintf("df_%s_bf.csv", main.code))

# create the output file
if (!file.exists(file.out)) {
  write("population-level,bf.log,priors", file.out, append = F)
}

write(sprintf("\n---------------------------------------\n%s:\t%s\n", now(), pr.desc), file.log, append = TRUE)
write(sprintf("%s:\t%s * %s * %s", now(), pred1, pred2, pred3), file.log, append = TRUE)
# determine the priors
if (pr.desc == "chosen") {
  # extract the priors
  priors = m.orig$prior
  # set the code
  code = main.code
  m = m.orig
} else if (pr.desc == "default") {
  # get the default priors
  priors = get_prior(m.orig[["formula"]][["formula"]], 
                     data = m.orig$data, 
                     family = m.orig[["formula"]][["family"]][["family"]])
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else if (substr(pr.desc,1,3) == "sdx") {
  # set the code
  code = paste(main.code, gsub("\\.", "-", pr.desc), sep = "_")
  # extract the multiplier
  mult = as.numeric(gsub("sdx", "", pr.desc))
  # extract the priors and adjust the standard deviations
  priors = m.orig$prior %>%
    mutate(
      prior = case_when(
        grepl(",", prior) == T ~ 
          sprintf("%s, %f)", gsub(", .*\\)", "", prior), 
                  as.numeric(gsub(".*, (.*)\\)$", "\\1", prior))*mult),
        T ~ prior)
    )
} else if (substr(pr.desc,1,3) == "ind") {
  # get the default priors
  priors = readRDS(sprintf("%s_priors_%s.rds", main.code, pr.desc))
  # set the code
  code = paste(main.code, pr.desc, sep = "_")
} else {
  # skip this iteration
  next
}

if (pr.desc != "chosen") {
  # redo the max model, no updating to avoid issues with bridgesampling
  m = brm(m.orig[["formula"]],
          m.orig[["data"]], prior = priors,
          iter = m.orig[["fit"]]@sim[["iter"]], warmup = m.orig[["fit"]]@sim[["warmup"]],
          family = m.orig[["formula"]][["family"]][["family"]],
          backend = "cmdstanr", 
          threads = threading(floor(ncores/(m.orig[["fit"]]@sim[["chains"]]))),
          file = file.path(sense_dir, sprintf("m_%s", code)),
          save_pars = save_pars(all = TRUE))
  # check for divergency issues
  div = sum(subset(nuts_params(m), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m), Parameter == "divergent__")$Value)
  if (div >= 0.05) {
    write(sprintf("divergence issues in full model,%.4f,%s",
                  NA,
                  pr.desc), file.out, append = T)
    stop("too many divergence issues in full model")
  }
}

# bridgesampling for max model
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s.rds", code)))) {
  MLL = bridgesampling::bridge_sampler(m, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL,  file.path(sense_dir, sprintf("MLL_%s.rds", code)))
} else {
  MLL = readRDS(file.path(sense_dir, sprintf("MLL_%s.rds", code)))
}
# check if we can use these estimates
if (all(MLL[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for full model", now()), file.log, append = TRUE)
  write(sprintf("no MLL for full model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# extract random effects from model
idx = min(gregexpr(pattern ='\\(', as.character(m[["formula"]][["formula"]])[3])[[1]])
random = substr(as.character(m[["formula"]][["formula"]])[3], idx, nchar(as.character(m[["formula"]][["formula"]])[3]))

# extract name of dependent variable
dvname = as.character(m[["formula"]][["formula"]])[[2]]

# intercept only model
f0 = brms::bf(sprintf('%s ~ 1 + %s', dvname, random))
write(sprintf("%s:\t1", now()), file.log, append = TRUE)
m.0 = update(m, f0, save_pars = save_pars(all = T), newdata = m.orig[["data"]],
             prior = priors %>% filter(class != "b"), 
             backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
             file = file.path(sense_dir, sprintf("m_%s_0", code)))
# check for divergency issues
div = sum(subset(nuts_params(m.0), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.0), Parameter == "divergent__")$Value)
if (div >= 0.05) {
  write(sprintf("divergence issues in intercept model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("too many divergence issues in intercept model")
}
# check if MLL exists, if not compute it
if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))) {
  MLL.0 = bridgesampling::bridge_sampler(m.0, maxiter = iter, recompile = T, repetitions = reps, silent = T)
  # save the MLL
  saveRDS(MLL.0,  file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
} else {
  MLL.0 = readRDS(file.path(sense_dir, sprintf("MLL_%s_0.rds", code)))
}
# check if we can use these estimates
if (all(MLL.0[['niter']] > iter)){
  write(sprintf("%s: ERROR no stable logml for full model", now()), file.log, append = TRUE)
  write(sprintf("no MLL for intercept model,%.4f,%s",
                NA,
                pr.desc), file.out, append = T)
  stop("logml could not be estimated within maxiter")
}

# compare model to intercept model
bf = median(as.numeric(bayes_factor(MLL,  MLL.0, log = T)[["bf"]]))

# save results
write(sprintf("1,%.4f,%s",
              0,
              pr.desc), file.out, append = T)
write(sprintf("%s * %s * %s,%.4f,%s", pred1, pred2, pred3,
              bf,
              pr.desc), file.out, append = T)

# create a list of all the fixed effects combinations 
fixed = c(
  pred1,
  pred2,
  pred3,
  sprintf('%s + %s', pred1, pred2),
  sprintf('%s + %s', pred1, pred3),
  sprintf('%s + %s', pred2, pred3),
  sprintf('%s + %s + %s', pred1, pred2, pred3),
  sprintf('%s + %s + %s:%s', pred1, pred2, pred1, pred2),
  sprintf('%s + %s + %s + %s:%s', pred1, pred2, pred3, pred1, pred2),
  sprintf('%s + %s + %s:%s', pred2, pred3, pred2, pred3),
  sprintf('%s + %s + %s + %s:%s', pred1, pred2, pred3, pred2, pred3),
  sprintf('%s + %s + %s + %s:%s + %s:%s', pred1, pred2, pred3, pred1, pred2, pred2, pred3),
  sprintf('%s + %s + %s:%s', pred1, pred3, pred1, pred3),
  sprintf('%s + %s + %s + %s:%s', pred1, pred2, pred3, pred1, pred3),
  sprintf('%s + %s + %s + %s:%s + %s:%s', pred1, pred2, pred3, pred1, pred2, pred1, pred3),
  sprintf('%s + %s + %s + %s:%s + %s:%s', pred1, pred2, pred3, pred2, pred3, pred1, pred3),
  sprintf('%s + %s + %s + %s:%s + %s:%s + %s:%s', pred1, pred2, pred3, pred1, pred2, pred2, pred3, pred1, pred3)
)

# LOOP THROUGH ALL THE MODELS

for (b in 1:length(fixed)) {
  # build the formula
  f = brms::bf(sprintf('%s ~ %s + %s', dvname, fixed[b], random))
  write(sprintf("%s:\t%s", now(), fixed[b]), file.log, append = TRUE)
  # update the priors
  preds = str_split(fixed[b], pattern = " \\+ ")[[1]]
  priors_current = priors %>% 
    filter((class == "b" & gsub('[[:digit:]]+', '', coef) %in% preds) | 
             (class == "b" & coef == "") | 
             (class != "b" ))
  # update the model
  m.up   = update(m, f, save_pars = save_pars(all = T),
                  prior = priors_current, 
                  backend = "cmdstanr", threads = threading(floor(ncores/(m[["fit"]]@sim[["chains"]]))),
                  file = file.path(sense_dir, sprintf("m_%s_%02d", code, b)))
  # check for divergency issues
  div = sum(subset(nuts_params(m.up), Parameter == "divergent__")$Value > 0)/length(subset(nuts_params(m.up), Parameter == "divergent__")$Value)
  if (div >= 0.05) {
    # save results
    write(sprintf("%s,%.4f,%s", fixed[b],
                  NA,
                  pr.desc), file.out, append = T)
  } else {
    # perform bridge sampling
    if (!file.exists(file.path(sense_dir, sprintf("MLL_%s_%02d.rds", code, b)))) {
      MLL.up = bridgesampling::bridge_sampler(m.up, maxiter = iter, recompile = T, repetitions = reps, silent = T)
      # save the MLL
      saveRDS(MLL.up,  file.path(sense_dir, sprintf("MLL_%s_%02d.rds", code, b)))
    } else {
      MLL.up = readRDS(file.path(sense_dir, sprintf("MLL_%s_%02d.rds", code, b)))
    }
    
    # compare model to intercept model
    if (all(MLL.up[['niter']] > iter)) {
      bf = NA
    } else {
      bf = median(as.numeric(bayes_factor(MLL.up,  MLL.0, log = T)[["bf"]]))
    }
    
    # save results
    write(sprintf("%s,%.4f,%s", fixed[b],
                  bf,
                  pr.desc), file.out, append = T)
  }
}
  
}
