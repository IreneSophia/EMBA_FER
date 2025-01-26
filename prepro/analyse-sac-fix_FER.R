# This script reads in the fixations of each participant and evaluates them to assess whether they are relevant for the analysis. It depends on the data created by the MATLAB preprocessing of the eye tracking data as well as the preprocessed behavioural data. (c) Irene Sophia Plank. 

# setup
library(tidyverse)
fl.path = '/home/emba/Documents/EMBA'
dt.path  = paste(fl.path, 'BVET', sep = "/")
dt.explo = paste(fl.path, 'BVET-explo', sep = "/")

# load the data -----------------------------------------------------------

# behavioural data
df.beh = rbind(readRDS(paste(dt.path,  "df_FER.RDS", sep = "/")),
               readRDS(paste(dt.explo, "df_FER.RDS", sep = "/")))

# fixation data
df.fix.all = c(list.files(path = dt.path,  pattern = "^FER-ET.*_fixations_AOI.csv", full.names = T),
               list.files(path = dt.explo, pattern = "^FER-ET.*_fixations_AOI.csv", full.names = T))%>%
  setNames(nm = .) %>%
  map_df(~read_csv(., show_col_types = F), .id = "fln") %>% 
  mutate(
    subID = gsub(".*FER-ET-(.+)_fixations.*", "\\1", fln),
    off_trialStm = as.numeric(gsub("pic_", "", off_trialStm)),
    on_trialStm  = as.numeric(gsub("pic_", "", on_trialStm))
  ) %>%
  mutate_if(is.character, as.factor)

# get subIDs
subIDs = unique(df.fix.all$subID)

# fixation analysis -------------------------------------------------------

# load the relevant fixation data in long format
df.fix = df.fix.all %>% 
  # sum up fixation durations for each trial: 
  # during a trial and start and end of fixation in the same trial
  filter(!is.na(off_trialNo) & 
           on_trialNo == off_trialNo) %>%
  group_by(subID, on_trialNo) %>%
  mutate(
    fix.total = sum(duration)
  ) %>%
  # only keep fixations in an AOI
  filter(!is.na(AOI)) %>% 
  group_by(subID, AOI, on_trialNo, fix.total) %>%
  summarise(
    n.fix = n(),
    fix.dur = sum(duration)
  ) %>%
  rename("trl" = "on_trialNo") %>%
  arrange(subID, trl, AOI)

# add zeros for all AOIs that were not fixated
df.fix = merge(df.fix, 
               data.frame(
                 subID    = rep(subIDs, each = max(df.fix$trl)*length(unique(df.fix$AOI))),
                 trl      = rep(rep(1:max(df.fix$trl), each = length(unique(df.fix$AOI))), times = length(subIDs)),
                 AOI      = rep(unique(df.fix$AOI), length.out = length(subIDs)*length(unique(df.fix$AOI))*max(df.fix$trl))), 
               all = T) %>%
  mutate(
    fix.dur = if_else(!is.na(fix.dur), fix.dur, 0),
    n.fix   = if_else(!is.na(n.fix), n.fix, 0)
  ) %>%
  arrange(subID, trl, AOI) %>%
  mutate(
    fix.perc = if_else(is.na(fix.total),0,fix.dur/fix.total)
  )

# merge with behavioural data to only keep participants whose behavioural data 
# was analysed
df.fix = merge(df.fix, df.beh) %>%
  mutate_if(is.character, as.factor) %>% 
  group_by(subID) %>%
  mutate(
    n.fix.total = sum(n.fix)
  ) %>%
  filter(
    n.fix.total > max(df.fix$trl)
  ) %>% ungroup()

# saccade analysis --------------------------------------------------------

# load the relevant saccade data in long format
df.sac.all = c(list.files(path = dt.path,  pattern = "^FER-ET.*_saccades_AOI.csv", full.names = T),
               list.files(path = dt.explo, pattern = "^FER-ET.*_saccades_AOI.csv", full.names = T))%>%
  setNames(nm = .) %>%
  map_df(~read_csv(., show_col_types = F), .id = "fln") %>% 
  mutate(
    subID = gsub(".*FER-ET-(.+)_saccades.*", "\\1", fln),
  ) %>%
  rename("trl" = "on_trialNo")

subIDs = unique(df.sac.all$subID)

# add zeros when no saccades
df.sac.all = merge(df.sac.all, 
               data.frame(
                 subID    = rep(subIDs, each = max(df.sac.all$trl)),
                 trl      = rep(1:max(df.sac.all$trl), times = length(subIDs))
                 ) %>% mutate(sac = 0), 
               all = T)

# merge with behavioural data for more info on each trial and to only include
# participants also included in the behavioural analysis
df.sac.all = merge(df.sac.all, df.beh)
subIDs = unique(df.sac.all$subID)

df.sac = df.sac.all %>% 
  # only keep those that move between AOIs
  filter(on_AOI != off_AOI) %>% 
  mutate(AOI = as.factor(paste(on_AOI, off_AOI))) %>%
  # sum the saccades for each trial
  group_by(subID, trl, AOI) %>%
  summarise(
    n.sac = n()
  )

# again, add zeros when no saccades for an AOI
df.sac = merge(df.sac, 
               data.frame(
                 subID    = rep(subIDs, each = max(df.sac$trl)*length(unique(df.sac$AOI))),
                 trl      = rep(rep(1:max(df.sac$trl), each = length(unique(df.sac$AOI))), times = length(subIDs)),
                 AOI      = rep(unique(df.sac$AOI), length.out = length(subIDs)*length(unique(df.sac$AOI))*max(df.sac$trl))), 
               all = T) %>%
  mutate(
    n.sac  = if_else(!is.na(n.sac), n.sac, 0)
  )

df.sac = merge(df.sac, df.beh) %>%
  # how many would it be if they had seen all frames
  mutate(
    n.sac = (n.sac*300) / frames
  ) %>%
  mutate_if(is.character, as.factor) %>% 
  # sum up number of saccades per participant to exclude participants with fewer
  # saccades than number of trials left
  group_by(subID) %>%
  mutate(
    n.sac.total = sum(n.sac)
  ) %>%
  filter(
    n.sac.total > max(df.sac$trl)
  ) %>% ungroup()

# explorative: first fixation ---------------------------------------------

df.first = df.fix.all %>% 
  filter(!is.na(AOI)) %>% 
  group_by(subID, on_trialNo) %>%
  mutate(
    rown = row_number()
  ) %>% ungroup() %>%
  filter(rown == 1) %>%
  select(subID, on_trialNo, on_trialStm, off_trialStm, AOI) %>%
  rename("trl" = "on_trialNo",
         "pic_start" = "on_trialStm",
         "pic_end"   = "off_trialStm") %>%
  mutate_if(is.character, as.factor)

# merge with behavioural data
df.first = merge(df.first, df.beh) %>%
  mutate_if(is.character, as.factor) %>% 
  group_by(subID) %>%
  mutate(
    n.first = n()
  ) %>%
  # only keep participants with at least 2/3 of trials
  filter(
    n.first > max(df.first$trl)*2/3
  ) %>% ungroup()

# explorative: last fixation before decision ------------------------------

df.last = df.fix.all %>% 
  filter(!is.na(AOI)) %>% 
  group_by(subID, on_trialNo) %>%
  mutate(
    rown = row_number(),
    last = max(rown)
  ) %>% ungroup() %>%
  filter(rown == last) %>%
  select(subID, on_trialNo, on_trialStm, off_trialStm, AOI) %>%
  rename("trl" = "on_trialNo",
         "pic_start" = "on_trialStm",
         "pic_end" = "off_trialStm")

df.last = merge(df.last, df.beh) %>% 
  filter(!is.na(AOI) & !is.na(frames)) %>%
  mutate_if(is.character, as.factor) %>% 
  group_by(subID) %>%
  mutate(
    n.last = n()
  ) %>%
  # only keep participants with at least 2/3 of trials
  filter(
    n.last > max(df.last$trl)*2/3
  ) %>% ungroup()

# Save --------------------------------------------------------------------

# save the data for analysis
save(file = paste(dt.path, "FER_ET_data.RData", sep = "/"), 
     list = c("df.fix", "df.sac", "df.sac.all", "df.first", "df.last"))
