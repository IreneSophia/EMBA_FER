# setup
library(tidyverse)
fl.path = '/home/emba/Documents/EMBA'
dt.path = paste(fl.path, 'BVET', sep = "/")


# load the data -----------------------------------------------------------

# behavioural data
df.beh = readRDS(paste(dt.path, "df_FER.RDS", sep = "/"))

# fixation data
df.fix.all = list.files(path = dt.path, pattern = "^FER-ET.*_fixations_AOI.csv", full.names = T) %>%
  setNames(nm = .) %>%
  map_df(~read_csv(., show_col_types = F), .id = "fln") %>% 
  mutate(
    subID = gsub(paste0(dt.path,"/FER-ET-"), "", gsub("_fixations_AOI.csv", "", fln)),
    off_trialStm = as.numeric(gsub("pic_", "", off_trialStm)),
    on_trialStm  = as.numeric(gsub("pic_", "", on_trialStm))
  )

# fixation analysis -------------------------------------------------------

# load the relevant fixation data in long format
df.fix = df.fix.all %>% 
  filter(!is.na(AOI) & 
           !is.na(off_trialNo) & 
           on_trialNo == off_trialNo) %>% 
  group_by(subID, AOI, on_trialNo) %>%
  summarise(
    n.fix = n(),
    fix.dur = sum(duration)
  ) %>%
  rename("trl" = "on_trialNo")

# add zeros for all AOIs that were not fixated
df.fix = merge(df.fix, 
               data.frame(
                 subID    = rep(unique(df.fix$subID), each = max(df.fix$trl)*length(unique(df.fix$AOI))),
                 trl      = rep(rep(1:max(df.fix$trl), each = length(unique(df.fix$AOI))), times = length(unique(df.fix$subID))),
                 AOI      = rep(unique(df.fix$AOI), length.out = length(unique(df.fix$subID))*length(unique(df.fix$AOI))*max(df.fix$trl))), 
               all = T) %>%
  mutate(
    fix.dur = if_else(!is.na(fix.dur), fix.dur, 0),
    n.fix   = if_else(!is.na(n.fix), n.fix, 0)
  )

# merge with behavioural data
df.fix = merge(df.fix, df.beh %>% filter(subID %in% unique(df.fix$subID)), all.x = T) %>%
  # how many would it be if they had seen all frames
  mutate(
    fix.dur = fix.dur * 300 / frames
  ) %>% 
  # only keep participants who are included in behavioural analysis
  filter(!is.na(emo)) %>%
  mutate_if(is.character, as.factor)

# saccade analysis --------------------------------------------------------

# load the relevant saccade data in long format
df.sac = list.files(path = dt.path, pattern = "^FER-ET.*_saccades_AOI.csv", full.names = T) %>%
  setNames(nm = .) %>%
  map_df(~read_csv(., show_col_types = F), .id = "fln") %>% 
  mutate(
    subID = gsub(paste0(dt.path,"/FER-ET-"), "", gsub("_saccades_AOI.csv", "", fln))
  ) %>% 
  filter(on_AOI != off_AOI) %>% 
  mutate(AOI = as.factor(paste(on_AOI, off_AOI))) %>%
  group_by(subID, on_trialNo, AOI) %>%
  summarise(
    n.sac = n()
  ) %>%
  rename("trl" = "on_trialNo")

# add zeros when no saccades
df.sac = merge(df.sac, 
               data.frame(
                 subID    = rep(unique(df.sac$subID), each = max(df.sac$trl)*length(unique(df.sac$AOI))),
                 trl      = rep(rep(1:max(df.sac$trl), each = length(unique(df.sac$AOI))), times = length(unique(df.sac$subID))),
                 AOI      = rep(unique(df.sac$AOI), length.out = length(unique(df.sac$subID))*length(unique(df.sac$AOI))*max(df.sac$trl))), 
               all = T) %>%
  mutate(
    n.sac   = if_else(!is.na(n.sac), n.sac, 0)
  )

# merge with behavioural data
df.sac = merge(df.sac, df.beh, all.x = T) %>%
  # how many would it be if they had seen all frames
  mutate(
    n.sac = (n.sac*300) / frames
  ) %>% filter(!is.na(emo)) %>%
  mutate_if(is.character, as.factor)

# explorative: first fixation ---------------------------------------------

df.fix.first = df.fix.all %>% 
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
df.fix.first = merge(df.fix.first, df.beh, all.x = T) %>%
  mutate_if(is.character, as.factor)

# explorative: last fixation before decision ------------------------------

df.fix.last = df.fix.all %>% 
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

df.fix.last = merge(df.fix.last, df.beh, all = T) %>% 
  filter(!is.na(AOI) & !is.na(frames)) %>%
  mutate_if(is.character, as.factor)

# Save --------------------------------------------------------------------

# save the data for analysis
save(file = paste(dt.path, "FER_ET_data.RData", sep = "/"), list = c("df.fix", "df.sac", "df.fix.first", "df.fix.last"))

