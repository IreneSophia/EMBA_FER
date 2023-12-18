# setup
library(tidyverse)
fl.path = '/home/emba/Documents/EMBA'
dt.path = paste(fl.path, 'BVET', sep = "/")

# load the behavioural data -----------------------------------------------

df.beh = readRDS(paste(dt.path, "df_FER.RDS", sep = "/"))

# fixation analysis -------------------------------------------------------

# load the relevant fixation data in long format
df.fix = list.files(path = dt.path, pattern = "FER-ET.*_fixations_AOI.csv", full.names = T) %>%
  setNames(nm = .) %>%
  map_df(~read_csv(., show_col_types = F), .id = "fln") %>% 
  mutate(
    subID = gsub(paste0(dt.path,"/FER-ET-"), "", gsub("_fixations_AOI.csv", "", fln))
  ) %>% 
  filter(!is.na(AOI)) %>% 
  group_by(subID, AOI, on_trialNo) %>%
  summarise(
    n = n(),
    fix.dur = median(duration)
  ) %>%
  rename("trl" = "on_trialNo")

# merge with behavioural data
df.fix = merge(df.fix, df.beh, all.x = T) %>%
  # how many would it be if they had seen all frames
  mutate(
    fix.dur = fix.dur * 300 / frames
  ) %>% filter(!is.na(fix.dur))

# visualise the distribution
ggplot(data = df.fix, aes(x = fix.dur)) +
  geom_density(alpha = .3, colour = "lightgrey", fill = "lightblue") + 
  theme_bw()

# saccade analysis --------------------------------------------------------

# load the relevant fixation data in long format
df.sac = list.files(path = dt.path, pattern = "FER-ET.*_saccades_AOI.csv", full.names = T) %>%
  setNames(nm = .) %>%
  map_df(~read_csv(., show_col_types = F), .id = "fln") %>% 
  mutate(
    subID = gsub(paste0(dt.path,"/FER-ET-"), "", gsub("_saccades_AOI.csv", "", fln))
  ) %>% 
  filter(on_AOI != off_AOI) %>% 
  group_by(subID, on_trialNo) %>%
  summarise(
    n.sac = n()
  ) %>%
  rename("trl" = "on_trialNo")

# merge with behavioural data
df.sac = merge(df.sac, df.beh, all.x = T) %>%
  # how many would it be if they had seen all frames
  mutate(
    n.sac = (n.sac*300) / frames
  ) %>% filter(!is.na(n.sac))

# visualise the distribution
ggplot(data = df.sac, aes(x = n.sac)) +
  geom_density(alpha = .3, colour = "lightgrey", fill = "lightblue") + 
  theme_bw()

# save the data for analysis
save(file = paste(dt.path, "FER_ET_data.RData", sep = "/"), list = c("df.fix", "df.sac"))

