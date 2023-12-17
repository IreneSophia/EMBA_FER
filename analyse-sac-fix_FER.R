# setup
library(tidyverse)
fl.path = '/home/emba/Documents/EMBA'
dt.path = paste(fl.path, 'BVET', sep = "/")

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
  )

# visualise the distribution
ggplot(data = df.fix, aes(x = fix.dur, fill = AOI)) +
  geom_density(alpha = .3, colour = "lightgrey") + 
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
  count()

# visualise the distribution
ggplot(data = df.sac, aes(x = n)) +
  geom_density(alpha = .3, colour = "lightgrey", fill = "lightblue") + 
  theme_bw()
