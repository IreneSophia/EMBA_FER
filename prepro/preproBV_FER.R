# preprocessing FER & CTR: behavioural data, (c) Irene Sophia Plank

library(tidyverse)       # tibble stuff

dt.path = paste('/home/emba/Documents/EMBA', 'BVET', sep = "/")
dt.path = paste('/home/emba/Documents/EMBA', 'BVET-explo', sep = "/")

# load the relevant data in long format
df.fer = list.files(path = dt.path, pattern = "FER-BV_*", full.names = T) %>%
  map_df(~read_csv(., show_col_types = F, col_types = "cdcdddc")) %>% 
  mutate(
    lastkey = as.numeric(lastkey),
    acc = emo == opt,
    emo = as.factor(emo),
    emo = recode_factor(emo, 
           "1" = "AF",
           "2" = "AN",
           "3" = "HA",
           "4" = "SA"),
    opt = as.factor(opt),
    opt = recode_factor(opt, 
           "1" = "AF",
           "2" = "AN",
           "3" = "HA",
           "4" = "SA"),
    frames = round(disc * 300), 
    disc = case_when(acc ~ disc)
    ) %>%
  select(subID, trl, video, frames, emo, opt, disc, acc)

df.ctr = list.files(path = dt.path, pattern = "CTR-BV_*", full.names = T) %>%
  map_df(~read_csv(., show_col_types = F, col_types = "cdcdddc")) %>% 
  mutate(
    lastkey = as.numeric(lastkey),
    acc = animal == opt,
    animal = as.factor(animal),
    animal = recode_factor(animal, 
           "1" = "ape",
           "2" = "dog",
           "3" = "cat",
           "4" = "lion"),
    opt = as.factor(opt),
    opt = recode_factor(opt, 
           "1" = "ape",
           "2" = "dog",
           "3" = "cat",
           "4" = "lion"),
    frames = round(disc * 300), 
    disc = case_when(acc ~ disc)
    ) %>%
  select(subID, trl, video, frames, animal, opt, disc, acc)

# who has to be excluded? 
exc1 = df.fer %>% group_by(subID) %>% summarise(acc = mean(acc)) %>% filter(acc < 2/3)
exc2 = df.ctr %>% group_by(subID) %>% summarise(acc = mean(acc)) %>% filter(acc < 2/3)
exc = c(as.character(exc1$subID), as.character(exc2$subID))
print(length(unique(exc))) # print how many have to be excluded

# load pilot participants and add to the list
pilot = read_csv(file.path(dt.path, "pilot-subIDs.csv"))
write(setdiff(exc, pilot$subID), file.path(dt.path, 'FER-exc.txt'))
exc   = c(exc, pilot$subID)

# exclude these participants
df.fer = df.fer %>% filter(!(subID %in% exc))
df.ctr = df.ctr %>% filter(!(subID %in% exc))

# save data frames
saveRDS(df.fer, file.path(dt.path, "df_FER.RDS"))
saveRDS(df.ctr, file.path(dt.path, "df_CTR.RDS"))

# how many are still left
length(unique(df.fer$subID))
