library(tidyverse)
library(ggrain)
library(ggpubr)

setwd('..')
load("FER_data.RData")
setwd('plots')

# graph settings 
c_light = "#a9afb2"; c_light_highlight = "#8ea5b2"; c_mid = "#6b98b2" 
c_mid_highlight = "#3585b2"; c_dark = "#0072b2"; c_dark_highlight = "#0058b2" 
c_green = "#009E73"
sz = 1
a = 0.5 # alpha

# custom colour palette
custom.col = c(c_dark_highlight, "#CC79A7", "#009E73", "#D55E00")

# settings to save pictures
w = 170
h = 100

# Discrimination ----------------------------------------------------------

df.fer.agg = rbind(df.fer, df.exp) %>%
  group_by(subID, diagnosis, emo) %>%
  summarise(
    disc     = mean(disc, na.rm = T),
    acc      = mean(acc), 
    fsr.disc = mean(fsr.disc),
    fsr.acc  = mean(acc)
  ) %>% ungroup() %>%
  mutate_if(is.character, as.factor)

df.fer.agg$diagnosis = factor(df.fer.agg$diagnosis, 
                              levels = c("ADHD", "ASD", "BOTH", "COMP"))

rbind(df.fer.agg %>% 
        rename("condition" = "emo") %>% 
        select(subID, diagnosis, condition, disc),
      df.fer.agg %>% 
        select(subID, diagnosis, fsr.disc) %>% 
        group_by(subID, diagnosis) %>% 
        summarise(condition = "species", disc = mean(fsr.disc))) %>%
  mutate(
    condition = fct_recode(condition, 
                           "fear" = "AF", 
                           "anger" = "AN", 
                           "happiness" = "HA", 
                           "sadness" = "SA"),
    disc = 100*disc,
    diagnosis = fct_recode(diagnosis, 
                           "ADHD+ASD" = "BOTH")
  ) %>%
  ggplot(aes(condition, disc, fill = diagnosis, colour= diagnosis)) + #
  geom_rain(rain.side = 'r',
            boxplot.args = list(color = "black", outlier.shape = NA, show_guide = FALSE, alpha = a),
            violin.args = list(color = "black", outlier.shape = NA, alpha = a),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(x = 0, width = 0.3), width = 0.3
            ),
            point.args = list(show_guide = FALSE, alpha = a),
            violin.args.pos = list(
              width = 0.6, position = position_nudge(x = 0.16)),
            point.args.pos = list(position = ggpp::position_dodgenudge(x = -0.25, width = 0.1))) +
  scale_fill_manual(values = custom.col) +
  scale_color_manual(values = custom.col) +
  ylim(0, 100) +
  labs(title = "Discrimination threshold for emotions and species", 
       x = "", 
       y = "Discrimination threshold (%)") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        legend.direction = "horizontal", legend.title = element_blank(),
        text = element_text(size = 15))

ggsave("discrimination.pdf",
       units  = "mm", width  = w, height = h, dpi    = 300)


# Accuracies --------------------------------------------------------------

rbind(df.fer.agg %>% 
        rename("condition" = "emo") %>% 
        select(subID, diagnosis, condition, acc),
      df.fer.agg %>% 
        select(subID, diagnosis, fsr.acc) %>% 
        group_by(subID, diagnosis) %>% 
        summarise(condition = "species", acc = mean(fsr.acc))) %>%
  mutate(
    condition = fct_recode(condition, 
                           "fear" = "AF", 
                           "anger" = "AN", 
                           "happiness" = "HA", 
                           "sadness" = "SA"),
    acc = acc*100,
    diagnosis = fct_recode(diagnosis, 
                           "ADHD+ASD" = "BOTH")
  ) %>%
  ggplot(aes(condition, acc, fill = diagnosis, colour= diagnosis)) + #
  geom_rain(rain.side = 'r',
            boxplot.args = list(color = "black", outlier.shape = NA, show_guide = FALSE, alpha = 0.5),
            violin.args = list(color = "black", outlier.shape = NA, alpha = 0.5),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(x = 0, width = 0.3), width = 0.3
            ),
            point.args = list(show_guide = FALSE, alpha = .5),
            violin.args.pos = list(
              width = 0.6, position = position_nudge(x = 0.16)),
            point.args.pos = list(position = ggpp::position_dodgenudge(x = -0.25, width = 0.1))) +
  scale_fill_manual(values = custom.col) +
  scale_color_manual(values = custom.col) +
  ylim(0, 100) +
  labs(title = "Accuracies for emotion and species recognition", x = "", y = "Accuracy (%)") +
  theme_bw() + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        legend.direction = "horizontal", legend.title = element_blank(), 
        text = element_text(size = 15))

ggsave("accuracy.pdf",
       units  = "mm", width  = w, height = h, dpi    = 300)

# Dwell times -------------------------------------------------------------

df.fix = df.fix %>%
  group_by(subID, diagnosis, emo, AOI) %>%
  summarise(
    fix.perc = mean(fix.perc, na.rm = T)
  ) %>% ungroup() %>%
  mutate_if(is.character, as.factor)

df.fix %>%
  mutate(
    emotion = fct_recode(emo, 
                         "fear" = "AF", 
                         "anger" = "AN", 
                         "happiness" = "HA", 
                         "sadness" = "SA"),
    AOI = fct_recode(AOI, "forehead" = "fore"),
    fix.perc = fix.perc * 100,
    diagnosis = fct_recode(diagnosis, 
                           "ADHD+ASD" = "BOTH")) %>%
  ggplot(aes(x = AOI, y = fix.perc, fill = diagnosis, colour = diagnosis)) + #
  geom_rain(rain.side = 'r',
            boxplot.args = list(color = "black", 
                                outlier.shape = NA, 
                                show.legend = FALSE, 
                                alpha = a),
            violin.args = list(color = "black", 
                               outlier.shape = NA, 
                               alpha = a),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(x = 0, width = 0.3), width = 0.3
            ),
            point.args = list(show.legend = FALSE, 
                              alpha = .5),
            violin.args.pos = list(
              width = 0.6, 
              position = position_nudge(x = 0.16)),
            point.args.pos = list(
              position = ggpp::position_dodgenudge(x = -0.25, 
                                                   width = 0.1))) +
  scale_fill_manual(values = custom.col) +
  scale_color_manual(values = custom.col) +
  ylim(0, 100) +
  labs(title = "Dwell times", 
       x = "", 
       y = "dwell times (%)") +
  theme_bw() + 
  facet_wrap(. ~ emotion) +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        legend.direction = "horizontal", legend.title = element_blank(), 
        text = element_text(size = 15))

ggsave("dwelltimes.pdf",
       units  = "mm", width  = w, height = 170, dpi    = 300)

# Saccades ----------------------------------------------------------------

df.sac = df.sac %>%
  # add up all saccades per trial
  group_by(subID, diagnosis, emo, trl) %>%
  summarise(
    n.sac = sum(n.sac, na.rm = T)
  ) %>%
  # average number of saccades per subject, diagnosis and emotion
  group_by(subID, diagnosis, emo) %>%
  summarise(
    n.sac = mean(n.sac, na.rm = T)
  ) %>% ungroup() %>%
  mutate_if(is.character, as.factor)

# rain plot
df.sac %>%
  mutate(
    emotion = fct_recode(emo, 
                         "fear" = "AF", 
                         "anger" = "AN", 
                         "happiness" = "HA", 
                         "sadness" = "SA"),
    diagnosis = fct_recode(diagnosis, 
                           "ADHD+ASD" = "BOTH")) %>%
  ggplot(aes(x = emotion, y = n.sac, fill = diagnosis, colour = diagnosis)) + #
  geom_rain(rain.side = 'r',
            boxplot.args = list(color = "black", 
                                outlier.shape = NA, 
                                show.legend = FALSE, 
                                alpha = a),
            violin.args = list(color = "black", 
                               outlier.shape = NA, 
                               alpha = a),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(x = 0, width = 0.3), width = 0.3
            ),
            point.args = list(show.legend = FALSE, alpha = .5),
            violin.args.pos = list(
              width = 0.6, position = position_nudge(x = 0.16)),
            point.args.pos = list(
              position = ggpp::position_dodgenudge(x = -0.25, 
                                                   width = 0.1))) +
  scale_fill_manual(values = custom.col) +
  scale_color_manual(values = custom.col) +
  labs(title = "Number of saccades", 
       x = "", 
       y = "no of saccades") +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.direction = "horizontal", 
        text = element_text(size = 15))

ggsave("saccades.pdf",
       units  = "mm", width  = w, height = h, dpi    = 300)
