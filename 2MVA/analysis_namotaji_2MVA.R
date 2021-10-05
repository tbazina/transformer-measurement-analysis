################################################################################
############## Analiza mjerenja na namotajima 2MVA 12.09.2020. ###############
################################################################################

# Source functions and objects file
source('functions.R')

# Plotting
library(ggplot2)
# Data paste
library(datapasta)
# Sorting
library(gtools)
# Fitting
library(broom)
library(rsm)
# Spatial data modelling
library(gstat)
library(sp)
# Gaussian process models
library(tgp)
library(mlegp)

################################################################################
################################################################################

# Transformator radi u kratkom spoju - 50 A, 1000 V na primaru.
# Mjerenja su provedena na 3 namotaja cilindričnog oblika.
# Mjerene su VN strana, NN strana i dvije bocne strane namotaja (ukupno 5
# povrsina).
# Koordinate točaka su zapisane kao duljina po kružnom luku od centralne 
# simetrale.
# Na svakoj mjernoj povrsini je raspoređeno 15 mjernih točaka
# Namotaji su oznaceni s brojevima 1, 2, 3

# Data for loop input - KOORDINATE NEKIH TOČAKA PROMIJENJENE (STVARNE KORDINATE)
# I DODANO MJERENJE BOCNIH STRANA NAMOTAJA

# base.doe <- tibble::tribble(
#  ~point.id, ~loc.x, ~loc.y,
#         1L,  -151L,   389L,
#         2L,  -125L,   216L,
#         3L,  -109L,   566L,
#         4L,   -80L,   106L,
#         5L,   -60L,   335L,
#         6L,   -51L,   524L,
#         7L,   -25L,   130L,
#         8L,     1L,    20L,
#         9L,    32L,   284L,
#        10L,    35L,   612L,
#        11L,    66L,   440L,
#        12L,    75L,   199L,
#        13L,    96L,    57L,
#        14L,   135L,   320L,
#        15L,   150L,   461L
#  )
# flank.doe <- base.doe %>%
#   mutate(loc.x = case_when(
#     loc.x == -151 ~ as.double(-140),
#     loc.x == 150 ~ as.double(140),
#     TRUE ~ as.double(loc.x)
#   ))
# 
# input.df <- base.doe %>% mutate(namotaj.id = 1, side = 'NN') %>%
#   full_join(base.doe %>% mutate(namotaj.id = 1, side = 'VN') %>%
#               filter(point.id != 11)) %>%
#   full_join(flank.doe %>% mutate(namotaj.id = 1, side = 'bok')) %>%
#   full_join(base.doe %>% mutate(namotaj.id = 2, side = 'NN')) %>%
#   full_join(base.doe %>% mutate(namotaj.id = 2, side = 'VN') %>%
#               filter(!point.id %in% c(10, 11))) %>%
#   full_join(base.doe %>% mutate(namotaj.id = 3, side = 'NN')) %>%
#   full_join(base.doe %>% mutate(namotaj.id = 3, side = 'VN') %>%
#               filter(!point.id %in% c(10, 11))) %>%
#   full_join(flank.doe %>% mutate(namotaj.id = 3, side = 'bok')) %>%
#   select(namotaj.id, side, point.id, loc.x, loc.y)
# 
# input.df %>% rename(Namotaj=namotaj.id, Strana=side, Tocka=point.id, x=loc.x,
#                     y=loc.y) %>%
#   write_excel_csv2('2MVA/DoE/Aktivni_dio/namotaji_stvarne_koordinate.csv')
# 
# input.df %>% group_by(namotaj.id, side) %>%
#   summarise(count = n())

# Data input function
# data.input.f <- function(dat, input.df) {
#   # Folder u kojem se nalaze mjerenja
#   namotaji.path <- '2MVA/mjerenja/namotaji/'
#   for (i in 1:length(input.df$point.id)) {
#     # print(input.df$namotaj.id[i])
#     # print(input.df$side[i])
#     # print(paste0('pt', input.df$point.id[i]))
#     # print(input.df$loc.x[i])
#     # print(input.df$loc.y[i])
#     read.path <- paste0(
#       namotaji.path,
#       switch (
#         input.df$side[i], 'NN' = 'NN_strana/n', 'VN' = 'VN_strana/n',
#         'bok' = 'bok_lijevo_desno/b'
#         ),
#       input.df$namotaj.id[i], '_', input.df$point.id[i]
#     )
#     # print(read.path)
#   print('#######################')
#     dat <- dat %>%
#       add_row(
#         !!!VibData(
#           point.id = input.df$point.id[i],
#           loc.x = input.df$loc.x[i],
#           loc.y = input.df$loc.y[i],
#           rib = input.df$side[i],
#           replication = input.df$namotaj.id[i],
#           decimal.separator = ",",
#           file.loc = read.path
#     )
#   )
#   }
#   return(dat)
# }

################################################################################
################################################################################
# Data input namotaj 1 NN pt1
# Input n1_1
# dat <- VibData(
#   point.id = input.df$point.id[1],
#   loc.x = input.df$loc.x[1],
#   loc.y = input.df$loc.y[1],
#   rib = input.df$side[1],
#   replication = input.df$namotaj.id[1],
#   decimal.separator = ',',
#   file.loc = paste0('2MVA/mjerenja/namotaji/NN_strana/n1_1')
# )

# Remove input data for first point (n1_1)
# print(slice(input.df, 1))
# input.df <- input.df %>% slice(-1)

# Input the rest of data points for n1, n2 and n3
# dat <- data.input.f(dat, input.df)

# Change replication to winding and rib to side
# dat <- dat %>% rename(wind = replication, side = rib)

# Select only necessary variables
# dat <- dat %>% select(-pacf, -displacement.orig)

# Recalculate FFT resolution because of sample length change
# dat <- dat %>% group_by(wind, side, point.id) %>%
#   mutate(d.freq = sample.rate/length(na.omit(velocity.amp))/2) %>% ungroup()

# Sanity data check
# dat %>% glimpse()
# dat %>% select(range, d.time, d.freq, sample.rate, sample.num) %>% distinct() %>% View()
# dat %>% group_by(wind, side, point.id) %>% summarise(
#   length = length(velocity),
#   min = min(velocity),
#   max = max(velocity),
#   mean = mean(velocity),
#   median = median(velocity),
#   ) %>%
#   summarise(
#     min.min = min(min),
#     max.max = max(max)
#   ) %>% View() 
# dat %>%
#   select(wind, side, point.id, peak.freq.orig, peak.vel.orig, peak.frequency, peak.velocity.amp) %>%
#   drop_na() %>% group_by(wind, side, point.id) %>% slice(1) %>%
#   mutate(
#     equal = if_else(
#       (peak.freq.orig <= (peak.frequency + 0.2)) & (peak.freq.orig >= (peak.frequency - 0.2)), T, F)) %>% View()

# Remove unnecessary variables
# dat <- dat %>% select(-peak.freq.orig, -peak.vel.orig, -peak.disp.orig,
#                       -peak.acc.orig, -velocity.orig)

# Create subtitles for plots
# dat <- dat %>% group_by(wind, side, point.id) %>%
#   mutate(subtitle =
#            paste0('Nam: ', wind, ' ', side, ' pt', point.id, ': ', '(', loc.x, ', ', loc.y, ')')
#       ) %>% ungroup()

# Save current workspace
# save.image('2MVA/namotaji.RData')

# Load workspace
load('2MVA/namotaji.RData')

################################################################################
################################################################################
# Vremenska domena plot
dat %>% filter(wind == 1 & (
  (side == 'NN' & point.id %in% c(2, 6, 14)) | (
    (side == 'VN' & point.id %in% c(3, 6, 9)) | (
      (side == 'bok' & point.id %in% c(1, 10, 11))
      )))
  ) %>%
  select(time, velocity, point.id, wind, side, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
  # coord_fixed(ratio = 0.0012) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    # breaks = seq(-1000, 1000, 1000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 7, 1),
    limits = c(NA, NA)
  ) +
  ggtitle("Namotaj 1 - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/nam_1_brzina_vrijeme.png',
       width = 17, height = 9, units = 'cm', dpi = 320)

dat %>% filter(wind == 2 & (
  (side == 'NN' & point.id %in% c(6, 14, 15)) | (
    (side == 'VN' & point.id %in% c(1, 6, 13))
      ))
  ) %>%
  select(time, velocity, point.id, wind, side, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
  # coord_fixed(ratio = 0.0012) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    # breaks = seq(-1000, 1000, 1000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 7, 1),
    limits = c(NA, NA)
  ) +
  ggtitle("Namotaj 2 - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/nam_2_brzina_vrijeme.png',
       width = 17, height = 6.5, units = 'cm', dpi = 320)

dat %>% filter(wind == 3 & (
  (side == 'NN' & point.id %in% c(10, 13, 14)) | (
    (side == 'VN' & point.id %in% c(6, 9, 15)) | (
      (side == 'bok' & point.id %in% c(2, 10, 11))
      )))
  ) %>%
  select(time, velocity, point.id, wind, side, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
  # coord_fixed(ratio = 0.0012) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    # breaks = seq(-1000, 1000, 1000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 7, 1),
    limits = c(NA, NA)
  ) +
  ggtitle("Namotaj 3 - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/nam_3_brzina_vrijeme.png',
       width = 17, height = 9, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Frekvencijska domena plot
dat %>% filter(wind == 1 & (
  (side == 'NN' & point.id %in% c(2, 6, 14)) | (
    (side == 'VN' & point.id %in% c(3, 6, 9)) | (
      (side == 'bok' & point.id %in% c(1, 10, 11))
      )))
  ) %>%
  select(frequency, velocity.amp, point.id, wind, side, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    # breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
    # limits = c(0, 1000)
  ) +
  ggtitle("Namotaj 1 - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/nam_1_brzina_freq.png',
       width = 17, height = 9, units = 'cm', dpi = 320)

dat %>% filter(wind == 2 & (
  (side == 'NN' & point.id %in% c(6, 14, 15)) | (
    (side == 'VN' & point.id %in% c(1, 6, 13))
      ))
  ) %>%
  select(frequency, velocity.amp, point.id, wind, side, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    # breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
    # limits = c(0, 1000)
  ) +
  ggtitle("Namotaj 2 - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/nam_2_brzina_freq.png',
       width = 17, height = 6.5, units = 'cm', dpi = 320)

dat %>% filter(wind == 3 & (
  (side == 'NN' & point.id %in% c(10, 13, 14)) | (
    (side == 'VN' & point.id %in% c(6, 9, 15)) | (
      (side == 'bok' & point.id %in% c(2, 10, 11))
      )))
  ) %>%
  select(frequency, velocity.amp, point.id, wind, side, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    # breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
    # limits = c(0, 1000)
  ) +
  ggtitle("Namotaj 3 - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/nam_3_brzina_freq.png',
       width = 17, height = 9, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Preliminary analysis
# Frequency table of peak.frequency
dat %>% group_by(wind, side, point.id) %>%
  select(wind, side, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  ungroup() %>% count(peak.frequency) %>% View()
  
# Sort by amplitudes
dat %>% group_by(wind, side, point.id) %>%
  select(wind, side, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  mutate(peak.velocity.amp = round(peak.velocity.amp)) %>% View()

# Fit only 100 Hz sine waves
dat_fit <- dat %>%
  mutate(
    om_100 = 2*pi*100,
    A.100.c.term = om_100*cos(om_100*time),
    A.100.s.term = om_100*sin(om_100*time),
    ) %>%
  nest_by(wind, side, point.id) %>%
  mutate(fitSine = list(lm(
    velocity ~ 0 + A.100.c.term + A.100.s.term,
    data = data)))

dat_fit %>% summarise(glance(fitSine)) %>% View()
dat_fit %>% summarise(tidy(fitSine))

# Plot sine fit for NN pt2 and pt6
fit_to_data_ <- function(dat_fit, point) {
  dat_fit %>% filter(
    wind == 1 & point.id == point & side == 'NN'
    ) %>% summarise(
      fit.data = predict(fitSine),
      r.squared = glance(fitSine)$r.squared
    )
}

dat %>% filter(
  wind == 1 & point.id %in% c(4, 2, 6) & side == 'NN'
  ) %>%
  group_by(wind, side, point.id) %>%
  mutate(
    velocity.fit = fit_to_data_(dat_fit, unique(point.id))$fit.data,
    r.squared = fit_to_data_(dat_fit, unique(point.id))$r.squared
  ) %>%
  select(time, velocity, velocity.fit, point.id, wind, side, subtitle, r.squared) %>%
  mutate(
    subtitle = round(r.squared, 2)
    ) %>%
  pivot_longer(
    cols = c(velocity, velocity.fit),
    names_to = 'name_groups',
    values_to = 'velocity'
  ) %>%
  mutate(
    name_groups = case_when(
      name_groups == 'velocity' ~ 'Izmjereni',
      name_groups == 'velocity.fit' ~ 'Modelirani',
      T ~ name_groups
    )
  ) %>%
  ggplot(aes(x = time, y = velocity, color = name_groups)) +
  facet_grid(subtitle ~ .,
             # scales = 'free_y',
             labeller = label_bquote(italic(R)^2 * ' = ' * .(subtitle))) +
  geom_line(size = 0.6) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    # breaks = seq(-1000, 1000, 1000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 7, 0.025),
    limits = c(2, 2.2)
  ) +
  scale_color_manual(
    name = 'Podaci: ',
    values = c('dodgerblue2', 'orangered1')
  ) +
  ggtitle("Primjer aproksimacije izmjerenih vibracija") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(face="plain", size = 10),
    axis.text.x = element_text(colour="black", size = 8),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/fit_sinusoida_primjer.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)
  
# Amplitudes and adj_r_squared
dat_model <- dat_fit %>% summarise(tidy(fitSine)) %>%
  group_by(wind, side, point.id) %>%
  select(wind, side, point.id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  summarise(
    A.100.v = 2*pi*100*sqrt(A.100.c.term^2 + A.100.s.term^2),
    A.100.x = sqrt(A.100.c.term^2 + A.100.s.term^2)
  ) %>% full_join(
    select(dat_fit %>% summarise(glance(fitSine)), wind, side, point.id,
           adj.r.squared),
    by = c('wind', 'side', 'point.id')
  )

dat_model %>% View()

# Summary of data with top 5 peaks and distinct frequency peaks
dat_summary <- dat %>%
  select(wind, side, point.id, loc.x, loc.y, velocity, peak.to.peak.vel,
         rms.vel, peak.frequency, peak.velocity.amp, frequency, d.freq,
         velocity.amp) %>%
  group_by(wind, side, point.id) %>%
  summarise(
    Namotaj = unique(wind), Strana=unique(side), Tocka = unique(point.id),
    Sirina = unique(loc.x), Visina = unique(loc.y),
    "Uvjet rada" = "Kratki spoj - 50 A - 1 kV primar",
    "Brzina min" = min(velocity), "Brzina max" = max(velocity),
    "Brzina sd" = sd(velocity),
    RMS = mean(rms.vel), 'Peak-to-peak' = unique(peak.to.peak.vel),
    "Peak freq 1" = peak.frequency[1],
    "Peak amp 1" = peak.velocity.amp[1],
    "Peak freq 2" = peak.frequency[2],
    "Peak amp 2" = peak.velocity.amp[2],
    "Peak freq 3" = peak.frequency[3],
    "Peak amp 3" = peak.velocity.amp[3],
    "Peak freq 4" = peak.frequency[4],
    "Peak amp 4" = peak.velocity.amp[4],
    "Peak freq 5" = peak.frequency[5],
    "Peak amp 5" = peak.velocity.amp[5],
    "25 Hz amp" = na.omit(velocity.amp[
      frequency >= (25.0-d.freq[1]/5) & frequency <= (25.0+d.freq[1]/5)
      ]),
    "50 Hz amp" = na.omit(velocity.amp[
      frequency >= (50.0-d.freq[1]/5) & frequency <= (50.0+d.freq[1]/5)
      ]),
    "100 Hz amp" = na.omit(velocity.amp[
      frequency >= (100.0-d.freq[1]/5) & frequency <= (100.0+d.freq[1]/5)
      ]),
    "200 Hz amp" = na.omit(velocity.amp[
      frequency >= (200.0-d.freq[1]/5) & frequency <= (200.0+d.freq[1]/5)
      ]),
    "300 Hz amp" = na.omit(velocity.amp[
      frequency >= (300.0-d.freq[1]/5) & frequency <= (300.0+d.freq[1]/5)
      ])
    ) %>%
  full_join(dat_model, by = c('wind', 'side', 'point.id')) %>% 
  rename(
    'Model 100 Hz amp' = A.100.v,
    'Model 100 Hz disp amp' = A.100.x,
    'Model Adj R Squared' = adj.r.squared
    ) %>%
  ungroup() %>% select(-wind, -side, -point.id)

dat_summary %>%
  write_excel_csv2(
    path = '2MVA/preliminarna_obrada/aktivni_dio/namotaji_summary.csv'
    )

# Load data summary
dat_summary <- read_csv2(
  file = '2MVA/preliminarna_obrada/aktivni_dio/namotaji_summary.csv'
    )

################################################################################
################################################################################
# Plotovi na namotajima
# Plot amplituda brzine Namotaj 1 - 100 Hz
dat_summary %>%
  select(wind = Namotaj, side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  filter(wind == 1) %>%
  mutate(
    side = paste0('Strana: ', side)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c('Strana: NN', 'Strana: bok', 'Strana: VN')) ~ .
    ) +
  coord_fixed() +
  geom_vline(aes(xintercept = 0)) +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-150, 150, 50),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 2000, 250),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  scale_size_area(
    max_size = 13,
    # range = c(5, 10),
    guide = "none"
  ) +
  ggtitle("Namotaj: 1 - Amplitude brzine - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/amplitude_100_hz_nam_1.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

# Plot amplituda brzine Namotaj 2 - 100 Hz
dat_summary %>%
  select(wind = Namotaj, side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  filter(wind == 2) %>%
  mutate(
    side = paste0('Strana: ', side)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c('Strana: NN', 'Strana: bok', 'Strana: VN')) ~ .
    ) +
  coord_fixed() +
  geom_vline(aes(xintercept = 0)) +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-150, 150, 50),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 800, 100),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  scale_size_area(
    max_size = 7,
    # range = c(5, 10),
    guide = "none"
  ) +
  ggtitle("Namotaj: 2 - Amplitude brzine - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/amplitude_100_hz_nam_2.png',
       width = 17, height = 15, units = 'cm', dpi = 320)

# Plot amplituda brzine Namotaj 3 - 100 Hz
dat_summary %>%
  select(wind = Namotaj, side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  filter(wind == 3) %>%
  mutate(
    side = paste0('Strana: ', side)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c('Strana: VN', 'Strana: bok', 'Strana: NN')) ~ .
    ) +
  coord_fixed() +
  geom_vline(aes(xintercept = 0)) +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-150, 150, 50),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 2000, 250),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  scale_size_area(
    max_size = 13,
    # range = c(5, 10),
    guide = "none"
  ) +
  ggtitle("Namotaj: 3 - Amplitude brzine - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/amplitude_100_hz_nam_3.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Surface modelling
# Loading summary and adding coordinates for wind 1 and 3 from sides
dat_surf <- read_csv2(
  file = '2MVA/preliminarna_obrada/aktivni_dio/namotaji_summary.csv'
  ) %>%
  select(
    wind = Namotaj, side = Strana, point.id = Tocka, x = Sirina, y = Visina,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    adj.r.sq = `Model Adj R Squared`
    ) %>%
  mutate(
    dist.wind = 202.5 * pi/ 2 + 50,
    x.full = case_when(
      (wind == 1 & side == 'NN') ~ as.integer(-dist.wind + x),
      (wind == 1 & side == 'VN') ~ as.integer(dist.wind + x),
      (wind == 3 & side == 'NN') ~ as.integer(dist.wind + x),
      (wind == 3 & side == 'VN') ~ as.integer(-dist.wind + x),
      TRUE ~ as.integer(x)
      )
    )

# Plotting coordinates for sanity check
dat_surf %>% filter(wind == 3) %>%
  ggplot(aes(x = x.full, y = y, label = paste0(side, point.id))) + geom_text()

# Extremes for coding
dat_surf <- dat_surf %>% group_by(wind, side) %>%
  select(wind, side, x, x.full, y, amp.v.100) %>%
  mutate(
    mean.x = mean(x),
    extr.x = max(max(x) - mean.x, abs(min(x) - mean.x)),
    mean.y = mean(y),
    extr.y = max(max(y) - mean.y, abs(min(y) - mean.y))
    ) %>% ungroup()

dat_surf %>% distinct(wind, side, mean.x, extr.x, mean.y, extr.y) %>% View()

# Coding data
dat_coded <- coded.data(
  data = 
    dat_surf %>%
    filter(side == 'NN' & wind %in% c(3)) %>%
    select(wind, x, y, amp.v.100) %>%
    mutate(wind = as.factor(wind)),
  x1 ~ (x + 0) / 159,
  x2 ~ (y - 310.5) / 310.5
  )

as.data.frame(dat_coded)

# RSM low r squared value
dat_coded.rsm <- rsm(amp.v.100 ~ SO(x1, x2) + PQ(x1, x2), data = dat_coded)
summary(dat_coded.rsm)
sqrt(mean(dat_coded.rsm$residuals^2))

# Gaussian process regression
X = dat_surf %>% filter(side == 'NN' & wind %in% c(3)) %>% select(x, y)
Z = dat_surf %>% filter(side == 'NN' & wind %in% c(3)) %>% select(amp.v.100)
dat_mod <- btgpllm(X = X, Z=Z)
# dat_mod
sqrt(mean((Z$amp.v.100 - dat_mod$Zp.mean)^2))
plot(dat_mod)

# Spatial data modelling - Kriging interpolation 100 Hz
krige.inter <- function(
  amp_df = dat_surf,
  amp = 'amp.v.100',
  plt_map = T,
  variogram_only = T,
  vgm_cutoff = 900,
  vgm_bin_num = 4,
  vgm_model = 'Gau',
  vgm_psill = NA,
  vgm_range = NA,
  vgm_alpha = seq(0, 135, 45),
  vgm_an_angle = 0,
  vgm_an_ratio = 1,
  grid_num = 300
  ) {
# Creating SpatialPointsDataFrame
dat_sp <- amp_df %$%
  SpatialPointsDataFrame(
    coords = select(., x.full, y),
    data = select(., !!amp, wind) 
    )
# Examining experimental variogram
var_exp <- variogram(
  formula(paste0(amp, '~wind')),
  dat_sp, cutoff = vgm_cutoff, width = vgm_cutoff/vgm_bin_num,
  alpha=vgm_alpha, map = plt_map
  )
# Plot variogram map
if (plt_map) {
  print(plot(var_exp, threshold = 3))
  return()
}
# Creating variogram fitting model
vgm_mod <- vgm(
  psill = vgm_psill, model = vgm_model,
  range = vgm_range, anis = c(vgm_an_angle, vgm_an_ratio)
  )
# Fitting varigram to experimenta data
var_fit <- fit.variogram(
  var_exp,
  model = vgm_mod)
# Plotting fitted variogram
if (variogram_only) {
  print(plot(var_exp, var_fit))
  return()
}
# Calculating grid limits from data
grid_limits <- amp_df %>%
  summarise(
    min.x = min(x.full)*1.03,
    max.x = max(x.full)*1.03,
    min.y = min(y)*0.1,
    max.y = max(y)*1.03
    )
# Creating predictive grid and converting to SpatialPixels (with ribs data)
pred.grid <- expand.grid(
  x = seq(grid_limits$min.x, grid_limits$max.x, length.out = grid_num),
  y = seq(grid_limits$min.y, grid_limits$max.y, length.out = grid_num)
  )

pred.grid <- SpatialPixels(SpatialPoints(pred.grid))

# Kriging interpolation
krige.pred <- krige(
  formula(paste0(amp, '~1')),
  locations = dat_sp,
  newdata = pred.grid,
  model = var_fit
  )
print(spplot(krige.pred['var1.pred'], scales = list(draw = T)))
# Return object
krige.pred
}

# Determining cutoff
dat_surf %>% filter(wind == 2 & side == 'VN') %>% select(x.full, y) %>%
  summarise(
    max.x = max(x.full),
    min.x = min(x.full),
    max.y = max(y),
    min.y = min(y),
  ) %>%
  mutate(
    diag = sqrt((max.x - min.x)^2 + (max.y - min.y)^2),
    diag.3 = diag/3,
    cutoff.max = sqrt(2) / 2 * diag
    )
# Testing parameters for interpolation
krige.inter(
      amp_df = dat_surf %>% filter(wind == 1),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 6,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Gau',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 135,
      vgm_an_ratio = 5/11,
      grid_num = 300
      )

# Creating tibble for contour and points plot
dat_disp_disp <- dat_surf %>%
  select(-amp.v.100, -adj.r.sq, -x, dist.wind) %>%
  rename(x = x.full) %>%
  mutate(point.id = as.character(point.id)) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    wind = 1,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 1),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 6,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Gau',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 135,
      vgm_an_ratio = 5/11,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    wind = 3,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 3),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 6,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Gau',
      vgm_alpha = seq(0, 126, 31.5),
      vgm_an_angle = 63,
      vgm_an_ratio = 8/11,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'side', 'x', 'y', 'amp.x.100'),
  y = tibble(
    wind = 2,
    side = 'NN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 2 & side == 'NN'),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 400,
      vgm_bin_num = 3,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 135),
      vgm_an_angle = 0,
      vgm_an_ratio = 1/2,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'side', 'x', 'y', 'amp.x.100'),
  y = tibble(
    wind = 2,
    side = 'VN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 2 & side == 'VN'),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 400,
      vgm_bin_num = 3,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 135),
      vgm_an_angle = 0,
      vgm_an_ratio = 3/8,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  )

# Plot amplituda pomaka - Namotaj 1 - 100 Hz
dat_disp_disp %>%
  filter(wind == 1) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.x.100.cnt = case_when(
      point.id == 'model' & amp.x.100 >= 0 ~ amp.x.100,
      point.id == 'model' & amp.x.100 <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    amp.x.100 = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.x.100
    ),
    point.id = case_when(
      point.id != 'model' ~ paste0(side, point.id),
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_contour_filled(aes(z = amp.x.100.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = dist.wind)) +
  geom_vline(aes(xintercept = -dist.wind)) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-600, 600, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Namotaj: 1 - Amplitude pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/contour_disp_nam_1_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

# Plot amplituda pomaka - Namotaj 3 - 100 Hz
dat_disp_disp %>%
  filter(wind == 3) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.x.100.cnt = case_when(
      point.id == 'model' & amp.x.100 >= 0 ~ amp.x.100,
      point.id == 'model' & amp.x.100 <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    amp.x.100 = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.x.100
    ),
    point.id = case_when(
      point.id != 'model' ~ paste0(side, point.id),
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_contour_filled(aes(z = amp.x.100.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = dist.wind)) +
  geom_vline(aes(xintercept = -dist.wind)) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-600, 600, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Namotaj: 3 - Amplitude pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/contour_disp_nam_3_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

# Plot amplituda pomaka - Namotaj 2 - 100 Hz
dat_disp_disp %>%
  filter(wind == 2) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.x.100.cnt = case_when(
      point.id == 'model' & amp.x.100 >= 0 ~ amp.x.100,
      point.id == 'model' & amp.x.100 <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    amp.x.100 = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.x.100
    ),
    point.id = case_when(
      point.id != 'model' ~ point.id,
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind),
    side = paste0('Strana: ', side)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(side ~ ., scales = 'fixed') +
  geom_contour_filled(aes(z = amp.x.100.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-300, 300, 50),
    limits = c(NA, NA),
    expand = c(0.03, 0.03)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.8
  ) +
  ggtitle("Namotaj: 2 - Amplitude pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/contour_disp_nam_2_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

## Modeling velocity
# Determining cutoff
dat_surf %>% filter(wind == 2 & side == 'VN') %>% select(x.full, y) %>%
  summarise(
    max.x = max(x.full),
    min.x = min(x.full),
    max.y = max(y),
    min.y = min(y),
  ) %>%
  mutate(
    diag = sqrt((max.x - min.x)^2 + (max.y - min.y)^2),
    diag.3 = diag/3,
    cutoff.max = sqrt(2) / 2 * diag
    )

# Testing parameters for interpolation
krige.inter(
      amp_df = dat_surf %>% filter(wind == 2 & side == 'VN'),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 400,
      vgm_bin_num = 3,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 135),
      vgm_an_angle = 0,
      vgm_an_ratio = 3/8,
      grid_num = 300
      )

# Creating tibble for contour and points plot
dat_disp_vel <- dat_surf %>%
  select(-amp.x.100, -adj.r.sq, -x, dist.wind) %>%
  rename(x = x.full) %>%
  mutate(point.id = as.character(point.id)) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.v.100'),
  y = tibble(
    wind = 1,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 1),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 6,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Gau',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 135,
      vgm_an_ratio = 5/11,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.v.100'),
  y = tibble(
    wind = 3,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 3),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 6,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Gau',
      vgm_alpha = seq(0, 126, 31.5),
      vgm_an_angle = 63,
      vgm_an_ratio = 8/11,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'side', 'x', 'y', 'amp.v.100'),
  y = tibble(
    wind = 2,
    side = 'NN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 2 & side == 'NN'),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 400,
      vgm_bin_num = 3,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 135),
      vgm_an_angle = 0,
      vgm_an_ratio = 1/2,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'side', 'x', 'y', 'amp.v.100'),
  y = tibble(
    wind = 2,
    side = 'VN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(wind == 2 & side == 'VN'),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 400,
      vgm_bin_num = 3,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 135),
      vgm_an_angle = 0,
      vgm_an_ratio = 3/8,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  )

# Plot amplituda brzine - Namotaj 1 - 100 Hz
dat_disp_vel %>%
  filter(wind == 1) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.v.100.cnt = case_when(
      point.id == 'model' & amp.v.100 >= 0 ~ amp.v.100,
      point.id == 'model' & amp.v.100 <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    amp.v.100 = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.v.100
    ),
    point.id = case_when(
      point.id != 'model' ~ paste0(side, point.id),
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_contour_filled(aes(z = amp.v.100.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = dist.wind)) +
  geom_vline(aes(xintercept = -dist.wind)) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-600, 600, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Namotaj: 1 - Amplitude brzina - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/contour_vel_nam_1_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

# Plot amplituda brzina - Namotaj 3 - 100 Hz
dat_disp_vel %>%
  filter(wind == 3) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.v.100.cnt = case_when(
      point.id == 'model' & amp.v.100 >= 0 ~ amp.v.100,
      point.id == 'model' & amp.v.100 <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    amp.v.100 = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.v.100
    ),
    point.id = case_when(
      point.id != 'model' ~ paste0(side, point.id),
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_contour_filled(aes(z = amp.v.100.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = dist.wind)) +
  geom_vline(aes(xintercept = -dist.wind)) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-600, 600, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Namotaj: 3 - Amplitude brzina - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/contour_vel_nam_3_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

# Plot amplituda pomaka - Namotaj 2 - 100 Hz
dat_disp_vel %>%
  filter(wind == 2) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.v.100.cnt = case_when(
      point.id == 'model' & amp.v.100 >= 0 ~ amp.v.100,
      point.id == 'model' & amp.v.100 <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    amp.v.100 = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.v.100
    ),
    point.id = case_when(
      point.id != 'model' ~ point.id,
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind),
    side = paste0('Strana: ', side)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(side ~ ., scales = 'fixed') +
  geom_contour_filled(aes(z = amp.v.100.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-300, 300, 50),
    limits = c(NA, NA),
    expand = c(0.03, 0.03)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.8
  ) +
  ggtitle("Namotaj: 2 - Amplitude brzina - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/contour_vel_nam_2_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Data input referenca dok radi samo generator
# Input referenca_25Hz 
dat_ref <- VibData(
  point.id = 'ref',
  loc.x = 0,
  loc.y = 0,
  rib = F,
  replication = 1,
  decimal.separator = ',',
  file.loc = paste0('2MVA/mjerenja/namotaji/NN_strana/referenca_25Hz')
)

dat_ref <- dat_ref %>%
      add_row(
        !!!VibData(
          point.id = 'ref',
          loc.x = 0,
          loc.y = 0,
          rib = F,
          replication = 2,
          decimal.separator = ",",
          file.loc = paste0('2MVA/mjerenja/namotaji/NN_strana/referenca_25Hz2')
    )
  )

# Frekvencijska domena plot referentne tocke
dat_ref %>%
  select(frequency, velocity.amp, point.id, replication) %>%
  drop_na() %>%
  mutate(subtitle = paste0('Referentno mjerenje: ', replication)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    # breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 200, 25),
    limits = c(0, 201)
    # limits = c(0, 1000)
  ) +
  ggtitle("Utjecaj generatora - Transformator ne radi") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/ref_vibracije.png',
       width = 17, height = 4.5, units = 'cm', dpi = 320)
