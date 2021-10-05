################################################################################
############## Analiza mjerenja na namotajima 110MVA 11.07.2020. ###############
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

################################################################################
################################################################################

# Transformatora radi u kratkom spoju - 70 % nazivnog opterećenja.
# Mjerenja su provedena na 3 namotaja cilindričnog oblika.
# Koordinate točaka su zapisane kao duljina po kružnom luku od centralne 
# simetrale.
# Na svakom namotaju je raspoređeno 29 mjernih točaka

# Data for loop input - KOORDINATE NEKIH TOČAKA PROMIJENJENE (STVARNE KORDINATE)
# input.df <- tibble(
#   namotaj.id = rep(c(1, 2, 3), each = 29),
#   !!!tibble::tribble(
#     ~point.id, ~loc.x, ~loc.y,
#         1, -1187,  842,
#         2, -1119, 1280,
#         3, -1048,  350,
#         4, -1001,  457,
#         5,  -977, 1128,
#         6,  -875,  985,
#         7,  -825,  181,
#         8,  -759,  616,
#         9,  -738, 1161,
#        10,  -687, 1135,
#        11,  -579,  917,
#        12,  -553,  364,
#        13,  -500,  495,
#        14,  -427, 1072,
#        15,  -374,  160,
#        16,  -318, 1404,
#        17,  -236,  797,
#        18,  -191,  284,
#        19,  -144,  590,
#        20,   -72, 1215,
#        21,    25, 1343,
#        22,    36, 1018,
#        23,   104,  409,
#        24,   167,  193,
#        25,   255,  662,
#        26,   306, 1103,
#        27,   341, 1344,
#        28,   392,  737,
#        29,   476,  245,
#         1, -488,  842,
#         2, -448, 1280,
#         3, -406,  255,
#         4, -379,  457,
#         5, -364, 1120,
#         6, -304,  985,
#         7, -275,  181,
#         8, -236,  616,
#         9, -224, 1161,
#        10, -194, 1340,
#        11, -130,  917,
#        12, -115,  364,
#        13,  -84,  495,
#        14,  -41, 1072,
#        15,  -10,  695,
#        16,   24, 1404,
#        17,   72,  797,
#        18,   98,  284,
#        19,  125,  590,
#        20,  168, 1215,
#        21,  225, 1350,
#        22,  232, 1018,
#        23,  271,  409,
#        24,  308,  918,
#        25,  360,  662,
#        26,  390, 1103,
#        27,  411, 1344,
#        28,  441,  737,
#        29,  490,  245,
#         1, -478,  842,
#         2, -421, 1280,
#         3, -361,  155,
#         4, -322,  457,
#         5, -301, 1410,
#         6, -215,  985,
#         7, -174,  181,
#         8, -118,  616,
#         9, -100, 1161,
#        10,  -57,  818,
#        11,   34,  917,
#        12,   56,  364,
#        13,  100,  495,
#        14,  161, 1072,
#        15,  206,  193,
#        16,  254, 1404,
#        17,  322,  797,
#        18,  360,  284,
#        19,  399,  590,
#        20,  460, 1215,
#        21,  542,  880,
#        22,  551, 1018,
#        23,  608,  409,
#        24,  661,  740,
#        25,  736,  662,
#        26,  778, 1103,
#        27,  808, 1344,
#        28,  851,  737,
#        29,  922,  245
#   )
# )
# 
# input.df %>% rename(Namotaj=namotaj.id, Tocka=point.id, x=loc.x, y=loc.y) %>%
#   write_excel_csv2('110MVA/DoE/Aktivni_dio/namotaji_stvarne_koordinate.csv')

# Data input function
data.input.f <- function(dat, input.df) {
  # Folder u kojem se nalaze mjerenja
  namotaji.path <- '110MVA/mjerenja/namotaji/'
  for (i in 1:length(input.df$point.id)) {
  #   print(paste0('pt', input.df$point.id[i]))
  #   print(input.df$loc.x[i])
  #   print(input.df$loc.y[i])
  #   print(input.df$namotaj.id[i])
  #   print(paste0(
  #               namotaji.path, 'n', input.df$namotaj.id[i], '_',
  #               input.df$point.id[i]
  #               ))
  # print('#######################')
    dat <- dat %>%
      add_row(
        !!!VibData(
          point.id = paste0('pt', input.df$point.id[i]),
          loc.x = input.df$loc.x[i],
          loc.y = input.df$loc.y[i],
          rib = F,
          replication = input.df$namotaj.id[i],
          decimal.separator = ",",
          file.loc = paste0(
            namotaji.path, 'n', input.df$namotaj.id[i], '_',
            input.df$point.id[i]
            )
    )
  )
  }
  return(dat)
}

################################################################################
################################################################################
# Data input namotaj 1
# Input n1_1
# dat <- VibData(
#   point.id = 'pt1',
#   loc.x = -1187,
#   loc.y = 842,
#   rib = F,
#   replication = 1,
#   decimal.separator = ',',
#   file.loc = paste0('110MVA/mjerenja/namotaji/n1_1')
# )

# Remove input data for first point (n1_1)
# input.df <- input.df %>% filter(not(namotaj.id == 1 & point.id == 1))

# Input the rest of data points for n1, n2 and n3
# dat <- data.input.f(dat, input.df)

# Change replication to winding
# dat <- dat %>% rename(wind = replication)

# Select only necessary variables
# dat <- dat %>% select(-rib, -pacf, -displacement.orig)

# Recalculate FFT resolution because of sample length change
# dat <- dat %>% group_by(wind, point.id) %>%
#   mutate(d.freq = sample.rate/length(na.omit(velocity.amp))/2) %>% ungroup()

# Sanity data check
# dat %>% glimpse()
# dat %>% select(range, d.time, d.freq, sample.rate, sample.num) %>% distinct() %>% View()
# dat %>% group_by(wind, point.id) %>% summarise(
#   length = length(velocity),
#   min = min(velocity),
#   max = max(velocity),
#   mean = mean(velocity),
#   median = median(velocity),
#   )
# dat %>%
#   select(wind, point.id, peak.freq.orig, peak.vel.orig, peak.frequency, peak.velocity.amp) %>%
#   drop_na() %>% group_by(wind, point.id) %>% slice(1) %>%
#   mutate(
#     equal = if_else(
#       (peak.freq.orig <= (peak.frequency + 0.000001)) & (peak.freq.orig >= (peak.frequency - 0.000001)), T, F)) %>% View()

# Remove unnecessary variables
# dat <- dat %>% select(-peak.freq.orig, -peak.vel.orig, -peak.disp.orig,
#                       -peak.acc.orig, -velocity.orig)

# Create subtitles for plots
# dat <- dat %>% group_by(wind, point.id) %>%
#   mutate(subtitle =
#            paste0('Nam: ', wind, ' ', point.id, ': ', '(', loc.x, ', ', loc.y, ')')
#       ) %>% ungroup()

# Save current workspace
# Namotaj 3, tocka 4 lose izmjerena (skracen vremenski interval)
# save.image('110MVA/namotaji.RData')

# Load workspace
load('110MVA/namotaji.RData')

################################################################################
################################################################################
# Vremenska domena plot
dat %>% filter(wind == 1) %>%
  select(time, velocity, point.id, wind, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
  scale_y_continuous(
    name = expression('Brzina vibracija ['*mu*'m/s]'),
    breaks = seq(-1000, 1000, 1000),
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
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/nam_1_brzina_vrijeme.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

dat %>% filter(wind == 2) %>%
  select(time, velocity, point.id, wind, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
  scale_y_continuous(
    name = expression('Brzina vibracija ['*mu*'m/s]'),
    breaks = seq(-2500, 2500, 2500),
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
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/nam_2_brzina_vrijeme.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

dat %>% filter(wind == 3) %>%
  select(time, velocity, point.id, wind, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
  scale_y_continuous(
    name = expression('Brzina vibracija ['*mu*'m/s]'),
    breaks = seq(-5000, 5000, 2500),
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
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/nam_3_brzina_vrijeme.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Frekvencijska domena plot
dat %>% filter(wind == 1) %>%
  select(frequency, velocity.amp, point.id, wind, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine vibracija ['*mu*'m/s]'),
    breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
  ) +
  ggtitle("Namotaj 1 - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/nam_1_brzina_freq.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

dat %>% filter(wind == 2) %>%
  select(frequency, velocity.amp, point.id, wind, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine vibracija ['*mu*'m/s]'),
    breaks = seq(0, 2500, 1000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
  ) +
  ggtitle("Namotaj 2 - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/nam_2_brzina_freq.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

dat %>% filter(wind == 3) %>%
  select(frequency, velocity.amp, point.id, wind, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, start = 8)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine vibracija ['*mu*'m/s]'),
    breaks = seq(0, 4000, 2000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
  ) +
  ggtitle("Namotaj 3 - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/nam_3_brzina_freq.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Preliminarna analiza
# Frequency table of peak.frequency
dat %>% group_by(wind, point.id) %>%
  select(wind, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  ungroup() %>% count(peak.frequency) %>% View()
  
# Sort by amplitudes
dat %>% group_by(wind, point.id) %>%
  select(wind, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  mutate(peak.velocity.amp = round(peak.velocity.amp)) %>% View()


# Fit only 50 Hz and 100 Hz sine waves
dat_fit <- dat %>% group_by(wind, point.id) %>%
  mutate(
    om_50 = 2*pi*50,
    om_100 = 2*pi*100,
    A.50.c.term = om_50*cos(om_50*time),
    A.50.s.term = om_50*sin(om_50*time),
    A.100.c.term = om_100*cos(om_100*time),
    A.100.s.term = om_100*sin(om_100*time),
    ) %>%
  do(fitSine = lm(
    velocity ~ 0 + A.50.c.term + A.50.s.term + A.100.c.term + A.100.s.term,
    data = .))

# glance(dat_fit, fitSine)

# Amplitudes and adj_r_squared
dat_model <- tidy(dat_fit, fitSine) %>% group_by(wind, point.id) %>%
  select(wind, point.id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  summarise(
    A.50.v = 2*pi*50*sqrt(A.50.c.term^2 + A.50.s.term^2),
    A.100.v = 2*pi*100*sqrt(A.100.c.term^2 + A.100.s.term^2),
    A.50.x = sqrt(A.50.c.term^2 + A.50.s.term^2),
    A.100.x = sqrt(A.100.c.term^2 + A.100.s.term^2)
  ) %>% full_join(
    select(glance(dat_fit, fitSine), wind, point.id, adj.r.squared), by = c('wind', 'point.id')
  )

# dat_model

# Summary of data with top 5 peaks and distinct frequency peaks
dat_summary <- dat %>%
  select(wind, point.id, loc.x, loc.y, velocity, peak.to.peak.vel,
         rms.vel, peak.frequency, peak.velocity.amp, frequency, d.freq,
         velocity.amp) %>%
  group_by(wind, point.id) %>%
  summarise(
    Namotaj = mean(wind), Tocka = unique(point.id),
    Sirina = unique(loc.x), Visina = unique(loc.y),
    "Uvjet rada" = "Kratki spoj - 70 % opterećenja",
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
  full_join(dat_model, by = c('wind', 'point.id')) %>% 
  rename(
    'Model 50 Hz amp' = A.50.v,
    'Model 100 Hz amp' = A.100.v,
    'Model 50 Hz disp amp' = A.50.x,
    'Model 100 Hz disp amp' = A.100.x,
    'Model Adj R Squared' = adj.r.squared
    ) %>%
  ungroup() %>% select(-wind, -point.id)

dat_summary %>%
  write_excel_csv2(path = '110MVA/preliminarna_obrada/aktivni_dio/namotaji_summary.csv')

# Plot amplituda brzine - 100 Hz
dat_summary %>%
  select(wind = Namotaj, point.id = Tocka, loc.x = Sirina, loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    wind = paste0('Namotaj: ', wind)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(wind ~ ., scales = 'free_x') +
  geom_vline(aes(xintercept = 0)) +
  geom_point(aes(fill = amp.100),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1600, 100),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-1500, 1500, 250),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 5000, 500),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Amplitude brzine vibracija - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/amplitude_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

# Plot amplituda brzine - 50 Hz
dat_summary %>%
  select(wind = Namotaj, point.id = Tocka, loc.x = Sirina, loc.y = Visina, amp.50 = `50 Hz amp`) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    wind = paste0('Namotaj: ', wind)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(wind ~ ., scales = 'free_x') +
  geom_vline(aes(xintercept = 0)) +
  geom_point(aes(fill = amp.50),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1600, 100),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-1500, 1500, 250),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 250, 50),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Amplitude brzine vibracija - 50 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/amplitude_50_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Surface modelling
# Loading summary
dat_surf <- read_csv2(
  file = '110MVA/preliminarna_obrada/aktivni_dio/namotaji_summary.csv'
) %>% select(wind = Namotaj, point.id = Tocka, x = Sirina, y = Visina,
             amp.v.50 = `Model 50 Hz amp`, amp.v.100 = `Model 100 Hz amp`,
             amp.x.50 = `Model 50 Hz disp amp`, amp.x.100 = `Model 100 Hz disp amp`,
             adj.r.sq = `Model Adj R Squared`)

dat_surf <- dat_surf %>% group_by(wind) %>% select(wind, x, y, amp.x.100) %>%
  mutate(
    mean.x = mean(x),
    extr.x = max(max(x) - mean.x, abs(min(x) - mean.x)),
    mean.y = mean(y),
    extr.y = max(max(y) - mean.y, abs(min(y) - mean.y))
    ) %>% ungroup()

# Coding data
dat_coded <- coded.data(
  data = dat_surf %>% filter(wind==3) %>% select(x, y, amp.x.100),
  x1 ~ (x - 215.931034) / 706.0690,
  x2 ~ (y - 771.5172) / 638.4828
  )

as.data.frame(dat_coded)

# RSM low r squared value
dat_coded.rsm <- rsm(amp.x.100 ~ SO(x1, x2), data = dat_coded)
summary(dat_coded.rsm)

# Spatial data modelling - Kriging interpolation wind 1 100 Hz
krige.inter <- function(
  amp_df = dat_surf,
  namotaj = 1,
  amp = 'amp.x.100',
  plt_map = F,
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
dat_sp <- amp_df %>% filter(wind==namotaj) %$%
  SpatialPointsDataFrame(
    coords = select(., x, y),
    data = select(., !!amp)
    )
# Examining experimental variogram
var_exp <- variogram(
  formula(paste0(amp, '~1')),
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
grid_limits <- amp_df %>% filter(wind==namotaj) %>%
  summarise(
    min.x = min(x)*1.1,
    max.x = max(x)*1.1,
    min.y = min(y)*0.9,
    max.y = max(y)*1.03
    )
# Creating predictive grid and converting to SpatialPixels
pred.grid <- expand.grid(
  x = seq(grid_limits$min.x, grid_limits$max.x, length.out = grid_num),
  y = seq(grid_limits$min.y, grid_limits$max.y, length.out = grid_num)
  )
pred.grid <- SpatialPixels(SpatialPoints(pred.grid))
# Kriging interpolation
krige.pred <- krige(
  formula(paste0(amp, '~1')), dat_sp, pred.grid, model = var_fit
  )
print(spplot(krige.pred['var1.pred'], scales = list(draw = T)))
# Return object
krige.pred
}

# Creating tibble for contour and points plot
dat_disp <- dat_surf %>% select(-amp.v.50, -amp.v.100, -adj.r.sq) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    wind = 1,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.x.100',
      namotaj = 1,
      vgm_alpha = seq(0, 175, 1),
      vgm_an_angle = 135,
      vgm_an_ratio = 0.6,
      variogram_only = F,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    wind = 2,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.x.100',
      namotaj = 2,
      vgm_alpha = seq(0, 175, 1),
      vgm_an_angle = 45,
      vgm_an_ratio = 0.71,
      variogram_only = F,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    wind = 3,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.x.100',
      namotaj = 3,
      vgm_alpha = seq(0, 175, 1),
      vgm_an_angle = 14,
      vgm_an_ratio = 0.77,
      variogram_only = F,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.50'),
  y = tibble(
    wind = 3,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.x.50',
      namotaj = 3,
      vgm_cutoff = 1000,
      vgm_bin_num = 4,
      vgm_psill = 0.05,
      vgm_range = NA,
      vgm_model = 'Bes',
      vgm_alpha = seq(0, 175, 8),
      vgm_an_angle = 50,
      vgm_an_ratio = 0.8,
      variogram_only = F,
      grid_num = 300,
      ))
    ) %>% rename(amp.x.50 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.50'),
  y = tibble(
    wind = 2,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.x.50',
      namotaj = 2,
      vgm_cutoff = 900,
      vgm_bin_num = 4,
      vgm_model = 'Gau',
      vgm_alpha = seq(0, 175, 1),
      vgm_an_angle = 45,
      vgm_an_ratio = 0.7,
      variogram_only = F,
      grid_num = 300,
      ))
    ) %>% rename(amp.x.50 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('wind', 'point.id', 'x', 'y', 'amp.x.50'),
  y = tibble(
    wind = 1,
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.x.50',
      namotaj = 1,
      vgm_cutoff = 900,
      vgm_bin_num = 4,
      vgm_model = 'Bes',
      vgm_alpha = seq(0, 175, 1),
      vgm_an_angle = 135,
      vgm_an_ratio = 0.85,
      variogram_only = F,
      grid_num = 300,
      ))
    ) %>% rename(amp.x.50 = var1.pred) %>% select(-var1.var)
  )
    
# Testing parameters for interpolation
    # krige.inter(
    #   amp_df = dat_surf,
    #   amp = 'amp.x.50',
    #   namotaj = 1,
    #   vgm_cutoff = 900,
    #   vgm_bin_num = 4,
    #   vgm_psill = NA,
    #   vgm_range = NA,
    #   vgm_model = 'Bes',
    #   vgm_alpha = seq(0, 175, 1),
    #   vgm_an_angle = 135,
    #   vgm_an_ratio = 0.85,
    #   variogram_only = F,
    #   plt_map = F
    #   )

# Plot amplituda pomaka - 50 Hz
dat_disp %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.x.50.cnt = case_when(
      point.id == 'model' & amp.x.50 >= 0 ~ amp.x.50,
      point.id == 'model' & amp.x.50 <= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    amp.x.50 = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.x.50
    ),
    point.id = case_when(
      point.id != 'model' ~ str_sub(point.id, start = 3),
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(wind ~ ., scales = 'free_x') +
  geom_contour_filled(aes(z = amp.x.50.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(label = point.id), na.rm = T) +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1600, 100),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-1500, 1500, 250),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Amplitude vibracijskog pomaka - 50 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/contour_disp_50_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

# Plot amplituda pomaka - 100 Hz
dat_disp %>%
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
      point.id != 'model' ~ str_sub(point.id, start = 3),
      TRUE ~ NA_character_
      ),
    wind = paste0('Namotaj: ', wind)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(wind ~ ., scales = 'free_x') +
  geom_contour_filled(aes(z = amp.x.100.cnt), na.rm = T) +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(label = point.id), na.rm = T) +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1600, 100),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-1500, 1500, 250),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Amplitude vibracijskog pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/contour_disp_100_hz.png',
       width = 25, height = 15, units = 'cm', dpi = 320)
