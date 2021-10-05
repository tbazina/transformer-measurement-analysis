################################################################################
################# Analiza mjerenja na jezgri 2MVA 12.09.2020. ##################
################################################################################

# Source functions and objects file
source('functions.R')

# Plotting
library(ggplot2)
# Data paste
library(datapasta)
# Sorting
library(gtools)

################################################################################
################################################################################

# Transformator radi u kratkom spoju - 50 A, 1000 V na primaru.
# Mjerenja su provedena na 6 točaka na jezgri.
# Mjerene su NN strana i dvije bočne strane jezgre (ukupno 5 točaka).
# Mjerne točke se nalaze na sjecištu simetrala.
# Na svakoj mjernoj povrsini je raspoređena 1 mjerna točka.
# Namotaji su označeni s brojevima 1, 2, 3

# Data for loop input - KOORDINATE SVIH TOČAKA PROMIJENJENE (STVARNE KORDINATE)

# input.df <- tibble::tribble(
#  ~point.id, ~loc.x, ~loc.y,
#         7L,  -70L,   17L,
#         8L,  -70L,   17L,
#         9L,  -70L,   17L,
#        13L,    0L,  -51L,
#        17L,    0L,  -51L,
#  ) %>% mutate(
#    side = case_when(
#      point.id %in% c(7, 8, 9) ~ 'NN',
#      point.id == 13 ~ 'bok lijevo',
#      point.id == 17 ~ 'bok desno',
#      TRUE ~ 'ERROR'
#    )
#  )
# 
# input.df %>% rename(Strana=side, Tocka=point.id, x=loc.x,
#                     y=loc.y) %>% select(Strana, Tocka, x, y) %>%
#   mutate(Tocka = paste0('S', Tocka)) %>%
#   write_excel_csv2('2MVA/DoE/Aktivni_dio/jezgra_stvarne_koordinate.csv')

# Data input function
# data.input.f <- function(dat, input.df) {
#   # Folder u kojem se nalaze mjerenja
#   jezgra.path <- '2MVA/mjerenja/jezgra/'
#   for (i in 1:length(input.df$point.id)) {
#     print(input.df$side[i])
#     print(paste0('S', input.df$point.id[i]))
#     print(input.df$loc.x[i])
#     print(input.df$loc.y[i])
#     read.path <- paste0(jezgra.path, 'j', input.df$point.id[i])
#     print(read.path)
#   print('#######################')
#     dat <- dat %>%
#       add_row(
#         !!!VibData(
#           point.id = input.df$point.id[i],
#           loc.x = input.df$loc.x[i],
#           loc.y = input.df$loc.y[i],
#           rib = input.df$side[i],
#           replication = 1,
#           decimal.separator = ",",
#           file.loc = read.path
#     )
#   )
#   }
#   return(dat)
# }

################################################################################
################################################################################
# Data input NN S7
# Input j7
# dat <- VibData(
#   point.id = input.df$point.id[1],
#   loc.x = input.df$loc.x[1],
#   loc.y = input.df$loc.y[1],
#   rib = input.df$side[1],
#   replication = 1,
#   decimal.separator = ',',
#   file.loc = paste0('2MVA/mjerenja/jezgra/j7')
# )

# Remove input data for first point (S7)
# print(slice(input.df, 1))
# input.df <- input.df %>% slice(-1)

# Input the rest of data points 
# dat <- data.input.f(dat, input.df)

# Change replication to winding and rib to side
# dat <- dat %>% rename(side = rib)

# Select only necessary variables
# dat <- dat %>% select(-pacf, -displacement.orig, -replication)

# Recalculate FFT resolution because of sample length change
# dat <- dat %>% group_by(point.id) %>%
#   mutate(d.freq = sample.rate/length(na.omit(velocity.amp))/2) %>% ungroup()

# Sanity data check
# dat %>% glimpse()
# dat %>% select(range, d.time, d.freq, sample.rate, sample.num) %>% distinct() %>% View()
# dat %>% group_by(side, point.id) %>% summarise(
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
#   select(side, point.id, peak.freq.orig, peak.vel.orig, peak.frequency, peak.velocity.amp) %>%
#   drop_na() %>% group_by(side, point.id) %>% slice(1) %>%
#   mutate(
#     equal = if_else(
#       (peak.freq.orig <= (peak.frequency + 0.2)) & (peak.freq.orig >= (peak.frequency - 0.2)), T, F)) %>% View()

# Remove unnecessary variables
# dat <- dat %>% select(-peak.freq.orig, -peak.vel.orig, -peak.disp.orig,
#                       -peak.acc.orig, -velocity.orig)

# Create subtitles for plots and reorder columns
# dat <- dat %>% group_by(side, point.id) %>%
#   mutate(subtitle =
#            paste0(side, ' S', point.id, ': ', '(', loc.x, ', ', loc.y, ')')
#       ) %>% ungroup() %>% relocate(side)

# Save current workspace
# save.image('2MVA/jezgra.RData')

# Load workspace
load('2MVA/jezgra.RData')

################################################################################
################################################################################
# Vremenska domena plot
dat %>% filter(point.id %in% c(9, 13, 17)) %>%
  select(time, velocity, point.id, side, subtitle) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
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
  ggtitle("Jezgra - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/jezg_brzina_vrijeme.png',
       width = 17, height = 4.5, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Frekvencijska domena plot
dat %>% filter(point.id %in% c(9, 13, 17)) %>%
  select(frequency, velocity.amp, point.id, side, subtitle) %>%
  drop_na() %>%
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
  ggtitle("Jezgra - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/jezg_brzina_freq.png',
       width = 17, height = 4.5, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Preliminary analysis
# Frequency table of peak.frequency
dat %>% group_by(side, point.id) %>%
  select(side, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  ungroup() %>% count(peak.frequency) %>% View()
  
# Sort by amplitudes
dat %>% group_by(side, point.id) %>%
  select(side, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  mutate(peak.velocity.amp = round(peak.velocity.amp)) %>% View()

# Fit only 100 Hz sine waves
dat_fit <- dat %>%
  mutate(
    om_100 = 2*pi*100,
    A.100.c.term = om_100*cos(om_100*time),
    A.100.s.term = om_100*sin(om_100*time),
    ) %>%
  nest_by(side, point.id) %>%
  mutate(fitSine = list(lm(
    velocity ~ 0 + A.100.c.term + A.100.s.term,
    data = data)))

dat_fit %>% summarise(glance(fitSine))
dat_fit %>% summarise(tidy(fitSine))

# Amplitudes and adj_r_squared
dat_model <- dat_fit %>% summarise(tidy(fitSine)) %>%
  group_by(side, point.id) %>%
  select(side, point.id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  summarise(
    A.100.v = 2*pi*100*sqrt(A.100.c.term^2 + A.100.s.term^2),
    A.100.x = sqrt(A.100.c.term^2 + A.100.s.term^2)
  ) %>% full_join(
    select(dat_fit %>% summarise(glance(fitSine)), side, point.id,
           adj.r.squared),
    by = c('side', 'point.id')
  )

dat_model %>% View()

# Summary of data with top 5 peaks and distinct frequency peaks
dat_summary <- dat %>%
  select(side, point.id, loc.x, loc.y, velocity, peak.to.peak.vel,
         rms.vel, peak.frequency, peak.velocity.amp, frequency, d.freq,
         velocity.amp) %>%
  group_by(side, point.id) %>%
  summarise(
    Strana=unique(side), Tocka = unique(point.id),
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
  full_join(dat_model, by = c('side', 'point.id')) %>% 
  rename(
    'Model 100 Hz amp' = A.100.v,
    'Model 100 Hz disp amp' = A.100.x,
    'Model Adj R Squared' = adj.r.squared
    ) %>%
  ungroup() %>% select(-side, -point.id)

dat_summary %>%
  write_excel_csv2(
    path = '2MVA/preliminarna_obrada/aktivni_dio/jezgra_summary.csv'
    )

# Load data summary
dat_summary <- read_csv2(
  file = '2MVA/preliminarna_obrada/aktivni_dio/jezgra_summary.csv'
    )

################################################################################
################################################################################
# Plotovi na jezgri
# Plot amplituda brzine Jezgra - 100 Hz
dat_summary %>%
  select(side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  mutate(
    side = paste0(side, ' S', point.id)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side,
      levels = c('bok desno S17',
                 'NN S7',
                 'NN S8',
                 'NN S9',
                 'bok lijevo S13')) ~ .,
    scales = "free_y",
    nrow = 1
    ) +
  # coord_fixed() +
  geom_vline(aes(xintercept = 0)) +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    # name = expression('Visina '*italic('y')*' [mm]'),
    name = NULL,
    # breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    labels = NULL
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    # breaks = seq(-150, 150, 50),
    limits = c(-90, 90)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 400, 25),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.6
  ) +
  scale_size_area(
    max_size = 9,
    # range = c(5, 10),
    guide = "none"
  ) +
  ggtitle("Jezgra - Amplitude brzine - 100 Hz") +
  theme_bw() +
  theme(
    axis.line.x = element_line(size=0.5, colour = "black"),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.1, 'lines'),
    legend.margin = margin(l=0, unit = 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/amplitude_100_hz_jezg.png',
       width = 17, height = 5, units = 'cm', dpi = 320)

# Plot amplituda pomaka Jezgra - 100 Hz
dat_summary %>%
  select(side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `Model 100 Hz disp amp`) %>%
  mutate(
    side = paste0(side, ' S', point.id)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side,
      levels = c('bok desno S17',
                 'NN S7',
                 'NN S8',
                 'NN S9',
                 'bok lijevo S13')) ~ .,
    scales = "free_y",
    nrow = 1
    ) +
  # coord_fixed() +
  geom_vline(aes(xintercept = 0)) +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    # name = expression('Visina '*italic('y')*' [mm]'),
    name = NULL,
    # breaks = seq(0, 700, 50),
    limits = c(NA, NA),
    labels = NULL
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    # breaks = seq(-150, 150, 50),
    limits = c(-90, 90)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m]'),
    breaks = seq(0, 0.5, 0.05),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.6
  ) +
  scale_size_area(
    max_size = 9,
    # range = c(5, 10),
    guide = "none"
  ) +
  ggtitle("Jezgra - Amplitude pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line.x = element_line(size=0.5, colour = "black"),
    axis.ticks.y.left = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.1, 'lines'),
    legend.margin = margin(l=0, unit = 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/aktivni_dio/slike/amplitude_disp_100_hz_jezg.png',
       width = 17, height = 5, units = 'cm', dpi = 320)
