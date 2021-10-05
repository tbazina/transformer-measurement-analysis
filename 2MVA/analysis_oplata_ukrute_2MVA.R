################################################################################
################# Analiza mjerenja na oplati 2MVA 06.03.2021. ##################
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
# Mjerenja su provedena na 4 strancice kotla s ukrutama pravokutnog oblika.
# Mjerene su VN strana, NN strana , bok lijevo BL i bok desno BD (ukupno 4
# povrsine kotla) i vidljivi dio ukruta s bočne lijeve UBL i bočne desne UBD
# strane. Ishodište mjernog koordinatnog sustava se nalazi u donjem lijevom kutu
# stranice oplate odnosno svake ukrute.
# Na bočnim mjernim povrsinama (BL i BD) raspoređeno je 15 mjernih točaka.
# Na VN i NN strani raspoređeno je 35 mjernih točaka.
# Na UBL i UBD stranama raspoređeno je po 8 mjernih točaka (dvije po svakoj ukruti)

# Data for loop input - KOORDINATE TOČAKA NISU PROMIJENJENE (STVARNE KORDINATE)

# base.doe <- tibble::tribble(
#       ~point.id,           ~loc.x,           ~loc.y,
#              1L,               4L,             360L,
#              2L,              52L,             712L,
#              3L,              87L,            1023L,
#              4L,             124L,             100L,
#              5L,             178L,            1209L,
#              6L,             208L,             575L,
#              7L,             239L,             313L,
#              8L,             286L,            1041L,
#              9L,             350L,             776L,
#             10L,             358L,              70L,
#             11L,             402L,             540L,
#             12L,             444L,            1144L,
#             13L,             502L,             907L,
#             14L,             535L,             393L,
#             15L,             559L,             702L,
#             16L,             592L,             231L,
#             17L,             648L,             885L,
#             18L,             709L,             455L,
#             19L,             734L,              20L,
#             20L,             750L,            1081L,
#             21L,             801L,             647L,
#             22L,             847L,             269L,
#             23L,             875L,             956L,
#             24L,             933L,             467L,
#             25L,             965L,             157L,
#             26L,             999L,            1106L,
#             27L,            1027L,             848L,
#             28L,            1095L,             617L,
#             29L,            1114L,             352L,
#             30L,            1146L,              55L,
#             31L,            1201L,             990L,
#             32L,            1244L,            1204L,
#             33L,            1276L,             505L,
#             34L,            1330L,             813L,
#             35L,            1379L,             189L
#   )
# flank.doe <- tibble::tribble(
#         ~point.id,           ~loc.x,           ~loc.y,
#                1L,              16L,             779L,
#                2L,              61L,             432L,
#                3L,              92L,            1132L,
#                4L,             144L,             213L,
#                5L,             180L,             671L,
#                6L,             196L,            1048L,
#                7L,             244L,             260L,
#                8L,             330L,              30L,
#                9L,             347L,             569L,
#               10L,             353L,            1225L,
#               11L,             409L,             881L,
#               12L,             425L,             399L,
#               13L,             464L,             115L,
#               14L,             534L,             640L,
#               15L,             562L,             923L
#     )
# rib.doe <-  tibble::tribble(
#          ~point.id,           ~loc.x,           ~loc.y,
#                 1L,              60L,             189L,
#                 2L,              60L,             378L,
#                 3L,              60L,             189L,
#                 4L,              60L,             378L,
#                 5L,              60L,             189L,
#                 6L,              60L,             378L,
#                 7L,              60L,             189L,
#                 8L,              60L,             378L
#      )

# input.df <- base.doe %>% mutate(side = 'VN') %>%
#   full_join(base.doe %>% mutate(side = 'NN')) %>%
#   full_join(flank.doe %>% mutate(side = 'BL')) %>%
#   full_join(flank.doe %>% mutate(side = 'BD')) %>%
#   full_join(rib.doe %>% mutate(side = 'UBL')) %>%
#   full_join(rib.doe %>% mutate(side = 'UBD')) %>%
#   select(side, point.id, loc.x, loc.y)
# 
# input.df %>% rename(Strana=side, Tocka=point.id, x=loc.x, y=loc.y) %>%
#   write_excel_csv2('2MVA/DoE/Oplata_i_prigusenje/modificirana_oplata_stvarne_koordinate.csv')
# 
# input.df %>% group_by(side) %>%
#   summarise(count = n())

# Data input function
# data.input.f <- function(dat, input.df) {
#   # Folder u kojem se nalaze mjerenja
#   oplata.path <- '2MVA/mjerenja/oplata_modificirana/'
#   for (i in 1:length(input.df$point.id)) {
#     # print(input.df$side[i])
#     # print(paste0('pt', input.df$point.id[i]))
#     # print(input.df$loc.x[i])
#     # print(input.df$loc.y[i])
#     read.path <- paste0(
#       oplata.path,
#       switch (
#         input.df$side[i], 'NN' = 'NN_', 'VN' = 'VN_', 'BL' = 'BL_', 'BD' = 'BD_',
#         'UBL' = 'UBL_', 'UBD' = 'UBD_'
#         ),
#       input.df$point.id[i]
#     )
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
# Data input VN pt1
# Input VN_1
# dat <- VibData(
#   point.id = input.df$point.id[1],
#   loc.x = input.df$loc.x[1],
#   loc.y = input.df$loc.y[1],
#   rib = input.df$side[1],
#   replication = 1,
#   decimal.separator = ',',
#   file.loc = paste0('2MVA/mjerenja/oplata_modificirana/VN_1')
# )

# Remove input data for first point (VN_1)
# print(slice(input.df, 1))
# input.df <- input.df %>% slice(-1)

# Input the rest of data points for VN, NN, BL, BD, UBL and UBD
# dat <- data.input.f(dat, input.df)

# Glimpse data
# dat %>% glimpse()

# Change rib to side
# dat <- dat %>% rename(side = rib)

# Select only necessary variables
# dat <- dat %>% select(-pacf, -displacement.orig, -replication)

# Recalculate FFT resolution because of sample length change
# dat <- dat %>% group_by(side, point.id) %>%
#   mutate(d.freq = sample.rate/length(na.omit(velocity.amp))/2) %>% ungroup()

# Sanity data check
# dat %>% group_by(side) %>% select(point.id) %>% distinct() %>%
#   summarise(count = n())
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

# Create subtitles for plots
# dat <- dat %>% group_by(side, point.id) %>%
#   mutate(subtitle =
#            paste0('Str: ', side, ' pt', point.id, ': ', '(', loc.x, ', ', loc.y, ')')
#       ) %>% ungroup()

# Save current workspace
# save.image('2MVA/oplata_modificirana.RData')

# Load workspace
load('2MVA/oplata_modificirana.RData')

################################################################################
################################################################################
# Vremenska domena plot
dat %>% filter(
  (side == 'VN' & point.id %in% c(3, 9, 14)
   ) | (side == 'NN' & point.id %in% c(3, 9, 14)
   ) | (side == 'BL' & point.id %in% c(3, 7, 9)
   ) | (side == 'BD' & point.id %in% c(3, 7, 9)
   ) | (side == 'UBL' & point.id %in% c(3, 7, 6)
   ) | (side == 'UBD' & point.id %in% c(3, 7, 6))
  ) %>%
  select(time, velocity, point.id, side, subtitle) %>%
  # mutate(subtitle = str_sub(subtitle, start = 9)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle,
                    levels = mixedsort(unique(subtitle), decreasing = F)) ~ .,
             ncol = 3
             ) +
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
  ggtitle("Vremenska domena - oplata i ukrute") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/brzina_vrijeme.png',
       width = 17, height = 21, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Frekvencijska domena plot
dat %>% filter(
  (side == 'VN' & point.id %in% c(3, 9, 14)
   ) | (side == 'NN' & point.id %in% c(3, 9, 14)
   ) | (side == 'BL' & point.id %in% c(3, 7, 9)
   ) | (side == 'BD' & point.id %in% c(3, 7, 9)
   ) | (side == 'UBL' & point.id %in% c(3, 7, 6)
   ) | (side == 'UBD' & point.id %in% c(3, 7, 6))
  ) %>%
  select(frequency, velocity.amp, point.id, side, subtitle) %>%
  drop_na() %>%
  # mutate(subtitle = str_sub(subtitle, start = 9)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle,
                    levels = mixedsort(unique(subtitle), decreasing = F)) ~ .,
             ncol = 3
             ) +
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
  ggtitle("Frekvencijska domena - oplata i ukrute") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/brzina_freq.png',
       width = 17, height = 21, units = 'cm', dpi = 320)

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
    path = '2MVA/preliminarna_obrada/oplata_modificirana/oplata_modificirana_summary.csv'
    )

# Load data summary
dat_summary <- read_csv2(
  file = '2MVA/preliminarna_obrada/oplata_modificirana/oplata_modificirana_summary.csv'
    )

# Check max 50 Hz amplitude and min 100 Hz amplitude
dat_summary %>%
  select(side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`, amp.50 = `50 Hz amp`) %>%
  select(amp.100, amp.50) %>%
  pivot_longer(cols=c('amp.100', 'amp.50')) %>%
  ggplot(aes(name, value)) +
  geom_boxplot(orientation = 'x')

# Quantiles of amplitudes (100 Hz vs 50 Hz)
dat_summary %>%
  select(side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `Model 100 Hz amp`, amp.50 = `50 Hz amp`) %>%
  select(amp.100, amp.50) %>%
  summarise(
    q.100 = quantile(amp.100, 0.19),
    q.50 = quantile(amp.50, 0.95)
  )

################################################################################
################################################################################
# Plotovi na oplati
# Plot amplituda brzine - VN i NN strana 100 Hz
dat_summary %>%
  select(side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  filter(side %in% c('VN', 'NN')) %>%
  mutate(
    side = paste0('Strana: ', side)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c(
        'Strana: VN', 'Strana: NN', 'Strana: BL', 'Strana: BD')) ~ ., ncol = 2
    ) +
  geom_rect(aes(xmin = 96, xmax = 96+16, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16+564, xmax = 96+16*2+564, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16*2+564*2, xmax = 96+16*3+564*2, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96, xmax = 96+16, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16+564, xmax = 96+16*2+564, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16*2+564*2, xmax = 96+16*3+564*2, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  coord_fixed() +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 200),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 1500, 100),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.85
  ) +
  scale_size_area(
    max_size = 10,
    # range = c(5, 10),
    guide = "none"
  ) +
  ggtitle("Strane: VN i NN - Amplitude brzine - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(
  filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/amplitude_100_hz_oplata_vn_nn.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

# Plot amplituda brzine - BL i BD strana 100 Hz
dat_summary %>%
  select(side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  filter(side %in% c('BL', 'BD')) %>%
  mutate(
    side = paste0('Strana: ', side)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c(
        'Strana: VN', 'Strana: NN', 'Strana: BL', 'Strana: BD')) ~ ., ncol = 2
    ) +
  coord_fixed() +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 100),
    limits = c(NA, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 1500, 100),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.8
  ) +
  scale_size_area(
    max_size = 9,
    # range = c(5, 10),
    guide = "none"
  ) +
  ggtitle("Strane: BL i BD - Amplitude brzine - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(
  filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/amplitude_100_hz_oplata_bl_bd.png',
       width = 17, height = 15, units = 'cm', dpi = 320)

# Plot amplituda brzine - UBL i UBD strana 100 Hz
dat_summary %>%
  select(side = Strana, point.id = Tocka, loc.x = Sirina,
         loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  filter(side %in% c('UBL', 'UBD')) %>%
  mutate(
    ukr = case_when(
      point.id %% 2 == 0 ~ paste(point.id - 1, point.id, sep=" - ") ,
      TRUE ~ paste(point.id, point.id +1, sep=" - ")
    )
  ) %>%
  mutate(
    side = paste(side, ukr)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c(
        'UBL 1 - 2', 'UBL 3 - 4', 'UBL 5 - 6', 'UBL 7 - 8',
        'UBD 1 - 2', 'UBD 3 - 4', 'UBD 5 - 6', 'UBD 7 - 8'
        )) ~ ., ncol = 8
    ) +
  coord_fixed() +
  geom_point(aes(fill = amp.100, size = amp.100),
             shape = 21, colour = "black",
             stroke = 1,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(0, 567)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 50),
    limits = c(0, 120)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 1500, 50),
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
  ggtitle("Strane: UBL i UBD - Amplitude brzine - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(
  filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/amplitude_100_hz_oplata_ubl_ubd.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Surface modelling
# Loading summary and adding coordinates for BD, NN and BL from VN
dat_surf <- read_csv2(
  file = '2MVA/preliminarna_obrada/oplata_modificirana/oplata_modificirana_summary.csv'
  ) %>%
  select(
    side = Strana, point.id = Tocka, x = Sirina, y = Visina,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    adj.r.sq = `Model Adj R Squared`
    ) 
  # mutate(
  #   x.VN = case_when(
  #     (side == 'BD') ~ as.integer(1380 + x),
  #     (side == 'NN') ~ as.integer(1380 + 590 + x),
  #     (side == 'BL') ~ as.integer(1380*2 + 590 + x),
  #     TRUE ~ as.integer(x)
  #     )
  #   )

# Histogram of amplitudes
dat_surf %>%
  ggplot(aes(x=(amp.v.100))) +
  facet_wrap(c('side'), scales = 'free_x') +
  geom_histogram(bins = 5, color='black')

# Plotting coordinates for sanity check
# dat_surf %>%
#   ggplot(aes(x = x.VN, y = y, label = paste0(side, point.id))) + geom_text()

# Extremes for coding
# dat_surf <- dat_surf %>% group_by(side) %>%
#   select(side, x, y, amp.v.100) %>%
#   mutate(
#     mean.x = mean(x),
#     extr.x = max(max(x) - mean.x, abs(min(x) - mean.x)),
#     mean.y = mean(y),
#     extr.y = max(max(y) - mean.y, abs(min(y) - mean.y))
#     ) %>% ungroup()

# dat_surf %>% distinct(side, mean.x, extr.x, mean.y, extr.y) %>% View()

# Coding data
# dat_coded <- coded.data(
#   data = 
#     dat_surf %>%
#     # filter(side %in% c('VN', 'NN')) %>%
#     filter(side %in% c('VN')) %>%
#     select(x, y, amp.v.100),
#     # mutate(side = as.factor(side)),
#   # x1 ~ (x- 295) / 295,
#   x1 ~ (x- 690) / 690,
#   x2 ~ (y - 621.5) / 621.5
#   )

# as.data.frame(dat_coded)

# RSM low r squared value
# dat_coded.rsm <- rsm(amp.v.100 ~ FO(x1, x2), data = dat_coded)
# dat_coded.rsm <- rsm(amp.v.100 ~ SO(x1, x2) + PQ(x1, x2), data = dat_coded)
# summary(dat_coded.rsm)
# sqrt(mean(dat_coded.rsm$residuals^2))

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
  vgm_map_threshold = 3,
  grid_num = 300
  ) {
# Creating SpatialPointsDataFrame
dat_sp <- amp_df %$%
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
  print(plot(var_exp, threshold = vgm_map_threshold))
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
    min.x = min(x)*0.1,
    max.x = max(x)*1.03,
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
dat_surf %>% filter(side == 'BL') %>% select(x, y) %>%
  summarise(
    max.x = max(x),
    min.x = min(x),
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
      amp_df = dat_surf %>% filter(side == 'BD'),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 135,
      vgm_an_ratio = 5/6,
      vgm_map_threshold = 3,
      grid_num = 300
      )

# Creating tibble for contour and points plot
# Modeling velocity
dat_disp_vel <- dat_surf %>%
  select(-amp.x.100, -adj.r.sq) %>%
  mutate(point.id = as.character(point.id)) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100'),
  y = tibble(
    side = 'VN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'VN'),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 1000,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 90,
      vgm_an_ratio = 3/7,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100'),
  y = tibble(
    side = 'NN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'NN'),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 900,
      vgm_bin_num = 5,
      vgm_psill = 25000,
      vgm_range = 200,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 90,
      vgm_an_ratio = 3/6,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100'),
  y = tibble(
    side = 'BL',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BL'),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 4,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 0,
      vgm_an_ratio = 6/6,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100'),
  y = tibble(
    side = 'BD',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BD'),
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 135,
      vgm_an_ratio = 5/6,
      vgm_map_threshold = 3,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  )

# Creating tibble for contour and points plot
# Modeling displacement
dat_disp_disp <- dat_surf %>%
  select(-amp.v.100, -adj.r.sq) %>%
  mutate(point.id = as.character(point.id)) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    side = 'VN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'VN'),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 1000,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 90,
      vgm_an_ratio = 3/7,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    side = 'NN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'NN'),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 900,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 90,
      vgm_an_ratio = 3/6,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    side = 'BL',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BL'),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 4,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 0,
      vgm_an_ratio = 6/6,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    side = 'BD',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BD'),
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 135,
      vgm_an_ratio = 5/6,
      vgm_map_threshold = 3,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  )

# Plot amplituda pomaka - Strana NN i VN - 100 Hz
dat_disp_disp %>%
  filter(side %in% c('VN', 'NN')) %>%
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
    side = paste0('Strana: ', side)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c(
        'Strana: VN', 'Strana: NN', 'Strana: BL', 'Strana: BD')) ~ ., ncol = 2
    ) +
  geom_contour_filled(aes(z = amp.x.100.cnt), na.rm = T) +
  geom_rect(aes(xmin = 96, xmax = 96+16, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16+564, xmax = 96+16*2+564, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16*2+564*2, xmax = 96+16*3+564*2, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96, xmax = 96+16, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16+564, xmax = 96+16*2+564, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16*2+564*2, xmax = 96+16*3+564*2, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 200),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Strane: VN i NN - Amplitude pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/contour_disp_vn_nn_100_hz.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

# Plot amplituda pomaka - Strana BL i BD - 100 Hz
dat_disp_disp %>%
  filter(side %in% c('BL', 'BD')) %>%
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
    side = paste0('Strana: ', side)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c(
        'Strana: VN', 'Strana: NN', 'Strana: BL', 'Strana: BD')) ~ ., ncol = 2
    ) +
  geom_contour_filled(aes(z = amp.x.100.cnt), na.rm = T) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.9
  ) +
  ggtitle("Strane: BL i BD - Amplitude pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/contour_disp_bl_bd_100_hz.png',
       width = 17, height = 15, units = 'cm', dpi = 320)

# Plot amplituda brzine - Strane VN i NN - 100 Hz
dat_disp_vel %>%
  filter(side %in% c('VN', 'NN')) %>%
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
    side = paste0('Strana: ', side)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c(
        'Strana: VN', 'Strana: NN', 'Strana: BL', 'Strana: BD')) ~ ., ncol = 2
    ) +
  geom_contour_filled(aes(z = amp.v.100.cnt), na.rm = T) +
  geom_rect(aes(xmin = 96, xmax = 96+16, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16+564, xmax = 96+16*2+564, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16*2+564*2, xmax = 96+16*3+564*2, ymin = 29.5, ymax = 29.5+567),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96, xmax = 96+16, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16+564, xmax = 96+16*2+564, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_rect(aes(xmin = 96+16*2+564*2, xmax = 96+16*3+564*2, ymin = 29.5+567+50, ymax = 29.5+567*2+50),
            fill = NA, linetype = 1, color = 'black') +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 200),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Strane: VN i NN - Amplitude brzina - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/contour_vel_vn_nn_100_hz.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

# Plot amplituda brzine - Strane BL i BD - 100 Hz
dat_disp_vel %>%
  filter(side %in% c('BL', 'BD')) %>%
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
    side = paste0('Strana: ', side)
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(
    factor(
      side, levels = c(
        'Strana: VN', 'Strana: NN', 'Strana: BL', 'Strana: BD')) ~ ., ncol = 2
    ) +
  geom_contour_filled(aes(z = amp.v.100.cnt), na.rm = T) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 100),
    limits = c(NA, NA),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.92
  ) +
  ggtitle("Strane: BL i BD - Amplitude brzina - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/preliminarna_obrada/oplata_modificirana/slike/contour_vel_bl_bd_100_hz.png',
       width = 17, height = 15, units = 'cm', dpi = 320)
