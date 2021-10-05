################################################################################
################ Analiza mjerenja na oplati 110MVA 18.07.2020. #################
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
# Mjerenja su provedena na oplati. Odabran je pravokutni dio 3600 × 2800
# Na oplati je raspoređeno ukupno 80 mjernih točaka, približno pola na rebrima.

# Data for loop input - KOORDINATE NEKIH TOČAKA PROMIJENJENE (STVARNE KORDINATE)
# input.df <- tibble::tribble(
#  ~point.id,           ~loc.x,           ~loc.y, ~Ukruta,
#         1L,               9L,             323L,    "DA",
#         2L,              51L,             732L,    "DA",
#         3L,              95L,             969L,    "DA",
#         4L,             173L,            2490L,    "DA",
#         5L,             195L,            1781L,    "DA",
#         6L,             248L,            1281L,    "NE",
#         7L,             314L,              26L,    "NE",
#         8L,             328L,            2149L,    "NE",
#         9L,             405L,            1557L,    "NE",
#        10L,             424L,             589L,    "NE",
#        11L,             475L,             269L,    "NE",
#        12L,             516L,            2740L,    "NE",
#        13L,             581L,             907L,    "DA",
#        14L,             615L,            1396L,    "DA",
#        15L,             678L,            2439L,    "DA",
#        16L,             715L,            2022L,    "DA",
#        17L,             752L,              92L,    "DA",
#        18L,             773L,             728L,    "DA",
#        19L,             857L,            1045L,    "NE",
#        20L,             891L,            1661L,    "NE",
#        21L,             945L,             398L,    "NE",
#        22L,             994L,            2543L,    "NE",
#        23L,            1034L,            2247L,    "NE",
#        24L,            1067L,            1162L,    "NE",
#        25L,            1130L,             107L,    "NE",
#        26L,            1158L,             635L,    "DA",
#        27L,            1211L,            1955L,    "DA",
#        28L,            1231L,            1417L,    "DA",
#        29L,            1287L,             840L,    "DA",
#        30L,            1320L,             193L,    "DA",
#        31L,            1365L,            2618L,    "NE",
#        32L,            1418L,            2326L,    "NE",
#        33L,            1460L,            1728L,    "NE",
#        34L,            1536L,             312L,    "NE",
#        35L,            1582L,             600L,    "NE",
#        36L,            1620L,             943L,    "NE",
#        37L,            1673L,            1247L,    "NE",
#        38L,            1689L,            2098L,    "NE",
#        39L,            1760L,            1526L,    "DA",
#        40L,            1795L,            1108L,    "DA",
#        41L,            1844L,             156L,    "DA",
#        42L,            1880L,            1839L,    "DA",
#        43L,            1945L,            2391L,    "NE",
#        44L,            1996L,            2691L,    "NE",
#        45L,            2022L,             744L,    "NE",
#        46L,            2064L,             428L,    "NE",
#        47L,            2123L,            1484L,    "NE",
#        48L,            2174L,            2216L,    "NE",
#        49L,            2225L,            1150L,    "NE",
#        50L,            2270L,            1899L,    "NE",
#        51L,            2300L,            2778L,    "DA",
#        52L,            2339L,             483L,    "DA",
#        53L,            2367L,            1595L,    "DA",
#        54L,            2444L,             792L,    "DA",
#        55L,            2495L,            2480L,    "DA",
#        56L,            2522L,              52L,    "NE",
#        57L,            2582L,            1326L,    "NE",
#        58L,            2611L,            2200L,    "NE",
#        59L,            2639L,            1619L,    "NE",
#        60L,            2730L,            1868L,    "NE",
#        61L,            2747L,            2629L,    "NE",
#        62L,            2772L,             382L,    "NE",
#        63L,            2842L,            1073L,    "NE",
#        64L,            2901L,            1337L,    "DA",
#        65L,            2913L,             552L,    "DA",
#        66L,            2986L,            2055L,    "DA",
#        67L,            3031L,            2356L,    "DA",
#        68L,            3092L,            1697L,    "NE",
#        69L,            3117L,            2721L,    "NE",
#        70L,            3173L,            2117L,    "NE",
#        71L,            3215L,             865L,    "NE",
#        72L,            3267L,            1207L,    "NE",
#        73L,            3296L,             224L,    "NE",
#        74L,            3357L,            1961L,    "NE",
#        75L,            3400L,            1452L,    "NE",
#        76L,            3439L,            2567L,    "DA",
#        77L,            3477L,            2292L,    "DA",
#        78L,            3533L,             523L,    "DA",
#        79L,            3592L,            1004L,    "DA",
#        80L,            3607L,            1791L,    "DA"
#    ) %>% rename(rib=Ukruta) %>% mutate(
#      rib = case_when(
#        rib == 'DA' ~ T,
#        rib == 'NE' ~ F,
#      )
#    )
# 
# input.df %>% mutate(
#      rib = case_when(
#        rib == T ~ 'DA',
#        rib == F ~ 'NE'
#      )) %>% rename(Tocka=point.id, x=loc.x, y=loc.y, Ukruta=rib) %>%
#   write_excel_csv2('110MVA/DoE/Oplata_i_prigusenje/oplata_stvarne_koordinate.csv')

# Data input function
# data.input.f <- function(dat, input.df) {
#   # Folder u kojem se nalaze mjerenja
#   oplata.path <- '110MVA/mjerenja/oplata/'
#   for (i in 1:length(input.df$point.id)) {
#     print(paste0('pt', input.df$point.id[i]))
#     print(input.df$loc.x[i])
#     print(input.df$loc.y[i])
#     print(input.df$rib[i])
#     print(paste0(
#                 oplata.path, 'pt_', input.df$point.id[i]
#                 ))
#     print('#######################')
#     dat <- dat %>%
#       add_row(
#         !!!VibData(
#           point.id = paste0('pt', input.df$point.id[i]),
#           loc.x = input.df$loc.x[i],
#           loc.y = input.df$loc.y[i],
#           rib = input.df$rib[i],
#           replication = 1,
#           decimal.separator = ",",
#           file.loc = paste0(
#                 oplata.path, 'pt_', input.df$point.id[i]
#                 )
#     )
#   )
#   }
#   return(dat)
# }

################################################################################
################################################################################
# Data input pt1
# dat <- VibData(
#   point.id = 'pt1',
#   loc.x = 9,
#   loc.y = 323,
#   rib = T,
#   replication = 1,
#   decimal.separator = ',',
#   file.loc = paste0('110MVA/mjerenja/oplata/pt_1')
# )

# Remove input data for first point (pt1)
# input.df <- input.df %>% filter(not(point.id == 1))

# Input the rest of data points
# dat <- data.input.f(dat, input.df)

# Select only necessary variables
# dat <- dat %>% select(-pacf, -displacement.orig)

# Recalculate FFT resolution because of sample length change
# dat <- dat %>% group_by(point.id) %>%
#   mutate(d.freq = sample.rate/length(na.omit(velocity.amp))/2) %>% ungroup()

# Sanity data check
# dat %>% glimpse()
# dat %>% select(range, d.time, d.freq, sample.rate, sample.num) %>% distinct() %>% View()
# dat %>% group_by(point.id) %>% summarise(
#   length = length(velocity),
#   min = min(velocity),
#   max = max(velocity),
#   mean = mean(velocity),
#   median = median(velocity),
#   volt.max = max(voltage),
#   volt.min = min(voltage)
#   ) %>% arrange(as.numeric(str_sub(point.id, 3))) %>% View()
# dat %>%
#   select(point.id, peak.freq.orig, peak.vel.orig, peak.frequency, peak.velocity.amp) %>%
#   drop_na() %>% group_by(point.id) %>% slice(1) %>%
#   mutate(
#     duplo = peak.velocity.amp * 2,
#     razlika = peak.vel.orig - peak.velocity.amp * 2,
#     equal = if_else(
#       (peak.freq.orig <= (peak.frequency + 0.000001)) & (peak.freq.orig >= (peak.frequency - 0.000001)), T, F)) %>% View()

# Remove unnecessary variables
# dat <- dat %>% select(-peak.freq.orig, -peak.vel.orig, -peak.disp.orig,
#                       -peak.acc.orig, -velocity.orig)

# Create subtitles for plots
# dat <- dat %>% group_by(point.id) %>%
#   mutate(subtitle =
#            str_sub(subtitle, start = 1, end = -8)
#       ) %>% ungroup()

# Save current workspace
# save.image('110MVA/oplata.RData')

# Load workspace
load('110MVA/oplata.RData')

################################################################################
################################################################################
# Vremenska domena plot
dat %>% filter(rib==F & (point.id %in% c('pt7', 'pt21', 'pt74'))) %>%
  select(time, velocity, point.id, rib, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, end = -12)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ ., ncol = 5) +
  geom_line(color= 'black', size = 0.4) +
  coord_fixed(ratio = 0.0008) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    breaks = seq(-2000, 2000, 1000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 7, 1),
    limits = c(NA, NA)
  ) +
  ggtitle("Ukruta: NE - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/ukruta_ne_brzina_vrijeme.png',
       width = 17, height = 5, units = 'cm', dpi = 320)

dat %>% filter(rib==T & point.id %in% c('pt1', 'pt4', 'pt15', 'pt42')) %>%
  select(time, velocity, point.id, rib, subtitle) %>%
  mutate(subtitle = str_sub(subtitle, end = -11)) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ ., ncol = 5) +
  geom_line(color= 'black', size = 0.4) +
  coord_fixed(ratio = 0.002) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    breaks = seq(-2000, 2000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 7, 1),
    limits = c(NA, NA)
  ) +
  ggtitle("Ukruta: DA - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/ukruta_da_brzina_vrijeme.png',
       width = 17, height = 5, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Frekvencijska domena plot
dat %>% filter(rib == F & (point.id %in% c('pt7', 'pt21', 'pt74'))) %>%
  select(frequency, velocity.amp, point.id, rib, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, end = -12)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ ., ncol = 5) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  coord_fixed(ratio = 0.12) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
  ) +
  ggtitle("Ukruta: NE - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/ukruta_ne_brzina_freq.png',
       width = 17, height = 5, units = 'cm', dpi = 320)

dat %>% filter(rib == T & point.id %in% c('pt1', 'pt4', 'pt15', 'pt42')) %>%
  select(frequency, velocity.amp, point.id, rib, subtitle) %>%
  drop_na() %>%
  mutate(subtitle = str_sub(subtitle, end = -11)) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ ., ncol = 5) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  coord_fixed(ratio = 0.21) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    # breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
  ) +
  ggtitle("Ukruta: DA - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/ukruta_da_brzina_freq.png',
       width = 17, height = 5, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Preliminarna analiza
# Frequency table of peak.frequency
dat %>% group_by(point.id, rib) %>%
  select(point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  ungroup() %>% count(peak.frequency) %>% View()
  
# Sort by amplitudes
dat %>% group_by(rib, point.id) %>%
  select(point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  mutate(peak.velocity.amp = round(peak.velocity.amp)) %>% View()

# Fit only 100 Hz sine waves
dat_fit <- dat %>% 
  mutate(
    om_100 = 2*pi*100,
    A.100.c.term = om_100*cos(om_100*time),
    A.100.s.term = om_100*sin(om_100*time),
    ) %>%
  nest_by(rib, point.id) %>%
  mutate(fitSine = list(lm(
    velocity ~ 0 + A.100.c.term + A.100.s.term,
    data = data)))

dat_fit %>% summarise(glance(fitSine))
dat_fit %>% summarise(tidy(fitSine))

# Amplitudes and adj_r_squared
dat_model <- dat_fit %>% summarise(tidy(fitSine)) %>% group_by(rib, point.id) %>%
  select(rib, point.id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  summarise(
    A.100.v = 2*pi*100*sqrt(A.100.c.term^2 + A.100.s.term^2),
    A.100.x = sqrt(A.100.c.term^2 + A.100.s.term^2)
  ) %>% full_join(
    select(dat_fit %>% summarise(glance(fitSine)), rib, point.id, adj.r.squared),
    by = c('rib', 'point.id')
  )

dat_model %>% View()

# Summary of data with top 5 peaks and distinct frequency peaks
dat_summary <- dat %>%
  select(rib, point.id, loc.x, loc.y, velocity, peak.to.peak.vel,
         rms.vel, peak.frequency, peak.velocity.amp, frequency, d.freq,
         velocity.amp) %>%
  group_by(rib, point.id) %>%
  summarise(
    Ukruta = unique(rib), Tocka = unique(point.id),
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
  full_join(dat_model, by = c('rib', 'point.id')) %>% 
  rename(
    'Model 100 Hz amp' = A.100.v,
    'Model 100 Hz disp amp' = A.100.x,
    'Model Adj R Squared' = adj.r.squared
    ) %>%
  ungroup() %>% select(-rib, -point.id)

dat_summary %>%
  write_excel_csv2(path = '110MVA/preliminarna_obrada/oplata/oplata_summary.csv')

# Load data summary
dat_summary <- read_csv2(
  file = '110MVA/preliminarna_obrada/oplata/oplata_summary.csv'
    )

################################################################################
################################################################################
# Signifikantnost frekvencija na 25, 50, 200 i 300 Hz
#TODO: DORADITI NASLOVE OSI I SPREMITI
dat_summary %>% 
  select(rib = Ukruta, point.id = Tocka, loc.x = Sirina, loc.y = Visina,
         amp.100 = `100 Hz amp`,
         amp.25 = `25 Hz amp`,
         amp.50 = `50 Hz amp`,
         amp.200 = `200 Hz amp`,
         amp.300 = `300 Hz amp`
         ) %>%
  mutate(
    amp.25.prop = amp.25/amp.100,
    amp.50.prop = amp.50/amp.100,
    amp.200.prop = amp.200/amp.100,
    amp.300.prop = amp.300/amp.100
  ) %>% gather(amp.25.prop, amp.50.prop, amp.200.prop, amp.300.prop,
               key = 'freqs', value = 'proportion') %>%
  ggplot(aes(y = proportion)) +
  facet_wrap(factor(freqs, levels = mixedsort(unique(freqs))) ~ ., ncol = 2) +
  geom_histogram(color= 'black', bins = 10) +
  scale_x_continuous(
    name = expression('Broj točaka'),
    breaks = seq(0, 80, 5),
    limits = c(NA, NA)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    name = "Postotak amplitude na 100 Hz",
    breaks = seq(0, 1, 0.05),
    limits = c(NA, NA)
  ) +
  ggtitle("Udio visine amplituda u 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.5, 'lines')
  )

################################################################################
################################################################################
# Plotovi na oplati
# Rib border coordinates
generate.rib.borders <- function() {
  # Power transfomer plate height and width
  width.x <- 3640
  height.y <- 2800
  # Power transformer rib dimensions
  rib.flat.x <- 210
  rib.rad.x <- 5
  rib.tot.x <- rib.flat.x + 2 * rib.rad.x
  # Power transformer dimensions between ribs
  flat.x <- 350
  element.no <- 13
  rib.coords <- c()
  # Plot ribs with radius
  rib.boundaries <- cumsum(
    c(0, rib.rad.x, rib.flat.x, rib.rad.x,
      rep(c(flat.x, rib.rad.x, rib.flat.x, rib.rad.x), times=6)))
  rib.boundaries
}

rib.borders <- generate.rib.borders()

# Plot amplituda brzine - 100 Hz
dat_summary %>%
  select(rib = Ukruta, point.id = Tocka, loc.x = Sirina, loc.y = Visina,
         amp.100 = `100 Hz amp`
         ) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    rib.txt = str_sub(rib, end = 1)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_vline(xintercept = rib.borders) +
  geom_point(aes(fill = amp.100),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 2800, 250),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 3800, 250),
    limits = c(0, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 1200, 200),
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
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/amplitude_100_hz.png',
       width = 25, height = 17, units = 'cm', dpi = 320)

# Plot amplituda pomaka - 100 Hz
dat_summary %>%
  select(rib = Ukruta, point.id = Tocka, loc.x = Sirina, loc.y = Visina,
         amp.100 = `Model 100 Hz disp amp`
         ) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    rib.txt = str_sub(rib, end = 1)
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_vline(xintercept = rib.borders) +
  geom_point(aes(fill = amp.100),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 2800, 250),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 3800, 250),
    limits = c(0, NA)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m]'),
    breaks = seq(0, 2.5, 0.25),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Amplitude pomaka vibracija - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/amplitude_disp_100_hz.png',
       width = 25, height = 17, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Surface modelling
# Loading summary
dat_surf <- read_csv2(
  file = '110MVA/preliminarna_obrada/oplata/oplata_summary.csv'
) %>% select(rib = Ukruta, point.id = Tocka, x = Sirina, y = Visina,
             amp.v.100 = `Model 100 Hz amp`,
             amp.x.100 = `Model 100 Hz disp amp`,
             adj.r.sq = `Model Adj R Squared`
             )

dat_surf <- dat_surf %>% group_by(rib) %>% select(rib, x, y, amp.x.100) %>%
  mutate(
    mean.x = mean(x),
    extr.x = max(max(x) - mean.x, abs(min(x) - mean.x)),
    mean.y = mean(y),
    extr.y = max(max(y) - mean.y, abs(min(y) - mean.y))
    ) %>% ungroup()

# Coding data
dat_coded <- coded.data(
  data = dat_surf %>% filter(rib == T) %>% select(x, y, amp.x.100),
  x1 ~ (x - 1751.382) / 1855.618,
  x2 ~ (y - 1357.441) / 1420.559
  )

as.data.frame(dat_coded)

# RSM low r squared value
dat_coded.rsm <- rsm(amp.x.100 ~ FO(x1, x2), data = dat_coded)
summary(dat_coded.rsm)

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
    coords = select(., x, y),
    data = select(., !!amp, rib) 
    )
# Examining experimental variogram
var_exp <- variogram(
  formula(paste0(amp, '~rib')),
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
    min.x = min(x)*0.1,
    max.x = max(x)*1.02,
    min.y = min(y)*0.1,
    max.y = max(y)*1.03
    )
# Creating predictive grid and converting to SpatialPixels (with ribs data)
pred.grid <- expand.grid(
  x = seq(grid_limits$min.x, grid_limits$max.x, length.out = grid_num),
  y = seq(grid_limits$min.y, grid_limits$max.y, length.out = grid_num)
  )

ribs.list <- list()
for (i in 1:length(rib.borders)) {
  j <- (i-1)*4
  a <- rib.borders[j+1]
  b <- rib.borders[j+4]
  ribs.list <- append(
    ribs.list,
    Polygons(list(Polygon(cbind(c(a, b, b, a, a), c(0, 0, 2800, 2800, 0)))),
             ID = i)
    )
  if (i >= length(rib.borders)/4) {
    ribs.poly <- SpatialPolygons(ribs.list)
    # str(ribs.poly)
    # plot(ribs.poly, axes = T)
    break()
  }
}

pred.grid <- SpatialPixelsDataFrame(SpatialPoints(
  coords = pred.grid),
  data = tibble(
    rib = SpatialPoints(pred.grid) %over% ribs.poly
  ) %>% mutate(
    rib = case_when(
      rib == NA ~ 1,
      TRUE ~ 0
    )
  )
)
# pred.grid <- SpatialPixels(SpatialPoints(pred.grid))

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

# Testing parameters for interpolation
krige.inter(
      amp_df = dat_surf,
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = T,
      vgm_cutoff = 1200,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = 800,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/8,
      grid_num = 300
      )

# Creating tibble for contour and points plot - pomak
dat_disp_disp <- dat_surf %>% select(-adj.r.sq) %>%
  full_join(by = c('point.id', 'x', 'y', 'amp.x.100'),
  y = tibble(
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.x.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 1200,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/8,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100 = var1.pred) %>% select(-var1.var)
  )

# Creating tibble for contour and points plot - brzina
dat_disp_vel <- dat_surf %>% select(-adj.r.sq) %>%
  full_join(by = c('point.id', 'x', 'y', 'amp.v.100'),
  y = tibble(
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf,
      amp = 'amp.v.100',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 1200,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = 800,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/8,
      grid_num = 300
      )) 
    ) %>% rename(amp.v.100 = var1.pred) %>% select(-var1.var)
  )
  

# Plot konture amplituda pomaka - 100 Hz
dat_disp_disp %>%
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
      )
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_contour_filled(aes(z = amp.x.100.cnt), na.rm = T) +
  geom_vline(xintercept = rib.borders) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 2800, 250),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 3800, 250),
    limits = c(0, NA)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Amplitude pomaka - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/contour_disp_100_hz.png',
       width = 25, height = 17, units = 'cm', dpi = 320)

# Plot konture amplituda brzine - 100 Hz
dat_disp_vel %>%
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
      point.id != 'model' ~ str_sub(point.id, start = 3),
      TRUE ~ NA_character_
      )
    ) %>% 
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  geom_contour_filled(aes(z = amp.v.100.cnt), na.rm = T) +
  geom_vline(xintercept = rib.borders) +
  geom_text(aes(label = point.id), na.rm = T) +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 2800, 250),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 3800, 250),
    limits = c(0, NA)
  ) +
  scale_fill_viridis_d(
    name = expression('Amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Amplitude brzina - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/oplata/slike/contour_vel_100_hz.png',
       width = 25, height = 17, units = 'cm', dpi = 320)
