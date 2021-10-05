################################################################################
########### Analiza mjerenja na stegama i jezgri 110MVA 11.07.2020. ############
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

################################################################################
################################################################################

# Transformatora radi u kratkom spoju - 70 % nazivnog opterećenja.
# Mjerenja su provedena na 2 stege kvadratnog oblika + 4 dodatne točke na jezgri.
# Koordinate točaka su zapisane kao udaljenost od donjeg lijevog kuta.
# Na scakoj stegi je raspoređeno 6 mjernih točaka.

# Data for loop input - KOORDINATE NEKIH TOČAKA PROMIJENJENE (STVARNE KORDINATE)
input.df <- tibble(
  stega.id = rep(c(1, 2), each = 6),
  !!!tibble::tribble(
    ~point.id, ~loc.x, ~loc.y,
          1,  580, 328,
          2,  780, 328,
          3, 2117, 328,
          4, 2317, 328,
          5, 3564, 328,
          6, 3854, 328,
          12,  580, 310,
          11,  800, 343,
          10, 2117, 332,
           9, 2277, 332,
           8, 3564, 336,
           7, 3854, 332
  )
)

input.df %>% rename(Namotaj=stega.id, Tocka=point.id, x=loc.x, y=loc.y) %>%
  write_excel_csv2('110MVA/DoE/Aktivni_dio/stege_stvarne_koordinate.csv')

# Data input function
data.input.f <- function(dat, input.df) {
  # Folder u kojem se nalaze mjerenja
  stega.path <- '110MVA/mjerenja/stege/'
  for (i in 1:length(input.df$point.id)) {
    dat <- dat %>%
      add_row(
        !!!VibData(
          point.id = paste0('pt', input.df$point.id[i]),
          loc.x = input.df$loc.x[i],
          loc.y = input.df$loc.y[i],
          rib = F,
          replication = input.df$stega.id[i],
          decimal.separator = ",",
          file.loc = paste0(
            stega.path, 's', input.df$point.id[i]
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
dat_stege <- VibData(
  point.id = 'pt1',
  loc.x = 580,
  loc.y = 328,
  rib = F,
  replication = 1,
  decimal.separator = ',',
  file.loc = paste0('110MVA/mjerenja/stege/s1')
)

# Remove input data for first point (s1)
input.df <- input.df %>% filter(not(stega.id == 1 & point.id == 1))

# Input the rest of data points for stege
dat_stege <- data.input.f(dat_stege, input.df)

# Input data points for jezgra
dat_stege <- dat_stege %>%
  add_row(
    !!!VibData(
      point.id = 'pt13',
      loc.x = -1,
      loc.y = 0,
      rib = F,
      replication = 3,
      decimal.separator = ",",
      file.loc = '110MVA/mjerenja/jezgra/j1_lijevo_lijevo'
      )
    )

dat_stege <- dat_stege %>%
  add_row(
    !!!VibData(
      point.id = 'pt14',
      loc.x = +1,
      loc.y = 0,
      rib = F,
      replication = 3,
      decimal.separator = ",",
      file.loc = '110MVA/mjerenja/jezgra/j2_lijevo_desno'
      )
    )

dat_stege <- dat_stege %>%
  add_row(
    !!!VibData(
      point.id = 'pt15',
      loc.x = -1,
      loc.y = 0,
      rib = F,
      replication = 4,
      decimal.separator = ",",
      file.loc = '110MVA/mjerenja/jezgra/j3_desno_lijevo'
      )
    )

dat_stege <- dat_stege %>%
  add_row(
    !!!VibData(
      point.id = 'pt16',
      loc.x = +1,
      loc.y = 0,
      rib = F,
      replication = 4,
      decimal.separator = ",",
      file.loc = '110MVA/mjerenja/jezgra/j4_desno_desno'
      )
    )

# Change replication to stega
dat_stege <- dat_stege %>% rename(stega = replication)

# Select only necessary variables
dat_stege <- dat_stege %>% select(-rib, -pacf, -displacement.orig)

# Recalculate FFT resolution because of sample length change
# dat_stege <- dat_stege %>% group_by(stega, point.id) %>%
#   mutate(d.freq = sample.rate/length(na.omit(velocity.amp))/2) %>% ungroup()

# Sanity data check
dat_stege %>% glimpse()
dat_stege %>% select(range, d.time, d.freq, sample.rate, sample.num) %>% distinct() %>% View()
dat_stege %>% group_by(stega, point.id) %>% summarise(
  length = length(velocity),
  min = min(velocity),
  max = max(velocity),
  mean = mean(velocity),
  median = median(velocity),
  )
dat_stege %>%
  select(stega, point.id, peak.freq.orig, peak.vel.orig, peak.frequency, peak.velocity.amp) %>%
  drop_na() %>% group_by(stega, point.id) %>% slice(1) %>%
  mutate(
    equal = if_else(
      (peak.freq.orig <= (peak.frequency + 0.000001)) & (peak.freq.orig >= (peak.frequency - 0.000001)), T, F)) %>% View()

# Remove unnecessary variables
dat_stege <- dat_stege %>% select(-peak.freq.orig, -peak.vel.orig, -peak.disp.orig,
                      -peak.acc.orig, -velocity.orig)

# Differentiate stege from jezgra
dat_stege <- dat_stege %>% mutate(
  stega = case_when(
    stega == 1 ~ 'Gornja',
    stega == 2 ~ 'Donja',
    stega == 3 ~ 'Lijevo',
    stega == 4 ~ 'Desno',
    TRUE ~ 'Nije ok'
  )
)

# Create subtitles for plots
dat_stege <- dat_stege %>% group_by(stega, point.id) %>%
  mutate(
    subtitle = case_when(
      stega %in% c('Donja', 'Gornja') ~ paste0(
        'Stega: ', stega, ' ', point.id, ': ', '(', loc.x, ', ', loc.y, ')'),
      stega %in% c('Lijevo', 'Desno') ~ paste0(
        'Jezgra: ', stega, ' ', point.id, ': ', '(', loc.x, ', ', loc.y, ')'),
      TRUE ~ 'Nije ok'
      )
    ) %>% ungroup()

# Save current workspace
save.image('110MVA/stege.RData')

# Load workspace
load('110MVA/stege.RData')

################################################################################
################################################################################
# Vremenska domena plot
dat_stege %>%
  select(time, velocity, point.id, stega, subtitle) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_line(color= 'black', size = 0.4) +
  scale_y_continuous(
    name = expression('Brzina vibracija ['*mu*'m/s]'),
    breaks = seq(-1000, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 7, 1),
    limits = c(NA, NA)
  ) +
  ggtitle("Stege i Jezgra - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/stege_brzina_vrijeme.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Frekvencijska domena plot
dat_stege %>%
  select(frequency, velocity.amp, point.id, stega, subtitle) %>%
  drop_na() %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ .) +
  geom_col(color= 'black', fill='black', width=0.6, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine vibracija ['*mu*'m/s]'),
    breaks = seq(0, 1000, 200),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 300, 50),
    limits = c(24, 301)
  ) +
  ggtitle("Stege i Jezgra - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/stege_brzina_freq.png',
       width = 25, height = 15, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Preliminarna analiza
# Frequency table of peak.frequency
dat_stege %>% group_by(stega, point.id) %>%
  select(stega, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  ungroup() %>% count(peak.frequency) %>% View()
  
# Sort by amplitudes
dat_stege %>% group_by(stega, point.id) %>%
  select(stega, point.id, peak.frequency, peak.velocity.amp) %>% drop_na() %>%
  mutate(peak.velocity.amp = round(peak.velocity.amp)) %>% View()

# Fit only 50 Hz and 100 Hz sine waves
dat_stege_fit <- dat_stege %>% group_by(stega, point.id) %>%
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

glance(dat_stege_fit, fitSine)

# Amplitudes and adj_r_squared
dat_stege_model <- tidy(dat_stege_fit, fitSine) %>% group_by(stega, point.id) %>%
  select(stega, point.id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  summarise(
    A.50.v = 2*pi*50*sqrt(A.50.c.term^2 + A.50.s.term^2),
    A.100.v = 2*pi*100*sqrt(A.100.c.term^2 + A.100.s.term^2),
    A.50.x = sqrt(A.50.c.term^2 + A.50.s.term^2),
    A.100.x = sqrt(A.100.c.term^2 + A.100.s.term^2)
  ) %>% full_join(
    select(glance(dat_stege_fit, fitSine), stega, point.id, adj.r.squared), by = c('stega', 'point.id')
  )

dat_stege_model

# Summary of data with top 5 peaks and distinct frequency peaks
dat_stege_summary <- dat_stege %>%
  select(stega, point.id, loc.x, loc.y, velocity, peak.to.peak.vel,
         rms.vel, peak.frequency, peak.velocity.amp, frequency, d.freq,
         velocity.amp) %>%
  group_by(stega, point.id) %>%
  summarise(
    Stega = unique(case_when(
      stega %in% c('Gornja', 'Donja') ~ paste0('Stega ', stega),
      stega %in% c('Lijevo', 'Desno') ~ paste0('Jezgra ', stega),
      TRUE ~ 'Nije ok'
    )),
    Tocka = unique(point.id),
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
  full_join(dat_stege_model, by = c('stega', 'point.id')) %>% 
  rename(
    'Model 50 Hz amp' = A.50.v,
    'Model 100 Hz amp' = A.100.v,
    'Model 50 Hz disp amp' = A.50.x,
    'Model 100 Hz disp amp' = A.100.x,
    'Model Adj R Squared' = adj.r.squared
    ) %>%
  ungroup() %>% select(-stega, -point.id)

dat_stege_summary %>%
  write_excel_csv2(path = '110MVA/preliminarna_obrada/aktivni_dio/stege_summary.csv')

# Plot amplituda brzine Stege - 100 Hz
dat_stege_summary %>% filter(Stega %in% c('Stega Gornja', 'Stega Donja')) %>%
  select(stega = Stega, point.id = Tocka, loc.x = Sirina, loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    stega = factor(stega, levels = mixedsort(unique(stega), decreasing = T))
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(~ stega, nrow = 2, scales = 'free') +
  geom_point(aes(fill = amp.100),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 200),
    limits = c(0, 588)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 5000, 250),
    limits = c(0, 4434)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    # breaks = seq(0, 5000, 500),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Stege i Jezgra - Amplitude brzine vibracija - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/stege_amplitude_100_hz.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

# Plot amplituda brzine Jezgra - 100 Hz
dat_stege_summary %>% filter(Stega %in% c('Jezgra Lijevo', 'Jezgra Desno')) %>%
  select(stega = Stega, point.id = Tocka, loc.x = Sirina, loc.y = Visina, amp.100 = `100 Hz amp`) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    stega = factor(stega, levels = mixedsort(unique(stega), decreasing = T))
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(~ stega, ncol = 2, scales = 'fixed') +
  geom_point(aes(fill = amp.100),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  geom_vline(xintercept = 0) +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    # breaks = seq(0, 700, 200),
    # limits = c(0, 588)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-2, 2, 1),
    limits = c(-2, 2)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    # breaks = seq(0, 5000, 500),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  # ggtitle("Stege i Jezgra - Amplitude brzine vibracija - 100 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/jezgra_amplitude_100_hz.png',
       width = 25, height = 4, units = 'cm', dpi = 320)

# Plot amplituda brzine Stege - 50 Hz
dat_stege_summary %>% filter(Stega %in% c('Stega Gornja', 'Stega Donja')) %>%
  select(stega = Stega, point.id = Tocka, loc.x = Sirina, loc.y = Visina, amp.50 = `50 Hz amp`) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    stega = factor(stega, levels = mixedsort(unique(stega), decreasing = T))
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(~ stega, nrow = 2, scales = 'free') +
  geom_point(aes(fill = amp.50),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 200),
    limits = c(0, 588)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 5000, 250),
    limits = c(0, 4434)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    # breaks = seq(0, 5000, 500),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  ggtitle("Stege i Jezgra - Amplitude brzine vibracija - 50 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/stege_amplitude_50_hz.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

# Plot amplituda brzine Jezgra - 50 Hz
dat_stege_summary %>% filter(Stega %in% c('Jezgra Lijevo', 'Jezgra Desno')) %>%
  select(stega = Stega, point.id = Tocka, loc.x = Sirina, loc.y = Visina, amp.50 = `50 Hz amp`) %>%
  mutate(
    point.id = str_sub(point.id, start = 3),
    stega = factor(stega, levels = mixedsort(unique(stega), decreasing = T))
    ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(~ stega, ncol = 2, scales = 'fixed') +
  geom_point(aes(fill = amp.50),
             shape = 21, colour = "black",
             stroke = 1, size = 7,
             ) +
  geom_text() +
  geom_vline(xintercept = 0) +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    # breaks = seq(0, 700, 200),
    # limits = c(0, 588)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-2, 2, 1),
    limits = c(-2, 2)
  ) +
  scale_fill_viridis_c(
    name = expression('Amplituda ['*mu*'m/s]'),
    # breaks = seq(0, 5000, 500),
    option = 'C',
    alpha = 1,
    begin = 0.3,
  ) +
  # ggtitle("Stege i Jezgra - Amplitude brzine vibracija - 50 Hz") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/aktivni_dio/slike/jezgra_amplitude_50_hz.png',
       width = 25, height = 4, units = 'cm', dpi = 320)
