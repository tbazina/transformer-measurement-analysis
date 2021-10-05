
###### Usporedba rezultata impact testa na stvarnoj oplati u Koncaru ###### 

## Source functions and objects file
source('functions.R')

library(ggplot2)
library(scales)

# Filtering signal
# library(signal)

################################################################################
################################################################################
# Data input function
data.input.f <- function(dat, point.ids, dir.locs, ribs, reps) {
  ribs.logic <- sapply(
    ribs, function(x) ifelse(x=='rebro', T, F), USE.NAMES = F
    )
  for (i in 1:length(point.ids)) {
    dat <- dat %>%
      add_row(
        !!!VibData(
          point.id = point.ids[i],
          loc.x = 0,
          loc.y = 0,
          rib = ribs.logic[i],
          replication = reps[i],
          decimal.separator = ",",
          file.loc = paste0(dir.locs[i], ribs[i], '/', point.ids[i])
    )
  )
  }
  return(dat)
}

# Data input prva tocka oplata
dat.test <- VibData(
  point.id = 'cekic_200',
  loc.x = 0,
  loc.y = 0,
  rib = F,
  replication = 1,
  decimal.separator = ",",
  file.loc = '110MVA/mjerenja/damping_test/oplata/cekic200'
)

# Data input oplata
point.ids <- c(
  'cekic200_1',
  'cekic200_2',
  'cekic200_3',
  'cekic200_4',
  'cekic200_5',
  'cekic300',
  'cekic300_1',
  'siljak',
  'siljak_1',
  'siljak_2',
  'siljak_3'
)
dir.locs <- rep('110MVA/mjerenja/damping_test/', 11)
ribs <- rep('oplata', 11)
reps <- c(2, 3, 4, 5, 6, 1, 2, 1, 2, 3, 4)

# Data input rebro
point.ids <- c(
  point.ids,
  'cekic200',
  'cekic200_1',
  'cekic200_2',
  'cekic200_3',
  'cekic200_4',
  'cekic200_5',
  'cekic300',
  'cekic300_1',
  'cekic300_2',
  'cekic300_3',
  'cekic300_4',
  'cekic300_5',
  'cekic300_6',
  'siljak',
  'siljak_1',
  'siljak_2',
  'siljak_3',
  'siljak_4',
  'siljak_5'
  )
dir.locs <- c(
  dir.locs,
  rep('110MVA/mjerenja/damping_test/', 19)
)
ribs <- c(ribs, rep('rebro', 19))
reps <- c(reps, seq(6), seq(7), seq(6))

# Input all data points
dat.test <- data.input.f(
  dat = dat.test,
  point.ids = point.ids,
  dir.locs = dir.locs,
  ribs = ribs,
  reps = reps
)

# Drop unnecessary variables
dat.test <- dat.test %>% select(-loc.x, -loc.y, - acf, - pacf)

# Fix names
dat.test %>% select(point.id) %>% unique()
dat.test <- dat.test %>% mutate(
  point.id = case_when(
    point.id == 'cekic200' ~ 'cekic200',
    point.id == 'cekic_200' ~ 'cekic200',
    point.id == 'cekic200_1' ~ 'cekic200',
    point.id == 'cekic200_2' ~ 'cekic200',
    point.id == 'cekic200_3' ~ 'cekic200',
    point.id == 'cekic200_4' ~ 'cekic200',
    point.id == 'cekic200_5' ~ 'cekic200',
    point.id == 'cekic300' ~ 'cekic300',
    point.id == 'cekic300_1' ~ 'cekic300',
    point.id == 'cekic300_2' ~ 'cekic300',
    point.id == 'cekic300_3' ~ 'cekic300',
    point.id == 'cekic300_4' ~ 'cekic300',
    point.id == 'cekic300_5' ~ 'cekic300',
    point.id == 'cekic300_6' ~ 'cekic300',
    point.id == 'siljak' ~ 'siljak',
    point.id == 'siljak_1' ~ 'siljak',
    point.id == 'siljak_2' ~ 'siljak',
    point.id == 'siljak_3' ~ 'siljak',
    point.id == 'siljak_4' ~ 'siljak',
    point.id == 'siljak_5' ~ 'siljak',
    T ~ 'nije ok'
  )
)
dat.test %>% select(point.id) %>% unique()


# Check peak frequencies
dat.test %>% 
  select(point.id, rib, replication, peak.freq.orig, peak.frequency, peak.velocity.amp) %>%
  drop_na() %>% group_by(rib, point.id, replication) %>% slice(1) %>% View()

# Vremenska domena - oplata
dat.test %>% filter(rib == F & point.id == 'cekic200') %>%
  ggplot(aes(x = time, y = velocity)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-40000, 40000, 15000),
    # limits = c(-2000, 2000)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.2, 0.025),
    limits = c(0, 0.15)
  ) +
  ggtitle("Vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic200_oplata_vrijeme.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == F & point.id == 'cekic300') %>%
  ggplot(aes(x = time, y = velocity)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-40000, 40000, 5000),
    # limits = c(-2000, 2000)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.25, 0.025),
    limits = c(0, 0.25)
  ) +
  ggtitle("Vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic300_oplata_vrijeme.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == F & point.id == 'siljak') %>%
  ggplot(aes(x = time, y = velocity)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-40000, 40000, 5000),
    # limits = c(-2000, 2000)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.25, 0.025),
    limits = c(0, 0.25)
  ) +
  ggtitle("Vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/siljak_oplata_vrijeme.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

# Vremenska domena - rebra
dat.test %>% filter(rib == T & point.id == 'cekic200') %>%
  ggplot(aes(x = time, y = velocity)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-50000, 50000, 25000),
    # limits = c(-2000, 2000)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.25, 0.025),
    limits = c(0, 0.25)
  ) +
  ggtitle("Vremenska domena - rebro") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic200_rebro_vrijeme.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == T & point.id == 'cekic300') %>%
  ggplot(aes(x = time, y = velocity)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-50000, 50000, 20000),
    # limits = c(-2000, 2000)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.25, 0.025),
    limits = c(0, 0.175)
  ) +
  ggtitle("Vremenska domena - rebro") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic300_rebro_vrijeme.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == T & point.id == 'siljak') %>%
  ggplot(aes(x = time, y = velocity)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-40000, 40000, 15000),
    # limits = c(-2000, 2000)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.25, 0.025),
    limits = c(0, 0.20)
  ) +
  ggtitle("Vremenska domena - rebro") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/siljak_rebro_vrijeme.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

# FFT - oplata
dat.test %>% filter(rib == F & point.id == 'cekic200') %>%
  ggplot(aes(x = frequency, y = velocity.amp)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color = factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Amplituda brzine vibracija [um/s]',
    breaks = seq(0, 3000, 1000),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencije [Hz]",
    breaks = seq(0, 2000, 100),
    limits = c(0, 1500)
  ) +
  ggtitle("FFT - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic200_oplata_FFT.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == F & point.id == 'cekic300') %>%
  ggplot(aes(x = frequency, y = velocity.amp)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color = factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Amplituda brzine vibracija [um/s]',
    breaks = seq(0, 2000, 50),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencije [Hz]",
    breaks = seq(0, 2000, 100),
    limits = c(0, 1100)
  ) +
  ggtitle("FFT - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic300_oplata_FFT.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == F & point.id == 'siljak') %>%
  ggplot(aes(x = frequency, y = velocity.amp)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color = factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Amplituda brzine vibracija [um/s]',
    breaks = seq(0, 2000, 50),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencije [Hz]",
    breaks = seq(0, 2000, 50),
    limits = c(0, 950)
  ) +
  ggtitle("FFT - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/siljak_oplata_FFT.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

# FFT - rebra
dat.test %>% filter(rib == T & point.id == 'cekic200') %>%
  ggplot(aes(x = frequency, y = velocity.amp)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color = factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Amplituda brzine vibracija [um/s]',
    breaks = seq(0, 3000, 500),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencije [Hz]",
    breaks = seq(0, 2000, 100),
    limits = c(0, 1800)
  ) +
  ggtitle("FFT - rebra") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic200_rebra_FFT.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == T & point.id == 'cekic300') %>%
  ggplot(aes(x = frequency, y = velocity.amp)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color = factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Amplituda brzine vibracija [um/s]',
    breaks = seq(0, 3000, 500),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencije [Hz]",
    breaks = seq(0, 2000, 100),
    limits = c(0, 1800)
  ) +
  ggtitle("FFT - rebra") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/cekic300_rebra_FFT.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

dat.test %>% filter(rib == T & point.id == 'siljak') %>%
  ggplot(aes(x = frequency, y = velocity.amp)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color = factor(replication)), size = 0.4) +
  scale_y_continuous(
    name = 'Amplituda brzine vibracija [um/s]',
    breaks = seq(0, 3000, 100),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencije [Hz]",
    breaks = seq(0, 3000, 100),
    limits = c(0, 1700)
  ) +
  ggtitle("FFT - rebra") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike_stvarna_oplata/siljak_rebra_FFT.png',
       width = 21, height = 29, units = 'cm', dpi = 320)

#TODO Filter Butterworth for rib=F between 735 and 745 Hz


# Slicing data
# >= 0.2 sec
# dat.filt <- dat.test %>% filter(time >= 0.2 & time <= 0.6)
# dat.filt %>%
#   ggplot(aes(x = time, y = velocity)) + facet_grid(point.id ~ .) +
#   geom_line(color= 'black', size = 0.4) +
#   scale_y_continuous(
#     name = 'Brzina vibracija [um/s]',
#     breaks = seq(-10000, 10000, 1000),
#     limits = c(NA, NA)
#   ) +
#   scale_x_continuous(
#     name = "Vrijeme [s]",
#     breaks = seq(0, 2, 0.2),
#     limits = c(NA, NA)
#   ) +
#   ggtitle("Vremenska domena") +
#   theme(
#     axis.line = element_line(size=0.5, colour = "black"),
#     plot.title = element_text(hjust = 0.5)
#   )
# ggsave(filename = '110MVA/Testovi/slike/vremenska_domena_dio.png',
#        width = 15, height = 22, units = 'cm', dpi = 320)

# Finding peaks in filtered data
peaks.func <- function(x, npeaks=1) {
  peak.vel.ind = findpeaks(x, nups=10, ndowns=10, npeaks=npeaks, sortstr=T)[,2]
  peak.vel = x[peak.vel.ind]
  res = tibble(peak.vel, peak.vel.ind)
  res <- res %>%
    add_row(peak.vel = rep(
      NA, length.out=(length(x) - nrow(res))))
  return(res)
}

# Damping estimation function
damp.est.func <- function(x.orig) {
  x = na.omit(x.orig)
  damp = c()
  for (i in 1:length(x)-1) {
    damp = c(damp, log(x[i] / x[i+1]) / 2 / pi)
  }
  damp = c(damp, rep(NA, length.out=(length(x.orig) - length(damp))))
  return(damp)
}

# Butterworth filter critical frequency for low-pass filter
## Determine nyquist and critical frequency for filter (median value)
crit.freqs <-
  dat.test %>%
  group_by(rib, point.id, replication) %>%
  select(peak.frequency, peak.velocity.amp, d.time) %>%
  drop_na() %>% filter(peak.velocity.amp == max(peak.velocity.amp)) %>%
  mutate(nyq.freq = 1/2/d.time,
         crit.freq = peak.frequency / nyq.freq,
         ) %>% group_by(rib) %>% summarise(crit.freq.med = median(crit.freq)) %>%
  deframe()

# Filtering function
filt.func <- function(x, filt) {
  as.numeric(signal::filtfilt(filt = filt, x))
}

# Calculate damping estimate - rebra cekic200
# Necessary to identify filter order, time interval and number of peaks by hand
#TODO: izvuci histograme
dat.test %>%
  filter(rib == T & point.id == 'cekic200' & not(replication %in% c(1, 2, 5))) %>%
  group_by(rib, point.id, replication) %>%
  mutate(
    vel.filt = case_when(
      rib == T ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['TRUE'] - 1e-4, crit.freqs['TRUE'] + 1e-4
        ), type = 'pass')),
      rib == F ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['FALSE'] - 1e-4, crit.freqs['FALSE'] + 1e-4
        ), type = 'pass')),
      TRUE ~ rep(0, times = length(velocity))
    ),
    vel.filt = as.numeric(ma(vel.filt, order = 1)),
    ) %>% 
  filter(time >= 0.09) %>%
  mutate(
    peak.vel = peaks.func(vel.filt, npeaks = 205)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 205)$peak.vel.ind,
    damp = damp.est.func(peak.vel)
  ) %>% 
  select(damp) %>% drop_na() %>%
  summarise(mean = mean(damp),
            sd = sd(damp),
            median = median(damp),
            min = min(damp),
            max = max(damp)
  ) %>% mutate(
    odstupanja = scales::percent(mean / min(mean))
  )
  ggplot(aes(x = time, y = vel.filt)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time[peak.vel.ind]),
             na.rm = T, shape = 21, colour = "black", fill = "dodgerblue4",
             size = 2, stroke = 1
             ) +
  scale_y_log10(
    name = 'Brzina vibracija [um/s]',
    # breaks = seq(-1500, 1500, 500),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    # limits = c(-1500, 1500)
  ) +
  annotation_logticks() +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.5, 0.01),
    limits = c(0.09, 0.3)
  ) +
  ggtitle("FIltrirana vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Calculate damping estimate - rebra cekic300
# Necessary to identify filter order, time interval and number of peaks by hand
dat.test %>%
  filter(rib == T & point.id == 'cekic300' & not(replication %in% c(1, 6))) %>%
  group_by(rib, point.id, replication) %>%
  mutate(
    vel.filt = case_when(
      rib == T ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['TRUE'] - 1e-4, crit.freqs['TRUE'] + 1e-4
        ), type = 'pass')),
      rib == F ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['FALSE'] - 1e-4, crit.freqs['FALSE'] + 1e-4
        ), type = 'pass')),
      TRUE ~ rep(0, times = length(velocity))
    ),
    vel.filt = as.numeric(ma(vel.filt, order = 1)),
    ) %>% 
  filter(time >= 0.09) %>%
  mutate(
    peak.vel = peaks.func(vel.filt, npeaks = 205)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 205)$peak.vel.ind,
    damp = damp.est.func(peak.vel)
  ) %>%
  select(damp) %>% drop_na() %>%
  summarise(mean = mean(damp),
            sd = sd(damp),
            median = median(damp),
            min = min(damp),
            max = max(damp)
  ) %>% mutate(
    odstupanja = scales::percent(mean / min(mean))
  )
  ggplot(aes(x = time, y = vel.filt)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time[peak.vel.ind]),
             na.rm = T, shape = 21, colour = "black", fill = "dodgerblue4",
             size = 2, stroke = 1
             ) +
  scale_y_log10(
    name = 'Brzina vibracija [um/s]',
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    # limits = c(-1500, 1500)
  ) +
  annotation_logticks() +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.5, 0.02),
    limits = c(0.09, 0.3)
  ) +
  ggtitle("FIltrirana vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Calculate damping estimate - rebra siljak
# Necessary to identify filter order, time interval and number of peaks by hand
dat.test %>%
  filter(rib == T & point.id == 'siljak' & not(replication %in% c())) %>%
  group_by(rib, point.id, replication) %>%
  mutate(
    vel.filt = case_when(
      rib == T ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['TRUE'] - 1e-4, crit.freqs['TRUE'] + 1e-4
        ), type = 'pass')),
      rib == F ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['FALSE'] - 1e-4, crit.freqs['FALSE'] + 1e-4
        ), type = 'pass')),
      TRUE ~ rep(0, times = length(velocity))
    ),
    vel.filt = as.numeric(ma(vel.filt, order = 1)),
    ) %>% 
  filter(time >= 0.09) %>%
  mutate(
    peak.vel = peaks.func(vel.filt, npeaks = 155)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 155)$peak.vel.ind,
    damp = damp.est.func(peak.vel)
  ) %>%
  select(damp) %>% drop_na() %>%
  summarise(mean = mean(damp),
            sd = sd(damp),
            median = median(damp),
            min = min(damp),
            max = max(damp)
  ) %>% mutate(
    odstupanja = scales::percent(mean / min(mean))
  )
  ggplot(aes(x = time, y = vel.filt)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time[peak.vel.ind]),
             na.rm = T, shape = 21, colour = "black", fill = "dodgerblue4",
             size = 2, stroke = 1
             ) +
  scale_y_log10(
    name = 'Brzina vibracija [um/s]',
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    # limits = c(-1500, 1500)
  ) +
  annotation_logticks() +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.5, 0.02),
    limits = c(0.09, 0.25)
  ) +
  ggtitle("FIltrirana vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Calculate damping estimate - oplata cekic200
# Necessary to identify filter order, time interval and number of peaks by hand
dat.test %>%
  filter(rib == F & point.id == 'cekic200' & not(replication %in% c(3))) %>%
  group_by(rib, point.id, replication) %>%
  mutate(
    vel.filt = case_when(
      rib == T ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['TRUE'] - 1e-4, crit.freqs['TRUE'] + 1e-4
        ), type = 'pass')),
      rib == F ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['FALSE'] - 1e-4, crit.freqs['FALSE'] + 1e-4
        ), type = 'pass')),
      TRUE ~ rep(0, times = length(velocity))
    ),
    vel.filt = as.numeric(ma(vel.filt, order = 1)),
    ) %>% 
  filter(time >= 0.08) %>%
  mutate(
    peak.vel = peaks.func(vel.filt, npeaks = 88)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 88)$peak.vel.ind,
    damp = damp.est.func(peak.vel)
  ) %>%
  select(damp) %>% drop_na() %>%
  summarise(mean = mean(damp),
            sd = sd(damp),
            median = median(damp),
            min = min(damp),
            max = max(damp)
  ) %>% mutate(
    odstupanja = scales::percent(mean / min(mean))
  )
  ggplot(aes(x = time, y = vel.filt)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time[peak.vel.ind]),
             na.rm = T, shape = 21, colour = "black", fill = "dodgerblue4",
             size = 2, stroke = 1
             ) +
  scale_y_log10(
    name = 'Brzina vibracija [um/s]',
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    # limits = c(-1500, 1500)
  ) +
  annotation_logticks() +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.5, 0.02),
    limits = c(0.08, 0.20)
  ) +
  ggtitle("FIltrirana vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Calculate damping estimate - oplata cekic300
# Necessary to identify filter order, time interval and number of peaks by hand
dat.test %>%
  filter(rib == F & point.id == 'cekic300' & not(replication %in% c())) %>%
  group_by(rib, point.id, replication) %>%
  mutate(
    vel.filt = case_when(
      rib == T ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['TRUE'] - 1e-4, crit.freqs['TRUE'] + 1e-4
        ), type = 'pass')),
      rib == F ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['FALSE'] - 1e-4, crit.freqs['FALSE'] + 1e-4
        ), type = 'pass')),
      TRUE ~ rep(0, times = length(velocity))
    ),
    vel.filt = as.numeric(ma(vel.filt, order = 1)),
    ) %>% 
  filter(time >= 0.1) %>%
  mutate(
    peak.vel = peaks.func(vel.filt, npeaks = 90)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 90)$peak.vel.ind,
    damp = damp.est.func(peak.vel)
  ) %>%
  select(damp) %>% drop_na() %>%
  summarise(mean = mean(damp),
            sd = sd(damp),
            median = median(damp),
            min = min(damp),
            max = max(damp)
  ) %>% mutate(
    odstupanja = scales::percent(mean / min(mean))
  )
  ggplot(aes(x = time, y = vel.filt)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time[peak.vel.ind]),
             na.rm = T, shape = 21, colour = "black", fill = "dodgerblue4",
             size = 2, stroke = 1
             ) +
  scale_y_log10(
    name = 'Brzina vibracija [um/s]',
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    # limits = c(-1500, 1500)
  ) +
  annotation_logticks() +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.5, 0.02),
    limits = c(0.1, 0.22)
  ) +
  ggtitle("FIltrirana vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Calculate damping estimate - oplata siljak
# Necessary to identify filter order, time interval and number of peaks by hand
dat.test %>%
  filter(rib == F & point.id == 'siljak' & not(replication %in% c())) %>%
  group_by(rib, point.id, replication) %>%
  mutate(
    vel.filt = case_when(
      rib == T ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['TRUE'] - 1e-4, crit.freqs['TRUE'] + 1e-4
        ), type = 'pass')),
      rib == F ~ filt.func(velocity, signal::butter(1, c(
        crit.freqs['FALSE'] - 1e-4, crit.freqs['FALSE'] + 1e-4
        ), type = 'pass')),
      TRUE ~ rep(0, times = length(velocity))
    ),
    vel.filt = as.numeric(ma(vel.filt, order = 1)),
    ) %>% 
  filter(time >= 0.1 & time < 0.24) %>%
  mutate(
    peak.vel = peaks.func(vel.filt, npeaks = 90)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 90)$peak.vel.ind,
    damp = damp.est.func(peak.vel)
  ) %>%
  select(damp) %>% drop_na() %>%
  summarise(mean = mean(damp),
            sd = sd(damp),
            median = median(damp),
            min = min(damp),
            max = max(damp)
  ) %>% mutate(
    odstupanja = scales::percent(mean / min(mean))
  )
  ggplot(aes(x = time, y = vel.filt)) + facet_grid(replication ~ point.id) +
  geom_line(aes(color= factor(replication)), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time[peak.vel.ind]),
             na.rm = T, shape = 21, colour = "black", fill = "dodgerblue4",
             size = 2, stroke = 1
             ) +
  scale_y_log10(
    name = 'Brzina vibracija [um/s]',
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    # limits = c(-1500, 1500)
  ) +
  annotation_logticks() +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.5, 0.02),
    # limits = c(0.1, 0.22)
  ) +
  ggtitle("FIltrirana vremenska domena - oplata") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )


# Print log.dec estimations
dat.filt %>% select(log.dec) %>% drop_na() %>%
  summarise(mean = mean(log.dec),
            sd = sd(log.dec),
            odstupanja = scales::percent(mean(log.dec) / 0.02188515)
            )

# Siljak estimates
dat.siljak <- dat.test %>% filter(point.id == 'siljak' & time > 0.2 & time < 1.6) %>%
  mutate(
    vel.filt = filt.func(velocity, signal::butter(2, 0.004, type = 'low')),
    vel.filt = as.numeric(ma(vel.filt, order = 11)),
    peak.vel = peaks.func(vel.filt, npeaks = 14)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 14)$peak.vel.ind,
    log.dec = log.dec.func(peak.vel)
    )

dat.siljak %>%
  ggplot(aes(x = time, y = vel.filt)) + facet_grid(point.id ~ .) +
  geom_line(color= 'black', size = 0.4) +
  geom_point(aes(y = peak.vel, x = time[peak.vel.ind]),
             na.rm = T, shape = 21, colour = "black", fill = "dodgerblue4",
             size = 2, stroke = 1
             ) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-10000, 10000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 2, 0.1),
    limits = c(NA, NA)
  ) +
  ggtitle("Vremenska domena - Siljak") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike/vremenska_domena_filter_siljak.png',
       width = 15, height = 22, units = 'cm', dpi = 320)

# Print log.dec estimations
dat.siljak %>% select(log.dec) %>%
  filter(log.dec >= 0.01) %>%
  drop_na() %>%
  summarise(mean = mean(log.dec),
            sd = sd(log.dec)
            )
