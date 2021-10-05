
###### Usporedba rezultata impact testa ###### 

## Source functions and objects file
source('functions.R')

library(ggplot2)

# Filtering signal
# library(signal)

################################################################################
################################################################################
# Data input Impact test proba
# Input first point cekic_300
dat.test <- VibData(
  point.id = 'cekic_300',
  loc.x = 0,
  loc.y = 0,
  rib = F,
  replication = 1,
  decimal.separator = ",",
  file.loc = '110MVA/Testovi/test_cekic_300.tsv'
)

# Input second point cekic_200
dat.test <- dat.test %>%
  add_row(
    !!!VibData(
      point.id = 'cekic_200',
      loc.x = 0,
      loc.y = 0,
      rib = F,
      replication = 2,
      decimal.separator = ",",
      file.loc = '110MVA/Testovi/test_cekic_200.tsv'
    )
  )

# Input third point siljak
dat.test <- dat.test %>%
  add_row(
    !!!VibData(
      point.id = 'siljak',
      loc.x = 0,
      loc.y = 0,
      rib = F,
      replication = 3,
      decimal.separator = ",",
      file.loc = '110MVA/Testovi/test_siljak.tsv'
    )
  )


# Vremenska domena
dat.test %>% 
  ggplot(aes(x = time, y = velocity)) + facet_grid(point.id ~ .) +
  geom_line(color= 'black', size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-10000, 10000, 1000),
    limits = c(-2000, 2000)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 2, 0.2),
    limits = c(0, NA)
  ) +
  ggtitle("Vremenska domena") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike/vremenska_domena.png',
       width = 15, height = 22, units = 'cm', dpi = 320)


# FFT
dat.test %>% 
  ggplot(aes(x = frequency, y = velocity.amp)) + facet_grid(point.id ~ .) +
  geom_line(color= 'black', size = 0.4) +
  scale_y_continuous(
    name = 'Amplituda brzine vibracija [um/s]',
    breaks = seq(0, 2000, 250),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencije [Hz]",
    breaks = seq(0, 1000, 10),
    limits = c(0, 100)
  ) +
  ggtitle("FFT") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike/FFT_dio.png',
       width = 15, height = 22, units = 'cm', dpi = 320)

# Slicing data
# >= 0.2 sec
dat.filt <- dat.test %>% filter(time >= 0.2 & time <= 0.6)
dat.filt %>%
  ggplot(aes(x = time, y = velocity)) + facet_grid(point.id ~ .) +
  geom_line(color= 'black', size = 0.4) +
  scale_y_continuous(
    name = 'Brzina vibracija [um/s]',
    breaks = seq(-10000, 10000, 1000),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 2, 0.2),
    limits = c(NA, NA)
  ) +
  ggtitle("Vremenska domena") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike/vremenska_domena_dio.png',
       width = 15, height = 22, units = 'cm', dpi = 320)

# Filtering data
filt.func <- function(x, filt) {
  as.numeric(signal::filter(filt = filt, x))
}

# Finding peaks in filtered data
peaks.func <- function(x, npeaks=1) {
  peak.vel.ind = findpeaks(x, nups=20, ndowns=20, npeaks=npeaks, sortstr=T)[,2]
  peak.vel = x[peak.vel.ind]
  res = tibble(peak.vel, peak.vel.ind)
  res <- res %>%
    add_row(peak.vel = rep(
      NA, length.out=(length(x) - nrow(res))))
  return(res)
}

# calculating log decrement
log.dec.func <- function(x.orig) {
  x = na.omit(x.orig)
  log.dec = c()
  for (i in 1:length(x)-1) {
    log.dec = c(log.dec, log(x[i] / x[i+1]) / 2 / pi)
  }
  log.dec = c(log.dec, rep(NA, length.out=(length(x.orig) - length(log.dec))))
  return(log.dec)
}

# Butterworth filter critical frequency for low-pass filter
## Determine nyquist and critical frequency for filter
dat.test %>%
  group_by(point.id) %>%
  select(peak.frequency, peak.velocity.amp, d.time) %>%
  drop_na() %>% filter(peak.velocity.amp == max(peak.velocity.amp)) %>%
  mutate(nyq.freq = 1/2/d.time,
         crit.freq = peak.frequency / nyq.freq,
         )

# Use filter order and critical frequency
dat.filt <- dat.filt %>% group_by(point.id) %>%
  mutate(
    vel.filt = filt.func(velocity, signal::butter(1, 0.004, type = 'low')),
    vel.filt = as.numeric(ma(vel.filt, order = 9)),
    peak.vel = peaks.func(vel.filt, npeaks = 4)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = 4)$peak.vel.ind,
    log.dec = log.dec.func(peak.vel)
    )
dat.filt %>%
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
  ggtitle("Vremenska domena - Filtrirano") +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = '110MVA/Testovi/slike/vremenska_domena_filter.png',
       width = 15, height = 22, units = 'cm', dpi = 320)

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
