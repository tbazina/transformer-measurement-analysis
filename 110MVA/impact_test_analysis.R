################################################################################
############# Analiza udarnog testa na oplati 110MVA 18.07.2020. ###############
################################################################################

# Source functions and objects file
source('functions.R')

# Plotting
library(ggplot2)
# Color palette
library(ggsci)
# Graphical scales
library(scales)
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

# Finding peaks in filtered data
peaks.func <- function(x.orig, npeaks=1, min.value=0) {
  x = na.omit(x.orig)
  peak.vel.ind = findpeaks(
    x, nups=10, ndowns=10, npeaks=npeaks, threshold = min.value,
    minpeakdistance = 150, sortstr=T)[,2]
  peak.vel = x[peak.vel.ind]
  peak.vel.ind = peak.vel.ind[peak.vel >= min.value]
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
    damp = c(damp, log(x[i] / x[i+1]) / sqrt(log(x[i] / x[i+1])^2 +  4 * pi^2))
  }
  damp = c(damp, rep(NA, length.out=(length(x.orig) - length(damp))))
  return(damp)
}

# Filtering function
filt.func <- function(x, filt) {
  as.numeric(signal::filtfilt(filt = filt, x))
}

################################################################################
################################################################################

# Transformator miruje.
# Mjerenja su provedena na oplati. Odabran je pravokutni dio 3600 × 2800
# Na oplati je raspoređeno ukupno 10 mjernih točaka, pola na rebrima.


# Data for loop input - KOORDINATE NEKIH TOČAKA PROMIJENJENE (STVARNE KORDINATE)
# input.df <- tibble::tribble(
#   ~point.id,         ~loc.x,           ~loc.y, ~Ukruta,
#        9L,             405L,            1557L,    "NE",
#       10L,             424L,             589L,    "NE",
#       36L,            1620L,             943L,    "NE",
#       68L,            3222L,            1697L,    "NE",
#       73L,            3296L,             224L,    "NE",
#        1L,             129L,             323L,    "DA",
#        5L,             150L,            1781L,    "DA",
#       40L,            1795L,            1108L,    "DA",
#       78L,            3533L,             523L,    "DA",
#       80L,            3532L,            1791L,    "DA"
#   ) %>% rename(rib=Ukruta) %>% mutate(
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
#   write_excel_csv2('110MVA/DoE/Oplata_i_prigusenje/impact_stvarne_koordinate.csv')

# Data input function
# data.input.f <- function(dat, input.df) {
#   for (i in 1:length(input.df$point.id)) {
#     print(paste0('pt', input.df$point.id[i]))
#     print(input.df$loc.x[i])
#     print(input.df$loc.y[i])
#     print(input.df$rib[i])
#     for (j in 1:4) {
#       if (input.df$rib[i]) {
#         # Folder u kojem se nalaze mjerenja
#         impact.path <- paste0(
#           '110MVA/mjerenja/impact/rebra/r_', input.df$point.id[i], '_', j
#           )
#       }
#       else {
#         # Folder u kojem se nalaze mjerenja
#         impact.path <- paste0(
#           '110MVA/mjerenja/impact/oplata/o_', input.df$point.id[i], '_', j
#           )
#       }
#       print(impact.path)
#       dat <- dat %>%
#         add_row(
#           !!!VibData(
#             point.id = paste0('pt', input.df$point.id[i]),
#             loc.x = input.df$loc.x[i],
#             loc.y = input.df$loc.y[i],
#             rib = input.df$rib[i],
#             replication = j,
#             decimal.separator = ",",
#             file.loc = impact.path
#       )
#     )
#     }
#     print('#######################')
#     }
#   return(dat)
# }

################################################################################
################################################################################
# Data input pt9 replication 1
# dat <- VibData(
#   point.id = 'pt9',
#   loc.x = 405,
#   loc.y = 1557,
#   rib = F,
#   replication = 1,
#   decimal.separator = ',',
#   file.loc = paste0('110MVA/mjerenja/impact/oplata/o_9_1')
# )

# Data input pt9 replication 1
# dat <- dat %>%
#   add_row(
#     !!!VibData(
#       point.id = 'pt9',
#       loc.x = 405,
#       loc.y = 1557,
#       rib = F,
#       replication = 2,
#       decimal.separator = ',',
#       file.loc = paste0('110MVA/mjerenja/impact/oplata/o_9_2')
# ))

# Data input pt9 replication 1
# dat <- dat %>%
#   add_row(
#     !!!VibData(
#       point.id = 'pt9',
#       loc.x = 405,
#       loc.y = 1557,
#       rib = F,
#       replication = 3,
#       decimal.separator = ',',
#       file.loc = paste0('110MVA/mjerenja/impact/oplata/o_9_3')
# ))

# Data input pt9 replication 1
# dat <- dat %>%
#   add_row(
#     !!!VibData(
#       point.id = 'pt9',
#       loc.x = 405,
#       loc.y = 1557,
#       rib = F,
#       replication = 4,
#       decimal.separator = ',',
#       file.loc = paste0('110MVA/mjerenja/impact/oplata/o_9_4')
# ))

# Remove input data for first point (pt9)
# input.df <- input.df %>% filter(not(point.id == 9))

# Input the rest of data points
# dat <- data.input.f(dat, input.df)

# Select only necessary variables
# dat <- dat %>% select(-pacf, -displacement.orig)

# Recalculate FFT resolution because of sample length change
# dat <- dat %>% group_by(point.id) %>%
#   mutate(d.freq = sample.rate/length(na.omit(velocity.amp))/2) %>% ungroup()

# Sanity data check
# dat %>% glimpse()
# dat %>% select(point.id, range, d.time, d.freq, sample.rate, sample.num) %>% distinct() %>% View()
# dat %>% group_by(point.id, replication) %>% summarise(
#   length = length(velocity),
#   min = min(velocity),
#   max = max(velocity),
#   mean = mean(velocity),
#   median = median(velocity),
#   volt.max = max(voltage),
#   volt.min = min(voltage)
#   ) %>% arrange(as.numeric(str_sub(point.id, 3))) %>% View()
# dat %>%
#   select(point.id, replication, peak.freq.orig, peak.vel.orig, peak.frequency, peak.velocity.amp) %>%
#   drop_na() %>% group_by(point.id, replication) %>% slice(1) %>%
#   mutate(
#     duplo = peak.velocity.amp * 2,
#     razlika = peak.vel.orig - peak.velocity.amp * 2,
#     equal = if_else(
#       (peak.freq.orig <= (peak.frequency + 0.000001)) & (peak.freq.orig >= (peak.frequency - 0.000001)), T, F)) %>% View()

# Remove unnecessary variables
# dat <- dat %>% select(-peak.freq.orig, -peak.vel.orig, -peak.disp.orig,
#                       -peak.acc.orig, -velocity.orig)

# Check subtitles
# dat %>% group_by(point.id, replication) %>% select(subtitle) %>% distinct() %>% View()

# Save current workspace
# save.image('110MVA/impact_test.RData')

# Load workspace
load('110MVA/impact_test.RData')

################################################################################
################################################################################
# Vremenska domena plot
dat %>% filter(rib==F) %>%
  filter(time >= 0.15 & time <= 0.9) %>%
  select(time, velocity, point.id, rib, subtitle, loc.x, loc.y, replication) %>%
  mutate(
    subtitle = str_replace(subtitle, "rib: FALSE ", ""),
    point.id = paste0(point.id, ' (', loc.x, ', ', loc.y, ')'),
    replication.txt = paste0('rep: ', replication)
    ) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_grid(factor(point.id, levels = mixedsort(unique(point.id))) ~ replication.txt) +
  # facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ ., ncol = 4) +
  geom_line(aes(color = point.id), size = 0.4) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    breaks = seq(-3000, 3000, 2000),
    limits = c(NA, NA)
  ) +
  scale_color_nejm(
    name = NULL
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 2, 0.1),
    limits = c(NA, NA)
  ) +
  ggtitle("Udarni test na neukrućenim dijelovima - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines'),
    legend.position = 'none'
  )
ggsave(filename = '110MVA/preliminarna_obrada/impact/slike/ukruta_ne_brzina_vrijeme.png',
       width = 25, height = 17, units = 'cm', dpi = 320)

dat %>% filter(rib==T) %>%
  filter(time >= 0.15 & time <= 0.9) %>%
  select(time, velocity, point.id, rib, subtitle, loc.x, loc.y, replication) %>%
  mutate(
    subtitle = str_replace(subtitle, "rib: TRUE ", ""),
    point.id = paste0(point.id, ' (', loc.x, ', ', loc.y, ')'),
    replication.txt = paste0('rep: ', replication)
    ) %>%
  ggplot(aes(x = time, y = velocity)) +
  facet_grid(factor(point.id, levels = mixedsort(unique(point.id))) ~ replication.txt) +
  # facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ ., ncol = 4) +
  geom_line(aes(color = point.id), size = 0.4) +
  scale_y_continuous(
    name = expression('Brzina ['*mu*'m/s]'),
    breaks = seq(-3000, 3000, 2000),
    limits = c(NA, NA)
  ) +
  scale_color_nejm(
    name = NULL
  ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 2, 0.1),
    limits = c(NA, NA)
  ) +
  ggtitle("Udarni test na ukrućenim dijelovima - Vremenska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines'),
    legend.position = 'none'
  )
ggsave(filename = '110MVA/preliminarna_obrada/impact/slike/ukruta_da_brzina_vrijeme.png',
       width = 25, height = 17, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Frekvencijska domena plot
dat %>% filter(rib == T) %>%
  filter(frequency >= 90 & frequency <= 110) %>%
  select(frequency, velocity.amp, point.id, rib, subtitle) %>%
  drop_na() %>%
  mutate(
    subtitle = str_replace(subtitle, "rib: FALSE ", "")
    ) %>%
  ggplot(aes(x = frequency, y = velocity.amp)) +
  facet_wrap(factor(subtitle, levels = mixedsort(unique(subtitle))) ~ ., ncol = 4) +
  geom_col(color= 'black', fill='black', width=0.5, size = 0.6) +
  scale_y_continuous(
    name = expression('Amplituda brzine vibracija ['*mu*'m/s]'),
    # breaks = seq(0, 1000, 500),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = "Frekvencija [Hz]",
    breaks = seq(0, 1000, 100),
    # limits = c(24, 301)
  ) +
  ggtitle("Udarni test - Ukruta: NE - Frekvencijska domena") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/impact/slike/ukruta_ne_brzina_freq.png',
       width = 19, height = 25, units = 'cm', dpi = 320)

################################################################################
################################################################################
# Sanity data check
dat %>% group_by(rib, point.id, replication) %>%
  select(d.time, time, sample.rate, sample.num) %>%
  summarise(
    n = n(),
    d.time = unique(d.time),
    sample.rate = unique(sample.rate),
    max.time = max(time),
    n.div.sample.rate = (n()-1)/unique(sample.rate),
    n.mul.d.time = (n()-1) * unique(d.time)
  ) %>% View()

# Butterworth filter critical frequency for band-pass filter
## Determine nyquist and critical frequency for filter (100 Hz)
dat <- dat %>%
  group_by(rib, point.id, replication) %>%
  mutate(nyq.freq = 1/2/d.time,
         crit.freq = 100 / nyq.freq,
         ) %>% ungroup()

# Filtering 100 Hz frequency
# Necessary to identify filter order, time interval and number of peaks by hand
#TODO: izvuci histograme
crit.freq.err <- 5e-5
ma.order <- 1
dat <- dat %>%
  group_by(rib, point.id, replication) %>%
  mutate(
    vel.filt = filt.func(
      velocity, signal::butter(1, c(unique(crit.freq)-crit.freq.err,
                                    unique(crit.freq)+crit.freq.err),
                               type = 'pass')),
    vel.filt = as.numeric(ma(vel.filt, order = ma.order))
    ) %>% ungroup()
  

# Calculate damping estimate - rebra
npeaks <- 300
# times for ribs == T
# cutoff.time.min <- 0.2
# cutoff.time.max <- 0.75
cutoff.time.min <- 0.2
cutoff.time.max <- 0.9
dat_fit <- dat %>%
  filter(!(point.id == 'pt5' & replication == 2) &
           !(point.id == 'pt40' & replication == 1) &
           !(point.id == 'pt80' & replication == 4) &
           !(point.id == 'pt68' & replication == 4)
         ) %>%
  group_by(rib, point.id, replication) %>%
  filter(time >= cutoff.time.min & time <= cutoff.time.max) %>%
  mutate(
    peak.vel = peaks.func(vel.filt, npeaks = npeaks, min.value = 5)$peak.vel,
    peak.vel.ind = peaks.func(vel.filt, npeaks = npeaks)$peak.vel.ind,
    damp = damp.est.func(peak.vel),
    time.fit = time[peak.vel.ind]
  ) %>% ungroup() %>%
  nest_by(rib, point.id, replication) %>%
  mutate(
    fit.log = list(lm(log(peak.vel) ~ time.fit, data = data)),
    fit.res = list(exp(predict(
      fit.log, newdata = data %>% mutate(time.fit = time)))),
    log.dec = list(tidy(fit.log) %>%
                     slice(2) %>%
                     mutate(estimate = -estimate/100) %>%
                     pull(estimate)),
    adj.r.squared = list(glance(fit.log)$adj.r.squared),
    rss = list(glance(fit.log)$deviance)
    )

# dat_fit
# dat_fit %>% summarise(deviance(fit.log))

# Summary faktora prigusenja po replikacijama
dat_fit_rep <- dat_fit %>% select(-fit.log, fit.res) %>%
  unnest(cols = c('data', 'log.dec', 'adj.r.squared', 'rss')) %>%
  select(rib, point.id, replication, peak.vel, damp, log.dec, adj.r.squared, rss) %>%
  drop_na() %>%
  summarise(
    .groups = 'drop_last',
    rep.damp = mean(damp),
    rep.damp.fit = unique(log.dec) / sqrt(4*pi^2 + unique(log.dec)^2),
    rep.damp.sd = sd(damp),
    rep.damp.min = min(damp),
    rep.damp.max = max(damp),
    sd.ratio = scales::percent(rep.damp.sd / rep.damp),
    fit.r.sq = unique(adj.r.squared),
    num.obs.rep = n()
  ) %>%
  mutate(
    rep.normalized = scales::percent(
      rep.damp / weighted.mean(rep.damp, w = num.obs.rep) - 1)
    ) 

dat_fit_rep %>%
  mutate(
    point.id = str_sub(point.id, start = 3)
  ) %>%
  rename(Tocka = point.id, Replikacija = replication, Rebro = rib,
         'Broj peakova' = num.obs.rep) %>%
  write_excel_csv2(path = '110MVA/preliminarna_obrada/impact/damp_replication_summary.csv')
  
# Summary faktora prigusenja po tockama
dat_fit_pt <- dat_fit_rep %>%
  summarise(
    .groups = 'drop_last',
    point.damp = weighted.mean(rep.damp, w = num.obs.rep),
    point.damp.fit = weighted.mean(rep.damp.fit, w = num.obs.rep),
    point.damp.min = min(rep.damp),
    point.damp.max = max(rep.damp),
    num.obs.point = n()
  ) %>%
  mutate(
    point.normalized = scales::percent(
      point.damp / weighted.mean(point.damp, w = num.obs.point) - 1)
    ) 

dat_fit_pt %>%
  mutate(
    point.id = str_sub(point.id, start = 3)
  ) %>%
  rename(Tocka = point.id, Rebro = rib, 'Broj replikacija' = num.obs.point) %>%
  write_excel_csv2(path = '110MVA/preliminarna_obrada/impact/damp_point_summary.csv')

# Summary faktora prigusenja po elementima (rebra, oplata)
dat_fit_glob <- dat_fit_pt %>%
  summarise(
    .groups = 'drop_last',
    glob.damp = weighted.mean(point.damp, w = num.obs.point),
    glob.damp.fit = weighted.mean(point.damp.fit, w = num.obs.point),
    glob.damp.min = min(point.damp),
    glob.damp.max = max(point.damp),
    num.obs.glob = n()
  ) %>%
  mutate(
    glob.normalized = scales::percent(
      glob.damp / weighted.mean(glob.damp, w = num.obs.glob) - 1)
    ) 

dat_fit_glob %>%
  rename(Rebro = rib, 'Broj tocaka' = num.obs.glob) %>%
  write_excel_csv2(path = '110MVA/preliminarna_obrada/impact/damp_glob_summary.csv')

# Damping plot - oplata
dat_fit %>% select(rib, point.id, replication, data, fit.res) %>%
  filter(rib == F) %>%
  unnest(cols = c('data', 'fit.res')) %>%
  select(
    rib, point.id, replication, loc.x, loc.y, time, time.fit, vel.filt, peak.vel,
    peak.vel.ind, fit.res
    ) %>%
  # filter(time >= cutoff.time.min & time <= cutoff.time.max) %>%
  mutate(
    point.id = paste0(point.id, ' (', loc.x, ', ', loc.y, ')'),
    replication.txt = paste0('rep: ', replication)
    ) %>%
  ggplot(aes(x = time, y = vel.filt)) +
  facet_grid(factor(point.id, levels = mixedsort(unique(point.id))) ~ replication.txt) +
  geom_line(aes(color = point.id), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time.fit, fill = 'Vršne vrijednosti'),
             na.rm = T, shape = 21, colour = "black",
             size = 1.5, stroke = 0.5, 
             ) +
  geom_line(aes(y = fit.res, color = 'Eksponencijalno prigušenje'),
            linetype = 'longdash', size = 0.9, lineend = 'round') +
  geom_hline(yintercept = 5) + 
  scale_color_nejm(
    name = NULL
  ) +
  scale_fill_manual(
    name = NULL,
    values = c('Vršne vrijednosti' = 'dodgerblue4')
  ) +
  scale_y_log10(
    name = expression('Brzina ['*mu*'m/s]'),
    breaks = c(1e2, 1e1, 1e-1, 1e-3),
    # breaks = breaks_log(n=5),
    labels = label_math(format = log10)
    # breaks = trans_breaks("log10", function(x) 10^x),
    # labels = trans_format("log10", math_format(10^.x))
  ) +
  annotation_logticks(sides = 'l') +
  # scale_y_continuous(
  #   name = expression('Brzina vibracija ['*mu*'m/s]'),
  #   breaks = seq(-200, 200, 20),
  #   # limits = c(-1500, 1500)
  # ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.9, 0.1),
    # limits = c(0.09, 0.3)
  ) +
  ggtitle("Prigušenje na 100 Hz - neukrućeni dijelovi") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines'),
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/impact/slike/ukruta_ne_damp_100_hz.png',
       width = 25, height = 17, units = 'cm', dpi = 320)


# Damping plot - rebra
dat_fit %>% select(rib, point.id, replication, data, fit.res) %>%
  filter(rib == T) %>%
  unnest(cols = c('data', 'fit.res')) %>%
  select(
    rib, point.id, replication, loc.x, loc.y, time, time.fit, vel.filt, peak.vel,
    peak.vel.ind, fit.res
    ) %>%
  # filter(time >= cutoff.time.min & time <= cutoff.time.max) %>%
  mutate(
    point.id = paste0(point.id, ' (', loc.x, ', ', loc.y, ')'),
    replication.txt = paste0('rep: ', replication)
    ) %>%
  ggplot(aes(x = time, y = vel.filt)) +
  facet_grid(factor(point.id, levels = mixedsort(unique(point.id))) ~ replication.txt) +
  geom_line(aes(color = point.id), size = 0.4) +
  geom_point(aes(y = peak.vel, x = time.fit, fill = 'Vršne vrijednosti'),
             na.rm = T, shape = 21, colour = "black",
             size = 1.5, stroke = 0.5, 
             ) +
  geom_line(aes(y = fit.res, color = 'Eksponencijalno prigušenje'),
            linetype = 'longdash', size = 0.9, lineend = 'round') +
  geom_hline(yintercept = 5) + 
  scale_color_nejm(
    name = NULL
  ) +
  scale_fill_manual(
    name = NULL,
    values = c('Vršne vrijednosti' = 'dodgerblue4')
  ) +
  scale_y_log10(
    name = expression('Brzina ['*mu*'m/s]'),
    breaks = c(1e2, 1e1, 1e-1, 1e-3),
    limits = c(NA, 1e2),
    # breaks = breaks_log(n=5),
    labels = label_math(format = log10)
    # breaks = trans_breaks("log10", function(x) 10^x),
    # labels = trans_format("log10", math_format(10^.x))
  ) +
  annotation_logticks(sides = 'l') +
  # scale_y_continuous(
  #   name = expression('Brzina vibracija ['*mu*'m/s]'),
  #   breaks = seq(-200, 200, 20),
  #   # limits = c(-1500, 1500)
  # ) +
  scale_x_continuous(
    name = "Vrijeme [s]",
    breaks = seq(0, 0.9, 0.1),
    # limits = c(0.09, 0.3)
  ) +
  ggtitle("Prigušenje na 100 Hz - ukrućeni dijelovi") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(0.2, 'lines'),
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0, 'lines')
  )
ggsave(filename = '110MVA/preliminarna_obrada/impact/slike/ukruta_da_damp_100_hz.png',
       width = 25, height = 17, units = 'cm', dpi = 320)
