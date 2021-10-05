
##### Analiza promjene kuta upada kod mjerenja #####

## Source functions and objects file
source('functions.R')


################################################################################
# Data input
# Input first point first replication
dat_inc <- VibData(
  point.id = 'pt1',
  loc.x = 0,
  loc.y = 10,
  rib = F,
  replication = 1,
  file.loc = 'mjerenja/promjena_kuta_upada/kut_upada_10'
)

# Input first point second replication
dat_inc <- dat_inc %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = 0,
      loc.y = 30,
      rib = F,
      replication = 2,
      file.loc = 'mjerenja/promjena_kuta_upada/kut_upada_30'
    )
  )

# Input first point third replication
dat_inc <- dat_inc %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = 0,
      loc.y = -30,
      rib = F,
      replication = 3,
      file.loc = 'mjerenja/promjena_kuta_upada/kut_upada_-30'
    )
  )

# Input first point fourth replication
dat_inc <- dat_inc %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = 15,
      loc.y = 30,
      rib = F,
      replication = 4,
      file.loc = 'mjerenja/promjena_kuta_upada/kut_upada_30_bocno_15'
    )
  )

# Input first point fifth replication
dat_inc <- dat_inc %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = -15,
      loc.y = 30,
      rib = F,
      replication = 5,
      file.loc = 'mjerenja/promjena_kuta_upada/kut_upada_30_bocno_-15'
    )
  )

################################################################################
# Plot velocity frequency spectrum for first point first replication
dat_inc %>%
  filter(point.id == 'pt1' & replication == 1) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/promjena_kuta_upada/freq/kut_10_0_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

# Plot velocity frequency spectrum for first point second replication
dat_inc %>%
  filter(point.id == 'pt1' & replication == 2) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/promjena_kuta_upada/freq/kut_30_0_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

# Plot velocity frequency spectrum for first point third replication
dat_inc %>%
  filter(point.id == 'pt1' & replication == 3) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/promjena_kuta_upada/freq/kut_-30_0_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

# Plot velocity frequency spectrum for first point fourth replication
dat_inc %>%
  filter(point.id == 'pt1' & replication == 4) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/promjena_kuta_upada/freq/kut_30_15_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

# Plot velocity frequency spectrum for first point fifth replication
dat_inc %>%
  filter(point.id == 'pt1' & replication == 5) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/promjena_kuta_upada/freq/kut_30_-15_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

################################################################################
# Plot velocity power spectral density for first point all replications
dat_inc %>%
  select(frequency, velocity.psd, subtitle, replication, loc.x, loc.y) %>%
  na.omit() %>%
  mutate(replication = paste0("kut: ", loc.y, ", ", loc.x)) %>%
  mutate(subtitle = str_sub(subtitle, 1, -8)) %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'replication',
    title = 'Velocity Power Spectral Density',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/promjena_kuta_upada/freq/all_vel_psd.png',
    fig.width = 15,
    fig.height = 8,
  )

################################################################################
# Plot Velocity power spectral density peaks
dat_inc %>%
  select(peak.frequency, peak.velocity.psd, subtitle, point.id, replication,
         loc.x, loc.y) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'peak.velocity.psd',
    x.name = 'peak.frequency',
    condition = 'point.id',
    groups = 'replication',
    title = 'Top 5 Velocity Power Spectral Density Peaks',
    subtitle = 'Comparison of all points',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 600, x.tick.num = 10,
    jitter.x = T, jitter.amount = 6,
    line.width = 6,
    aspect = 1,
    show.legend = F,
    col.palette.name = 'Set1',
    col.different.num = 6,
    col.line.num = 1,
    col.alpha = 1,
    save.fig = T,
    fig.filename = 'slike/promjena_kuta_upada/freq/all_vel_psd_comp.png',
    fig.width = 10,
    fig.height = 10,
  )

################################################################################
## Cross-correlation plots
# CCF first point, first vs second replication
dat_inc %>%
  filter(
    (point.id == 'pt1' & replication == 1) |
      (point.id == 'pt1' & replication == 2)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 10 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -19)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_2 = na.omit(rep_2),
         time = head(time, length(time)/2),
         subtitle = paste0(subtitle[1], " - ", subtitle[length(subtitle)])) %$%
  tibble(ccf = as.vector(ccf(rep_1, rep_2, lag.max = 2*length(.$rep_1),
                   type = 'correlation', plot = F)$acf),
         time = c(rev(-time[-1]), time),
         subtitle = subtitle[1],
         ccf.type = 'CCF'
         ) %>%
  acf.pacf.plot(
    y.name = 'ccf',
    x.name = 'time',
    title = 'Cross-correlation',
    condition = 'ccf.type',
    subtitle = .$subtitle[1],
    x.min = min(.$time), x.max = max(.$time),
    line.width = 0.2,
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/promjena_kuta_upada/corr/rep_1_rep_2_ccf.png'
  )

# CCF first point, first vs third replication
dat_inc %>%
  filter(
    (point.id == 'pt1' & replication == 1) |
      (point.id == 'pt1' & replication == 3)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 10 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -19)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_3 = na.omit(rep_3),
         time = head(time, length(time)/2),
         subtitle = paste0(subtitle[1], " - ", subtitle[length(subtitle)])) %$%
  tibble(ccf = as.vector(ccf(rep_1, rep_3, lag.max = 2*length(.$rep_1),
                   type = 'correlation', plot = F)$acf),
         time = c(rev(-time[-1]), time),
         subtitle = subtitle[1],
         ccf.type = 'CCF'
         ) %>%
  acf.pacf.plot(
    y.name = 'ccf',
    x.name = 'time',
    title = 'Cross-correlation',
    condition = 'ccf.type',
    subtitle = .$subtitle[1],
    x.min = min(.$time), x.max = max(.$time),
    line.width = 0.2,
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/promjena_kuta_upada/corr/rep_1_rep_3_ccf.png'
  )

# CCF first point, first vs fourth replication
dat_inc %>%
  filter(
    (point.id == 'pt1' & replication == 1) |
      (point.id == 'pt1' & replication == 4)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 10 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -19)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_4 = na.omit(rep_4),
         time = head(time, length(time)/2),
         subtitle = paste0(subtitle[1], " - ", subtitle[length(subtitle)])) %$%
  tibble(ccf = as.vector(ccf(rep_1, rep_4, lag.max = 2*length(.$rep_1),
                   type = 'correlation', plot = F)$acf),
         time = c(rev(-time[-1]), time),
         subtitle = subtitle[1],
         ccf.type = 'CCF'
         ) %>%
  acf.pacf.plot(
    y.name = 'ccf',
    x.name = 'time',
    title = 'Cross-correlation',
    condition = 'ccf.type',
    subtitle = .$subtitle[1],
    x.min = min(.$time), x.max = max(.$time),
    line.width = 0.2,
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/promjena_kuta_upada/corr/rep_1_rep_4_ccf.png'
  )

# CCF first point, first vs fifth replication
dat_inc %>%
  filter(
    (point.id == 'pt1' & replication == 1) |
      (point.id == 'pt1' & replication == 5)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 10 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -19)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_5 = na.omit(rep_5),
         time = head(time, length(time)/2),
         subtitle = paste0(subtitle[1], " - ", subtitle[length(subtitle)])) %$%
  tibble(ccf = as.vector(ccf(rep_1, rep_5, lag.max = 2*length(.$rep_1),
                   type = 'correlation', plot = F)$acf),
         time = c(rev(-time[-1]), time),
         subtitle = subtitle[1],
         ccf.type = 'CCF'
         ) %>%
  acf.pacf.plot(
    y.name = 'ccf',
    x.name = 'time',
    title = 'Cross-correlation',
    condition = 'ccf.type',
    subtitle = .$subtitle[1],
    x.min = min(.$time), x.max = max(.$time),
    line.width = 0.2,
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/promjena_kuta_upada/corr/rep_1_rep_5_ccf.png'
  )

################################################################################
## Plot Velocity peaks cosine fit
# y-axis angle fit
cos_lm <- dat_inc %>%
  select(peak.frequency, peak.velocity.psd, peak.velocity.amp, subtitle, point.id, replication,
         loc.x, loc.y) %>%
  na.omit() %>%
  filter(loc.x == 0) %>%
  arrange(desc(peak.velocity.psd),
          desc(peak.velocity.amp)) %>%
  filter(peak.frequency > 439.5 & peak.frequency < 440.5) %$%
  lm(peak.velocity.amp ~ cos(deg2rad(loc.y)), data = .)

plt <- dat_inc %>%
  select(peak.frequency, peak.velocity.psd, peak.velocity.amp, subtitle, point.id, replication,
         loc.x, loc.y) %>%
  na.omit() %>%
  filter(loc.x == 0) %>%
  arrange(desc(peak.velocity.psd),
          desc(peak.velocity.amp)) %>%
  filter(peak.frequency > 439.5 & peak.frequency < 440.5) %$%
  xyplot(.$peak.velocity.amp ~ .$loc.y)

cos_pred <- tibble(
  loc.y = c(-40:40),
  peak.velocity.amp = predict(cos_lm, data.frame(loc.y = loc.y))  
)
# Saving image
png(
  filename = 'slike/promjena_kuta_upada/corr/cosine_fit_y_angle.png',
  units = 'in',
  width = 15,
  height = 10,
  pointsize = 12 * 300 / 72,
  res = 300,
  bg = 'white'
)
cos_pred %$%
xyplot(peak.velocity.amp ~ loc.y, data = ., type='l') + plt
dev.off()

# x-axis angle fit and plot
cos_lm_side <- dat_inc %>%
  select(peak.frequency, peak.velocity.psd, peak.velocity.amp, subtitle, point.id, replication,
         loc.x, loc.y) %>%
  na.omit() %>%
  filter(loc.y == 30) %>%
  arrange(desc(peak.velocity.psd),
          desc(peak.velocity.amp)) %>%
  filter(peak.frequency > 439.5 & peak.frequency < 440.5) %$%
  lm(peak.velocity.amp ~ cos(deg2rad(loc.x)), data = .)
  
plt_side <- dat_inc %>%
  select(peak.frequency, peak.velocity.psd, peak.velocity.amp, subtitle, point.id, replication,
         loc.x, loc.y) %>%
  na.omit() %>%
  filter(loc.y == 30) %>%
  arrange(desc(peak.velocity.psd),
          desc(peak.velocity.amp)) %>%
  filter(peak.frequency > 439.5 & peak.frequency < 440.5) %$%
  xyplot(.$peak.velocity.amp ~ .$loc.x)

cos_pred_side <- tibble(
  loc.x = c(-20:20),
  peak.velocity.amp = predict(cos_lm_side, data.frame(loc.x = loc.x))  
)
# Saving image
png(
  filename = 'slike/promjena_kuta_upada/corr/cosine_fit_x_angle.png',
  units = 'in',
  width = 15,
  height = 10,
  pointsize = 12 * 300 / 72,
  res = 300,
  bg = 'white'
)
cos_pred_side %$%
xyplot(peak.velocity.amp ~ loc.x, data = ., type='l') + plt_side
dev.off()

# y-axis error for +- 30 deg
cos_lm
cos_pred %>%
  filter(loc.y == 0 | loc.y == -30 | loc.y == 30) %>%
  mutate(perc.err =
           (max(peak.velocity.amp) - peak.velocity.amp) / peak.velocity.amp * 100
         )

# x-axis error for +- 15 deg
cos_lm_side
cos_pred_side %>%
  filter(loc.x == 0 | loc.x == -15 | loc.x == 15) %>%
  mutate(perc.err =
           (max(peak.velocity.amp) - peak.velocity.amp) / peak.velocity.amp * 100
         )

################################################################################
## Print peaks
dat_inc %>%
  filter(replication == 5) %>%
  select(peak.velocity.ind, peak.frequency, peak.velocity.amp, peak.velocity.psd) %>%
  na.omit()
