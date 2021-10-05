
##### Data analysis and plot #####

## Source functions and objects file
source('functions.R')


################################################################################
# Data input
# Input first point first replication
dat <- VibData(
  point.id = 'pt1',
  loc.x = 1,
  loc.y = 1,
  rib = F,
  replication = 1,
  file.loc = 'mjerenja/probne_tocke/1_lijevo_dolje_dis'
)

# Input first point second replication
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = 1,
      loc.y = 1,
      rib = F,
      replication = 2,
      file.loc = 'mjerenja/probne_tocke/1_lijevo_dolje_vel'
    )
  )

# Input second point first replication
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt2',
      loc.x = 2,
      loc.y = 1,
      rib = T,
      replication = 1,
      file.loc = 'mjerenja/probne_tocke/2_desno_dolje_dis'
    )
  )

# Input second point second replication
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt2',
      loc.x = 2,
      loc.y = 1,
      rib = T,
      replication = 2,
      file.loc = 'mjerenja/probne_tocke/2_desno_dolje_vel'
    )
  )

# Input third point first replication
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt3',
      loc.x = 1,
      loc.y = 2,
      rib = F,
      replication = 1,
      file.loc = 'mjerenja/probne_tocke/3_lijevo_gore_dis'
    )
  )

# Input third point second replication
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt3',
      loc.x = 1,
      loc.y = 2,
      rib = F,
      replication = 2,
      file.loc = 'mjerenja/probne_tocke/3_lijevo_gore_vel'
    )
  )
# Input fourth point first replication
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt4',
      loc.x = 2,
      loc.y = 2,
      rib = T,
      replication = 1,
      file.loc = 'mjerenja/probne_tocke/4_desno_gore_dis'
    )
  )

# Input fourth point second replication
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt4',
      loc.x = 2,
      loc.y = 2,
      rib = T,
      replication = 2,
      file.loc = 'mjerenja/probne_tocke/4_desno_gore_vel'
    )
  )

################################################################################
# Plot velocity frequency spectrum for first point second replication for comparison
dat %>%
  filter(point.id == 'pt1' & replication == 2) %>%
  select(frequency, velocity.orig, velocity.amp, subtitle) %>%
  na.omit() %>%
  rename(software.values = velocity.orig, custom.values = velocity.amp) %>%
  gather(key='type', value='velocity', software.values, custom.values, -c(frequency, subtitle)) %>% 
  freq.spec.plot(
    y.name = 'velocity',
    x.name = 'frequency',
    condition = 'type',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/lijevo_dolje_vel_freq_comparison.png',
    fig.width = 13,
    fig.height = 9,
  )

# Plot velocity frequency spectrum for second point second replication for comparison
dat %>%
  filter(point.id == 'pt2' & replication == 2) %>%
  select(frequency, velocity.orig, velocity.amp, subtitle) %>%
  na.omit() %>%
  rename(software.values = velocity.orig, custom.values = velocity.amp) %>%
  gather(key='type', value='velocity', software.values, custom.values, -c(frequency, subtitle)) %>%
  freq.spec.plot(
    y.name = 'velocity',
    x.name = 'frequency',
    condition = 'type',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/desno_dolje_vel_freq_comparison.png',
    fig.width = 13,
    fig.height = 9,
  )

# Plot velocity frequency spectrum for third point second replication for comparison
dat %>%
  filter(point.id == 'pt3' & replication == 2) %>%
  select(frequency, velocity.orig, velocity.amp, subtitle) %>%
  na.omit() %>%
  rename(software.values = velocity.orig, custom.values = velocity.amp) %>%
  gather(key='type', value='velocity', software.values, custom.values, -c(frequency, subtitle)) %>%
  freq.spec.plot(
    y.name = 'velocity',
    x.name = 'frequency',
    condition = 'type',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/lijevo_gore_vel_freq_comparison.png',
    fig.width = 13,
    fig.height = 9,
  )

# Plot velocity frequency spectrum for fourth point second replication for comparison
dat %>%
  filter(point.id == 'pt4' & replication == 2) %>%
  select(frequency, velocity.orig, velocity.amp, subtitle) %>%
  na.omit() %>%
  rename(software.values = velocity.orig, custom.values = velocity.amp) %>%
  gather(key='type', value='velocity', software.values, custom.values, -c(frequency, subtitle)) %>%
  freq.spec.plot(
    y.name = 'velocity',
    x.name = 'frequency',
    condition = 'type',
    title = 'Velocity Frequency Spectrum',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m/s * ']'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/desno_gore_vel_freq_comparison.png',
    fig.width = 13,
    fig.height = 9,
  )

# Plot velocity frequency spectrum for first point first replication
dat %>%
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
    fig.filename = 'slike/probne_tocke/lijevo_dolje_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

# Plot velocity frequency spectrum for second point first replication
dat %>%
  filter(point.id == 'pt2' & replication == 1) %>%
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
    fig.filename = 'slike/probne_tocke/desno_dolje_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

# Plot velocity frequency spectrum for third point first replication
dat %>%
  filter(point.id == 'pt3' & replication == 1) %>%
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
    fig.filename = 'slike/probne_tocke/lijevo_gore_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

# Plot velocity frequency spectrum for fourth point first replication
dat %>%
  filter(point.id == 'pt4' & replication == 1) %>%
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
    fig.filename = 'slike/probne_tocke/desno_gore_vel_freq.png',
    fig.width = 13,
    fig.height = 5,
  )

################################################################################
# Plot velocity power spectral density for first point both replications
dat %>%
  filter(
    (point.id == 'pt1' & replication == 1) | 
      (point.id == 'pt1' & replication == 2)
    ) %>%
  select(frequency, velocity.psd, subtitle, replication) %>%
  na.omit() %>%
  mutate(replication = paste("replication:", replication)) %>%
  mutate(subtitle = str_sub(subtitle, 1, -8)) %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'replication',
    title = 'Velocity Power Spectral Density',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/lijevo_dolje_vel_psd.png',
    fig.width = 13,
    fig.height = 9,
  )

# Comparison with psd and spec.pgram
# dat %>%
#   filter(
#     (point.id == 'pt1' & replication == 1)
#     ) %>%
#   select(velocity, sample.rate, time, velocity.amp, frequency, velocity.psd) %>%
#   mutate(spec = velocity.psd,
#          freq = frequency) %$%
#   # tibble(!!!(pspectrum(.$velocity, x.frqsamp = .$sample.rate[1], Nyquist.normalize = F,
#   #           plot = F, niter = 4, AR = F)[1:2]),
#   #        ) %$%
#   # tibble(
#   #   !!!(spec.pgram(ts(.$velocity, frequency = .$sample.rate[1]),
#   #                  demean = T, plot = F)[1:2])
#   #   ) %>%
#   # mutate(spec = spec*2) %$%
#   # tibble(
#   #   !!!(spec.fft(.$velocity, .$time, center = F)[1:2])
#   # ) %>%
#   # mutate(freq = fx,
#   #        spec = Mod(A*2)) %$%
#   # tibble(
#   #   freq = na.omit(.$freq),
#   #   spec = Mod(fft(.$velocity)[1:length(freq)] / length(.$velocity) * 2),
#   # ) %$%
#   tibble(
#     !!!(freq.spec(.$velocity, .$sample.rate[1])[c('amp', 'freq', 'psd')])
#   ) %>%
#   rename(spec = psd) %$%
#   xyplot(spec~freq, data = ., type='h', xlim = c(20, 600)) #, ylim = c(0, 1e6))



# Plot velocity power spectral density for second point both replication
dat %>%
  filter(
    (point.id == 'pt2' & replication == 1) |
      (point.id == 'pt2' & replication == 2)
    ) %>%
  select(frequency, velocity.psd, subtitle, replication) %>%
  na.omit() %>%
  mutate(replication = paste("replication:", replication)) %>%
  mutate(subtitle = str_sub(subtitle, 1, -8)) %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'replication',
    title = 'Velocity Power Spectral Density',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/desno_dolje_vel_psd.png',
    fig.width = 13,
    fig.height = 9,
  )

# Plot velocity power spectral density for third point both replication
dat %>%
  filter(
    (point.id == 'pt3' & replication == 1) |
      (point.id == 'pt3' & replication == 2)
    ) %>%
  select(frequency, velocity.psd, subtitle, replication) %>%
  na.omit() %>%
  mutate(replication = paste("replication:", replication)) %>%
  mutate(subtitle = str_sub(subtitle, 1, -8)) %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'replication',
    title = 'Velocity Power Spectral Density',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/lijevo_gore_vel_psd.png',
    fig.width = 13,
    fig.height = 9,
  )

# Plot velocity power spectral density for fourth point both replications
dat %>%
  filter(
    (point.id == 'pt4' & replication == 1) | 
      (point.id == 'pt4' & replication == 2)
    ) %>%
  select(frequency, velocity.psd, subtitle, replication) %>%
  na.omit() %>%
  mutate(replication = paste("replication:", replication)) %>%
  mutate(subtitle = str_sub(subtitle, 1, -8)) %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'replication',
    title = 'Velocity Power Spectral Density',
    subtitle = unlist(.$subtitle)[1],
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    line.width = 3,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/desno_gore_vel_psd.png',
    fig.width = 13,
    fig.height = 9,
  )

################################################################################
#TODO: add more gridlines (ablines)
# Plot Velocity time series for first point first replication
dat %>%
  filter(point.id == 'pt1' & replication == 1) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 2, 2.1),
      x.max = c(max(.$time), 3, 2.2),
      save.fig = F,
      fig.filename = 'slike/probne_tocke/lijevo_dolje_vel_time_1.png'
    )

# Plot Velocity time series for first point second replication
dat %>%
  filter(point.id == 'pt1' & replication == 2) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 1.3, 1.6),
      x.max = c(max(.$time), 1.901, 1.8),
      panel.names = c('Entire Dataset', '0.6 s', ' 200 ms'),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/lijevo_dolje_vel_time_2.png'
    )

# Plot Velocity time series for second point first replication
dat %>%
  filter(point.id == 'pt2' & replication == 1) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 0.5, 1.4),
      x.max = c(max(.$time), 1.5, 1.5),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_dolje_vel_time_1.png'
    )

# Plot Velocity time series for second point second replication
dat %>%
  filter(point.id == 'pt2' & replication == 2) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 0.5, 1.4),
      x.max = c(max(.$time), 1.5, 1.5),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_dolje_vel_time_2.png'
    )

# Plot Velocity time series for third point first replication
dat %>%
  filter(point.id == 'pt3' & replication == 1) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 0.5, 0.6),
      x.max = c(max(.$time), 1.5, 0.7001),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/lijevo_gore_vel_time_1.png'
    )

# Plot Velocity time series for third point second replication
dat %>%
  filter(point.id == 'pt3' & replication == 2) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 0.5, 0.6),
      x.max = c(max(.$time), 1.5, 0.7001),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/lijevo_gore_vel_time_2.png'
    )

# Plot Velocity time series for fourth point first replication
dat %>%
  filter(point.id == 'pt4' & replication == 1) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 0.5, 0.6),
      x.max = c(max(.$time), 1.5, 0.701),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_gore_vel_time_1.png'
    )

# Plot Velocity time series for fourth point second replication
dat %>%
  filter(point.id == 'pt4' & replication == 2) %>%
  select(time, velocity, subtitle) %>%
    time.series.plot(
      y.name = 'velocity',
      x.name = 'time',
      title = 'Velocity Time Domain',
      subtitle = unlist(.$subtitle)[1],
      ylab = expression(paste('Velocity [', mu, 'm/s]', sep = "")),
      x.min = c(min(.$time), 0.5, 0.6),
      x.max = c(max(.$time), 1.5, 0.701),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_gore_vel_time_2.png'
    )

################################################################################
# Plot Velocity power spectral density peaks
dat %>%
  select(peak.frequency, peak.velocity.psd, subtitle, point.id, replication) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'peak.velocity.psd',
    x.name = 'peak.frequency',
    condition = 'point.id',
    groups = 'replication',
    title = 'Top 5 Velocity Power Spectral Density Peaks',
    subtitle = 'Comparison of all points',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 450, x.tick.num = 10,
    jitter.x = T, jitter.amount = 7,
    line.width = 4,
    aspect = 1,
    show.legend = F,
    col.palette.name = 'Set1',
    col.different.num = 6,
    col.line.num = 1,
    col.alpha = 1,
    save.fig = T,
    fig.filename = 'slike/probne_tocke/all_points_vel_psd_peaks.png',
    fig.width = 10,
    fig.height = 10,
  )
################################################################################

################################################################################
# Zero voltage pattern
# Zero voltage pattern for first point first replication
dat %>%
  filter(point.id == 'pt1' & replication == 1) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 2 & time <= 3) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 1)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 2, 2.1),
    x.max = c(max(.$time), 3, 2.2),
    x.tick.num = 50,
    y.tick.num = 300,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/lijevo_dolje_vol_interr_1.png'
  )

# Zero voltage pattern for first point second replication
dat %>%
  filter(point.id == 'pt1' & replication == 2) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 0 & time <= 3.5) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 1)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 1.3, 1.6),
    x.max = c(max(.$time), 1.901, 1.8),
    x.tick.num = 50,
    y.tick.num = 300,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/lijevo_dolje_vol_interr_2.png'
  )

# Zero voltage pattern for second point first replication
dat %>%
  filter(point.id == 'pt2' & replication == 1) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 0 & time <= 3.5) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 1)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 0.5, 1.4),
    x.max = c(max(.$time), 1.5, 1.5),
    x.tick.num = 100,
    y.tick.num = 250,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/desno_dolje_vol_interr_1.png'
  )

# Zero voltage pattern for second point second replication
dat %>%
  filter(point.id == 'pt2' & replication == 2) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 0 & time <= 3.5) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 1)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 0.5, 1.4),
    x.max = c(max(.$time), 1.5, 1.5),
    x.tick.num = 100,
    y.tick.num = 250,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/desno_dolje_vol_interr_2.png'
  )

# Zero voltage pattern for third point first replication
dat %>%
  filter(point.id == 'pt3' & replication == 1) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 0 & time <= 3.5) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 0)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 0.5, 0.6),
    x.max = c(max(.$time), 1.5, 0.7001),
    x.tick.num = 50,
    y.tick.num = 300,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/lijevo_gore_vol_interr_1.png'
  )

# Zero voltage pattern for third point second replication
dat %>%
  filter(point.id == 'pt3' & replication == 2) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 0 & time <= 3.5) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 0)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 0.5, 0.6),
    x.max = c(max(.$time), 1.5, 0.7001),
    x.tick.num = 60,
    y.tick.num = 300,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/lijevo_gore_vol_interr_2.png'
  )

# Zero voltage pattern for fourth point first replication
dat %>%
  filter(point.id == 'pt4' & replication == 1) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 0 & time <= 3.5) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 0)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 0.5, 0.6),
    x.max = c(max(.$time), 1.5, 0.701),
    x.tick.num = 60,
    y.tick.num = 300,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/desno_gore_vol_interr_1.png'
  )

# Zero voltage pattern for fourth point second replication
dat %>%
  filter(point.id == 'pt4' & replication == 2) %>%
  select(time, voltage, subtitle, sample.rate) %>%
  filter(time >= 0 & time <= 3.5) %>%
  mutate(grad.volt = gradient(.$voltage)) %>%
  filter(grad.volt == 0 & voltage >= -0.0001 & voltage <= 0.0001) %>%
  mutate(grad.time = gradient(.$time)) %>%
  filter(grad.time >= 0.005) %>%
  mutate(voltage = voltage * 1e6) %>%
  slice(which(row_number() %% 2 == 0)) %>%
  mutate(freq.interr = 1 / gradient(.$time)) %>%
  time.series.plot(
    y.name = 'voltage',
    x.name = 'time',
    title = paste(
      'Voltage Measurement Interrupts:', round(mean(.$freq.interr), 1), 'Hz'),
    subtitle = unlist(.$subtitle)[1],
    ylab = expression(paste('Voltage [', mu, V, ']', sep = "")),
    x.min = c(min(.$time), 0.5, 0.6),
    x.max = c(max(.$time), 1.5, 0.701),
    x.tick.num = 60,
    y.tick.num = 300,
    save.fig = T,
    plot.type = 'p',
    point.cex = 0.8,
    fig.filename = 'slike/probne_tocke/desno_gore_vol_interr_2.png'
  )
################################################################################

################################################################################
## Autocorrelation plots
# ACF and PACF first point first replication
dat %>%
  filter(point.id == 'pt1' & replication == 1) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/lijevo_dolje_vel_acf_1.png'
  )
  
# ACF and PACF first point second replication
dat %>%
  filter(point.id == 'pt1' & replication == 2) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/lijevo_dolje_vel_acf_2.png'
  )

# ACF and PACF second point first replication
dat %>%
  filter(point.id == 'pt2' & replication == 1) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_dolje_vel_acf_1.png'
  )
# ACF and PACF second point second replication
dat %>%
  filter(point.id == 'pt2' & replication == 2) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_dolje_vel_acf_2.png'
  )

# ACF and PACF third point first replication
dat %>%
  filter(point.id == 'pt3' & replication == 1) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/lijevo_gore_vel_acf_1.png'
  )

# ACF and PACF third point second replication
dat %>%
  filter(point.id == 'pt3' & replication == 2) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/lijevo_gore_vel_acf_2.png'
  )

# ACF and PACF fourth point first replication
dat %>%
  filter(point.id == 'pt4' & replication == 1) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_gore_vel_acf_1.png'
  )

# ACF and PACF fourth point second replication
dat %>%
  filter(point.id == 'pt4' & replication == 2) %>%
  select(acf, pacf, subtitle, time) %>%
  slice(-1) %>%
  rename(ACF = acf, 'Partial ACF' = pacf) %>%
  gather(acf.type, acf.value, -c(subtitle, time)) %>%
  acf.pacf.plot(
      y.name = 'acf.value',
      x.name = 'time',
      condition = 'acf.type',
      subtitle = .$subtitle[1],
      x.min = -0.01, x.max = max(.$time),
      save.fig = T,
      fig.filename = 'slike/probne_tocke/desno_gore_vel_acf_2.png'
  )
################################################################################

################################################################################
## Cross-correlation plots
# CCF first point, first vs second replication
dat %>%
  filter(
    (point.id == 'pt1' & replication == 1) |
      (point.id == 'pt1' & replication == 2)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 8 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -8)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_2 = na.omit(rep_2),
         time = time,
         subtitle = subtitle[1] ) %$%
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
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/probne_tocke/corr/lijevo_dolje_vel_ccf_1_2.png'
  )
  
# CCF second point, first vs second replication
dat %>%
  filter(
    (point.id == 'pt2' & replication == 1) |
      (point.id == 'pt2' & replication == 2)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 8 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -8)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_2 = na.omit(rep_2),
         time = time,
         subtitle = subtitle[1] ) %$%
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
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/probne_tocke/corr/desno_dolje_vel_ccf_1_2.png'
  )

# CCF third point, first vs second replication
dat %>%
  filter(
    (point.id == 'pt3' & replication == 1) |
      (point.id == 'pt3' & replication == 2)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 8 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -8)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_2 = na.omit(rep_2),
         time = time,
         subtitle = subtitle[1] ) %$%
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
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/probne_tocke/corr/lijevo_gore_vel_ccf_1_2.png'
  )

# CCF fourth point, first vs second replication
dat %>%
  filter(
    (point.id == 'pt4' & replication == 1) |
      (point.id == 'pt4' & replication == 2)
    ) %>%
  select(velocity, subtitle, time, replication) %>%
  # Downsample before plot
  slice(which(row_number() %% 8 == 0)) %>%
  mutate(replication = paste0("rep_", replication),
         subtitle = str_sub(subtitle, 1, -8)) %>%
  spread(key = replication, value = velocity) %$%
  tibble(rep_1 = na.omit(rep_1),
         rep_2 = na.omit(rep_2),
         time = time,
         subtitle = subtitle[1] ) %$%
  tibble(ccf = as.vector(ccf(rep_1, rep_2, lag.max = 2*length(.$rep_1),
                   type = 'correlation', plot = T)$acf),
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
    save.fig = T,
    fig.width = 14,
    fig.height = 5,
    fig.filename = 'slike/probne_tocke/corr/desno_gore_vel_ccf_1_2.png'
  )
################################################################################

################################################################################
# No comparison
# Displacement frequency spectrum
# freq.spec.plot(
#   data = lijevo.dolje.dis.1$freq.domain,
#   y.name = 'displacement',
#   x.name = 'frequency',
#   title = 'Displacement Frequency Spectrum',
#   subtitle = with(lijevo.dolje.dis.1$info, paste(
#     '(', loc.x, ', ', loc.y, ')', sep = "")),
#   ylab = 'Displacement [nm]',
#   x.max = 800,
#   save.fig = T,
#   fig.filename = 'slike/probne_tocke/lijevo_dolje_dis_freq.png'
# )

################################################################################
# # Automatic packages
# # Power spectrum
# psdcore(
#   X.d = `Velocity (um/s)`,
#   X.frq = 1 / `dt  (sec)`[1],
#   refresh = T,
#   verbose = T,
#   plot = T
# )
# 
# pspectrum(
#   `Velocity (um/s)`,
#   niter = 5,
#   x.frqsamp = 1 / `dt  (sec)`[1],
#   Nyquist.normalize = F,
#   plot = T
# )
# 
# # Ne radi
# fft_obj <-spec.fft(
#   y = `Velocity (um/s)`,
#   x = `Time (sec)`
# )
# plot(fft_obj)
# 
