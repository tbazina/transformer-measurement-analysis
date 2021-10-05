################################################################################
################### Konacna analiza mjerenja 2MVA i 110 MVA ####################
################################################################################

# Source functions and objects file
source('functions.R')

# Data paste
library(datapasta)
# Sorting
library(gtools)
# Correlation analysis
# library(correlationfunnel)
# EDA
# library(DataExplorer)
# Spatial point pattern analysis and modelling
# library(spatstat)
# library(sm)
# Distribution check
library(fitdistrplus)
library(mixtools)
library(actuar)
# Plotting
library(ggplot2)
library(ggsci)
library(gridExtra)
library(plotly)
library(cowplot)
# Correlation analysis
# remotes::install_github("r-link/corrmorant")
library(corrmorant)
# Spatial data modelling
library(gstat)
library(sp)
# Use dplyr select
select <- dplyr::select


################################################################################
################################################################################
# Data input summary-a svih mjerenja 2 MVA i 110 MVA
# Data input summary 110MVA namotaji
dat_summ <- read_csv2(
  '110MVA/preliminarna_obrada/aktivni_dio/namotaji_summary.csv'
) %>% mutate(
  trans = '110MVA',
  Strana = 'NN',
  Namotaj = str_c('namotaj_', Namotaj),
  Tocka = as.numeric(str_sub(Tocka, start = 3))
) %>% 
  select(
    trans = trans, measure = Namotaj, side = Strana, point.id = Tocka,
    x = Sirina, y = Visina, rms = `RMS`, ptp = `Peak-to-peak`,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    amp.v.50 = `Model 50 Hz amp`, amp.x.50 = `Model 50 Hz disp amp`,
    peak.freq.1 = 'Peak freq 1', peak.amp.1 = 'Peak amp 1',
    peak.freq.2 = 'Peak freq 2', peak.amp.2 = 'Peak amp 2',
    vel.min = 'Brzina min', vel.max = 'Brzina max',
    adj.R.sq = 'Model Adj R Squared'
    )
  
# Data input summary 110MVA jezgra i stege
dat_summ <- dat_summ %>% bind_rows(
read_csv2(
  '110MVA/preliminarna_obrada/aktivni_dio/stege_summary.csv'
) %>%
  mutate(
    trans = '110MVA',
    Strana = case_when(
      Stega == 'Jezgra Desno' ~ 'BD',
      Stega == 'Jezgra Lijevo' ~ 'BL',
      TRUE ~ 'NN'
      ),
    Tocka = as.numeric(str_sub(Tocka, start = 3))
) %>% 
  select(
    trans = trans, measure = Stega, side = Strana, point.id = Tocka,
    x = Sirina, y = Visina, rms = `RMS`, ptp = `Peak-to-peak`,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    amp.v.50 = `Model 50 Hz amp`, amp.x.50 = `Model 50 Hz disp amp`,
    peak.freq.1 = 'Peak freq 1', peak.amp.1 = 'Peak amp 1',
    peak.freq.2 = 'Peak freq 2', peak.amp.2 = 'Peak amp 2',
    vel.min = 'Brzina min', vel.max = 'Brzina max',
    adj.R.sq = 'Model Adj R Squared'
    )
)

# Data input summary 110MVA oplata
dat_summ <- dat_summ %>% bind_rows(
read_csv2(
  '110MVA/preliminarna_obrada/oplata/oplata_summary.csv'
) %>%
  mutate(
    trans = '110MVA',
    Strana = 'NN',
    measure = case_when(
      Ukruta == T ~ 'oplata_ukr',
      Ukruta == F ~ 'oplata_opl',
      TRUE ~ 'NN'
      ),
    Tocka = as.numeric(str_sub(Tocka, start = 3))
) %>% 
  select(
    trans = trans, measure = measure, side = Strana, point.id = Tocka,
    x = Sirina, y = Visina, rms = `RMS`, ptp = `Peak-to-peak`,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    amp.v.50 = `50 Hz amp`,
    peak.freq.1 = 'Peak freq 1', peak.amp.1 = 'Peak amp 1',
    peak.freq.2 = 'Peak freq 2', peak.amp.2 = 'Peak amp 2',
    vel.min = 'Brzina min', vel.max = 'Brzina max',
    adj.R.sq = 'Model Adj R Squared'
    )
)

# Data input summary 2MVA namotaji
dat_summ <- dat_summ %>% bind_rows(
read_csv2(
  file = '2MVA/preliminarna_obrada/aktivni_dio/namotaji_summary.csv'
) %>% mutate(
  trans = '2MVA',
  measure = str_c('namotaj_', Namotaj),
  Strana = case_when(
    measure == 'namotaj_1' & Strana == 'bok' ~ 'BL',
    measure == 'namotaj_3' & Strana == 'bok' ~ 'BD',
    TRUE ~ Strana
  )
) %>% 
  select(
    trans = trans, measure = measure, side = Strana, point.id = Tocka,
    x = Sirina, y = Visina, rms = `RMS`, ptp = `Peak-to-peak`,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    amp.v.50 = `50 Hz amp`,
    peak.freq.1 = 'Peak freq 1', peak.amp.1 = 'Peak amp 1',
    peak.freq.2 = 'Peak freq 2', peak.amp.2 = 'Peak amp 2',
    vel.min = 'Brzina min', vel.max = 'Brzina max',
    adj.R.sq = 'Model Adj R Squared'
    )
)
  
# Data input summary 2MVA jezgra
dat_summ <- dat_summ %>% bind_rows(
read_csv2(
  file = '2MVA/preliminarna_obrada/aktivni_dio/jezgra_summary.csv'
) %>%
  mutate(
  trans = '2MVA',
  measure = 'jezgra',
  Strana = case_when(
    Strana == 'bok desno' ~ 'BD',
    Strana == 'bok lijevo' ~ 'BL',
    TRUE ~ 'NN'
  )
) %>% 
  select(
    trans = trans, measure = measure, side = Strana, point.id = Tocka,
    x = Sirina, y = Visina, rms = `RMS`, ptp = `Peak-to-peak`,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    amp.v.50 = `50 Hz amp`,
    peak.freq.1 = 'Peak freq 1', peak.amp.1 = 'Peak amp 1',
    peak.freq.2 = 'Peak freq 2', peak.amp.2 = 'Peak amp 2',
    vel.min = 'Brzina min', vel.max = 'Brzina max',
    adj.R.sq = 'Model Adj R Squared'
    )
)

#  Data input summary 2MVA oplata
dat_summ <- dat_summ %>% bind_rows(
read_csv2(
  file = '2MVA/preliminarna_obrada/oplata/oplata_summary.csv'
) %>%
  mutate(
  trans = '2MVA',
  measure = 'oplata',
) %>% 
  select(
    trans = trans, measure = measure, side = Strana, point.id = Tocka,
    x = Sirina, y = Visina, rms = `RMS`, ptp = `Peak-to-peak`,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    amp.v.50 = `50 Hz amp`,
    peak.freq.1 = 'Peak freq 1', peak.amp.1 = 'Peak amp 1',
    peak.freq.2 = 'Peak freq 2', peak.amp.2 = 'Peak amp 2',
    vel.min = 'Brzina min', vel.max = 'Brzina max',
    adj.R.sq = 'Model Adj R Squared'
    )
)
  
#  Data input summary 2MVA oplata modificirana
dat_summ <- dat_summ %>% bind_rows(
read_csv2(
  file = '2MVA/preliminarna_obrada/oplata_modificirana/oplata_modificirana_summary.csv'
) %>%
  mutate(
  trans = '2MVA',
  measure = case_when(
    Strana == 'UBL' ~ 'oplata_mod_ukr',
    Strana == 'UBD' ~ 'oplata_mod_ukr',
    TRUE ~ 'oplata_mod_opl'
    )
) %>% 
  select(
    trans = trans, measure = measure, side = Strana, point.id = Tocka,
    x = Sirina, y = Visina, rms = `RMS`, ptp = `Peak-to-peak`,
    amp.v.100 = `Model 100 Hz amp`, amp.x.100 = `Model 100 Hz disp amp`,
    amp.v.50 = `50 Hz amp`,
    peak.freq.1 = 'Peak freq 1', peak.amp.1 = 'Peak amp 1',
    peak.freq.2 = 'Peak freq 2', peak.amp.2 = 'Peak amp 2',
    vel.min = 'Brzina min', vel.max = 'Brzina max',
    adj.R.sq = 'Model Adj R Squared'
    )
)

# Change winding sequence for 110MVA namotaj_1 -> namotaj_3 and vice versa
dat_summ <- dat_summ %>% 
  mutate(
    measure = case_when(
      trans == '110MVA' & measure == 'namotaj_1' ~ 'namotaj_3',
      trans == '110MVA' & measure == 'namotaj_3' ~ 'namotaj_1',
      TRUE ~ measure
    )
  ) 

dat_summ %>% glimpse()
dat_summ %>% head()

dat_summ %>% select(trans, measure, side) %>%
  group_by(trans, measure, side) %>%
  summarise(
    count = n()
  ) %>%
  arrange(trans, measure, side) %>% View()

################################################################################
############################### Oplata  ########################################
################################################################################
# Box plot oplate 110MVA - 2MVA usporedbe
dat_summ_opl <- dat_summ %>% select(trans, measure, side, amp.v.100) %>%
  filter(str_detect(measure, 'oplata') & side %in% c('VN', 'NN', 'BL', 'BD')) %>%
  mutate(
    feature = case_when(
      trans == '110MVA' & measure == 'oplata_opl' ~ 'OPL',
      trans == '110MVA' & measure == 'oplata_ukr' ~ 'UKR',
      trans == '2MVA' & measure == 'oplata' ~ side,
      trans == '2MVA' & measure == 'oplata_mod_opl' ~ side,
    ),
    measure = case_when(
      trans == '110MVA' & measure == 'oplata_opl' ~ 'kotao',
      trans == '110MVA' & measure == 'oplata_ukr' ~ 'kotao',
      trans == '2MVA' & measure == 'oplata' ~ 'početni kotao',
      trans == '2MVA' & measure == 'oplata_mod_opl' ~ 'modificirani kotao',
    )
  ) 

dat_summ_opl <- dat_summ_opl %>%
  mutate(
    feature = case_when(
      trans == '110MVA' ~ 'NN',
      trans == '2MVA' ~ 'SVE',
      T ~ side
      ),
    side = case_when(
      trans == '110MVA' ~ 'NN',
      trans == '2MVA' ~ 'SVE',
      T ~ side
      )
    ) %>%
  add_row(
    dat_summ_opl
    )

dat_summ_opl_med <- dat_summ_opl %>%
  mutate(
    title = paste(trans, measure, sep = ' - '),
    title = factor(title, levels = c(
      '110MVA - kotao', '2MVA - početni kotao', '2MVA - modificirani kotao')),
    side = factor(side, levels = c('VN', 'NN', 'BL', 'BD', 'SVE')),
    feature = factor(
      feature, levels = c('OPL', 'UKR', 'VN', 'NN', 'BL', 'BD', 'SVE')),
  ) %>%
  group_by(title, feature) %>%
  summarise(
    median.v = median(amp.v.100),
    min.v = min(amp.v.100),
    max.v = max(amp.v.100)
    ) %>% ungroup()

dat_summ_opl %>%
  mutate(
    title = paste(trans, measure, sep = ' - '),
    title = factor(title, levels = c(
      '110MVA - kotao', '2MVA - početni kotao', '2MVA - modificirani kotao')),
    side = factor(side, levels = c('VN', 'NN', 'BL', 'BD', 'SVE')),
    feature = factor(
      feature, levels = c('OPL', 'UKR', 'VN', 'NN', 'BL', 'BD', 'SVE')),
  ) %>%
  ggplot(aes(x = feature, y = amp.v.100, fill = side)) +
  facet_wrap(
    title ~ .,
    scales = 'free_x') +
  geom_boxplot(width = 0.6, alpha = 1, outlier.size = 1, outlier.stroke = 0.5,
               outlier.color = 'black', outlier.fill = 'grey', outlier.shape = 21,
               varwidth = F, size = 0.2, color = 'black'
               # fill = 'dodgerblue2'
               ) + 
  # geom_dotplot(
  #   colour = "black", stroke = 0.5,
  #   binaxis='y', stackdir='center', dotsize=0.8, binwidth = 20) +
  stat_summary(aes(group=feature), fun = median, geom = 'text',
               label=dat_summ_opl_med %>% pull(median.v) %>% round(),
               position = position_dodge2(0.6),
               size=2.2, color="black", vjust=-0.3, fontface = 2) +
  stat_summary(aes(group=feature), fun = min, geom = 'text',
               label=dat_summ_opl_med %>% pull(min.v) %>% round(),
               position = position_dodge2(0.6),
               size=2, color="black", vjust=1.3) +
  stat_summary(aes(group=feature), fun = max, geom = 'text',
               label=dat_summ_opl_med %>% pull(max.v) %>% round(),
               position = position_dodge2(0.6),
               size=2, color="black", vjust=-0.5) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    breaks = seq(0, 1500, 200),
    # limits=c(0, NA)
    ) +
  scale_x_discrete(name = NULL) +
  scale_fill_nejm(name = 'Mjerene strane') +
  ggtitle("Box-plot - oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(face="bold", size = 10),
    axis.text.x = element_text(colour="black", size = 8),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/box_plot_oplata.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# Razlike medijana i max vrijednosti prije i nakon modifikacija
dat_summ_opl_med %>%
  filter(str_detect(title, '2MVA')) %>%
  pivot_wider(names_from = title, values_from = c('median.v', 'min.v', 'max.v')
              ) %>%
  mutate(
    median.diff = .[[3]] - .[[2]],
    max.diff = .[[7]] - .[[6]]
  ) %>% select(-(2:7)) %>%
  write_csv2('izvjestaj_final/2MVA_razlike_min_medijan_modifikacije.csv')

################################################################################
########################## Oplata 110 MVA ######################################
################################################################################

dat_summ_opl <- dat_summ %>%
  select(trans, measure, side, point.id, x, y, amp.v.100) %>%
  filter(trans == '110MVA' & str_detect(measure, 'oplata') & side == 'NN') %>%
  mutate(
    feature = case_when(
      trans == '110MVA' & measure == 'oplata_opl' ~ 'OPL',
      trans == '110MVA' & measure == 'oplata_ukr' ~ 'UKR',
    ),
    measure = case_when(
      trans == '110MVA' & measure == 'oplata_opl' ~ 'kotao',
      trans == '110MVA' & measure == 'oplata_ukr' ~ 'kotao',
    )
  ) %>%
  mutate(
    trans = factor(trans),
    measure = factor(measure),
    side = factor(side),
    feature = factor(feature, levels = c('OPL', 'UKR')),
  ) %>%
  select(trans, measure, side, feature, point.id, x, y, amp.v.100) %>%
  arrange(trans, measure, side, point.id, feature)

dat_summ_opl %>% group_by(trans, measure, side, feature) %>%
  summarise(
    count = n()
  )

# Histogram usporedba oplata 110MVA
fig_hist <- dat_summ_opl %>%
  add_row(dat_summ_opl %>% mutate(
    feature = factor('SVE')
  )) %>%
  mutate(
    feature = factor(feature, levels = c('OPL', 'UKR', 'SVE'))
  ) %>%
  group_by(trans, measure, side, feature) %>%
  mutate(mean.v.100 = mean(amp.v.100)) %>%
  ungroup() %>%
  ggplot(aes(x=amp.v.100)) +
  facet_grid(. ~ feature, scales = 'free_x') +
  geom_histogram(aes(fill=side),
                 bins = 6, alpha = 0.9, boundary = 0,
                 color="black",  position = 'identity'
                 ) +
  geom_vline(aes(xintercept=mean.v.100), linetype="dashed", size=0.8) +
  scale_y_continuous(
    name = 'Broj mjernih točaka',
    breaks = seq(0, 50, 2),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    breaks = seq(0, 6000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_manual(
    name = 'Strana:',
    values = c("dodgerblue3", "darksalmon")
  ) +
  # ggtitle("Razdioba amplituda - kotao 110MVA") +
  theme_bw() + theme(
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 7),
    axis.title = element_text(size = 7),
    axis.text.x = element_text(colour="black", size = 5.5),
    axis.text.y = element_text(colour="black", size = 5.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 1.5, size = 11)
  )
# ggsave(filename = 'izvjestaj_final/slike/histogram_oplata_110MVA.png',
#        width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)


## Q-Q plot oplata 110MVA
prepare_Q_Q_pair <- function(sx, sy) {
  # Approximate values if vector have different lenghts
  sx <- sort(sx)
  sy <- sort(sy)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx)
    sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx)
    sy <- approx(1L:leny, sy, n = lenx)$y
  # Standardize vectors
  sx.stand <- (sx - mean(sx)) / sd(sx)
  sy.stand <- (sy - mean(sy)) / sd(sy)
  # Normalize vectors
  sx.norm <- (sx - min(sx) * 0.999) / (max(sx) * 1.001 - min(sx) * 0.999)
  sy.norm <- (sy - min(sy) * 0.999) / (max(sy) * 1.001 - min(sy) * 0.999)
  return(
    list(
      sx = sx, sy = sy,
      sx.norm = sx.norm, sy.norm = sy.norm,
      sx.stand = sx.stand, sy.stand = sy.stand
      )
    )
}

dat_summ_opl_q_q <-
  dat_summ_opl %>%
  select(-point.id, -x, -y) %>%
  group_by(feature) %>%
  mutate(
    ids = 1:n()
  ) %>%
  pivot_wider(names_from = feature, values_from = amp.v.100) %>%
  unite('OPL_UKR', 'OPL', 'UKR', sep = '_', remove = F) %>%
  select(-UKR, -OPL, -measure) %>%
  pivot_longer(cols = c('OPL_UKR'),
               names_to = 'measure',
               values_to = 'x_y') %>%
  separate(col = x_y, into = c('x', 'y'), sep = '_') %>%
  select(-ids) %>% 
  mutate(
    trans = factor(trans),
    side = factor(side, levels = c('NN')),
    measure = factor(measure,
                     levels = c('OPL_UKR', 'OPL_SVE', 'UKR_SVE'))
  ) %>% arrange(trans, side, measure) %>%
  group_by(trans, side, measure) %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  )
  
dat_summ_opl_q_q <- tibble(
    trans = unique(dat_summ_opl_q_q$trans),
    side = unique(dat_summ_opl_q_q$side),
    measure = unique(dat_summ_opl_q_q$measure),
    x = prepare_Q_Q_pair(dat_summ_opl_q_q$x, dat_summ_opl_q_q$y)$sx.norm,
    y = prepare_Q_Q_pair(dat_summ_opl_q_q$x, dat_summ_opl_q_q$y)$sy.norm,
  )
  
fig_qq <- dat_summ_opl_q_q %>%
  ggplot(aes(x=x, y=y, fill = side)) +
  facet_grid(. ~ measure, scales = 'fixed') +
  coord_fixed() + 
  geom_abline(slope = 1, size = 0.8)+
  geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  scale_y_continuous(
    name = 'Normalizirana amplituda brzine',
    # breaks = seq(0, 50, 1),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = 'Normalizirana amplituda brzine',
    # breaks = seq(0, 2000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_manual(
    name = 'Strana',
    values = c("dodgerblue3", "darksalmon")
  ) +
  # ggtitle("Q-Q plot - 110MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'none',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(size = 7),
    axis.text.x = element_text(colour="black", size = 5.5),
    axis.text.y = element_text(colour="black", size = 5.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
# ggsave(filename = 'izvjestaj_final/slike/q-q_oplata_110MVA.png',
#        width = 8, height = 8, units = 'cm', dpi = 320, pointsize = 12)


plot_grid(
  ggdraw() +
    draw_label(
      "Razdioba amplituda - 110MVA - Oplata",
      fontface = 'plain',
      size = 12,
      x = 0.52,
      y = 0.8,
      hjust = 0.5
      ),
  plot_grid(
    fig_hist,
    fig_qq,
    # nrow = 1,
    rel_widths = c(1.6, 1),
    labels = c(
      'Histogram razdiobe',
      'Q - Q plot'
      ),
    label_y = c(1.07, 1.07),
    label_x = c(0.2, 0.3),
    label_size = 11,
    label_fontface = 'plain'
    ),
  ncol = 1,
  rel_heights = c(0.15, 1)
)
ggsave(filename = 'izvjestaj_final/slike/histogram_qq_oplata_110MVA.png',
       width = 16, height = 6.5, units = 'cm', dpi = 320, pointsize = 12)

# Check distribution
dist_fit_data <-
  dat_summ_opl %>%
  group_by(trans, side, measure, feature) %>%
  select(trans, side, measure, point.id, feature, amp.v.100) %>%
  arrange(trans, side, measure, feature) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx.norm,
  )

dist_characteristics <- dist_fit_data %>%
  summarise(
    min.dist = descdist(amp.v.100, graph = F)$min,
    max.dist = descdist(amp.v.100, graph = F)$max,
    median.dist = descdist(amp.v.100, graph = F)$median,
    mean.dist = descdist(amp.v.100, graph = F)$mean,
    sd.dist = descdist(amp.v.100, graph = F)$sd,
    skewness.dist = descdist(amp.v.100, graph = F)$skewness,
    kurtosis.dist = descdist(amp.v.100, graph = F)$kurtosis,
  )

dist_characteristics %>% 
  write_csv2('izvjestaj_final/dist_characteristics_oplata_110MVA.csv')

## Plot distributions
dist_fit <- dist_fit_data %>%
  ungroup() %>%
  nest_by(trans, side, measure) %>%
  mutate(
    fit.plot = list(descdist(data$amp.v.100, boot = 1000, graph = T)),
    fit.beta = list(fitdist(
      data = data$amp.v.100, distr = 'beta', method = 'mme', lower = c(0, 0),
      start = list(shape1 = 0.6, shape2 = 1.2)
      ))
  ) %>% select(-fit.plot)

dist_fit <-
  dist_fit %>%
  unnest(data) %>% ungroup() %>%
  select(-measure, -amp.v.100, -point.id) %>%
  unique() %>%
  group_by(feature) %>%
  summarise(
    plot = map(fit.beta, function(x) {plot(x)}),
    distname = unlist(map(fit.beta, function(x) {x$distname})),
    aic = unlist(map(fit.beta, function(x) {x$aic})),
    bic = unlist(map(fit.beta, function(x) {x$bic})),
    shape1 = unlist(map(fit.beta, function(x) {x$estimate[[1]]})),
    shape2 = unlist(map(fit.beta, function(x) {x$estimate[[2]]})),
  ) %>% select(-plot)

dist_fit %>% 
  write_csv2('izvjestaj_final/dist_fit_oplata_110MVA.csv')

theor_quant_func <- function(quantiles, feature, dist_fit) {
  s = unique(feature)
  shape1 = dist_fit %>% filter(feature == s) %>% pull(shape1)
  shape2 = dist_fit %>% filter(feature == s) %>% pull(shape2)
  dbeta(quantiles, shape1, shape2)
  # rbeta(length(quantiles), shape1, shape2)
}

# Distribution density plot
dist_fit_dens <- dat_summ_opl %>% 
  group_by(trans, side, measure, feature) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx.norm,
    ) %>%
  group_by(trans, side, measure, feature) %>%
  mutate(
    mean.v.100 = mean(amp.v.100)
  ) %>%
  ungroup() %>% select(-trans, -point.id, -measure) %>%
  arrange(feature) %>% group_by(feature) %>%
  mutate(
    quantiles = seq(0.04, 0.98, length.out = n()),
    theor_quant = map_dbl(quantiles, theor_quant_func, feature, dist_fit)
  ) %>%
  ggplot(aes(x = amp.v.100)) +
  facet_wrap(. ~ feature, scales = 'free') +
  geom_histogram(aes(y = ..density.., fill=feature),
                 breaks = seq(0, 1.01, 0.2), alpha = 0.9, center = 0,
                 color="black",  position = 'identity', closed = 'left'
                 ) +
  geom_vline(aes(xintercept=mean.v.100), linetype="dashed", size=0.8) +
  geom_line(
    aes(y = theor_quant, x = quantiles, color = 'Modelirana gustoća'),
    position = 'identity',
    size = 0.8
  ) +
  scale_y_continuous(
    name = 'Gustoća razdiobe',
    breaks = seq(0, 50, 0.25),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Normalizirana amplituda brzine'),
    breaks = seq(0, 1, 0.20)
    # limits = c(0, NA)
    ) +
  scale_color_grey(
    name = NULL,
    start = 0,
  ) +
  scale_fill_nejm(
    name = 'Značajka:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  # ggtitle("Modelirana razdioba amplituda - 110MVA - Namotaji") +
  theme_bw() + theme(
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 7),
    axis.title = element_text(size = 7),
    axis.text.x = element_text(colour="black", size = 5.5),
    axis.text.y = element_text(colour="black", size = 5.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 1.5, size = 11)
  )

dist_fit_dens
# Distribution parameters for Q - Q plot
dist_fit_qq_params = list(
  shape1 = dist_fit %>% filter(feature == 'OPL') %>% pull(shape1),
  shape2 = dist_fit %>% filter(feature == 'OPL') %>% pull(shape2)
  )

# Q - Q plot for modeled distribution check
dist_fit_qq <- dat_summ_opl %>% 
  group_by(trans, side, measure, feature) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx.norm,
    ) %>%
  ungroup() %>% select(-trans, -point.id, -measure, side) %>%
  group_by(feature) %>%
  ggplot(aes(sample=amp.v.100, fill = feature)) +
  facet_grid(. ~ feature, scales = 'fixed') +
  coord_fixed() + 
  geom_abline(slope = 1, size = 0.8) +
  geom_qq(
    shape = 21, colour = "black", size = 1.5, stroke = 0.7,
    distribution = qbeta,
    dparams =  list(
      shape1 = dist_fit_qq_params$shape1,
      shape2 = dist_fit_qq_params$shape2
      )
    ) +
  # geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  scale_y_continuous(
    name = 'Normalizirana amplituda brzine',
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    name = 'Modelirana normalizirana amplituda brzine',
    # breaks = seq(0, 2000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'none',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(size = 7),
    axis.text.x = element_text(colour="black", size = 5.5),
    axis.text.y = element_text(colour="black", size = 5.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )

plot_grid(
  ggdraw() +
    draw_label(
      "Modelirana razdioba amplituda - 110MVA - Oplata - NN strana",
      fontface = 'plain',
      size = 12,
      x = 0.52,
      y = 0.8,
      hjust = 0.5
      ),
  plot_grid(
    dist_fit_dens,
    dist_fit_qq,
    # nrow = 1,
    rel_widths = c(1, 1.1),
    labels = c(
      'Gustoća razdiobe',
      'Q - Q plot'
      ),
    label_y = c(1.07, 1.07),
    label_x = c(0.15, 0.35),
    label_size = 11,
    label_fontface = 'plain'
    ),
  ncol = 1,
  rel_heights = c(0.15, 1)
)
ggsave(filename = 'izvjestaj_final/slike/qq_model_distribucija_oplata_110MVA.png',
       width = 16, height = 6, units = 'cm', dpi = 320, pointsize = 12)

# DoE scatter plot
dat_summ_opl %>%
  group_by(feature) %>%
  mutate(
    mean.v.100 = mean(amp.v.100)
  ) %>%
  rename(
    'Širina x' = x,
    'Visina y' = y
  ) %>%
  pivot_longer(
    cols = c('Širina x', 'Visina y'),
    names_to = 'coord_name',
    values_to = 'coord_value'
  ) %>%
  ggplot(aes(x = coord_value, y = amp.v.100,
             fill = feature
             )
         ) +
  facet_grid(feature ~ coord_name, scales = 'free') +
  geom_hline(aes(yintercept=mean.v.100), linetype="dashed", size=0.8) +
  geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  geom_smooth(method = 'loess', formula = y ~ x, span = 1.0) +
  scale_y_continuous(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 2000, 200),
    # limits = c(0, 1)
  ) +
  scale_x_continuous(
    name = 'Koordinate [mm]',
    breaks = seq(0, 6000, 500)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Značajka:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Rasipanje po koordinatama - 110MVA - Oplata - NN strana") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 9),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 12)
  )
ggsave(filename = 'izvjestaj_final/slike/rasipanje_oplata_110MVA.png',
       width = 16, height = 7, units = 'cm', dpi = 320, pointsize = 12)

################################################################################
############################ Oplata 2MVA #######################################
################################################################################

dat_summ_opl <- dat_summ %>%
  select(trans, measure, side, point.id, x, y, amp.v.100) %>%
  filter(trans == '2MVA' & str_detect(measure, 'oplata'))  %>%
  mutate(
    feature = case_when(
      measure == 'oplata_mod_ukr' ~ 'UKR',
      TRUE ~ 'OPL'
    ),
    measure = case_when(
      measure == 'oplata_mod_opl' ~ 'oplata_mod',
      measure == 'oplata_mod_ukr' ~ 'oplata_mod',
      TRUE ~ measure
    )
  ) %>%
  mutate(
    trans = factor(trans),
    measure = factor(measure, levels = c('oplata', 'oplata_mod')),
    side = factor(side, levels = c('VN', 'NN', 'BL', 'BD', 'UBL', 'UBD')),
    feature = factor(feature, levels = c('OPL', 'UKR')),
  ) %>%
  select(trans, measure, feature, side, point.id, x, y, amp.v.100) %>%
  arrange(trans, measure, feature, side, point.id)

dat_summ_opl %>% group_by(trans, measure, feature, side) %>%
  summarise(
    count = n()
  )

# Histogram usporedba oplata 2MVA po stranama (VN, NN, BL, BD)
# prije i nakon modifikacije
dat_summ_opl %>%
  filter(feature == 'OPL') %>%
  select(-feature) %>%
  mutate(
    measure = as.character(measure),
    measure = case_when(
      measure == 'oplata' ~ 'Kotao',
      measure == 'oplata_mod' ~ 'Modificirani kotao',
      T ~ measure
    )
  ) %>%
  group_by(trans, measure, side) %>%
  mutate(mean.v.100 = mean(amp.v.100)) %>%
  ungroup() %>%
  ggplot(aes(x=amp.v.100)) +
  facet_wrap(measure ~ side, scales = 'free_x', nrow = 2) +
  geom_histogram(aes(fill=side),
                 bins = 4, alpha = 0.9, boundary = 0.0,
                 color="black",  position = 'identity', closed = 'right'
                 ) +
  geom_vline(aes(xintercept=mean.v.100), linetype="dashed", size=0.8) +
  scale_y_continuous(
    name = 'Broj mjernih točaka',
    breaks = seq(0, 50, 2),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    # breaks = seq(0, 2000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Razdioba amplituda po stranama - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(colour="black", size = 6.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 6.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/histogram_oplata_2MVA_strane.png',
       width = 16, height = 11, units = 'cm', dpi = 320, pointsize = 12)

# Check distribution
dist_fit_data <- dat_summ_opl %>%
  filter(feature == 'OPL') %>%
  select(-feature) %>%
  group_by(trans, measure, side) %>%
  select(trans, measure, side, point.id, amp.v.100) %>%
  arrange(trans, measure, side) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx.norm,
  )

dist_characteristics <- dist_fit_data %>%
  summarise(
    min.dist = descdist(amp.v.100, graph = F)$min,
    max.dist = descdist(amp.v.100, graph = F)$max,
    median.dist = descdist(amp.v.100, graph = F)$median,
    mean.dist = descdist(amp.v.100, graph = F)$mean,
    sd.dist = descdist(amp.v.100, graph = F)$sd,
    skewness.dist = descdist(amp.v.100, graph = F)$skewness,
    kurtosis.dist = descdist(amp.v.100, graph = F)$kurtosis,
  )

dist_characteristics %>%
  # View()
  write_csv2('izvjestaj_final/dist_characteristics_oplata_2MVA.csv')

## Q-Q plot oplata 2MVA
dat_summ_opl_q_q_combs <-
  dat_summ_opl %>%
  filter(feature == 'OPL') %>%
  select(-feature, -x, -y) %>%
  pivot_wider(names_from = c('measure', 'side'), values_from = amp.v.100) %>%
  select(-trans, -point.id) %>%
  names()

# combinations(length(dat_summ_opl_q_q_combs), 2, dat_summ_opl_q_q_combs)
#   unite('oplata_BD-oplata_BL', 'oplata_BD', 'oplata_BL', sep = '_', remove = F)

dat_summ_opl_q_q <- dat_summ_opl %>%
  filter(feature == 'OPL') %>%
  select(-feature, -x, -y) %>%
  pivot_wider(names_from = c('measure', 'side'), values_from = amp.v.100) %>%
  unite('oplata_BD-\noplata_BL',    'oplata_BD',     'oplata_BL', sep = '_', remove = F) %>%
  unite('oplata_BD-\noplata_mod_BD', 'oplata_BD',     'oplata_mod_BD', sep = '_', remove = F) %>%
  unite('oplata_BD-\noplata_mod_BL', 'oplata_BD',     'oplata_mod_BL', sep = '_', remove = F) %>%
  unite('oplata_BD-\noplata_mod_NN', 'oplata_BD',     'oplata_mod_NN', sep = '_', remove = F) %>%
  unite('oplata_BD-\noplata_mod_VN', 'oplata_BD',     'oplata_mod_VN', sep = '_', remove = F) %>%
  unite('oplata_BD-\noplata_NN'    , 'oplata_BD',     'oplata_NN', sep = '_', remove = F) %>%
  unite('oplata_BD-\noplata_VN'    , 'oplata_BD',     'oplata_VN', sep = '_', remove = F) %>%
  unite('oplata_BL-\noplata_mod_BD', 'oplata_BL',     'oplata_mod_BD', sep = '_', remove = F) %>%
  unite('oplata_BL-\noplata_mod_BL', 'oplata_BL',     'oplata_mod_BL', sep = '_', remove = F) %>%
  unite('oplata_BL-\noplata_mod_NN', 'oplata_BL',     'oplata_mod_NN', sep = '_', remove = F) %>%
  unite('oplata_BL-\noplata_mod_VN', 'oplata_BL',     'oplata_mod_VN', sep = '_', remove = F) %>%
  unite('oplata_BL-\noplata_NN'    , 'oplata_BL',     'oplata_NN', sep = '_', remove = F) %>%
  unite('oplata_BL-\noplata_VN'    , 'oplata_BL',     'oplata_VN', sep = '_', remove = F) %>%
  unite('oplata_mod_BD-\noplata_mod_BL', 'oplata_mod_BD', 'oplata_mod_BL', sep = '_', remove = F) %>%
  unite('oplata_mod_BD-\noplata_mod_NN', 'oplata_mod_BD', 'oplata_mod_NN', sep = '_', remove = F) %>%
  unite('oplata_mod_BD-\noplata_mod_VN', 'oplata_mod_BD', 'oplata_mod_VN', sep = '_', remove = F) %>%
  unite('oplata_mod_BD-\noplata_NN'    , 'oplata_mod_BD', 'oplata_NN', sep = '_', remove = F) %>%
  unite('oplata_mod_BD-\noplata_VN'    , 'oplata_mod_BD', 'oplata_VN', sep = '_', remove = F) %>%
  unite('oplata_mod_BL-\noplata_mod_NN', 'oplata_mod_BL', 'oplata_mod_NN', sep = '_', remove = F) %>%
  unite('oplata_mod_BL-\noplata_mod_VN', 'oplata_mod_BL', 'oplata_mod_VN', sep = '_', remove = F) %>%
  unite('oplata_mod_BL-\noplata_NN'    , 'oplata_mod_BL', 'oplata_NN', sep = '_', remove = F) %>%
  unite('oplata_mod_BL-\noplata_VN'    , 'oplata_mod_BL', 'oplata_VN', sep = '_', remove = F) %>%
  unite('oplata_mod_NN-\noplata_mod_VN', 'oplata_mod_NN', 'oplata_mod_VN', sep = '_', remove = F) %>%
  unite('oplata_mod_NN-\noplata_NN'    , 'oplata_mod_NN', 'oplata_NN', sep = '_', remove = F) %>%
  unite('oplata_mod_NN-\noplata_VN'    , 'oplata_mod_NN', 'oplata_VN', sep = '_', remove = F) %>%
  unite('oplata_mod_VN-\noplata_NN'    , 'oplata_mod_VN', 'oplata_NN', sep = '_', remove = F) %>%
  unite('oplata_mod_VN-\noplata_VN'    , 'oplata_mod_VN', 'oplata_VN', sep = '_', remove = F) %>%
  unite('oplata_NN-\noplata_VN'    , 'oplata_NN',     'oplata_VN', sep = '_', remove = F) %>%
  select(
      -c("oplata_VN", "oplata_NN", "oplata_BL", "oplata_BD", "oplata_mod_VN",
        "oplata_mod_NN", "oplata_mod_BL", "oplata_mod_BD")
    ) %>%
  pivot_longer(cols = -c('trans', 'point.id'),
               names_to = 'measure',
               values_to = 'x_y') %>%
  separate(col = x_y, into = c('x', 'y'), sep = '_') %>%
  select(-point.id) %>%
  mutate(
    trans = factor(trans),
  ) %>%
  arrange(trans, measure) %>%
  group_by(trans, measure) %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>%
  summarise(
    trans = unique(trans),
    measure = unique(measure),
    x = prepare_Q_Q_pair(x, y)$sx.stand,
    y = prepare_Q_Q_pair(x, y)$sy.stand,
  )


dat_summ_opl_q_q %>%
  # filter(
  #   str_detect(measure, 'oplata_mod_BL') | str_detect(measure, 'oplata_mod_BD')
  #   ) %>%
  ggplot(aes(x=x, y=y, fill = trans)) +
  facet_wrap(. ~ measure, scales = 'fixed', nrow = 4) +
  coord_fixed() +
  geom_abline(slope = 1, size = 0.8)+
  geom_point(shape = 21, colour = "black", size = 1.0, stroke = 0.5) +
  scale_y_continuous(
    name = 'Standardizirana amplituda brzine',
    # breaks = seq(0, 5, 0.2),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = 'Standardizirana amplituda brzine',
    # breaks = seq(0, 5, 0.2)
    # limits = c(0, NA)
    ) +
  scale_fill_manual(
    name = 'Strana',
    values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Q-Q plot - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'none',
    panel.background = element_blank(),
    panel.spacing.x = unit(0.5, 'mm'),
    panel.spacing.y = unit(0.5, "mm"),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    strip.text = element_text(
      colour="black", size = 4.5,
      margin = margin(0.5, 0, 0.5, 0, unit = 'mm')
      ),
    axis.title = element_text(size = 8),
    axis.text.x = element_text(colour="black", size = 4.5),
    axis.text.y = element_text(colour="black", size = 4.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/q-q_oplata_2MVA_sve.png',
       width = 16, height = 12.5, units = 'cm', dpi = 320, pointsize = 12)


## Fit distributions
dist_fit <- dist_fit_data %>%
  ungroup() %>%
  nest_by(trans) %>%
  mutate(
    fit.plot = list(descdist(data$amp.v.100, boot = 1000, graph = F)),
    fit.beta = list(fitdist(
      data = data$amp.v.100, distr = 'beta', method = 'mme', lower = c(0, 0),
      start = list(shape1 = 0.6, shape2 = 1.2)
      ))
  ) %>% select(-fit.plot)

dist_fit <-
  dist_fit %>%
  unnest(data) %>% ungroup() %>%
  select(-amp.v.100, -point.id, -side) %>%
  unique() %>%
  mutate(
    measure = paste0(measure, collapse = '+')
    ) %>%
  unique() %>%
  group_by(trans, measure) %>%
  summarise(
    plot = map(fit.beta, function(x) {plot(x)}),
    distname = unlist(map(fit.beta, function(x) {x$distname})),
    aic = unlist(map(fit.beta, function(x) {x$aic})),
    bic = unlist(map(fit.beta, function(x) {x$bic})),
    shape1 = unlist(map(fit.beta, function(x) {x$estimate[[1]]})),
    shape2 = unlist(map(fit.beta, function(x) {x$estimate[[2]]})),
  ) %>% select(-plot) %>% ungroup()

dist_fit %>% 
  write_csv2('izvjestaj_final/dist_fit_oplata_2MVA_sve.csv')

theor_quant_func <- function(quantiles, dist_fit) {
  shape1 = dist_fit %>% pull(shape1)
  shape2 = dist_fit %>% pull(shape2)
  dbeta(quantiles, shape1, shape2)
  # rbeta(length(quantiles), shape1, shape2)
}

# Distribution density plot
dat_summ_opl %>%
  filter(feature == 'OPL') %>%
  select(-feature) %>%
  mutate(
    measure = as.character(measure),
    measure = case_when(
      measure == 'oplata' ~ 'Kotao',
      measure == 'oplata_mod' ~ 'Modificirani kotao',
      T ~ measure
    )
  ) %>%
  group_by(trans, measure, side) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx.norm,
    mean.v.100 = mean(amp.v.100)
    ) %>%
  ungroup() %>%
  select(-trans, -point.id, -x, -y) %>%
  arrange(measure, side) %>% group_by(measure, side) %>%
  mutate(
    quantiles = seq(0.06, 0.98, length.out = n()),
    theor_quant = map_dbl(quantiles, theor_quant_func, dist_fit)
  ) %>%
  ggplot(aes(x=amp.v.100)) +
  facet_grid(measure ~ side, scales = 'free_y') +
  geom_histogram(aes(y = ..density.., fill=side),
                 breaks = seq(0, 1.08, 0.27),
                 alpha = 0.9,
                 color="black",  position = 'identity', closed = 'left'
                 ) +
  geom_vline(aes(xintercept=mean.v.100), linetype="dashed", size=0.8) +
  geom_line(
    aes(y = theor_quant, x = quantiles, color = 'Modelirana gustoća'),
    position = 'identity',
    size = 0.8
  ) +
  scale_y_continuous(
    name = 'Gustoća razdiobe',
    breaks = seq(0, 10, 0.5),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Normalizirana amplituda brzine'),
    breaks = seq(0, 1, 0.25)
    # limits = c(0, NA)
    ) +
  scale_color_grey(
    name = NULL,
    start = 0,
  ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Modelirana razdioba amplituda - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(colour="black", size = 6.5),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 6.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/model_distribucija_oplata_2MVA_sve.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# Distribution parameters for Q - Q plot
dist_fit_qq_params = list(
  shape1 = dist_fit %>% pull(shape1),
  shape2 = dist_fit %>% pull(shape2)
  )

# Q - Q plot for modeled distribution check
dat_summ_opl %>% 
  filter(feature == 'OPL') %>%
  select(-feature) %>%
  mutate(
    measure = as.character(measure),
    measure = case_when(
      measure == 'oplata' ~ 'Kotao',
      measure == 'oplata_mod' ~ 'Modificirani kotao',
      T ~ measure
    )
  ) %>%
  group_by(trans, measure, side) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx.norm
    ) %>%
  ungroup() %>%
  ggplot(aes(sample=amp.v.100, fill = side)) +
  facet_grid(measure ~ side, scales = 'fixed') +
  coord_fixed() + 
  geom_abline(slope = 1, size = 0.8) +
  geom_qq(
    shape = 21, colour = "black", size = 1.5, stroke = 0.7,
    distribution = qbeta,
    dparams =  list(
      shape1 = dist_fit_qq_params$shape1,
      shape2 = dist_fit_qq_params$shape2
      )
    ) +
  # geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  scale_y_continuous(
    name = 'Normalizirana amplituda brzine',
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    name = 'Modelirana normalizirana amplituda brzine',
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1)
    ) +
  scale_fill_nejm(
    name = 'Strana',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle('Q - Q plot - Modelirana razdioba - 2MVA - Oplata') +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing.x = unit(2.5, 'mm'),
    panel.spacing.y = unit(1, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(
      colour="black", size = 6.5,
      margin = margin(0.5, 1.5, 0.5, 1.5, unit = 'mm')
      ),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 6.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/qq_model_distribucija_oplata_2MVA_sve.png',
       width = 16, height = 9, units = 'cm', dpi = 320, pointsize = 12)

# DoE scatter plot
dat_summ_opl %>%
  filter(feature == 'OPL') %>%
  select(-feature) %>%
  mutate(
    measure = as.character(measure),
    measure = case_when(
      measure == 'oplata' ~ 'Kotao',
      measure == 'oplata_mod' ~ 'Modificirani kotao',
      T ~ measure
    )
  ) %>%
  group_by(trans, measure, side) %>%
  mutate(
    mean.v.100 = mean(amp.v.100)
  ) %>%
  rename(
    'Širina x' = x,
    'Visina y' = y
  ) %>%
  pivot_longer(
    cols = c('Širina x', 'Visina y'),
    names_to = 'coord_name',
    values_to = 'coord_value'
  ) %>%
  # filter(side == 'NN') %>%
  ggplot(aes(x = coord_value, y = amp.v.100, fill = side)) +
  facet_grid(side + measure ~ coord_name, scales = 'free') +
  geom_hline(aes(yintercept=mean.v.100), linetype="dashed", size=0.8) +
  geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  geom_smooth(method = 'loess', formula = y ~ x, span = 1.0) +
  scale_y_continuous(
    name = expression('Amplituda ['*mu*'m/s]'),
    breaks = seq(0, 2000, 250),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = 'Koordinate [mm]',
    breaks = seq(0, 3000, 200),
    limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Rasipanje po koordinatama - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing.x = unit(1, 'mm'),
    panel.spacing.y = unit(1, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(
      colour="black", size = 7.0,
      margin = margin(1.0, 1.0, 1.0, 1.0, unit = 'mm')
      ),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 7.0),
    axis.text.y = element_text(colour="black", size = 7.0),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/rasipanje_oplata_2MVA.png',
       width = 16, height = 20, units = 'cm', dpi = 320, pointsize = 12)

# Correlation analysis
dat_summ_opl %>% 
  group_by(trans, side, measure) %>%
  summarise(
    count = n()
  )

fig_corr <- dat_summ_opl %>%
  filter(feature == 'OPL') %>%
  select(-feature) %>%
  mutate(
    measure = as.character(measure),
    measure = case_when(
      measure == 'oplata' ~ 'Kotao',
      measure == 'oplata_mod' ~ 'Modificirani kotao',
      T ~ measure
    )
  ) %>%
  select(-x, -y) %>%
  mutate(
    areas = case_when(
      side %in% c('VN', 'NN') ~ 'big',
      side %in% c('BL', 'BD') ~ 'small',
      T ~ 'not ok'
    )
  ) %>%
  pivot_wider(
    names_from = c(side, measure),
    values_from = amp.v.100
    ) %>%
  select(-trans, -point.id) %>%
  nest_by(areas) %>%
  mutate(
    plot =  list(ggcorrm(data = data %>% select(!where(~all(is.na(.x))))) +
  lotri(
    geom_smooth(method = "lm", se = T, formula = y ~ x)
    ) +
  lotri(
    geom_point(alpha = 0.5)
    ) +
  utri_corrtext(
    nrow = 1, squeeze = 0.3, corr_method = 'pearson', corr_size = F
    ) +
  dia_names(
    y_pos = 0.10, size = 1.6
    ) +
  dia_histogram(
    lower = 0.25, upper = 0.90, color = 1, fill = 'grey80', bins = 4,
    position = 'identity', boundary = 0.0
    ) +
  # ggtitle('Korelacijske matrica - 2MVA - Oplata - VN, NN') + 
  scale_x_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]')
  ) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]')
  ) +
  theme(
    text = element_text(size = 5.2),
    axis.title.x = element_text(size = 4.5),
    axis.title.y = element_text(size = 4.5),
    legend.position = 'top',
    panel.background = element_blank(),
    # panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 9),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(colour="black", size = 3.5),
    axis.text.y = element_text(colour="black", size = 3.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 12)
      )
  ))

# fig_corr$plot[2]

plot_grid(
  ggdraw() +
  draw_label(
    'Korelacijske matrice - 2MVA - Oplata',
    fontface = 'plain',
    size = 12,
    x = 0.5,
    y = 0.45,
    hjust = 0.5
    ),
  plot_grid(
    fig_corr$plot[[1]],
    fig_corr$plot[[2]],
    nrow = 1,
    rel_widths = c(1.0, 1.0)
    # labels = c(
    #   'Gustoća razdiobe',
    #   'Q - Q plot'
    #   ),
    # label_y = c(1.07, 1.07),
    # label_x = c(0.15, 0.35),
    # label_size = 11,
    # label_fontface = 'plain'
    ),
  ncol = 1,
  rel_heights = c(0.07, 1)
)

ggsave(filename = 'izvjestaj_final/slike/corr_matrix_oplata_2MVA_sve.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

## DoE mean plot
dat_summ_opl_v_x <- dat_summ %>%
  select(
    trans, measure, side, point.id, x, y, amp.v.100, amp.x.100, rms, ptp,
    vel.min, vel.max
    ) %>%
  filter(trans == '2MVA' & str_detect(measure, 'oplata'))  %>%
  mutate(
    feature = case_when(
      measure == 'oplata_mod_ukr' ~ 'UKR',
      TRUE ~ 'OPL'
    ),
    measure = case_when(
      measure == 'oplata_mod_opl' ~ 'oplata_mod',
      measure == 'oplata_mod_ukr' ~ 'oplata_mod',
      TRUE ~ measure
    )
  ) %>%
  mutate(
    trans = factor(trans),
    measure = factor(measure, levels = c('oplata', 'oplata_mod')),
    side = factor(side, levels = c('VN', 'NN', 'BL', 'BD', 'UBL', 'UBD')),
    feature = factor(feature, levels = c('OPL', 'UKR')),
  ) %>%
  select(
    trans, measure, feature, side, point.id, x, y, amp.v.100, amp.x.100, rms,
    ptp, vel.min, vel.max
    ) %>%
  arrange(trans, measure, feature, side, point.id) %>%
  filter(feature == 'OPL') %>%
  select(-feature) %>%
  mutate(
    measure = as.character(measure),
    measure = case_when(
      measure == 'oplata' ~ 'Kotao',
      measure == 'oplata_mod' ~ 'Modificirani kotao',
      T ~ measure
    ),
    measure = factor(measure)
  )

dat_summ_opl_v_x %>%
  group_by(trans, measure, side) %>%
  summarise(
    across(
      .cols = c(amp.v.100, amp.x.100),
      .fns = list(mean = mean, median = median, max = max)
           )
    ) %>%
  rename(
   'Brzina\n Prosjek' = amp.v.100_mean,
   'Brzina\n Medijan' = amp.v.100_median,
   'Brzina\n Max' = amp.v.100_max,
   'Pomak\n Prosjek' = amp.x.100_mean,
   'Pomak\n Medijan' = amp.x.100_median,
   'Pomak\n Max' = amp.x.100_max
  ) %>%
  pivot_longer(
    cols = contains(c('Prosjek', 'Medijan', 'Max'))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = measure, y = value, fill = side, group = side)) +
  facet_grid(name ~ side, scales = 'free') +
  geom_line(
    color = 'black',
    linetype = '1232',
    size = 0.8
    ) +
  geom_point(
    shape = 21, colour = "black", size = 1.0, stroke = 1
    ) +
  scale_y_continuous(
    name = expression('Pomak ['*mu*'m]  /  Brzina ['*mu*'m/s]')
    # breaks = seq(0, 2000, 250),
    # limits = c(0, NA)
  ) +
  scale_x_discrete(
    name = NULL,
    expand = expansion(0, 0.2)
    # breaks = seq(0, 3000, 200),
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Dijagram prosjeka prije i nakon modifikacije - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing.x = unit(1, 'mm'),
    panel.spacing.y = unit(1.5, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(
      colour="black", size = 7.0,
      margin = margin(1.0, 1.0, 1.0, 1.0, unit = 'mm')
      ),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.0, hjust = 0.7),
    axis.text.y = element_text(colour="black", size = 5.5, vjust = 0.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/doe_mean_oplata_2MVA.png',
       width = 16, height = 10, units = 'cm', dpi = 320, pointsize = 12)

## DoE dispersion plot
dat_summ_opl_v_x %>%
  group_by(trans, measure, side) %>%
  summarise(
    across(
      .cols = c(amp.v.100, amp.x.100),
      .fns = list(IQR = IQR, mad = mad, sd = sd)
           )
    ) %>%
  rename(
   'Brzina\n IQR' = amp.v.100_IQR,
   'Brzina\n MAD' = amp.v.100_mad,
   'Brzina\n SD' = amp.v.100_sd,
   'Pomak\n IQR' = amp.x.100_IQR,
   'Pomak\n MAD' = amp.x.100_mad,
   'Pomak\n SD' = amp.x.100_sd
  ) %>%
  pivot_longer(
    cols = contains(c('IQR', 'mad', 'sd'))
  ) %>%
  ungroup() %>%
  ggplot(aes(x = measure, y = value, fill = side, group = side)) +
  facet_grid(name ~ side, scales = 'free') +
  geom_line(
    color = 'black',
    linetype = '1232',
    size = 0.8
    ) +
  geom_point(
    shape = 21, colour = "black", size = 1.0, stroke = 1
    ) +
  scale_y_continuous(
    name = expression('Pomak ['*mu*'m]  /  Brzina ['*mu*'m/s]')
    # breaks = seq(0, 2000, 250),
    # limits = c(0, NA)
  ) +
  scale_x_discrete(
    name = NULL,
    expand = expansion(0, 0.2)
    # breaks = seq(0, 3000, 200),
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Dijagram raspršenosti prije i nakon modifikacije - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing.x = unit(1, 'mm'),
    panel.spacing.y = unit(1.5, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(
      colour="black", size = 7.0,
      margin = margin(1.0, 1.0, 1.0, 1.0, unit = 'mm')
      ),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.0, hjust = 0.7),
    axis.text.y = element_text(colour="black", size = 5.5, vjust = 0.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/doe_rasprsenost_oplata_2MVA.png',
       width = 16, height = 10, units = 'cm', dpi = 320, pointsize = 12)

## Razlike prije i nakon modifikacija
dat_diff_opl <- dat_summ_opl_v_x %>%
  select(-vel.min, -vel.max, -rms, -ptp) %>%
  mutate(
    measure = as.character(measure),
    measure = case_when(
      measure == 'Kotao' ~ 'oplata',
      measure == 'Modificirani kotao' ~ 'oplata_mod',
      T ~ measure
      ),
    measure = factor(measure)
    ) %>%
  pivot_wider(
    names_from = c(measure),
    values_from = c(amp.v.100, amp.x.100)
    ) %>%
  mutate(
    amp.v.100.diff = amp.v.100_oplata_mod - amp.v.100_oplata,
    amp.v.100.proc = amp.v.100.diff / amp.v.100_oplata,
    amp.x.100.diff = amp.x.100_oplata_mod - amp.x.100_oplata,
    amp.x.100.proc = amp.x.100.diff / amp.x.100_oplata,
  ) %>%
  group_by(trans, side) %>%
  mutate(
    mean.v.100.diff = mean(amp.v.100.diff),
    mean.v.100.proc = mean(amp.v.100.proc),
    mean.x.100.diff = mean(amp.x.100.diff),
    mean.x.100.proc = mean(amp.x.100.proc)
  ) %>%
  ungroup()

# Apsolutna razlika - summary + fitting pravca
dat_diff_opl %>%
  nest_by(trans, side) %>%
  mutate(
    fitDiff.v = list(
      lm(amp.v.100.diff ~ 0 + amp.v.100_oplata, data = data))
    ) %>% summarise(tidy(fitDiff.v)) %>%
  select(-std.error, -statistic) %>%
  rename(nagib_pravca = estimate) %>%
  full_join(
    select(dat_diff_opl, trans, side, starts_with('mean')) %>% unique(),
    by = c('trans', 'side') 
  ) %>%
  write_csv2('izvjestaj_final/2MVA_apsolutna_relativna_razlika_modifikacija.csv')


## Apsolutna razlika brzina i pomaka prije i nakon modifikacije
dat_diff_opl %>%
  select(-x, -y) %>%
  rename(
    'Razlika brzina' = amp.v.100.diff,
    'Razlika pomaka' = amp.x.100.diff
  ) %>%
  pivot_longer(
    cols = c('Razlika brzina', 'Razlika pomaka'),
    names_to = 'measure',
    values_to = 'amp.diff'
  ) %>%
  mutate(
    average.line = case_when(
      measure == 'Razlika brzina' ~ mean.v.100.diff,
      measure == 'Razlika pomaka' ~ mean.x.100.diff,
      T ~ NA_real_
    ),
    amp.opl = case_when(
      measure == 'Razlika brzina' ~ amp.v.100_oplata,
      measure == 'Razlika pomaka' ~ amp.x.100_oplata,
      T ~ NA_real_
    )
  ) %>%
  ggplot(aes(x = amp.opl, y = amp.diff, fill = side)) +
  facet_wrap(measure ~ side, scales = 'free', ncol = 4) +
  geom_hline(aes(yintercept = average.line), linetype="dashed", size=0.6) +
  geom_hline(aes(yintercept = 0), linetype="solid", size=0.2) +
  geom_point(shape = 21, colour = "black", size = 1.0, stroke = 0.7) +
  geom_smooth(method = 'lm', formula = y ~ 0 + x, span = 1.0) +
  scale_y_continuous(
    name = expression('Razlika amplituda pomaka ['*mu*'m] / brzina ['*mu*'m/s]'),
    # breaks = seq(0, 2000, 250),
    # limits = c(-1.5, 1.5)
  ) +
  scale_x_continuous(
    name = expression('Amplitude pomaka ['*mu*'m] / brzina ['*mu*'m/s] prije modifikacije'),
    # breaks = seq(0, 3000, 200),
    limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Apsolutna razlika amplituda nakon modifikacije - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing.x = unit(1, 'mm'),
    panel.spacing.y = unit(1, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(
      colour="black", size = 7.0,
      margin = margin(1.0, 1.0, 1.0, 1.0, unit = 'mm')
      ),
    axis.title = element_text(size = 8),
    axis.text.x = element_text(colour="black", size = 7.0),
    axis.text.y = element_text(colour="black", size = 7.0),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/diff_amplitude_abs_oplata_2MVA.png',
       width = 16, height = 10, units = 'cm', dpi = 320, pointsize = 12)

## Relativna razlika brzina i pomaka prije i nakon modifikacija
dat_diff_opl %>%
  select(-x, -y) %>%
  filter(amp.v.100.proc < 5) %>%
  rename(
    'Razlika brzina/pomaka' = amp.v.100.proc,
  ) %>%
  ggplot(aes(x = amp.v.100_oplata, y = `Razlika brzina/pomaka`, fill = side)) +
  facet_grid(. ~ side, scales = 'free_x') +
  geom_hline(aes(yintercept = mean.v.100.proc), linetype="dashed", size = 0.6) +
  geom_hline(aes(yintercept = 0), linetype="solid", size = 0.2) +
  geom_point(shape = 21, colour = "black", size = 1.0, stroke = 0.7) +
  geom_smooth(method = 'lm', formula = y ~ x, span = 1.0) +
  scale_y_continuous(
    name = expression('Razlika amplituda pomaka / brzina [ / ]'),
    breaks = seq(-2, 2, 0.5),
    limits = c(-1.3, 1.02)
  ) +
  scale_x_continuous(
    name = expression('Amplitude brzina ['*mu*'m/s] prije modifikacije'),
    sec.axis = sec_axis(
      trans = ~ . / 2 / pi / 100,
      name = expression('Amplitude pomaka ['*mu*'m] prije modifikacije'),
      guide = guide_axis(
        position = 'top'
        )
      ),
    # breaks = seq(0, 3000, 200),
    limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:'
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Relativna razlika amplituda nakon modifikacije - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing.x = unit(1, 'mm'),
    panel.spacing.y = unit(1, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(1, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(
      colour="black", size = 7.0,
      margin = margin(1.0, 1.0, 1.0, 1.0, unit = 'mm')
      ),
    axis.title = element_text(size = 8),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.x.top = element_text(colour="black", size = 5.8, hjust = 0.5),
    axis.text.y = element_text(colour="black", size = 7.0),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/diff_amplitude_rel_oplata_2MVA.png',
       width = 16, height = 7, units = 'cm', dpi = 320, pointsize = 12)

################################################################################
################################################################################
# Surface modelling amplitude difference
# Loading summary and adding coordinates for BD, NN and BL from VN
dat_diff_opl %>% glimpse()

dat_surf <- dat_diff_opl %>%
  select(trans, side, point.id, x, y, amp.v.100.diff, amp.x.100.diff) 


# Histogram of amplitudes
dat_surf %>%
  ggplot(aes(x=amp.v.100.diff)) +
  facet_wrap(c('side'), scales = 'free_x') +
  geom_histogram(bins = 4, color='black')

# Histogram smanjenja amplituda oplata 2MVA po stranama nakon modifikacije
dat_surf %>%
  group_by(trans, side) %>%
  mutate(
    mean.v.100.diff = mean(amp.v.100.diff),
    mean.x.100.diff = mean(amp.x.100.diff),
    ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(amp.v.100.diff, amp.x.100.diff),
    names_to = 'measure',
    values_to = 'amp.diff'
  ) %>%
  mutate(
    mean.line = case_when(
      measure == 'amp.v.100.diff' ~ mean.v.100.diff,
      measure == 'amp.x.100.diff' ~ mean.x.100.diff,
      T ~ NA_real_
    ),
    measure = case_when(
      measure == 'amp.v.100.diff' ~ 'Razlika brzina',
      measure == 'amp.x.100.diff' ~ 'Razlika pomaka',
      T ~ NA_character_
    )
  ) %>%
  ggplot(aes(x=amp.diff)) +
  facet_wrap(measure ~ side, scales = 'free_x', nrow = 2) +
  geom_histogram(aes(fill=side),
                 bins = 5, alpha = 0.9, boundary = 0.0,
                 color="black",  position = 'identity', closed = 'right',
                 orientation = 'x'
                 ) +
  geom_vline(aes(xintercept=mean.line), linetype="dashed", size=0.6) +
  geom_vline(aes(xintercept = 0), linetype="solid", size = 0.2) +
  scale_y_continuous(
    name = 'Broj mjernih točaka',
    breaks = seq(0, 50, 2),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Razlika amplituda brzina ['*mu*'m/s] / pomaka ['*mu*'m]'),
    # breaks = seq(0, 2000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Razdioba razlika amplituda po stranama nakon modifikacije - 2MVA - Oplata") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    legend.text = element_text(size = 7),
    legend.key.size = unit(5, 'mm'),
    legend.title = element_text(size = 8),
    strip.text = element_text(
      colour="black", size = 7.0,
      margin = margin(1.0, 1.0, 1.0, 1.0, unit = 'mm')
      ),
    axis.title = element_text(size = 8),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 6.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(size = 10, hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/histogram_diff_oplata_2MVA_strane.png',
       width = 16, height = 10, units = 'cm', dpi = 320, pointsize = 12)

# Plotting coordinates for sanity check
# dat_surf %>%
#   ggplot(aes(x = x, y = y, label = paste0(side, point.id))) + geom_text()

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
dat_surf %>% filter(side == 'BD') %>% select(x, y) %>%
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
      amp = 'amp.v.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = 8000,
      vgm_range = 100,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 0, 1),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/5,
      vgm_map_threshold = 4,
      grid_num = 300
      )

# Creating tibble for contour and points plot
# Modeling velocity
dat_disp_vel <- dat_surf %>%
  select(-amp.x.100.diff, -trans) %>%
  mutate(point.id = as.character(point.id)) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100.diff'),
  y = tibble(
    side = 'VN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'VN'),
      amp = 'amp.v.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 0,
      vgm_an_ratio = 8/8,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100.diff = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100.diff'),
  y = tibble(
    side = 'NN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'NN'),
      amp = 'amp.v.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 750,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 0, 1),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/9,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100.diff = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100.diff'),
  y = tibble(
    side = 'BL',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BL'),
      amp = 'amp.v.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = 40000,
      vgm_range = 100,
      vgm_model = 'Exp',
      vgm_alpha = seq(135, 135, 1),
      vgm_an_angle = 135,
      vgm_an_ratio = 3/5,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100.diff = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.v.100.diff'),
  y = tibble(
    side = 'BD',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BD'),
      amp = 'amp.v.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = 8000,
      vgm_range = 100,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 0, 1),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/5,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.v.100.diff = var1.pred) %>% select(-var1.var)
  )

# Creating tibble for contour and points plot
# Modeling displacement
dat_disp_disp <- dat_surf %>%
  select(-amp.v.100.diff, -trans) %>%
  mutate(point.id = as.character(point.id)) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100.diff'),
  y = tibble(
    side = 'VN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'VN'),
      amp = 'amp.x.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 800,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 135, 45),
      vgm_an_angle = 0,
      vgm_an_ratio = 8/8,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100.diff = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100.diff'),
  y = tibble(
    side = 'NN',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'NN'),
      amp = 'amp.x.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 750,
      vgm_bin_num = 5,
      vgm_psill = NA,
      vgm_range = NA,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 0, 1),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/9,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100.diff = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100.diff'),
  y = tibble(
    side = 'BL',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BL'),
      amp = 'amp.x.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = 40000,
      vgm_range = 100,
      vgm_model = 'Exp',
      vgm_alpha = seq(135, 135, 1),
      vgm_an_angle = 135,
      vgm_an_ratio = 3/5,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100.diff = var1.pred) %>% select(-var1.var)
  ) %>%
  full_join(by = c('side', 'point.id', 'x', 'y', 'amp.x.100.diff'),
  y = tibble(
    side = 'BD',
    point.id = 'model',
    !!!as_tibble(krige.inter(
      amp_df = dat_surf %>% filter(side == 'BD'),
      amp = 'amp.x.100.diff',
      plt_map = F,
      variogram_only = F,
      vgm_cutoff = 700,
      vgm_bin_num = 4,
      vgm_psill = 8000,
      vgm_range = 100,
      vgm_model = 'Exp',
      vgm_alpha = seq(0, 0, 1),
      vgm_an_angle = 0,
      vgm_an_ratio = 5/5,
      vgm_map_threshold = 4,
      grid_num = 300
      ))
    ) %>% rename(amp.x.100.diff = var1.pred) %>% select(-var1.var)
  )

# Plot amplituda pomaka - Strana NN i VN - 100 Hz
dat_disp_disp %>%
  filter(side %in% c('VN', 'NN')) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.x.100.cnt = case_when(
      point.id == 'model' ~ amp.x.100.diff,
      TRUE ~ NA_real_
    ),
    amp.x.100.diff = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.x.100.diff
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
    name = expression('Razlika amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.0,
  ) +
  ggtitle("Razlika amplituda pomaka nakon modifikacije - 2MVA - Oplata VN i NN") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 9),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = 'izvjestaj_final/slike/diff_contour_disp_2MVA_oplata_vn_nn.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

# Plot amplituda pomaka - Strana BL i BD - 100 Hz
dat_disp_disp %>%
  filter(side %in% c('BL', 'BD')) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.x.100.cnt = case_when(
      point.id == 'model' ~  amp.x.100.diff,
      TRUE ~ NA_real_
    ),
    amp.x.100.diff = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.x.100.diff
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
    name = expression('Razlika amplituda ['*mu*'m]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.9
  ) +
  ggtitle("Razlika amplituda pomaka nakon modifikacije - 2MVA - Oplata BL i BD") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 9),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = 'izvjestaj_final/slike/diff_contour_disp_2MVA_oplata_bl_bd.png',
       width = 17, height = 15, units = 'cm', dpi = 320)

# Plot amplituda brzine - Strane VN i NN - 100 Hz
dat_disp_vel %>%
  filter(side %in% c('VN', 'NN')) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.v.100.cnt = case_when(
      point.id == 'model' ~ amp.v.100.diff,
      TRUE ~ NA_real_
    ),
    amp.v.100.diff = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.v.100.diff
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
    name = expression('Razlika amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.0,
  ) +
  ggtitle("Razlika amplituda brzina nakon modifikacije - 2MVA - Oplata VN i NN") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 9),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = 'izvjestaj_final/slike/diff_contour_vel_2MVA_oplata_vn_nn.png',
       width = 25, height = 11, units = 'cm', dpi = 320)

# Plot amplituda brzine - Strane BL i BD - 100 Hz
dat_disp_vel %>%
  filter(side %in% c('BL', 'BD')) %>%
  rename(loc.x = x, loc.y = y) %>%
  mutate(
    amp.v.100.cnt = case_when(
      point.id == 'model' ~ amp.v.100.diff,
      TRUE ~ NA_real_
    ),
    amp.v.100.diff = case_when(
      point.id == 'model' ~ NA_real_,
      TRUE ~ amp.v.100.diff
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
    name = expression('Razlika amplituda ['*mu*'m/s]'),
    option = 'C',
    alpha = 1,
    begin = 0.3,
    end = 0.9
  ) +
  ggtitle("Razlika amplituda brzina nakon modifikacije - 2MVA - Oplata BL i BD") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 9),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = 'izvjestaj_final/slike/diff_contour_vel_2MVA_oplata_bl_bd.png',
       width = 17, height = 15, units = 'cm', dpi = 320)
