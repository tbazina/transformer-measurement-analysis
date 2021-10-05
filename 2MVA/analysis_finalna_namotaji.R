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
library(corrmorant)
# Statistical significance testing
library(car)
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

# Change winding sequence for 110MVA namotja_1 -> namotaj_3 and vice versa
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

# Saving summary of all points
dat_summ %>%
  mutate(
    ukruta = case_when(
      measure %in% c('oplata_ukr', 'oplata_mod_ukr') ~ 'DA',
      measure %in% c('oplata', 'oplata_opl', 'oplata_mod_opl') ~ 'NE',
      T ~ '-'
    ),
    measure = case_when(
      measure %in% c('oplata_ukr', 'oplata', 'oplata_opl') ~ 'kotao',
      measure %in% c('oplata_mod_ukr', 'oplata_mod_opl') ~ 'modificirani kotao',
      measure %in% c('Jezgra Desno', 'Jezgra Lijevo') ~ 'jezgra',
      measure == c('Stega Donja') ~ 'stega donja',
      measure == c('Stega Gornja') ~ 'stega gornja',
      T ~ measure
    )
  ) %>%
  relocate(ukruta, .after = point.id) %>%
  mutate(
    trans = ordered(trans, levels = c('110MVA', '2MVA')),
    measure = ordered(
      measure,
      levels = c(
        'namotaj_1', 'namotaj_2', 'namotaj_3', 'jezgra', 'stega donja',
        'stega gornja', 'kotao', 'modificirani kotao'
        )),
    side = ordered(side, levels = c('VN', 'NN', 'BL', 'BD', 'UBL', 'UBD')),
    ukruta = ordered(ukruta, levels = c('NE', 'DA', '-'))
  ) %>%
  arrange(
    trans, measure, side, point.id, ukruta
  ) %>%
  write_csv2('izvjestaj_final/summary_statistics_per_point.csv')
  

################################################################################
############################# Summary statistics ###############################
################################################################################
# TODO: export number of points
# TODO: export amplitude frequency per sides
dat_summ %>%
  mutate(
    ukruta = case_when(
      measure %in% c('oplata_ukr', 'oplata_mod_ukr') ~ 'DA',
      measure %in% c('oplata', 'oplata_opl', 'oplata_mod_opl') ~ 'NE',
      T ~ '-'
    ),
    measure = case_when(
      measure %in% c('oplata_ukr', 'oplata', 'oplata_opl') ~ 'kotao',
      measure %in% c('oplata_mod_ukr', 'oplata_mod_opl') ~ 'modificirani kotao',
      measure %in% c('Jezgra Desno', 'Jezgra Lijevo') ~ 'jezgra',
      measure == c('Stega Donja') ~ 'stega donja',
      measure == c('Stega Gornja') ~ 'stega gornja',
      T ~ measure
    )
  ) %>%
  relocate(ukruta, .after = point.id) %>%
  mutate(
    trans = ordered(trans, levels = c('110MVA', '2MVA')),
    measure = ordered(
      measure,
      levels = c(
        'namotaj_1', 'namotaj_2', 'namotaj_3', 'jezgra', 'stega donja',
        'stega gornja', 'kotao', 'modificirani kotao'
        )),
    side = ordered(side, levels = c('VN', 'NN', 'BL', 'BD', 'UBL', 'UBD')),
    ukruta = ordered(ukruta, levels = c('NE', 'DA', '-'))
  ) %>%
  arrange(
    trans, measure, side, point.id, ukruta
  ) %>%
  group_by(trans, measure, side, ukruta) %>%
  summarise(
    broj.tocaka = n(),
    rms.median = median(rms),
    rms.mean = mean(rms),
    rms.min = min(rms),
    rms.max = max(rms),
    rms.sd = sd(rms),
    rms.IQR = IQR(rms),
    rms.mad = mad(rms),
    ptp.median = median(ptp),
    ptp.mean = mean(ptp),
    ptp.min = min(ptp),
    ptp.max = max(ptp),
    ptp.sd = sd(ptp),
    ptp.IQR = IQR(ptp),
    ptp.mad = mad(ptp),
    amp.v.100.median = median(amp.v.100),
    amp.v.100.mean = mean(amp.v.100),
    amp.v.100.min = min(amp.v.100),
    amp.v.100.max = max(amp.v.100),
    amp.v.100.sd = sd(amp.v.100),
    amp.v.100.IQR = IQR(amp.v.100),
    amp.v.100.mad = mad(amp.v.100),
    amp.x.100.median = median(amp.x.100),
    amp.x.100.mean = mean(amp.x.100),
    amp.x.100.min = min(amp.x.100),
    amp.x.100.max = max(amp.x.100),
    amp.x.100.sd = sd(amp.x.100),
    amp.x.100.IQR = IQR(amp.x.100),
    amp.x.100.mad = mad(amp.x.100)
    ) %>%
  write_csv2('izvjestaj_final/summary_statistics.csv')

# Box plot za signifikantnost 50 Hz
dat_summ %>% select(trans, measure, side, amp.v.100, amp.v.50) %>%
  mutate(
    measure = case_when(
      str_detect(measure, 'oplata') ~ 'oplata',
      str_detect(measure, 'namotaj') ~ 'namotaji',
      str_detect(measure, 'jezgra') ~ 'jezgra',
      str_detect(measure, 'Jezgra') ~ 'jezgra',
      str_detect(measure, 'Stega') ~ 'stege',
      T ~ measure,
    )
  ) %>% pivot_longer(
    c('amp.v.50', 'amp.v.100')
  ) %>% mutate(
    name = case_when(
      name == 'amp.v.50' ~ '50 Hz',
      name == 'amp.v.100' ~ '100 Hz',
      TRUE ~ name
    )
  ) %>%
  ggplot(aes(x = name, y = value, fill=trans)) +
  facet_wrap(. ~ measure, scales = 'free_y') +
  geom_boxplot(width = 0.2, alpha = 1, outlier.size = 1, outlier.stroke = 0.1) + 
  # geom_dotplot(
  #   colour = "black", stroke = 2,
  #   binaxis='y', stackdir='center', dotsize=1, binwidth = 6) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    # breaks = seq(0, 400, 50),
    # limits=c(0, NA)
    ) +
  scale_x_discrete(name = NULL) +
  scale_fill_nejm(name = 'Transformator') +
  ggtitle("Box-plot - značajnost frekvencija") +
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
ggsave(filename = 'izvjestaj_final/slike/box_plot_znacajnost.png',
       width = 16, height = 14, units = 'cm', dpi = 320, pointsize = 12)

# Korelacija rms i amp.v
dat_summ %>% 
  select(trans, measure, side, point.id, rms, ptp, amp.v.100) %>%
  group_by(trans, measure, side) %>%
  filter(n() >= 10) %>%
  summarise(
    corr.rms = cor(rms, amp.v.100),
    corr.ptp = cor(ptp, amp.v.100)
  ) %>% View()

dat_summ %>% 
  select(trans, measure, side, point.id, rms, ptp, amp.v.100) %>%
  mutate(
    amp.rms = rms / amp.v.100,
    amp.ptp = amp.v.100 / ptp
  ) %>% 
  group_by(trans, measure, side) %>%
    filter(n() >= 10) %>%
    summarise(
      rms.min = min(amp.rms),
      rms.max = max(amp.rms),
      rms.mean = mean(amp.rms),
      rms.weighted.mean = weighted.mean(amp.rms, amp.v.100),
      rms.weighted.ideal.diff = (rms.weighted.mean - sqrt(2)/2)/sqrt(2)*2,
      rms.median = median(amp.rms)
    ) %>% 
  write_csv2('izvjestaj_final/odnos_rms_amp_100Hz.csv')
  
dat_summ %>% select(trans, measure, side) %>%
  group_by(trans, measure, side) %>%
  summarise(
    count = n()
  ) %>%
  arrange(trans, measure, side) %>% View()

################################################################################
######################### ANOVA and t-tests ####################################
################################################################################
# Number of points
dat_summ %>% select(trans, measure, side) %>%
  group_by(trans, measure, side) %>%
  summarise(
    count = n()
  ) %>%
  arrange(trans, measure, side) %>%
  write_csv2('izvjestaj_final/num_points_measured.csv')

# Check sample mean normality for t-test and ANOVA
# means_temp = replicate(1000L, mean(rbeta(30, 0.5106437, 1.0534401)))
# qqnorm(means_temp)
# qqline(means_temp)

# 110MVA - 2MVA namotaji mean difference
dat_summ %>% select(trans, measure, side, amp.v.100) %>%
  filter(side == 'NN' & (str_detect(measure, 'namotaj'))) %>%
  select(-side) %>%
  mutate(
    trans = ordered(trans, levels = c('110MVA', '2MVA')),
    measure = ordered(measure, levels = c('namotaj_1', 'namotaj_2', 'namotaj_3'))
  ) %>%
  arrange(trans, measure) %>%
  mutate(
    measure = str_c(trans, measure, sep = '_')
  ) %$%
  # aov(amp.v.100 ~ measure, data = .) %>%
  # car::Anova(type = 'III')
  pairwise.t.test(.$amp.v.100, .$measure, p.adjust.method = 'BH', pool.sd = F)
  # .$p.value %>% View()
  
# 110MVA - 2MVA opata mean difference
dat_summ %>% select(trans, measure, side, amp.v.100) %>%
  filter(side == 'NN' & (str_detect(measure, 'oplata'))) %>%
  select(-side) %>%
  mutate(
    trans = ordered(trans, levels = c('110MVA', '2MVA')),
    measure = ordered(measure, levels = c('oplata_opl', 'oplata_ukr', 'oplata', 'oplata_mod_opl'))
  ) %>%
  arrange(trans, measure) %>%
  mutate(
    measure = str_c(trans, measure, sep = '_')
  ) %$%
  # aov(amp.v.100 ~ measure, data = .) %>%
  # Anova(type = 'III')
  pairwise.t.test(.$amp.v.100, .$measure, p.adjust.method = 'BH', pool.sd = F)
  # .$p.value %>% View()

# 110MVA namotaji - stege - jezgra mean difference
dat_summ %>% select(trans, measure, side, amp.v.100) %>%
  filter(
    trans == '110MVA' &
      (str_detect(measure, 'namotaj') | str_detect(measure, 'Stega') |
         str_detect(measure, 'Jezgra'))
    ) %>%
  select(-side) %>%
  mutate(
    measure = case_when(
      str_detect(measure, 'Jezgra') ~ 'jezgra',
      str_detect(measure, 'Stega') ~ 'stege',
      T ~ measure
    ),
    measure = ordered(measure, levels = c('namotaj_1', 'namotaj_2', 'namotaj_3',
                                          'stege', 'jezgra'))
  ) %>%
  arrange(trans, measure) %>% 
  mutate(
    measure = str_c(trans, measure, sep = '_')
  ) %$%
  # aov(amp.v.100 ~ measure, data = .) %>%
  # Anova(type = 'III')
  pairwise.t.test(.$amp.v.100, .$measure, p.adjust.method = 'BH', pool.sd = F)
  # .$p.value %>% View()

# 2MVA namotaji - jezgra - Anova - Pairwise t-test mean difference
dat_summ %>% select(trans, measure, side, amp.v.100) %>%
  filter(
    trans == '2MVA' &
      (str_detect(measure, 'namotaj') | str_detect(measure, 'jezgra'))
    ) %>%
  mutate(
    measure = ordered(
      measure, levels = c('namotaj_1', 'namotaj_2', 'namotaj_3', 'jezgra')
      ),
    side = case_when(
      measure == 'jezgra' ~ 'SVE',
      T ~ side
    ),
    side = ordered(side, levels = c('VN', 'NN', 'BL', 'BD', 'SVE'))
  ) %>%
  mutate(
    measure = str_c(trans, measure, side, sep = '_')
  ) %>%
  arrange(trans, measure, side) %$%
  # aov(amp.v.100 ~ 0 + measure, data = .) %>%
  # Anova(type = 'III')
  # TukeyHSD()
  # TukeyHSD(which = 'side')
  pairwise.t.test(.$amp.v.100, .$measure, p.adjust.method = 'BH', pool.sd = F) %$%
  .$p.value %>% View()
  
# 2MVA - Početni kotao - strane mean difference
dat_summ %>% select(trans, measure, side, amp.v.100) %>%
  filter(trans == '2MVA' & str_detect(measure, 'oplata') &
           !str_detect(measure, 'ukr') & !str_detect(measure, 'mod')) %>%
  mutate(
    trans = ordered(trans, levels = c('2MVA')),
    measure = ordered(measure, levels = c('oplata', 'oplata_mod_opl')),
    side = ordered(side, levels = c('VN', 'NN', 'BL', 'BD'))
  ) %>%
  arrange(trans, measure, side) %>%
  mutate(
    measure = str_c(trans, measure, side, sep = '_')
  ) %$%
  # aov(amp.v.100 ~ 0 + measure, data = .) %>%
  # Anova(type = 'III')
  pairwise.t.test(.$amp.v.100, .$measure, p.adjust.method = 'BH', pool.sd = F) %$%
  .$p.value %>% View()

# 2MVA - Modificirani kotao - ukrute - strane mean difference
dat_summ %>% select(trans, measure, side, amp.v.100) %>%
  filter(trans == '2MVA' & str_detect(measure, 'oplata_mod')) %>%
  mutate(
    trans = ordered(trans, levels = c('2MVA')),
    measure = ordered(measure, levels = c('oplata_mod_opl', 'oplata_mod_ukr')),
    side = ordered(side, levels = c('VN', 'NN', 'BL', 'BD', 'UBL', 'UBD'))
  ) %>%
  arrange(trans, measure, side) %>%
  mutate(
    measure = str_c(trans, measure, side, sep = '_')
  ) %$%
  # aov(amp.v.100 ~ 0 + measure, data = .) %>%
  # Anova(type = 'III')
  pairwise.t.test(.$amp.v.100, .$measure, p.adjust.method = 'BH', pool.sd = F) %$%
  .$p.value %>% View()
  
# 2MVA - Modificirani kotao - Početni kotao - strane mean difference
dat_summ %>% select(trans, measure, side, point.id, amp.v.100) %>%
  filter(trans == '2MVA' & str_detect(measure, 'oplata') &
           !str_detect(measure, 'ukr')) %>%
  mutate(
    trans = ordered(trans, levels = c('2MVA')),
    measure = ordered(measure, levels = c('oplata', 'oplata_mod_opl')),
    side = ordered(side, levels = c('VN', 'NN', 'BL', 'BD'))
  ) %>%
  arrange(trans, side, measure) %>%
  pivot_wider(
    names_from = measure,
    values_from = amp.v.100
  ) %>%
  nest_by(trans, side) %>%
  mutate(
    t_test = list(t.test(
      data$oplata, data$oplata_mod_opl, paired = T
    ))
  ) %>%
  summarise(tidy(t_test)) %>%
  write_csv2('izvjestaj_final/2MVA_t_test_razlika_modifikacija.csv')
  
################################################################################
############################# Namotaji #########################################
################################################################################
# Box plot namotaja 110MVA - 2MVA usporedbe
dat_summ_nam <- dat_summ %>% select(trans, measure, side, point.id, amp.v.100) %>%
  filter(str_detect(measure, 'namotaj')) %>%
  mutate(
    side = case_when(
      trans == '110MVA' ~ side,
      trans == '2MVA' ~ 'SVE',
      T ~ side
      )
    ) %>%
  add_row(
    dat_summ %>% select(trans, measure, side, amp.v.100) %>%
      filter(str_detect(measure, 'namotaj') & side == 'NN' & trans == '2MVA') %>%
      mutate(side = 'NN')
    )
dat_summ_nam_med <- dat_summ_nam %>% group_by(measure, trans, side) %>%
  summarise(
    median.v = median(amp.v.100),
    min.v = min(amp.v.100),
    max.v = max(amp.v.100)
    ) %>% ungroup()
dat_summ_nam %>%
  ggplot(aes(x = trans, y = amp.v.100, fill = side)) +
  facet_wrap(. ~ measure, scales = 'fixed') +
  geom_boxplot(width = 0.6, alpha = 1, outlier.size = 1, outlier.stroke = 0.5,
               outlier.color = 'black', outlier.fill = 'grey', outlier.shape = 21,
               varwidth = F, size = 0.2, color = 'black'
               # fill = 'dodgerblue2'
               ) + 
  stat_summary(aes(group=side), fun = median, geom = 'text',
               label=dat_summ_nam_med %>% pull(median.v) %>% round(),
               position = position_dodge2(0.6),
               size=2.2, color="black", vjust=-0.3, fontface = 2) +
  stat_summary(aes(group=side), fun = min, geom = 'text',
               label=dat_summ_nam_med %>% pull(min.v) %>% round(),
               position = position_dodge2(0.6),
               size=2, color="black", vjust=1.3) +
  stat_summary(aes(group=side), fun = max, geom = 'text',
               label=dat_summ_nam_med %>% pull(max.v) %>% round(),
               position = position_dodge2(0.6),
               size=2, color="black", vjust=-0.5) +
  # geom_dotplot(
  #   colour = "black", stroke = 2,
  #   binaxis='y', stackdir='center', dotsize=1, binwidth = 6) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    breaks = seq(0, 5000, 500),
    # limits=c(0, NA)
    ) +
  scale_x_discrete(name = NULL) +
  scale_fill_nejm(name = 'Mjerene strane') +
  ggtitle("Box-plot - namotaji") +
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
ggsave(filename = 'izvjestaj_final/slike/box_plot_namotaji.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

################################################################################
######################### Namotaji 110MVA ######################################
################################################################################

# Histogram usporedba namotaji 110MVA
dat_summ_nam %>%
  group_by(trans, measure, side) %>%
  mutate(median.v.100 = median(amp.v.100)) %>%
  ungroup() %>% filter(trans == '110MVA') %>%
  ggplot(aes(x=amp.v.100)) +
  facet_grid(. ~ measure, scales = 'free_x') +
  geom_histogram(aes(fill=measure),
                 bins = 6, alpha = 0.9, boundary = 0,
                 color="black",  position = 'identity'
                 ) +
  geom_vline(aes(xintercept=median.v.100), linetype="dashed", size=0.8) +
  scale_y_continuous(
    name = 'Broj mjernih točaka',
    breaks = seq(0, 50, 1),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    breaks = seq(0, 6000, 500)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Namotaj',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Razdioba amplituda - 110MVA") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 5),
    axis.text.y = element_text(colour="black", size = 6),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/histogram_namotaji_110MVA.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

## Q-Q plot namotaji 110MVA
prepare_Q_Q_pair <- function(sx, sy) {
  # Standardize vectors
  # sx <- (sx - mean(sx)) / sd(sx)
  # sy <- (sy - mean(sy)) / sd(sy)
  # Normalize vectors
  sx <- (sx - min(sx) * 0.999) / (max(sx) * 1.001 - min(sx) * 0.999)
  sy <- (sy - min(sy) * 0.999) / (max(sy) * 1.001 - min(sy) * 0.999)
  # Approximate values if vector have different lenghts
  sx <- sort(sx)
  sy <- sort(sy)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx)
    sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx)
    sy <- approx(1L:leny, sy, n = lenx)$y
  return(list(sx = sx, sy = sy))
}

dat_summ_nam_q_q <- dat_summ_nam %>%
  filter(trans=='110MVA') %>%
  arrange(trans, measure, side, point.id) %>%
  mutate(
    side = factor(side),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_3', 'namotaj_2'))
  ) %>%
  pivot_wider(names_from = measure, values_from = amp.v.100) %>%
  unite(namotaj_1_2, namotaj_1, namotaj_2, sep = '_', remove = F) %>%
  unite(namotaj_1_3, namotaj_1, namotaj_3, sep = '_', remove = F) %>%
  unite(namotaj_2_3, namotaj_2, namotaj_3, sep = '_', remove = F) %>%
  select(-namotaj_1, -namotaj_2, -namotaj_3) %>%
  pivot_longer(cols = c('namotaj_1_2', 'namotaj_1_3', 'namotaj_2_3'),
               names_to = 'measure',
               values_to = 'x_y') %>%
  separate(col = x_y, into = c('x', 'y'), sep = '_') %>%
  select(-point.id) %>%
  mutate(
    trans = factor(trans),
    side = factor(side, levels = c('NN')),
    measure = factor(measure,
                     levels = c('namotaj_1_2', 'namotaj_1_3', 'namotaj_2_3'))
  ) %>% arrange(trans, side, measure) %>%
  group_by(trans, side, measure) %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>%
  mutate(
    x = prepare_Q_Q_pair(x, y)$sx,
    y = prepare_Q_Q_pair(x, y)$sy,
  )

dat_summ_nam_q_q %>%
  ggplot(aes(x=x, y=y, fill = side)) +
  facet_grid(. ~ measure, scales = 'fixed') +
  coord_fixed() +
  geom_abline(slope = 1, size = 0.8)+
  geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  # geom_smooth(method = lm) +
  scale_y_continuous(
    name = 'Standardizirana amplituda brzine',
    breaks = seq(-5, 5, 1),
    # limits = c(-3, 3)
  ) +
  scale_x_continuous(
    name = 'Standardizirana amplituda brzine',
    breaks = seq(-5, 5, 1),
    # limits = c(-3, 3)
    ) +
  scale_fill_nejm(
    name = 'Strana',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Q-Q plot - 110MVA - Namotaji") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/q-q_namotaji_110MVA.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# Check distribution
dist_fit_data <- dat_summ_nam %>%
  filter(trans == '110MVA') %>%
  mutate(
    side = factor(side),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_2', 'namotaj_3'))
  ) %>%
  select(trans, side, measure, amp.v.100) %>%
  arrange(trans, side, measure) %>%
  group_by(trans, side, measure) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx,
  )

# dist_fit_data %>% View()

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
  write_csv2('izvjestaj_final/dist_characteristics_namotaji_110MVA.csv')

## Plot distributions
dist_fit <- dist_fit_data %>%
  ungroup() %>%
  nest_by(trans, side) %>%
  # nest_by(trans, side, measure) %>%
  mutate(
    fit.plot = list(descdist(data$amp.v.100, boot = 1000, graph = F)),
    fit.beta = list(fitdist(
      data = data$amp.v.100, distr = 'beta', method = 'mme', lower = c(0, 0),
      start = list(shape1 = 0.6, shape2 = 1.2)
      ))
  ) %>% select(-fit.plot)

dist_fit <- dist_fit %>%
  unnest(data) %>% ungroup() %>%
  select(-measure, -amp.v.100) %>%
  # select(-amp.v.100) %>%
  unique() %>%
  group_by(side) %>%
  # group_by(side, measure) %>%
  summarise(
    plot = map(fit.beta, function(x) {plot(x)}),
    distname = unlist(map(fit.beta, function(x) {x$distname})),
    aic = unlist(map(fit.beta, function(x) {x$aic})),
    bic = unlist(map(fit.beta, function(x) {x$bic})),
    shape1 = unlist(map(fit.beta, function(x) {x$estimate[[1]]})),
    shape2 = unlist(map(fit.beta, function(x) {x$estimate[[2]]})),
  ) %>% select(-plot)

dist_fit %>% 
  write_csv2('izvjestaj_final/dist_fit_namotaji_110MVA.csv')

theor_quant_func <- function(quantiles, side, dist_fit) {
  s = unique(side)
  shape1 = dist_fit %>% filter(side == s) %>% pull(shape1)
  shape2 = dist_fit %>% filter(side == s) %>% pull(shape2)
  dbeta(quantiles, shape1, shape2)
  # rbeta(length(quantiles), shape1, shape2)
}

dist_fit_dens <- dat_summ_nam %>% 
  filter(trans=='110MVA') %>%
  arrange(trans, measure, side) %>%
  mutate(
    side = factor(side),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_3', 'namotaj_2'))
  ) %>%
  group_by(trans, side, measure) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx,
    ) %>%
  group_by(trans, side) %>%
  mutate(
    mean.v.100 = mean(amp.v.100)
  ) %>%
  ungroup() %>% select(-trans, -point.id, -measure) %>%
  arrange(side) %>% group_by(side) %>%
  mutate(
    quantiles = seq(0.02, 0.98, length.out = n()),
    theor_quant = map_dbl(quantiles, theor_quant_func, side, dist_fit)
  ) %>%
  ggplot(aes(x = amp.v.100)) +
  facet_wrap(. ~ side, scales = 'free') +
  geom_histogram(aes(y = ..density.., fill=side),
                 breaks = seq(0, 1.01, 0.1428572), alpha = 0.9, center = 0,
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
  scale_fill_manual(
    name = 'Strana:',
    values = c("dodgerblue3", "darksalmon")
  ) +
  # ggtitle("Modelirana razdioba amplituda - 110MVA - Namotaji") +
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
    legend.title = element_text(size = 7),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 1.5, size = 11)
  )

# Q - Q plot for modeled distribution check
dist_fit_qq <- dat_summ_nam %>%
  filter(trans=='110MVA') %>%
  arrange(trans, measure, side) %>%
  mutate(
    side = factor(side),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_3', 'namotaj_2'))
  ) %>%
  group_by(trans, side, measure) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx,
    ) %>%
  group_by(trans, side) %>%
  ungroup() %>% select(-trans, -point.id, -measure) %>%
  arrange(side) %>% group_by(side) %>%
  mutate(
    shape1 = map_dbl(
      side, function(s) {dist_fit %>% filter(side == s) %>% pull(shape1)}
      ),
    shape2 = map_dbl(
      side, function(s) {dist_fit %>% filter(side == s) %>% pull(shape2)}
      ),
  ) %>%
  do(
    plot = ggplot(data = ., aes(sample=amp.v.100, fill = side)) +
  facet_grid(. ~ side, scales = 'fixed') +
  coord_fixed() + 
  geom_qq(
    shape = 21, colour = "black", size = 1.5, stroke = 0.7,
    distribution = qbeta,
    dparams =  list(
      shape1 = unique(.$shape1),
      shape2 = unique(.$shape2)
      )
    ) +
  geom_abline(slope = 1, size = 0.8) +
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
  scale_fill_manual(
    name = 'Strana',
    values = c("dodgerblue3", "darksalmon")
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
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    # axis.title.x = element_blank(),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
  )

plot_grid(
  ggdraw() +
    draw_label(
      "Modelirana razdioba amplituda - 110MVA - Namotaji",
      fontface = 'plain',
      size = 12,
      x = 0.52,
      y = 0.8,
      hjust = 0.5
      ),
  plot_grid(
    dist_fit_dens,
    dist_fit_qq$plot[[1]],
    # nrow = 1,
    rel_widths = c(1, 1.2),
    labels = c(
      'Gustoća razdiobe',
      'Q - Q plot'
      ),
    label_y = c(1.07, 1.07),
    label_x = c(0.2, 0.4),
    label_size = 11,
    label_fontface = 'plain'
    ),
  ncol = 1,
  rel_heights = c(0.15, 1)
)
ggsave(filename = 'izvjestaj_final/slike/qq_model_distribucija_namotaji_110MVA.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# DoE scatter plot
dat_summ_nam_coord <- dat_summ %>%
  select(trans, side, measure, point.id, x, y, amp.v.100, rms, ptp) %>%
  filter(str_detect(measure, 'namotaj')) %>%
  filter(trans=='110MVA') %>%
  arrange(trans, side, measure, point.id) %>%
  mutate(
    side = factor(side),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_2', 'namotaj_3'))
  )

dat_summ_nam_coord %>%
  group_by(trans, side, measure) %>%
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
  ggplot(aes(x = coord_value, y = amp.v.100, fill = measure)) +
  facet_grid(measure ~ coord_name, scales = 'free') +
  geom_hline(aes(yintercept=mean.v.100), linetype="dashed", size=0.8) +
  geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  geom_smooth(method = 'loess', formula = y ~ x, span = 1.0) +
  scale_y_continuous(
    name = expression('Amplituda ['*mu*'m/s]'),
    # breaks = seq(0, 1, 0.25),
    # limits = c(0, 1)
  ) +
  scale_x_continuous(
    name = 'Koordinate [mm]',
    breaks = seq(-2000, 2000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Namotaj:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Rasipanje po koordinatama - 110MVA - Namotaji") +
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
ggsave(filename = 'izvjestaj_final/slike/rasipanje_namotaji_110MVA.png',
       width = 16, height = 9, units = 'cm', dpi = 320, pointsize = 12)

################################################################################
########################### Namotaji 2MVA ######################################
################################################################################

# Box plot namotaja 2MVA usporedbe po namotajima i stranama
dat_summ_nam <- dat_summ %>% select(trans, measure, side, point.id, amp.v.100) %>%
  filter(str_detect(measure, 'namotaj') & trans == '2MVA') %>%
  add_row(
    dat_summ %>% select(trans, measure, side, amp.v.100) %>%
      filter(str_detect(measure, 'namotaj') & trans == '2MVA') %>%
      mutate(side = 'SVE')
    )
dat_summ_nam_med <- dat_summ_nam %>%
  mutate(
    side = factor(side, levels = c('VN', 'NN', 'BL', 'BD', 'SVE'))
    ) %>%
  group_by(measure, side) %>%
  summarise(
    median.v = median(amp.v.100),
    min.v = min(amp.v.100),
    max.v = max(amp.v.100)
    ) %>% ungroup()

dat_summ_nam %>% mutate(
  side = factor(side, levels = c('VN', 'NN', 'BL', 'BD', 'SVE'))
  ) %>%
  ggplot(aes(x = side, y = amp.v.100, fill = side)) +
  facet_wrap(
    measure ~ .,
    scales = 'free_x') +
  geom_boxplot(width = 0.6, alpha = 1, outlier.size = 1, outlier.stroke = 0.5,
               outlier.color = 'black', outlier.fill = 'grey', outlier.shape = 21,
               varwidth = F, size = 0.2, color = 'black'
               # fill = 'dodgerblue2'
               ) + 
  geom_dotplot(
    colour = "black", stroke = 0.5,
    binaxis='y', stackdir='center', dotsize=0.8, binwidth = 20) +
  stat_summary(aes(group=side), fun = median, geom = 'text',
               label=dat_summ_nam_med %>% pull(median.v) %>% round(),
               position = position_dodge2(0.6),
               size=2.2, color="black", vjust=-0.3, fontface = 2) +
  stat_summary(aes(group=side), fun = min, geom = 'text',
               label=dat_summ_nam_med %>% pull(min.v) %>% round(),
               position = position_dodge2(0.6),
               size=2, color="black", vjust=1.3) +
  stat_summary(aes(group=side), fun = max, geom = 'text',
               label=dat_summ_nam_med %>% pull(max.v) %>% round(),
               position = position_dodge2(0.6),
               size=2, color="black", vjust=-0.5) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    breaks = seq(0, 5000, 250),
    # limits=c(0, NA)
    ) +
  scale_x_discrete(name = NULL) +
  scale_fill_nejm(name = 'Mjerene strane') +
  ggtitle("Box-plot - namotaji 2MVA") +
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
ggsave(filename = 'izvjestaj_final/slike/box_plot_namotaji_2MVA.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# Histogram usporedba namotaji 2MVA po stranama (VN, NN, BL, BD)
dat_summ_nam %>%
  mutate(
    side = case_when(
      side == 'BD' ~ 'BL/BD',
      side == 'BL' ~ 'BL/BD',
      T ~ side
    ),
    side = factor(side, levels = c('VN', 'NN', 'BL/BD')),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_3', 'namotaj_2'))
  ) %>%
  group_by(trans, measure, side) %>%
  mutate(mean.v.100 = mean(amp.v.100)) %>%
  ungroup() %>% filter(side != 'SVE') %>%
  ggplot(aes(x=amp.v.100)) +
  facet_wrap(measure ~ side, scales = 'free_x') +
  geom_histogram(aes(fill=side),
                 bins = 6, alpha = 0.9, boundary = 0.0,
                 color="black",  position = 'identity', closed = 'right'
                 ) +
  geom_vline(aes(xintercept=mean.v.100), linetype="dashed", size=0.8) +
  scale_y_continuous(
    name = 'Broj mjernih točaka',
    breaks = seq(0, 50, 1),
    limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]'),
    breaks = seq(0, 2000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Razdioba amplituda po stranama - 2MVA - Namotaji") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/histogram_namotaji_2MVA_strane.png',
       width = 16, height = 16, units = 'cm', dpi = 320, pointsize = 12)

## Q-Q plot namotaji 2MVA
prepare_Q_Q_pair <- function(sx, sy) {
  # Standardize vectors
  # sx <- (sx - mean(sx)) / sd(sx)
  # sy <- (sy - mean(sy)) / sd(sy)
  # Normalize vectors
  sx <- (sx - min(sx) * 0.999) / (max(sx) * 1.001 - min(sx) * 0.999)
  sy <- (sy - min(sy) * 0.999) / (max(sy) * 1.001 - min(sy) * 0.999)
  # Approximate values if vector have different lenghts
  sx <- sort(sx)
  sy <- sort(sy)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx)
    sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx)
    sy <- approx(1L:leny, sy, n = lenx)$y
  return(list(sx = sx, sy = sy))
}

dat_summ_nam_q_q <- dat_summ_nam %>%
  mutate(
    side = case_when(
      side == 'BD' ~ 'BL/BD',
      side == 'BL' ~ 'BL/BD',
      T ~ side
    ),
    side = factor(side, levels = c('VN', 'NN', 'BL/BD')),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_3', 'namotaj_2'))
  ) %>%
  group_by(trans, measure, side) %>%
  ungroup() %>%
  filter(side != 'SVE') %>%
  group_by(trans, side) %>%
  pivot_wider(names_from = measure, values_from = amp.v.100) %>%
  group_by(trans, side) %>%
  unite(namotaj_1_2, namotaj_1, namotaj_2, sep = '_', remove = F) %>%
  unite(namotaj_1_3, namotaj_1, namotaj_3, sep = '_', remove = F) %>%
  unite(namotaj_2_3, namotaj_2, namotaj_3, sep = '_', remove = F) %>%
  select(-namotaj_1, -namotaj_2, -namotaj_3) %>%
  pivot_longer(cols = c('namotaj_1_2', 'namotaj_1_3', 'namotaj_2_3'),
               names_to = 'measure',
               values_to = 'x_y') %>%
  ungroup() %>%
  separate(col = x_y, into = c('x', 'y'), sep = '_') %>%
  filter(!(side == 'BL/BD' & measure %in% c('namotaj_1_2', 'namotaj_2_3'))) %>%
  select(-point.id) %>% 
  mutate(
    trans = factor(trans),
    side = factor(side, levels = c('VN', 'NN', 'BL/BD')),
    measure = factor(measure,
                     levels = c('namotaj_1_2', 'namotaj_1_3', 'namotaj_2_3'))
  ) %>% arrange(trans, side, measure) %>%
  group_by(trans, side, measure) %>% drop_na() %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>% drop_na() %>%
  mutate(
    x = prepare_Q_Q_pair(x, y)$sx,
    y = prepare_Q_Q_pair(x, y)$sy,
  )

dat_summ_nam_q_q %>%
  ggplot(aes(x=x, y=y, fill = side)) +
  facet_grid(side ~ measure, scales = 'fixed') +
  coord_fixed() + 
  geom_abline(slope = 1, size = 0.8)+
  geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  scale_y_continuous(
    name = 'Standardizirana amplituda brzine',
    # breaks = seq(0, 50, 1),
    # limits = c(0, NA)
  ) +
  scale_x_continuous(
    name = 'Standardizirana amplituda brzine',
    # breaks = seq(0, 2000, 250)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Q-Q plot - 2MVA - Namotaji") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    # panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/q-q_namotaji_2MVA_strane.png',
       width = 16, height = 16, units = 'cm', dpi = 320, pointsize = 12)

# Check distribution
dist_fit_data <-
  dat_summ_nam %>%
  mutate(
    side = case_when(
      side == 'BD' ~ 'BL/BD',
      side == 'BL' ~ 'BL/BD',
      T ~ side
    ),
    side = factor(side, levels = c('VN', 'NN', 'BL/BD')),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_2', 'namotaj_3'))
  ) %>%
  filter(side != 'SVE') %>%
  group_by(trans, side, measure) %>%
  select(trans, side, measure, point.id, amp.v.100) %>%
  arrange(trans, side, measure) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx,
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
  write_csv2('izvjestaj_final/dist_characteristics_namotaji_2MVA.csv')

fig <- dist_characteristics %>% 
  plot_ly(
    x=~skewness.dist, y=~kurtosis.dist, z=~mean.dist, text = ~measure,
    type="scatter3d", mode="markers", color=~side, colors = 
    ) %>%
  layout(
    title = list(
      text = 'Karakteristike distribucije - 2MVA - Namotaji',
      y = 0.99,
      font = list(
        size = 22
      )
    ),
    legend = list(
      orientation = 'h',
      x = 0.35,
      y = 0.99,
      font = list(
        size = 18
      )
    ),
    scene = list(
      xaxis = list(
        title = 'Kurtosis',
        ticks = 'outside',
        zeroline = T,
        showline = T,
        titlefont = list(
          size = 18
        )
      ),
      yaxis = list(
        title = 'Asimetrija',
        ticks = 'outside',
        zeroline = T,
        showline = T,
        titlefont = list(
          size = 18
        )
      ),
      zaxis = list(
        title = 'Prosječna vrijednost',
        ticks = 'outside',
        zeroline = T,
        showline = T,
        titlefont = list(
          size = 18
        )
      )
    )
  )


fig
## Download plot as png and save manually

## Plot distributions
dist_fit <- dist_fit_data %>%
  ungroup() %>%
  nest_by(trans, side) %>%
  mutate(
    fit.plot = list(descdist(data$amp.v.100, boot = 1000, graph = F)),
    fit.beta = list(fitdist(
      data = data$amp.v.100, distr = 'beta', method = 'mme', lower = c(0, 0),
      start = list(shape1 = 0.6, shape2 = 1.2)
      ))
  ) %>% select(-fit.plot)

dist_fit <- dist_fit %>%
  unnest(data) %>% ungroup() %>% select(-measure, -point.id, -amp.v.100) %>%
  unique() %>% group_by(side) %>%
  summarise(
    plot = map(fit.beta, function(x) {plot(x)}),
    distname = unlist(map(fit.beta, function(x) {x$distname})),
    aic = unlist(map(fit.beta, function(x) {x$aic})),
    bic = unlist(map(fit.beta, function(x) {x$bic})),
    shape1 = unlist(map(fit.beta, function(x) {x$estimate[[1]]})),
    shape2 = unlist(map(fit.beta, function(x) {x$estimate[[2]]})),
  ) %>% select(-plot)

dist_fit %>% 
  write_csv2('izvjestaj_final/dist_fit_namotaji_2MVA.csv')

theor_quant_func <- function(quantiles, side, dist_fit) {
  s = unique(side)
  shape1 = dist_fit %>% filter(side == s) %>% pull(shape1)
  shape2 = dist_fit %>% filter(side == s) %>% pull(shape2)
  dbeta(quantiles, shape1, shape2)
  # rbeta(length(quantiles), shape1, shape2)
}

dat_summ_nam %>%
  mutate(
    side = case_when(
      side == 'BD' ~ 'BL/BD',
      side == 'BL' ~ 'BL/BD',
      T ~ side
    ),
    side = factor(side, levels = c('VN', 'NN', 'BL/BD')),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_3', 'namotaj_2'))
  ) %>%
  group_by(trans, side, measure) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx,
    ) %>%
  group_by(trans, side) %>%
  mutate(
    mean.v.100 = mean(amp.v.100)
  ) %>%
  filter(side != 'SVE') %>% ungroup() %>% select(-trans, -point.id, -measure) %>%
  arrange(side) %>% group_by(side) %>%
  mutate(
    quantiles = seq(0.05, 0.95, length.out = n()),
    theor_quant = map_dbl(quantiles, theor_quant_func, side, dist_fit)
  ) %>%
  ggplot(aes(x = amp.v.100)) +
  facet_wrap(. ~ side, scales = 'free_x') +
  geom_histogram(aes(y = ..density.., fill=side),
                 breaks = seq(0, 1, 0.2), alpha = 0.9, center = 0,
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
    name = 'Strana:',
  ) +
  ggtitle("Modelirana razdioba amplituda po stranama - 2MVA - Namotaji") +
  theme_bw() + theme(
    # panel.border = element_rect(),
    legend.position = 'top',
    panel.background = element_blank(),
    panel.spacing = unit(2, 'mm'),
    panel.grid.minor.y = element_blank(),
    legend.box.spacing = unit(2, 'mm'),
    legend.spacing = unit(2, 'mm'),
    legend.margin = margin(0, 0, 0, 0, 'mm'),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(filename = 'izvjestaj_final/slike/model_distribucija_namotaji_2MVA.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# Q - Q plot for modeled distribution check
dist_fit_qq <- dat_summ_nam %>%
  mutate(
    side = case_when(
      side == 'BD' ~ 'BL/BD',
      side == 'BL' ~ 'BL/BD',
      T ~ side
    ),
    side = factor(side, levels = c('VN', 'NN', 'BL/BD')),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_3', 'namotaj_2'))
  ) %>%
  group_by(trans, side, measure) %>%
  mutate(
    amp.v.100 = prepare_Q_Q_pair(amp.v.100, amp.v.100)$sx,
    ) %>%
  group_by(trans, side) %>%
  filter(side != 'SVE') %>% ungroup() %>% select(-trans, -point.id, -measure) %>%
  arrange(side) %>% group_by(side) %>%
  mutate(
    shape1 = map_dbl(
      side, function(s) {dist_fit %>% filter(side == s) %>% pull(shape1)}
      ),
    shape2 = map_dbl(
      side, function(s) {dist_fit %>% filter(side == s) %>% pull(shape2)}
      ),
  ) %>%
  do(
    plot = ggplot(data = ., aes(sample=amp.v.100, fill = side)) +
  facet_grid(. ~ side, scales = 'fixed') +
  coord_fixed() + 
  geom_qq(
    shape = 21, colour = "black", size = 1.5, stroke = 0.7,
    distribution = qbeta,
    dparams =  list(
      shape1 = unique(.$shape1),
      shape2 = unique(.$shape2)
      )
    ) +
  geom_abline(slope = 1, size = 0.8) +
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
  scale_fill_manual(
    name = 'Strana',
    values = c("dodgerblue3", "darksalmon")
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
    axis.title = element_text(size = 10),
    axis.text.x = element_text(colour="black", size = 6.5),
    axis.title.x = element_blank(),
    axis.text.y = element_text(colour="black", size = 8),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5)
  )
  )

cowplot::plot_grid(
  dist_fit_qq$plot[[1]],
  dist_fit_qq$plot[[2]] + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ),
  dist_fit_qq$plot[[3]] + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ),
  nrow = 1,
  rel_widths = c(1.2, 1, 0.98),
  # labels = list(
  #   title = 'Q - Q plot - Modelirana razdioba - 2MVA - Namotaji',
  #   subtitle = 'Modelirana normalizirana amplituda brzine'
  #   ),
  labels = c(
    'Q - Q plot - Modelirana razdioba - 2MVA - Namotaji',
    'Modelirana normalizirana amplituda brzine'
    ),
  label_y = c(1, 0.13),
  label_x = c(-0.1, -1.1),
  label_size = 12,
  label_fontface = 'plain'
  )
ggsave(filename = 'izvjestaj_final/slike/qq_model_distribucija_namotaji_2MVA.png',
       width = 16, height = 8, units = 'cm', dpi = 320, pointsize = 12)

# DoE scatter plot
dat_summ_nam_coord <- dat_summ %>%
  select(trans, side, measure, point.id, x, y, amp.v.100, rms, ptp) %>%
  filter(str_detect(measure, 'namotaj') & trans == '2MVA') %>%
  arrange(trans, side, measure, point.id) %>%
  mutate(
    side = case_when(
      side == 'BD' ~ 'BL/BD',
      side == 'BL' ~ 'BL/BD',
      T ~ side
    ),
    side = factor(side, levels = c('VN', 'NN', 'BL/BD')),
    measure = factor(measure, levels = c('namotaj_1', 'namotaj_2', 'namotaj_3'))
  ) %>% arrange(trans, side, measure, point.id)

dat_summ_nam_coord %>%
  group_by(trans, side, measure) %>%
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
  ggplot(aes(x = coord_value, y = amp.v.100, fill = side)) +
  facet_grid(side+measure ~ coord_name, scales = 'free') +
  geom_hline(aes(yintercept=mean.v.100), linetype="dashed", size=0.8) +
  geom_point(shape = 21, colour = "black", size = 1.5, stroke = 0.7) +
  geom_smooth(method = 'loess', formula = y ~ x, span = 1.0) +
  scale_y_continuous(
    name = expression('Amplituda ['*mu*'m/s]'),
    # breaks = seq(0, 1, 0.25),
    # limits = c(0, 1)
  ) +
  scale_x_continuous(
    name = 'Koordinate [mm]',
    breaks = seq(-2000, 2000, 100)
    # limits = c(0, NA)
    ) +
  scale_fill_nejm(
    name = 'Strana:',
    # values = c("dodgerblue3", "darksalmon")
  ) +
  ggtitle("Rasipanje po koordinatama - 2MVA - Namotaji") +
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
ggsave(filename = 'izvjestaj_final/slike/rasipanje_namotaji_2MVA.png',
       width = 16, height = 20, units = 'cm', dpi = 320, pointsize = 12)

# Correlation analysis
dat_summ_nam_coord %>% 
  group_by(trans, side, measure) %>%
  summarise(
    count = n()
  )

dat_summ_nam_coord %>% glimpse()
  
dat_summ_nam_coord %>% select(-rms, -ptp, -x, -y) %>%
  pivot_wider(
    names_from = c(measure, side),
    values_from = amp.v.100
    ) %>% arrange(trans, point.id) %>%
  select(-trans, -point.id) %>%
  ggcorrm() +
  lotri(
    geom_smooth(method = "lm", se = T, formula = y ~ x)
    ) +
  lotri(
    geom_point(alpha = 0.5)
    ) +
  utri_corrtext(
    nrow = 1, squeeze = 0.9, corr_method = 'pearson'
    ) +
  dia_names(
    y_pos = 0.10, size = 2.0
    ) +
  dia_histogram(
    lower = 0.25, upper = 0.90, color = 1, fill = 'grey80', bins = 6,
    position = 'identity', boundary = 0.0
    ) +
  ggtitle('Korelacijska matrica - 2MVA - Namotaji') + 
  scale_x_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]')
  ) +
  scale_y_continuous(
    name = expression('Amplituda brzine ['*mu*'m/s]')
  ) +
  theme(
    text = element_text(size = 7.1),
    axis.title.x = element_text(size = 7.5),
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
    axis.text.x = element_text(colour="black", size = 4.5),
    axis.text.y = element_text(colour="black", size = 4.5),
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 12)
      )
ggsave(filename = 'izvjestaj_final/slike/corr_matrix_namotaji_2MVA.png',
       width = 16, height = 17, units = 'cm', dpi = 320, pointsize = 12)
  


################################################################################
################################################################################
# Correlation funnel
dat_summ_seg <- dat_summ %>%
  select(-amp.x, -rms, -ptp, -x, -y) %>%
  mutate(measure.side = str_c(measure, side, sep = '_')) %>%
  select(-measure, -side) %>%
  pivot_wider(names_from = measure.side, values_from=amp.v,
              id_cols=point.id, values_fill=NA)
  # select(-point.id, -starts_with('oplata'), oplata_BL) %>%
dat_summ_bin <- dat_summ_seg %>% drop_na() %>% binarize(n_bins = 2)

dat_summ_bin %>% glimpse()
dat_summ_bin %>% correlate("oplata_BL__183.02971_Inf") %>%
  plot_correlation_funnel(interactive = F)

################################################################################
################################################################################
# Data structure
dat_summ_seg %>% plot_missing()
dat_summ_seg %>% plot_histogram(geom_histogram_args = list(bins = 4))
dat_summ_seg %>% select(starts_with('namotaj')) %>% plot_qq()
dat_summ_seg %>% select(starts_with('namotaj'), -contains('VN'), oplata_BD, oplata_BL) %>%
  drop_na() %>% plot_correlation(maxcat = 5)
dat_summ_seg %>% plot_boxplot(by = 'oplata_BL')
dat_summ %>% select(amp.v) %>%
  plot_histogram(geom_histogram_args = list(bins = 30))

################################################################################
################################################################################
# Creating planar data pattern
# dimenzije NN i VN strane kotla x:0 - 1380; y: 0 - 1243
# dimenzije BL i BD strane kotla x:0 - 578; y: 0 - 1243

dat_summ %>% filter(measure == 'oplata' & side == 'NN') %>% summarise(
  min.x = min(x),
  max.x = max(x),
  min.y = min(y),
  max.y = max(y)
)
  
opl_nn <- dat_summ %>% filter(measure == 'oplata' & side == 'BL') %$% 
  ppp(.$x, .$y, c(0, 578), c(0, 1243), marks = .$amp.v)
opl_vn <- dat_summ %>% filter(measure == 'oplata' & side == 'VN') %$% 
  ppp(.$x, .$y, c(0, 1380), c(0, 1243), marks = .$amp.v)
opl_vn_aff <- affine(opl_vn, mat = diag(c(-1, 1)))
opl_vn <- opl_vn_aff
plot(opl_nn)
plot(opl_vn)
plot(opl_vn_aff)

summary(opl_vn)

# Cutting and binning for multitype marks
opl_nn_bin <- cut(opl_nn, breaks = 3)
opl_vn_bin <- cut(opl_vn, breaks = 3)
plot(opl_vn_bin)
plot(split(opl_vn_bin))
plot(density(split(opl_vn_bin), 100), ribbon=F)
plot(opl_nn_bin)  
plot(split(opl_nn_bin))
plot(split(opl_nn_bin), main='Halo',
     panel.begin=function(i, y, ...) {plot(density(y), ...)}
)
summary(opl_nn_bin)
summary(opl_vn_bin)

# Simple summaries of neighbouring marks
plot(pairdist(opl_nn))
plot(distmap(opl_nn))
nndist(opl_nn, k=2) %>% summary( )
plot(opl_nn %mark% (nndist(opl_nn)), markscale = 1, main = "Stienen diagram")
## Radijusi oko 200 najvišu korelaciju
## contingency table of the marks of all points within a given radius
mark.table <- marktable(opl_nn_bin, N=3)
row.names(mark.table) <- round(opl_nn$marks)
plot(mark.table)
summary(marktable(opl_vn_bin, R=200))
marktable(opl_vn_bin, R=200)
plot(markstat(opl_vn, length, R=200))
plot(markstat(opl_nn, length, R=200))

# Numeric marks: distribution and trend
hist(opl_nn$marks, breaks = 4)
hist(opl_vn$marks, breaks = 6)
plot(Smooth(opl_vn, 80))
plot(Smooth(opl_nn, 80))

# Summary functions
K <- alltypes(opl_nn_bin, "G", r = 0:400, correction='km')
plot(K, legend = T)
plot(pcf(K, method = 'c', spar = 1), lwd = 2)

# Mark correlation function
mark.M <- markcorr(opl_nn, r=0:500, correction = 'translation', method = 'sm', normalise = T)
mark.V <- markvario(opl_nn, r=0:500, method = 'sm', normalise = T)
plot(mark.M, xlim = c(0, 600))

# Fitting Poisson models
low <- split(opl_nn_bin)$`(20.1,324]`
low <- affine(low, mat = diag(c(1/1000, 1/1000)))
min(nndist(low))*0.9999
fit <- ppm(low ~ 1)
fit <- ppm(low ~ polynom(x, y, 2))
fit
# plot(low)
# plot(fit)
diagnose.ppm(fit)
qqplot.ppm(fit, 39)
Q.test <- quadrat.test(fit, nx=2, ny=2)
plot(low)
plot(Q.test, add=T)
plot(residuals(fit))
plot(predict(fit))
plot(split(opl_nn_bin))
plot(predict(fit), main='Halo',
     panel.end=function(i, y, ...) {plot((y), add=T, ...)}
)

Q <- quadrat.test(opl_vn, nx=2, ny = 2)
plot(Q, add = T)
fit <- ppm(opl_nn_bin, ~x+y)
plot(fit, how = 'contour')
plot(distmap(opl_nn))
