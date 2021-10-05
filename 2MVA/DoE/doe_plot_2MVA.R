################################################################################
########################## Funkcije za plot DoE 2MVA ###########################
################################################################################


# Manipulation
library(tidyverse)
# Plotting
library(ggplot2)
# Data paste
library(datapasta)
# Sorting
library(gtools)
library(svglite)
library(gdtools)
# Fitting


################################################################################
################################## Namotaji ####################################
################################################################################
# Namotaji - input
dat_nam <- tibble::tribble(
   ~point.id, ~loc.x, ~loc.y,
          1L,  -151L,   389L,
          2L,  -125L,   216L,
          3L,  -109L,   566L,
          4L,   -80L,   106L,
          5L,   -60L,   335L,
          6L,   -51L,   524L,
          7L,   -25L,   130L,
          8L,     1L,    20L,
          9L,    32L,   284L,
         10L,    35L,   612L,
         11L,    66L,   440L,
         12L,    75L,   199L,
         13L,    96L,    57L,
         14L,   135L,   320L,
         15L,   150L,   461L
  ) %>% mutate(
    wind = 1,
    side = 'NN'
  ) %>% select(side, wind, point.id, loc.x, loc.y)

dat_nam <- dat_nam %>%
  full_join(dat_nam %>% mutate(side = 'NN', wind = 2)) %>%
  full_join(dat_nam %>% mutate(side = 'NN', wind = 3)) %>%
  full_join(dat_nam %>% mutate(side = 'VN', wind = 1)) %>%
  full_join(dat_nam %>% mutate(side = 'VN', wind = 2)) %>%
  full_join(dat_nam %>% mutate(side = 'VN', wind = 3))


# Namotaji - plot
# VN strana
dat_nam %>%
  filter(side == 'VN') %>%
  mutate(
    wind = paste0('Namotaj: ', wind)
  ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(factor(wind, levels = mixedsort(unique(wind), decreasing = F)) ~ ., ncol = 3) +
  geom_vline(aes(xintercept = 0), linetype = 'twodash') +
  geom_point(shape = 21, colour = "red",
             stroke = 1, size = 8,
             ) +
  geom_text(fontface= "bold") +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-200, 200, 50),
    limits = c(NA, NA)
  ) +
  ggtitle("DoE - Namotaji - VN strana") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/DoE/Aktivni_dio/namotaji_vn_doe.png',
       width = 25, height = 17, units = 'cm', dpi = 320)

# NN strana
dat_nam %>%
  filter(side == 'NN') %>%
  mutate(
    wind = paste0('Namotaj: ', wind)
  ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id)) +
  facet_wrap(factor(wind, levels = mixedsort(unique(wind), decreasing = T)) ~ ., ncol = 3) +
  geom_vline(aes(xintercept = 0), linetype = 'twodash') +
  geom_point(shape = 21, colour = "red",
             stroke = 1, size = 8,
             ) +
  geom_text(fontface= "bold") +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 700, 50),
    limits = c(NA, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(-200, 200, 50),
    limits = c(NA, NA)
  ) +
  ggtitle("DoE - Namotaji - NN strana") +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(1, 'lines')
  )
ggsave(filename = '2MVA/DoE/Aktivni_dio/namotaji_nn_doe.png',
       width = 25, height = 17, units = 'cm', dpi = 320)

################################################################################
################################### Oplata #####################################
################################################################################
# Oplata - input
# dat_opl <- tibble::tribble(
#  ~point.id,  ~loc.x,  ~loc.y,
#         1L,      4L,    360L,
#         2L,     52L,    712L,
#         3L,     87L,   1023L,
#         4L,    124L,    112L,
#         5L,    178L,   1209L,
#         6L,    208L,    575L,
#         7L,    239L,    313L,
#         8L,    286L,   1041L,
#         9L,    350L,    776L,
#        10L,    358L,     75L,
#        11L,    402L,    540L,
#        12L,    444L,   1144L,
#        13L,    502L,    907L,
#        14L,    535L,    393L,
#        15L,    559L,    702L,
#        16L,    592L,    231L,
#        17L,    648L,    885L,
#        18L,    709L,    455L,
#        19L,    734L,     20L,
#        20L,    750L,   1081L,
#        21L,    801L,    647L,
#        22L,    847L,    269L,
#        23L,    875L,    956L,
#        24L,    933L,    467L,
#        25L,    965L,    147L,
#        26L,    999L,   1106L,
#        27L,   1027L,    848L,
#        28L,   1095L,    617L,
#        29L,   1114L,    352L,
#        30L,   1146L,     55L,
#        31L,   1201L,    990L,
#        32L,   1244L,   1204L,
#        33L,   1276L,    505L,
#        34L,   1330L,    813L,
#        35L,   1379L,    189L
#   ) %>% mutate(
#     side = 'prednja/stražnja',
#     xmin = 0, xmax = 1380, ymin = 0, ymax = 1243
#   ) %>% full_join(
# tibble::tribble(
#  ~point.id, ~loc.x,  ~loc.y,
#         1L,    16L,    779L,
#         2L,    61L,    432L,
#         3L,    92L,   1132L,
#         4L,   144L,    213L,
#         5L,   180L,    671L,
#         6L,   196L,   1048L,
#         7L,   244L,    260L,
#         8L,   330L,     40L,
#         9L,   347L,    569L,
#        10L,   353L,   1225L,
#        11L,   409L,    881L,
#        12L,   425L,    399L,
#        13L,   464L,    115L,
#        14L,   534L,    640L,
#        15L,   562L,    923L
#   ) %>% mutate(
#     side = 'bočna lijeva/desna',
#     xmin = 0, xmax = 578, ymin = 0, ymax = 1243
#   ), by = c('side', 'point.id', 'loc.x', 'loc.y', 'xmin', 'xmax', 'ymin', 'ymax')
# ) %>% select(side, point.id, loc.x, loc.y ,xmin, xmax, ymin, ymax)

# Reading data and adding rib measuring points
dat_opl <- read.csv2(
  '2MVA/DoE/Oplata_i_prigusenje/oplata_stvarne_koordinate.csv',
  col.names = c('side', 'point.id', 'loc.x', 'loc.y')) %>%
  full_join(
    tibble(
      side = c(rep('BLR', 8), rep('BDR', 8)),
      point.id = rep(1:8, 2),
      loc.x = 60,
      loc.y = rep(c(189, 378), 8)
    )
  )

# Saving modified hull point locations
dat_opl %>% rename(Strana=side, Tocka=point.id, x=loc.x, y=loc.y) %>%
  write_csv2('2MVA/DoE/Oplata_i_prigusenje/modoficirani_kotao_doe.csv')

dat_opl <- dat_opl %>%
  filter(side %in% c('VN', 'BL', 'BLR')) %>%
  mutate(
    xmin = case_when(
      side == 'VN' ~ 0,
      side == 'BL' ~ 0,
      side == 'BLR' ~ 0,
      TRUE ~ 5000
      ),
    xmax = case_when(
      side == 'VN' ~ 1380,
      side == 'BL' ~ 578,
      side == 'BLR' ~ 120,
      TRUE ~ 5000
      ),
    ymin = case_when(
      side == 'VN' ~ 0,
      side == 'BL' ~ 0,
      side == 'BLR' ~ 0,
      TRUE ~ 5000
      ),
    ymax = case_when(
      side == 'VN' ~ 1243,
      side == 'BL' ~ 1243,
      side == 'BLR' ~ 567,
      TRUE ~ 5000
      ),
    side = case_when(
      side == 'VN' ~ 'prednja/stražnja',
      side == 'BL' ~ 'bočna lijeva/desna',
      side == 'BLR' ~ 'rebra',
      TRUE ~ 'nije ok'
      ),
    ) 

# Oplata - plot DoE
dat_opl %>% 
  # filter(side == 'prednja/stražnja') %>%
  # filter(side == 'bočna lijeva/desna') %>%
  filter(side == 'rebra' & point.id %in% c(7, 8)) %>%
  mutate(
    side = paste0('Strana: ', side)
  ) %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id, xmin = xmin, xmax = xmax,
             ymin = ymin, ymax = ymax)) +
  # facet_grid( ~ factor(side, levels = mixedsort(unique(side), decreasing = T)),
  #             scales = 'free_x', space = 'free_x') +
  geom_rect(fill = NA, linetype = 'solid', size = 0.8, color='black') +
  geom_point(shape = 21, colour = "red",
             stroke = 1, size = 8,
             ) +
  geom_text(fontface= "bold") +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    # limits = c(-100, NA)
    limits = c(0, 567)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 100),
    # limits = c(-150, 750)
    limits = c(0, 120)
  ) +
  # ggtitle('Strana: prednja VN/stražnja NN') +
  # ggtitle('Strana: bočna lijeva/desna') +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0)),
    panel.spacing = unit(1, 'lines'),
    rect = element_rect(fill = 'transparent'),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    axis.title = element_blank(), # remove axis title
    axis.text = element_blank(), # remove axis text
    axis.ticks = element_blank(), # remove axis ticks
    panel.border = element_blank(), # remove panel border
    panel.grid.major = element_blank(), # remove major gridlines
    panel.grid.minor = element_blank(), # remove minor gridlines
    axis.line.x = element_blank(), # remove x axis
    axis.line.y = element_blank() # remove y axis
  )
# ggsave(filename = '2MVA/DoE/Oplata_i_prigusenje/oplata_doe.png',
#        width = 25, height = 17, units = 'cm', dpi = 320)
# ggsave(filename = '2MVA/DoE/Oplata_i_prigusenje/oplata_modificirana_prednja_transparent_doe.png',
#        width = 20, height = 18, units = 'cm', dpi = 320, bg = 'transparent')
# ggsave(filename = '2MVA/DoE/Oplata_i_prigusenje/oplata_modificirana_bocna_transparent_doe.png',
#        width = 15, height = 18, units = 'cm', dpi = 320, bg = 'transparent')
ggsave(filename = '2MVA/DoE/Oplata_i_prigusenje/oplata_modificirana_rebra7_transparent_doe.png',
       width = 6, height = 8, units = 'cm', dpi = 320, bg = 'transparent')

################################################################################
################################# Prigušenje ###################################
################################################################################
dat_imp <-
tibble::tribble(
 ~point.id,   ~loc.x,   ~loc.y,
      "P1",     200L,    1040L,
      "P2",    1180L,    1040L,
      "P3",     200L,     200L,
      "P4",    1180L,     200L,
      "P5",     690L,     620L
  ) %>% mutate(
    xmin = 0, xmax = 1380, ymin = 0, ymax = 1243
  )

dat_imp %>%
  ggplot(aes(x = loc.x, y = loc.y, label = point.id, xmin = xmin, xmax = xmax,
             ymin = ymin, ymax = ymax)) +
  geom_rect(fill = NA, linetype = 'solid', size = 0.8, color='black') +
  geom_point(shape = 23, colour = "red",
             stroke = 1, size = 8, fill = 'darkolivegreen1'
             ) +
  geom_text(fontface= "bold") +
  coord_fixed() +
  scale_y_continuous(
    name = expression('Visina '*italic('y')*' [mm]'),
    breaks = seq(0, 1300, 100),
    limits = c(-100, NA)
  ) +
  scale_x_continuous(
    name = expression('Širina '*italic('x')*' [mm]'),
    breaks = seq(0, 1500, 100)
    # limits = c(NA, NA)
  ) +
  ggtitle('Udarni test - prednja VN strana') +
  theme_bw() +
  theme(
    axis.line = element_line(size=0.5, colour = "black"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0)),
    panel.spacing = unit(1, 'lines'),
    rect = element_rect(fill = 'transparent'),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
# ggsave(filename = '2MVA/DoE/Oplata_i_prigusenje/oplata_doe.png',
#        width = 25, height = 17, units = 'cm', dpi = 320)
ggsave(filename = '2MVA/DoE/Oplata_i_prigusenje/impact_transparent_doe.png',
       width = 20, height = 18, units = 'cm', dpi = 320, bg = 'transparent')
