
##### Measurement data analysis and plot #####

## Source functions and objects file
source('functions.R')

################################################################################
################################################################################
# Input args from Rscript
args = commandArgs(TRUE)
# Kratki_Spoj_1_mjerenje_napon_100
rep1.path = args[1]
# rep1.path <- '/home/tbazina/mjerenje-vibracija-transformatora/mjerenja/Kratki_Spoj_1_mjerenje_napon_100/'
rep1.path <- './mjerenja/Kratki_Spoj_1_mjerenje_napon_100/'
# Kratki_Spoj_2_mjerenje_napon_80
rep2.path = args[2]
rep2.path <- './mjerenja/Kratki_Spoj_2_mjerenje_napon_80/'
# Prazni_Hod_1_mjerenje_nazivna_indukcija
rep3.path = args[3]
rep3.path <- './mjerenja/Prazni_Hod_1_mjerenje_nazivna_indukcija/'
# Prazni_Hod_2_mjerenje_1.3T
rep4.path = args[4]
rep4.path <- './mjerenja/Prazni_Hod_2_mjerenje_1.3T/'
# Folder in which to save figures
figs.path = args[5]
figs.path = './slike/stvarna_mjerenja/'

# Load saved image
load(".RData")

################################################################################
################################################################################
# Data input Kratki_Spoj_1_mjerenje_napon_100
# Input first point Kratki_Spoj_1_mjerenje_napon_100
dat <- VibData(
  point.id = 'pt1',
  loc.x = 125,
  loc.y = 1345,
  rib = T,
  replication = 1,
  file.loc = paste0(rep1.path, 'Pt1')
)

# Input second point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt2',
      loc.x = 166,
      loc.y = 313,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt2')
    )
  )

# Input third point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt3',
      loc.x = 340,
      loc.y = 2052,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt3')
    )
  )

# Input fourth point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt4',
      loc.x = 516,
      loc.y = 839,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt4')
    )
  )

# Input fifth point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt5',
      loc.x = 650,
      loc.y = 977,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt5')
    )
  )

# Input 6. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt6',
      loc.x = 693,
      loc.y = 2285,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt6')
    )
  )

# Input 7. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt7',
      loc.x = 862,
      loc.y = 1567,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt7')
    )
  )

# Input 8. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt8',
      loc.x = 900,
      loc.y = 469,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt8')
    )
  )

# Input 9. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt9',
      loc.x = 1189,
      loc.y = 1405,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt9')
    )
  )

# Input 10. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt10',
      loc.x = 1273,
      loc.y = 2411,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt10')
    )
  )

# Input 11. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt11',
      loc.x = 1427,
      loc.y = 1968,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt11')
    )
  )

# Input 12. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt12',
      loc.x = 1600,
      loc.y = 487,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt12')
    )
  )

# Input 13. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt13',
      loc.x = 1750,
      loc.y = 1681,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt13')
    )
  )

# Input 14. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt14',
      loc.x = 1750,
      loc.y = 921,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt14')
    )
  )

# Input 15. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt15',
      loc.x = 2150,
      loc.y = 1194,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt15')
    )
  )

# Input 16. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt16',
      loc.x = 2200,
      loc.y = 2119,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt16')
    )
  )

# Input 17. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt17',
      loc.x = 2330,
      loc.y = 1890,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt17')
    )
  )

# Input 18. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt18',
      loc.x = 2350,
      loc.y = 196,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt18')
    )
  )

# Input 19. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt19',
      loc.x = 2550,
      loc.y = 1445,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt19')
    )
  )

# Input 20. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt20',
      loc.x = 2735,
      loc.y = 122,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt20')
    )
  )

# Input 21. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt21',
      loc.x = 2900,
      loc.y = 2331,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt21')
    )
  )

# Input 22. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt22',
      loc.x = 2950,
      loc.y = 1059,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt22')
    )
  )

# Input 23. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt23',
      loc.x = 3211,
      loc.y = 697,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt23')
    )
  )

# Input 24. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt24',
      loc.x = 3270,
      loc.y = 1749,
      rib = F,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt24')
    )
  )

# Input 25. point Kratki_Spoj_1_mjerenje_napon_100
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt25',
      loc.x = 3530,
      loc.y = 596,
      rib = T,
      replication = 1,
      file.loc = paste0(rep1.path, 'Pt25')
    )
  )

# Save current workspace
save.image()
################################################################################
################################################################################
# Data input Kratki_Spoj_2_mjerenje_napon_80
# Input first point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = 125,
      loc.y = 1345,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt1')
    )
  )

# Input second point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt2',
      loc.x = 166,
      loc.y = 313,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt2')
    )
  )

# Input third point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt3',
      loc.x = 340,
      loc.y = 2052,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt3')
    )
  )

# Input fourth point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt4',
      loc.x = 516,
      loc.y = 839,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt4')
    )
  )

# Input fifth point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt5',
      loc.x = 650,
      loc.y = 977,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt5')
    )
  )

# Input 6. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt6',
      loc.x = 693,
      loc.y = 2285,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt6')
    )
  )

# Input 7. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt7',
      loc.x = 862,
      loc.y = 1567,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt7')
    )
  )

# Input 8. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt8',
      loc.x = 900,
      loc.y = 469,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt8')
    )
  )

# Input 9. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt9',
      loc.x = 1189,
      loc.y = 1405,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt9')
    )
  )

# Input 10. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt10',
      loc.x = 1273,
      loc.y = 2411,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt10')
    )
  )

# Input 11. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt11',
      loc.x = 1427,
      loc.y = 1968,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt11')
    )
  )

# Input 12. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt12',
      loc.x = 1600,
      loc.y = 487,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt12')
    )
  )

# Input 13. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt13',
      loc.x = 1750,
      loc.y = 1681,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt13')
    )
  )

# Input 14. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt14',
      loc.x = 1750,
      loc.y = 921,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt14')
    )
  )

# Input 15. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt15',
      loc.x = 2150,
      loc.y = 1194,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt15')
    )
  )

# Input 16. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt16',
      loc.x = 2200,
      loc.y = 2119,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt16')
    )
  )

# Input 17. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt17',
      loc.x = 2330,
      loc.y = 1890,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt17')
    )
  )

# Input 18. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt18',
      loc.x = 2350,
      loc.y = 196,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt18')
    )
  )

# Input 19. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt19',
      loc.x = 2550,
      loc.y = 1445,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt19')
    )
  )

# Input 20. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt20',
      loc.x = 2735,
      loc.y = 122,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt20')
    )
  )

# Input 21. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt21',
      loc.x = 2900,
      loc.y = 2331,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt21')
    )
  )

# Input 22. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt22',
      loc.x = 2950,
      loc.y = 1059,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt22')
    )
  )

# Input 23. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt23',
      loc.x = 3211,
      loc.y = 697,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt23')
    )
  )

# Input 24. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt24',
      loc.x = 3270,
      loc.y = 1749,
      rib = F,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt24')
    )
  )

# Input 25. point Kratki_Spoj_1_mjerenje_napon_80
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt25',
      loc.x = 3530,
      loc.y = 596,
      rib = T,
      replication = 2,
      file.loc = paste0(rep2.path, 'Pt25')
    )
  )

################################################################################
################################################################################
# Data input Prazni_Hod_1_mjerenje_nazivna_indukcija
# Input first point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = 125,
      loc.y = 1345,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt1')
    )
  )

# Input second point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt2',
      loc.x = 166,
      loc.y = 313,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt2')
    )
  )

# Input third point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt3',
      loc.x = 340,
      loc.y = 2052,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt3')
    )
  )

# Input fourth point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt4',
      loc.x = 516,
      loc.y = 839,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt4')
    )
  )

# Input fifth point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt5',
      loc.x = 650,
      loc.y = 977,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt5')
    )
  )

# Input 6. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt6',
      loc.x = 693,
      loc.y = 2285,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt6')
    )
  )

# Input 7. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt7',
      loc.x = 862,
      loc.y = 1567,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt7')
    )
  )

# Input 8. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt8',
      loc.x = 900,
      loc.y = 469,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt8')
    )
  )

# Input 9. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt9',
      loc.x = 1189,
      loc.y = 1405,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt9')
    )
  )

# Input 10. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt10',
      loc.x = 1273,
      loc.y = 2411,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt10')
    )
  )

# Input 11. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt11',
      loc.x = 1427,
      loc.y = 1968,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt11')
    )
  )

# Input 12. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt12',
      loc.x = 1600,
      loc.y = 487,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt12')
    )
  )

# Input 13. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt13',
      loc.x = 1750,
      loc.y = 1681,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt13')
    )
  )

# Input 14. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt14',
      loc.x = 1750,
      loc.y = 921,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt14')
    )
  )

# Input 15. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt15',
      loc.x = 2150,
      loc.y = 1194,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt15')
    )
  )

# Input 16. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt16',
      loc.x = 2200,
      loc.y = 2119,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt16')
    )
  )

# Input 17. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt17',
      loc.x = 2330,
      loc.y = 1890,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt17')
    )
  )

# Input 18. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt18',
      loc.x = 2350,
      loc.y = 196,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt18')
    )
  )

# Input 19. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt19',
      loc.x = 2550,
      loc.y = 1445,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt19')
    )
  )

# Input 20. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt20',
      loc.x = 2735,
      loc.y = 122,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt20')
    )
  )

# Input 21. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt21',
      loc.x = 2900,
      loc.y = 2331,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt21')
    )
  )

# Input 22. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt22',
      loc.x = 2950,
      loc.y = 1059,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt22')
    )
  )

# Input 23. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt23',
      loc.x = 3211,
      loc.y = 697,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt23')
    )
  )

# Input 24. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt24',
      loc.x = 3270,
      loc.y = 1749,
      rib = F,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt24')
    )
  )

# Input 25. point Prazni_Hod_1_mjerenje_nazivna_indukcija
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt25',
      loc.x = 3530,
      loc.y = 596,
      rib = T,
      replication = 3,
      file.loc = paste0(rep3.path, 'Pt25')
    )
  )

################################################################################
################################################################################
# Data input Prazni_Hod_2_mjerenje_1.3T
# Input first point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt1',
      loc.x = 125,
      loc.y = 1345,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt1')
    )
  )

# Input second point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt2',
      loc.x = 166,
      loc.y = 313,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt2')
    )
  )

# Input third point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt3',
      loc.x = 340,
      loc.y = 2052,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt3')
    )
  )

# Input fourth point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt4',
      loc.x = 516,
      loc.y = 839,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt4')
    )
  )

# Input fifth point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt5',
      loc.x = 650,
      loc.y = 977,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt5')
    )
  )

# Input 6. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt6',
      loc.x = 693,
      loc.y = 2285,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt6')
    )
  )

# Input 7. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt7',
      loc.x = 862,
      loc.y = 1567,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt7')
    )
  )

# Input 8. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt8',
      loc.x = 900,
      loc.y = 469,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt8')
    )
  )

# Input 9. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt9',
      loc.x = 1189,
      loc.y = 1405,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt9')
    )
  )

# Input 10. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt10',
      loc.x = 1273,
      loc.y = 2411,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt10')
    )
  )

# Input 11. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt11',
      loc.x = 1427,
      loc.y = 1968,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt11')
    )
  )

# Input 12. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt12',
      loc.x = 1600,
      loc.y = 487,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt12')
    )
  )

# Input 13. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt13',
      loc.x = 1750,
      loc.y = 1681,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt13')
    )
  )

# Input 14. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt14',
      loc.x = 1750,
      loc.y = 921,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt14')
    )
  )

# Input 15. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt15',
      loc.x = 2150,
      loc.y = 1194,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt15')
    )
  )

# Input 16. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt16',
      loc.x = 2200,
      loc.y = 2119,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt16')
    )
  )

# Input 17. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt17',
      loc.x = 2330,
      loc.y = 1890,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt17')
    )
  )

# Input 18. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt18',
      loc.x = 2350,
      loc.y = 196,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt18')
    )
  )

# Input 19. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt19',
      loc.x = 2550,
      loc.y = 1445,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt19')
    )
  )

# Input 20. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt20',
      loc.x = 2735,
      loc.y = 122,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt20')
    )
  )

# Input 21. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt21',
      loc.x = 2900,
      loc.y = 2331,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt21')
    )
  )

# Input 22. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt22',
      loc.x = 2950,
      loc.y = 1059,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt22')
    )
  )

# Input 23. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt23',
      loc.x = 3211,
      loc.y = 697,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt23')
    )
  )

# Input 24. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt24',
      loc.x = 3270,
      loc.y = 1749,
      rib = F,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt24')
    )
  )

# Input 25. point Prazni_Hod_2_mjerenje_1.3T
dat <- dat %>%
  add_row(
    !!!VibData(
      point.id = 'pt25',
      loc.x = 3530,
      loc.y = 596,
      rib = T,
      replication = 4,
      file.loc = paste0(rep4.path, 'Pt25')
    )
  )

################################################################################
################################################################################
## Subtitle and id modifications
dat <- dat %>%
  # filter(point.id %in% paste0("pt", seq(9))) %>%
  mutate(point.id = case_when(
    point.id %in% paste0("pt", seq(9)) ~ paste0(
      substr(point.id, 1, 2), "0", substr(point.id, 3, 3)),
    TRUE ~ point.id
  )) %>%
  mutate(subtitle = paste0(
             point.id, ': (', loc.x, ', ', loc.y, ') rib: ', rib, ' rep: ',
             replication)
         )

################################################################################
################################################################################
## Data manipulation, statistics and summary, and export
# Save entire dataset to csv
dat %>%
  mutate(velocity.fft = as.character(velocity.fft)) %$%
  write_csv(., path = './data_out/dataset.csv')

# Summary of data with top 5 peaks and distinct frequency peaks
dat_summary <- dat %>%
  select(replication, point.id, loc.x, loc.y, rib, velocity, peak.to.peak.vel,
         rms.vel, peak.frequency, peak.velocity.amp, peak.velocity.psd,
         frequency, d.freq, velocity.amp, velocity.psd) %>%
  group_by(replication, point.id) %>%
  summarise(
    Length = mean(loc.x), Height = mean(loc.y), rib = all(rib),
    "Operating condition" = case_when(
      replication == 1 ~ 'Short circuit - Voltage 100 %',
      replication == 2 ~ 'Short circuit - Voltage 80 %',
      replication == 3 ~ 'Idle operation - Nominal induction',
      replication == 4 ~ 'Idle operation - Induction 1,3 T',
      TRUE ~ 'Error')[1],
    "Velocity min" = min(velocity), "Velocity max" = max(velocity),
    "Velocity sd" = sd(velocity),
    RMS = mean(rms.vel), 'Peak-to-peak' = mean(peak.to.peak.vel),
    "Peak frequency 1" = peak.frequency[1],
    "Peak spectrum 1" = peak.velocity.amp[1],
    "Peak PSD 1" = peak.velocity.psd[1],
    "Peak frequency 2" = peak.frequency[2],
    "Peak spectrum 2" = peak.velocity.amp[2],
    "Peak PSD 2" = peak.velocity.psd[2],
    "Peak frequency 3" = peak.frequency[3],
    "Peak spectrum 3" = peak.velocity.amp[3],
    "Peak PSD 3" = peak.velocity.psd[3],
    "Peak frequency 4" = peak.frequency[4],
    "Peak spectrum 4" = peak.velocity.amp[4],
    "Peak PSD 4" = peak.velocity.psd[4],
    "Peak frequency 5" = peak.frequency[5],
    "Peak spectrum 5" = peak.velocity.amp[5],
    "Peak PSD 5" = peak.velocity.psd[5],
    "25 Hz spectrum" = na.omit(velocity.amp[
      frequency >= (25.02441-d.freq[1]/3) & frequency <= (25.02441+d.freq[1]/3)
      ]),
    "100 Hz spectrum" = na.omit(velocity.amp[
      frequency >= (100.02136-d.freq[1]/3) & frequency <= (100.02136+d.freq[1]/3)
      ]),
    "200 Hz spectrum" = max(na.omit(velocity.amp[
      frequency >= (200.04272-d.freq[1]/3) & frequency <= (200.119019+d.freq[1]/3)
      ])),
    "300 Hz spectrum" = max(na.omit(velocity.amp[
      frequency >= (300.06409-d.freq[1]/3) & frequency <= (300.140381+d.freq[1]/3)
      ])),
    "25 Hz PSD" = na.omit(velocity.psd[
      frequency >= (25.02441-d.freq[1]/3) & frequency <= (25.02441+d.freq[1]/3)
      ]),
    "100 Hz PSD" = na.omit(velocity.psd[
      frequency >= (100.02136-d.freq[1]/3) & frequency <= (100.02136+d.freq[1]/3)
      ]),
    "200 Hz PSD" = max(na.omit(velocity.psd[
      frequency >= (200.04272-d.freq[1]/3) & frequency <= (200.119019+d.freq[1]/3)
      ])),
    "300 Hz PSD" = max(na.omit(velocity.psd[
      frequency >= (300.06409-d.freq[1]/3) & frequency <= (300.140381+d.freq[1]/3)
      ])),
    )

dat_summary %$%
  write_excel_csv2(., path = './data_out/dataset_summary.csv')

# Unique peaks
dat %>%
  select(replication, point.id, peak.frequency) %>%
  na.omit() %>%
  group_by(replication, point.id) %>%
  slice(-4, -5) %>%
  ungroup() %>%
  select(peak.frequency) %>%
  unique() %>%
  arrange(desc(peak.frequency)) %>%
  rename("Unique peak frequencies" = peak.frequency) %>%
  write_excel_csv2(path = './data_out/unique_peaks.csv')

# Rib effect
dat_rib_effect <- dat_summary %>%
  select(replication, point.id, rib, `25 Hz spectrum`, `100 Hz spectrum`,
         `200 Hz spectrum`, `300 Hz spectrum`, RMS) %>%
  rename(vel.25.Hz = `25 Hz spectrum`, vel.100.Hz = `100 Hz spectrum`,
         vel.200.Hz = `200 Hz spectrum`, vel.300.Hz = `300 Hz spectrum`, vel.RMS = RMS) %>%
  nest(data = c(rib, point.id, vel.25.Hz, vel.100.Hz, vel.200.Hz, vel.300.Hz, vel.RMS)) %>% 
  mutate(t.test.25.Hz = map(data, ~t.test(.$vel.25.Hz ~ .$rib)),
         res.25.Hz = map(t.test.25.Hz, glance),
         t.test.100.Hz = map(data, ~t.test(.$vel.100.Hz ~ .$rib)),
         res.100.Hz = map(t.test.100.Hz, glance),
         t.test.200.Hz = map(data, ~t.test(.$vel.200.Hz ~ .$rib)),
         res.200.Hz = map(t.test.200.Hz, glance),
         t.test.300.Hz = map(data, ~t.test(.$vel.300.Hz ~ .$rib)),
         res.300.Hz = map(t.test.300.Hz, glance),
         t.test.RMS = map(data, ~t.test(.$vel.RMS ~ .$rib)),
         res.RMS = map(t.test.RMS, glance)
         ) %>% select(-starts_with("t.test")) %>%
  mutate_at(vars(starts_with("res")),
            ~map(., ~select(., estimate, estimate1, estimate2, p.value))) %>%
  mutate(
    res.25.Hz = map(res.25.Hz,
                    ~rename(., mean.diff.25.Hz = estimate,
                            mean.plate.25.Hz = estimate1 ,
                            mean.rib.25.Hz = estimate2,
                            p.value.25.Hz = p.value)),
    res.100.Hz = map(res.100.Hz,
                    ~rename(., mean.diff.100.Hz = estimate,
                            mean.plate.100.Hz = estimate1 ,
                            mean.rib.100.Hz = estimate2,
                            p.value.100.Hz = p.value)),
    res.200.Hz = map(res.200.Hz,
                    ~rename(., mean.diff.200.Hz = estimate,
                            mean.plate.200.Hz = estimate1 ,
                            mean.rib.200.Hz = estimate2,
                            p.value.200.Hz = p.value)),
    res.300.Hz = map(res.300.Hz,
                    ~rename(., mean.diff.300.Hz = estimate,
                            mean.plate.300.Hz = estimate1 ,
                            mean.rib.300.Hz = estimate2,
                            p.value.300.Hz = p.value)),
    res.RMS = map(res.RMS,
                    ~rename(., mean.diff.RMS = estimate,
                            mean.plate.RMS = estimate1 ,
                            mean.rib.RMS = estimate2,
                            p.value.RMS = p.value)),
    ) %>% unnest(c(res.25.Hz, res.100.Hz, res.200.Hz, res.300.Hz, res.RMS))
  
# Save rib effect
dat_rib_effect %>% select(-data) %>%
  write_excel_csv2(path = 'data_out/dataset_rib_effect.csv')

# Plot rib effect
dat_rib_effect %>%
  select(-data, -starts_with("mean.diff"), -starts_with("p.value")) %>%
  ungroup() %>% pivot_longer(starts_with("mean"), names_to = "name") %>%
  mutate(
    freq = map_chr(name, ~case_when(
      grepl('25.Hz', .)  ~ '25 Hz',
      grepl('100.Hz', .) ~ '100 Hz',
      grepl('200.Hz', .) ~ '200 Hz',
      grepl('300.Hz', .) ~ '300 Hz',
      grepl('RMS', .) ~ 'RMS',
      TRUE ~ 'None',)),
    rib = map_lgl(name, ~case_when(
      grepl('plate', .)  ~ F,
      grepl('rib', .)  ~ T,
      TRUE ~ F,))
    ) %>%
  mutate(
    freq = factor(freq,
                  levels = c('25 Hz', '100 Hz', '200 Hz', '300 Hz', 'RMS'),
                  ordered = T)) %>%
  mutate(replication = case_when(
    replication == 1 ~ 'Short circuit - Voltage 100 %',
    replication == 2 ~ 'Short circuit - Voltage 80 %',
    replication == 3 ~ 'Idle operation - Nominal induction',
    replication == 4 ~ 'Idle operation - Induction 1,3 T',
    TRUE ~ "None",
  )) %>% mutate(
    replication = factor(replication, ordered = T,
                            levels = c('Short circuit - Voltage 100 %',
                                       'Short circuit - Voltage 80 %',
                                       'Idle operation - Nominal induction',
                                       'Idle operation - Induction 1,3 T'))) %>%
  rib.effect.plot(
    y.name = 'value',
    x.name = 'freq',
    condition = 'replication',
    groups = 'rib',
    title = 'Rib effect - distinct frequencies',
    subtitle = 'Space-time average of vibration velocity',
    table.panel.order = T,
    aspect = 3/4,
    y.tick.num = 10,
    log.y = 10,
    line.width = 1.5,
    show.legend = list(text =
                         c('rib: FALSE',
                           'rib: TRUE')),
    col.palette.name = 'Set1',
    col.different.num = 6,
    col.line.num = 1,
    col.alpha = 1,
    fig.height = 8,
    save.fig = T,
    fig.filename = paste0(figs.path, 'rib_effect.png')
  )

################################################################################
################################################################################
# Plot velocity frequency spectrum for Kratki_Spoj_1_mjerenje_napon_100
dat %>%
  filter(replication == 1) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity frequency spectrum',
    subtitle = 'Short circuit - Voltage 100 %',
    ylab = expression('Velocity [' * mu * m/s * ']'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 7,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_100_vel_freq.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot velocity frequency spectrum for Kratki_Spoj_2_mjerenje_napon_80
dat %>%
  filter(replication == 2) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity frequency spectrum',
    subtitle = 'Short circuit - Voltage 80 %',
    ylab = expression('Velocity [' * mu * m/s * ']'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 7,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_80_vel_freq.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot velocity frequency spectrum for Prazni_Hod_1_mjerenje_nazivna_indukcija
dat %>%
  filter(replication == 3) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity frequency spectrum',
    subtitle = 'Idle operation - Nominal induction',
    ylab = expression('Velocity [' * mu * m/s * ']'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 8,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_nominal_induction_vel_freq.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot velocity frequency spectrum for Prazni_Hod_2_mjerenje_1.3T
dat %>%
  filter(replication == 4) %>%
  select(frequency, velocity.amp, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity frequency spectrum',
    subtitle = 'Idle operation - Induction 1,3 T',
    ylab = expression('Velocity [' * mu * m/s * ']'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 8,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_induction_1.3T_vel_freq.png'),
    fig.width = 23,
    fig.height = 16,
  )

################################################################################
################################################################################
# Plot velocity power spectral density for Kratki_Spoj_1_mjerenje_napon_100
dat %>%
  filter(replication == 1) %>%
  select(frequency, velocity.psd, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity power spectral density',
    subtitle = 'Short circuit - Voltage 100 %',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 6,
    log.y = 10,
    yscale.components.function = yscale.components.log10ticks,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_100_vel_psd.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot velocity power spectral density for Kratki_Spoj_2_mjerenje_napon_80
dat %>%
  filter(replication == 2) %>%
  select(frequency, velocity.psd, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity power spectral density',
    subtitle = 'Short circuit - Voltage 80 %',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 6,
    log.y = 10,
    yscale.components.function = yscale.components.log10ticks,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_80_vel_psd.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot velocity power spectral density for Prazni_Hod_1_mjerenje_nazivna_indukcija
dat %>%
  filter(replication == 3) %>%
  select(frequency, velocity.psd, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity power spectral density',
    subtitle = 'Idle operation - Nominal induction',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 7,
    log.y = 10,
    yscale.components.function = yscale.components.log10ticks,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_nominal_induction_vel_psd.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot velocity power spectral density for Prazni_Hod_2_mjerenje_1.3T
dat %>%
  filter(replication == 4) %>%
  select(frequency, velocity.psd, subtitle) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'frequency',
    condition = 'subtitle',
    title = 'Velocity power spectral density',
    subtitle = 'Idle operation - Induction 1,3 T',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 600,
    x.tick.num = 10, y.tick.num = 6,
    log.y = 10,
    yscale.components.function = yscale.components.log10ticks,
    line.width = 3,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_induction_1.3T_vel_psd.png'),
    fig.width = 23,
    fig.height = 16,
  )

################################################################################
################################################################################
## Autocorrelation plots
# ACF for Kratki_Spoj_1_mjerenje_napon_100
dat %>%
  filter(replication == 1) %>%
  select(acf, subtitle, time) %>%
  acf.pacf.plot(
    y.name = 'acf',
    x.name = 'time',
    condition = 'subtitle',
    title = 'Velocity autocorrelation',
    subtitle = 'Short circuit - Voltage 100 %',
    ylab='Autocorrelation [ / ]',
    x.min = -0.01, x.max = max(.$time),
    x.tick.num = 20, y.tick.num = 10,
    aspect = 1/2,
    strip.left = F,
    strip.top = T,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_100_vel_acf.png'),
    fig.width = 23,
    fig.height = 16,
  )

# ACF for Kratki_Spoj_2_mjerenje_napon_80
dat %>%
  filter(replication == 2) %>%
  select(acf, subtitle, time) %>%
  acf.pacf.plot(
    y.name = 'acf',
    x.name = 'time',
    condition = 'subtitle',
    title = 'Velocity autocorrelation',
    subtitle = 'Short circuit - Voltage 80 %',
    ylab='Autocorrelation [ / ]',
    x.min = -0.01, x.max = max(.$time),
    x.tick.num = 20, y.tick.num = 10,
    aspect = 1/2,
    strip.left = F,
    strip.top = T,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_80_vel_acf.png'),
    fig.width = 23,
    fig.height = 16,
  )

# ACF for Prazni_Hod_1_mjerenje_nazivna_indukcija
dat %>%
  filter(replication == 3) %>%
  select(acf, subtitle, time) %>%
  acf.pacf.plot(
    y.name = 'acf',
    x.name = 'time',
    condition = 'subtitle',
    title = 'Velocity autocorrelation',
    subtitle = 'Idle operation - Nominal induction',
    ylab='Autocorrelation [ / ]',
    x.min = -0.01, x.max = max(.$time),
    x.tick.num = 20, y.tick.num = 10,
    aspect = 1/2,
    strip.left = F,
    strip.top = T,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_nominal_induction_vel_acf.png'),
    fig.width = 23,
    fig.height = 16,
  )

# ACF for Prazni_Hod_2_mjerenje_1.3T
dat %>%
  filter(replication == 4) %>%
  select(acf, subtitle, time) %>%
  acf.pacf.plot(
    y.name = 'acf',
    x.name = 'time',
    condition = 'subtitle',
    title = 'Velocity autocorrelation',
    subtitle = 'Idle operation - Induction 1,3 T',
    ylab='Autocorrelation [ / ]',
    x.min = -0.01, x.max = max(.$time),
    x.tick.num = 20, y.tick.num = 10,
    aspect = 1/2,
    strip.left = F,
    strip.top = T,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_induction_1.3T_vel_acf.png'),
    fig.width = 23,
    fig.height = 16,
  )

################################################################################
################################################################################
## Peaks plots
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
    subtitle = 'Comparison of all measurements',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 600, x.tick.num = 15,
    log.y = 10,
    yscale.components.function = yscale.components.log10ticks,
    y.tick.num = 1000,
    jitter.x = T, jitter.amount = 5,
    line.width = 4,
    aspect = 1/2,
    show.legend = list(text =
                         c('Short circuit - Voltage 100 %',
                           'Short circuit - Voltage 80 %',
                           'Idle operation - Nominal induction',
                           'Idle operation - Induction 1,3 T')),
    col.palette.name = 'Set1',
    col.different.num = 6,
    col.line.num = 1,
    col.alpha = 1,
    save.fig = T,
    fig.filename = paste0(figs.path, 'all_measurements_vel_psd_peaks.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot Velocity spectrum peaks
dat %>%
  select(peak.frequency, peak.velocity.amp, subtitle, point.id, replication) %>%
  na.omit() %>%
  freq.spec.plot(
    y.name = 'peak.velocity.amp',
    x.name = 'peak.frequency',
    condition = 'point.id',
    groups = 'replication',
    title = 'Top 5 Velocity Spectrum Peaks',
    subtitle = 'Comparison of all measurements',
    ylab = expression('Velocity [' * mu * m/s * ']'),
    x.max = 600, x.tick.num = 15,
    # log.y = 10,
    # yscale.components.function = yscale.components.log10ticks,
    y.tick.num = 7,
    jitter.x = T, jitter.amount = 6,
    line.width = 5,
    aspect = 1/2,
    show.legend = list(text =
                         c('Short circuit - Voltage 100 %',
                           'Short circuit - Voltage 80 %',
                           'Idle operation - Nominal induction',
                           'Idle operation - Induction 1,3 T')),
    col.palette.name = 'Set1',
    col.different.num = 6,
    col.line.num = 1,
    col.alpha = 1,
    save.fig = T,
    fig.filename = paste0(figs.path, 'all_measurements_vel_amp_peaks.png'),
    fig.width = 23,
    fig.height = 16,
  )

################################################################################
################################################################################
## Distinct frequencies plot
# Plot Velocity spectrum for distinct frequencies
dat_summary %>%
  ungroup() %>%
  select(replication, point.id, Length, Height, rib, `25 Hz spectrum`, `100 Hz spectrum`,
         `200 Hz spectrum`, `300 Hz spectrum`) %>%
  mutate_at(vars(matches("spectrum")), as.character) %>%
  gather(key = "distinct.frequencies", value = "velocity.amp", "25 Hz spectrum",
         "100 Hz spectrum", "200 Hz spectrum", "300 Hz spectrum") %>%
  mutate(distinct.frequencies = as.numeric(str_sub(distinct.frequencies, 1, 3)),
         velocity.amp = as.numeric(velocity.amp)) %>%
  freq.spec.plot(
    y.name = 'velocity.amp',
    x.name = 'distinct.frequencies',
    condition = 'point.id',
    groups = 'replication',
    title = 'Velocity Spectrum Peaks at distinct frequencies',
    subtitle = 'Comparison of all measurements',
    ylab = expression('Velocity [' * mu * m/s * ']'),
    x.max = 400, x.tick.num = 15,
    # log.y = 10,
    # yscale.components.function = yscale.components.log10ticks,
    y.tick.num = 7,
    jitter.x = T, jitter.amount = 6,
    line.width = 6,
    aspect = 1/2,
    show.legend = list(text =
                         c('Short circuit - Voltage 100 %',
                           'Short circuit - Voltage 80 %',
                           'Idle operation - Nominal induction',
                           'Idle operation - Induction 1,3 T')),
    col.palette.name = 'Set1',
    col.different.num = 6,
    col.line.num = 1,
    col.alpha = 1,
    save.fig = T,
    fig.filename = paste0(figs.path, 'all_measurements_vel_amp_dist_freq.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Plot Velocity power spectral density for distinct frequencies
dat_summary %>%
  ungroup() %>%
  select(replication, point.id, Length, Height, rib, `25 Hz PSD`, `100 Hz PSD`,
         `200 Hz PSD`, `300 Hz PSD`) %>%
  mutate_at(vars(matches("PSD")), as.character) %>%
  gather(key = "distinct.frequencies", value = "velocity.psd", "25 Hz PSD",
         "100 Hz PSD", "200 Hz PSD", "300 Hz PSD") %>%
  mutate(distinct.frequencies = as.numeric(str_sub(distinct.frequencies, 1, 3)),
         velocity.psd = as.numeric(velocity.psd)) %>%
  freq.spec.plot(
    y.name = 'velocity.psd',
    x.name = 'distinct.frequencies',
    condition = 'point.id',
    groups = 'replication',
    title = 'Velocity Power spectral density at distinct frequencies',
    subtitle = 'Comparison of all measurements',
    ylab = expression('Velocity [' * mu * m^2 * ' / (' * s^2 * Hz * ') ]'),
    x.max = 400, x.tick.num = 15,
    log.y = 10,
    yscale.components.function = yscale.components.log10ticks,
    y.tick.num = 1000,
    jitter.x = T, jitter.amount = 6,
    line.width = 6,
    aspect = 1/2,
    show.legend = list(text =
                         c('Short circuit - Voltage 100 %',
                           'Short circuit - Voltage 80 %',
                           'Idle operation - Nominal induction',
                           'Idle operation - Induction 1,3 T')),
    col.palette.name = 'Set1',
    col.different.num = 6,
    col.line.num = 1,
    col.alpha = 1,
    save.fig = T,
    fig.filename = paste0(figs.path, 'all_measurements_vel_psd_dist_freq.png'),
    fig.width = 23,
    fig.height = 16,
  )

################################################################################
################################################################################
## Stationarity test
# Normalized RMS for Kratki_Spoj_1_mjerenje_napon_100
dat %>%
  select(point.id, loc.x, loc.y, replication, subtitle, velocity) %>%
  group_by(replication, point.id) %>%
  mutate(split.segments = rep(1:100, each=ceiling(n()/100))[1:n()]) %>%
  group_by(replication, point.id, split.segments) %>%
  summarise(segment.rms = sqrt(mean(velocity^2)),
            subtitle = subtitle[1]) %>%
  mutate(segment.rms.n = segment.rms/mean(segment.rms)) %>%
  filter(replication == 1) %>%
  freq.spec.plot(
    y.name = 'segment.rms.n',
    x.name = 'split.segments',
    condition = 'subtitle',
    title = 'Normalized RMS by 100 frames',
    subtitle = 'Short circuit - Voltage 100 %',
    ylab = expression('Normalized RMS [ / ]'),
    xlab = expression('Frame number [ / ]'),
    plot.type = c('l'),
    x.min = 1, x.max = 100,
    x.tick.num = 9, y.tick.num = 10,
    line.width = 6,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_100_normal_rms.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Normalized RMS for Kratki_Spoj_1_mjerenje_napon_80
dat %>%
  select(point.id, loc.x, loc.y, replication, subtitle, velocity) %>%
  group_by(replication, point.id) %>%
  mutate(split.segments = rep(1:100, each=ceiling(n()/100))[1:n()]) %>%
  group_by(replication, point.id, split.segments) %>%
  summarise(segment.rms = sqrt(mean(velocity^2)),
            subtitle = subtitle[1]) %>%
  mutate(segment.rms.n = segment.rms/mean(segment.rms)) %>%
  filter(replication == 2) %>%
  freq.spec.plot(
    y.name = 'segment.rms.n',
    x.name = 'split.segments',
    condition = 'subtitle',
    title = 'Normalized RMS by 100 frames',
    subtitle = 'Short circuit - Voltage 80 %',
    ylab = expression('Normalized RMS [ / ]'),
    xlab = expression('Frame number [ / ]'),
    plot.type = c('l'),
    x.min = 1, x.max = 100,
    x.tick.num = 9, y.tick.num = 8,
    line.width = 6,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Short_circuit_voltage_80_normal_rms.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Normalized RMS for Prazni_Hod_1_mjerenje_nazivna_indukcija
dat %>%
  select(point.id, loc.x, loc.y, replication, subtitle, velocity) %>%
  group_by(replication, point.id) %>%
  mutate(split.segments = rep(1:100, each=ceiling(n()/100))[1:n()]) %>%
  group_by(replication, point.id, split.segments) %>%
  summarise(segment.rms = sqrt(mean(velocity^2)),
            subtitle = subtitle[1]) %>%
  mutate(segment.rms.n = segment.rms/mean(segment.rms)) %>%
  filter(replication == 3) %>%
  freq.spec.plot(
    y.name = 'segment.rms.n',
    x.name = 'split.segments',
    condition = 'subtitle',
    title = 'Normalized RMS by 100 frames',
    subtitle = 'Idle operation - Nominal induction',
    ylab = expression('Normalized RMS [ / ]'),
    xlab = expression('Frame number [ / ]'),
    plot.type = c('l'),
    x.min = 1, x.max = 100,
    x.tick.num = 9, y.tick.num = 8,
    line.width = 6,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_nominal_induction_normal_rms.png'),
    fig.width = 23,
    fig.height = 16,
  )

# Normalized RMS for Prazni_Hod_2_mjerenje_1.3T
dat %>%
  select(point.id, loc.x, loc.y, replication, subtitle, velocity) %>%
  group_by(replication, point.id) %>%
  mutate(split.segments = rep(1:100, each=ceiling(n()/100))[1:n()]) %>%
  group_by(replication, point.id, split.segments) %>%
  summarise(segment.rms = sqrt(mean(velocity^2)),
            subtitle = subtitle[1]) %>%
  mutate(segment.rms.n = segment.rms/mean(segment.rms)) %>%
  filter(replication == 4) %>%
  freq.spec.plot(
    y.name = 'segment.rms.n',
    x.name = 'split.segments',
    condition = 'subtitle',
    title = 'Normalized RMS by 100 frames',
    subtitle = 'Idle operation - Induction 1,3 T',
    ylab = expression('Normalized RMS [ / ]'),
    xlab = expression('Frame number [ / ]'),
    plot.type = c('l'),
    x.min = 1, x.max = 100,
    x.tick.num = 9, y.tick.num = 8,
    line.width = 6,
    aspect = 1/2,
    show.legend = F,
    save.fig = T,
    fig.filename = paste0(figs.path, 'Idle_operation_induction_1.3T_normal_rms.png'),
    fig.width = 23,
    fig.height = 16,
  )

################################################################################
################################################################################
## Cross-correlations
# Cross-correlation calculation
print('Calculating cross-correlation ...')
dat_ccf <- dat %>%
  # filter(point.id %in% c('pt03', 'pt06', 'pt21')) %>%
  select(replication, point.id, time, velocity) %>%
  spread(key = replication, value = velocity) %>%
  # Promijeniti downsample
  slice(which(row_number() %% 800 == 0)) %>%
  na.omit() %>%
  group_by(point.id) %>%
  nest() %>%
  mutate(ccf.dat = map(data,
                   function(df) {
                     df_names <- combn(names(df)[-1], 2)
                     out <- list()
                     for (i in 1:ncol(df_names)) {
                       x <- df[[df_names[,i][1]]]
                       y <- df[[df_names[,i][2]]]
                       out_name <- paste0("rep", df_names[,i][1], "-",
                                          df_names[,i][2])
                       out[[out_name]] <- as.vector(
                         ccf(x, y, lag.max = 2*length(x), plot = F)$acf
                         )
                     }
                     return(out)
                      }),
         time = map(data, function(df) {
                     df_names <- combn(names(df)[-1], 2)
                     out <- list()
                     for (i in 1:ncol(df_names)) {
                       out_name <- paste0("rep", df_names[,i][1], "-",
                                          df_names[,i][2])
                       out[[out_name]] <- c(rev(-df$time[-1]), df$time)
                     }
                     return(out)
                     })
         ) %>% select(point.id, ccf.dat, time) %>%
  unnest_legacy(ccf.dat, time) %>%
  unnest_legacy(ccf.dat, time, .id = "replication.ccf") %>%
  select(point.id, replication.ccf, time, ccf.dat) %>%
  group_by(point.id) %>%
  mutate(replication.ccf = case_when(
    # Promijeniti ovisno o broju kombinacija
    replication.ccf %% 6 == 1 ~ '1-2',
    replication.ccf %% 6 == 2 ~ '1-3',
    replication.ccf %% 6 == 3 ~ '1-4',
    replication.ccf %% 6 == 4 ~ '2-3',
    replication.ccf %% 6 == 5 ~ '2-4',
    replication.ccf %% 6 == 0 ~ '3-4',
    TRUE ~ '0'
  )) %>%
  mutate(subtitle = paste0(point.id, ', rep: ', replication.ccf))
print('Finished cross-correlation calculation ...')

# Save current workspace
save.image()

# Plot ccf for all points all measurements
dat_ccf %>%
  acf.pacf.plot(
    y.name = 'ccf.dat',
    x.name = 'time',
    condition = 'subtitle',
    title = 'Međukorelacija brzine po pojedinim točkama kroz sva mjerenja',
    subtitle = 'Usporedba svih mjernih točaka',
    ylab = 'Međukorelacija [ / ]',
    xlab = 'Vrijeme [s]',
    x.min = min(.$time), x.max = max(.$time),
    x.tick.num = 12, y.tick.num = 7,
    aspect = 1/3,
    strip.left = F,
    strip.top = T,
    save.fig = T,
    fig.filename = paste0(figs.path, 'all_measurements_vel_ccf.png'),
    fig.width = 16,
    fig.height = 23,
  )

# 
dat_ccf %>%
  group_by(point.id, replication.ccf) %>% summarise(ccf.max = max(ccf.dat)) %>%
  # group_by(replication.ccf) %>% summarise(ccf.mean = mean(ccf.max))
  mutate(replication.ccf = case_when(
    replication.ccf == "1-2" ~ "Kratki spoj, napon 100 % - Napon 80 %",
    replication.ccf == "1-3" ~ "Kratki spoj, napon 100 % - Prazni hod, nominalna indukcija",
    replication.ccf == "1-4" ~ "Kratki spoj, napon 100 % - Prazni hod, indukcija 1,3 T",
    replication.ccf == "2-3" ~ "Kratki spoj, napon 80 % - Prazni hod, nominalna indukcija",
    replication.ccf == "2-4" ~ "Kratki spoj, napon 80 % - Prazni hod, indukcija 1,3 T",
    replication.ccf == "3-4" ~ "Prazni hod, nominalna indukcija - Prazni hod, indukcija 1,3 T",
    TRUE ~ "None"
  )) %>%
  spread(key = replication.ccf, value = ccf.max) %$%
  write_excel_csv2(., path = './data_out/dataset_summary_ccf.csv')

################################################################################
################################################################################
## Noise - Vibration connection
# Noise sound power [W]
dat_noise <- tribble(
  ~replication,  ~sound.power,
  1,  0.00004532744649895240,
  2,  0.00003135893132064630,
  3,  0.00006114504288974570,
  4,  0.00005204287185097840
) %>% mutate(sound.power =sound.power*1e6)

# Join data for modeling
dat_noise <- dat_summary %>% group_by(replication) %>% select(replication,
  point.id, RMS, `25 Hz spectrum`, `100 Hz spectrum`, `200 Hz spectrum`,
  `300 Hz spectrum`) %>% summarise(
    sq.me.RMS = mean(RMS)^2,
    sq.me.25.Hz = mean(`25 Hz spectrum`)^2,
    sq.me.100.Hz = mean(`100 Hz spectrum`)^2,
    sq.me.200.Hz = mean(`200 Hz spectrum`)^2,
    sq.me.300.Hz = mean(`300 Hz spectrum`)^2,
    ) %$% full_join(dat_noise, ., by = c("replication"))

# Sound power correlation
dat_noise %>%
  gather(key = "sq.me.freq", value = "sq.me.amp", -sound.power, -replication) %>%
  nest(data = c(replication, sound.power, sq.me.amp)) %>%
  mutate(cor = map(data, ~ cor.test(~sound.power + sq.me.amp, data = ., conf.level = 0.68)),
         results = map(cor, glance)
         ) %>% unnest(results) %>%
  select(-data, -statistic, -p.value, -parameter, -alternative, -cor) %>%
  mutate(conf.level = 0.68) %>% rename(correlation = estimate) %>%
  select(sq.me.freq, correlation, conf.low, conf.high, conf.level, method) %>%
  write_excel_csv2(path = './data_out/sound_power_correlation.csv')
  
# Sound resistance and efficiency
tibble(
  freq = seq(25, 800, by = 1),
  rad.eff = sapply(freq,
                   function(freq) baff.plate.rad.eff(3.6, 2.6, 0.008, freq)),
  rad.res = rad.eff / 1e6 * 1.2 * 343 * 3.6 * 2.6
) %$%
  xyplot(rad.res ~ freq, data = ., type = "l", scales = list(
    x = list(log = F), y = list(log = 10)),
    yscale.components = yscale.components.logpower,
    xscale.components = xscale.components.default,
    )

# Radiation resistance at different frequencies
rad_eff <- tibble(
  freq = c(25, 100, 200, 300),
  rad.eff = sapply(freq,
                   function(freq) baff.plate.rad.eff(3.6, 2.6, 0.008, freq)),
  rad.res = rad.eff / 1e6 * 1.2 * 343 * 3.6 * 2.6,
  norm.rad.res = rad.res / min(rad.res)
)

rad_eff %>%
  write_excel_csv2(path = './data_out/sound_power_radiation_resistance.csv')

# Comparison with measured sound power
dat_noise %>%
  mutate(sound.power.th = sq.me.25.Hz*rad_eff$rad.res[1] +
           sq.me.100.Hz*rad_eff$rad.res[2] + sq.me.200.Hz*rad_eff$rad.res[3] +
           sq.me.300.Hz*rad_eff$rad.res[4]) %>%
  write_excel_csv2(path = './data_out/sound_power_theor_real.csv')
  

################################################################################
################################################################################
# Read modeled data from xlsx and remove negative values
dat_model <- tibble(
  replication = rep(1:4, each = 5),
  freq = rep(c("25 Hz", "100 Hz", "200 Hz", "300 Hz", "RMS"), times = 4),
) %>%
  mutate(data = map(c(3:6, 19, 7:10, 20, 11:14, 21, 15:18, 22),
                    ~ read_excel('data_out/dataset_koncar_plohe.xlsx', sheet = .,
                                 col_types = "numeric", col_names = F,
                                 .name_repair = "universal"))) %>%
  unnest(cols = data) %>%
  mutate_at(vars(starts_with("...")), function(.) {.[.<0] = 0; return(.)}) %>%
  nest(data = paste0("...", 1:15))
  
# Contourplots of modeled data
dat_model %>%
  mutate(subtitle = case_when(
    replication == 1 ~ 'Short circuit - Voltage 100 %',
    replication == 2 ~ 'Short circuit - Voltage 80 %',
    replication == 3 ~ 'Idle operation - Nominal induction',
    replication == 4 ~ 'Idle operation - Induction 1,3 T',
    TRUE ~ "None"
  ),
  title = paste0('Vibration velocity surface distribution - ', freq),
  fig.filename = paste0('slike/stvarna_mjerenja/contourplots/', replication, '_',
                        gsub(" ", "_", freq), '.png')) %>%
  group_by(replication, freq) %>%
  mutate(plt = map(data, ~contour.plot.rib.freq(
    matrix.data = t(unname(as.matrix(.))),
    title = title, subtitle = subtitle, save.fig = T,
    fig.filename = fig.filename
  )))

# Contourplots of modeled data font modifications for DAS2020
dat_model %>% filter(replication == 1 & freq == "RMS") %>%
  select(data) %>% unnest(data) %$%
  contour.plot.rib.freq(
    matrix.data = t(unname(as.matrix(.))),
    title='', subtitle='',
    xlab='Width [mm]', ylab='Height [mm]',
    fig.width = 5,
    fig.height = 3,
    aspect = "iso",
    z.tick.num = 5,
    x.tick.spc = 1200,
    y.tick.spc = 1250,
    x.lab.rot = 0,
    contour.num = 10,
    cnt.lbl = F,
    cnt.lbl.cex = 0.8,
    x.lab.cex = 1.1,
    y.lab.cex = 1.1,
    ck.lbl.cex = 1.0,
    axis.lab.cex = 1.1,
    fig.dpi = 360,
    save.fig = T,
    fig.filename = 'slike/stvarna_mjerenja/contourplots/RMS_1_DAS2020.png'
  )

