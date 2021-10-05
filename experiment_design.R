
##### Data analysis and plot #####

## Libraries
# DoE
library(DoE.wrapper)
library(DiceDesign)
# Data Table
library(data.table)
# Cleaning data
library(tidyr)
library(tibble)
library(dplyr)
library(magrittr)
# Plotting
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(tactile)


################################################################################
## Random number for seed
# DoE.seed <- round(runif(1) * 1000000)
DoE.seed <- 35440
print(c('Seed: ', DoE.seed))

## Inputa data
# Power transfomer plate height and width
width.x <- 3610
height.y <- 2600
# Power transformer rib dimensions
rib.flat.x <- 130
rib.rad.x <- 30
# Power transformer dimensions between ribs
flat.x <- 380
element.no <- 13

map_to_plate.x <- function(x) {
  # Map values from 0 to 1 to real dimensions of power transformer
  # Evade rib radius
  
  # Find first lower border
  border <- floor(x * element.no) / element.no
  
  # Determine if is a rib or not and create multiplier
  if ((border * element.no) %% 2 == 0) {
    multiplier <- rib.flat.x
  }
  else {
    multiplier <- flat.x
  }
  
  # Determine preceding length
  prec.length.x <- cumsum(c(
    30, rep(c(rib.flat.x + rib.rad.x, flat.x + rib.rad.x), times = 6)
    ))
  prec.length.ind <- round(border * element.no) + 1
  
  return(
    round(
      (x - border) * element.no * multiplier + prec.length.x[prec.length.ind],
      digits = 1
    )
    )
}

################################################################################
## Latin Hypercube experiment design
# Relative coordinates
pts.rel <- lhs.design(
  nruns = 26, nfactors = 2, type = 'optimum', seed = DoE.seed,
  factor.names = list(
    loc.x = c(0, 1),
    loc.y = c(0, 1)
    )
  )

# Absolute coordinates
pts.abs <- pts.rel %>%
  mutate(loc.y = round(loc.y * height.y, digits = 1),
         loc.x = sapply(loc.x, map_to_plate.x))

################################################################################
# Plot DoE points
# Plot ribs with radius
rib.boundaries <- cumsum(
  c(0, rib.rad.x, rib.flat.x, rib.rad.x,
    rep(c(flat.x, rib.rad.x, rib.flat.x, rib.rad.x), times=6)))

# Color palette
custom.colors <- brewer.pal(7, 'Set1')
# Add alpha
custom.colors.settings <- list(
  plot.symbol = list(
    cex = 2.4,
    col = custom.colors[1],
    fill = '#ffffff',
    pch = 21),
  superpose.symbol = list(
    cex = rep(1.4, 7),
    col = custom.colors,
    pch = rep(20, 7)),
  add.text = list(
    cex = 0.9,
    col = '#000000'
  )
)

# Grids and ticks
x.at <- seq(0, width.x, by = 200)
y.at <- seq(0, height.y, by = 200)

# Saving image
png(
  filename = 'DoE/lh_26.png',
  units = 'in',
  width = 15,
  height = 10,
  pointsize = 12 * 300 / 72,
  res = 300,
  bg = 'white'
)
pts.abs %$%
  xyplot(loc.y ~ loc.x, data = .,
         main = 'DoE 26 mjernih točaka',
         sub = 'Optimized Latin Hypercube Design',
         xlab = 'Širina [mm]',
         ylab = 'Visina [mm]',
         par.settings = custom.colors.settings,
         scales = list(
           x = list(at = x.at, rot = 45),
           y = list(at = y.at)
         ),
         panel = function(...) {
           panel.abline(v = x.at, h = y.at, col.line = 'lightgrey', ...)
           panel.xyplot(...)
           panel.abline(v = rib.boundaries)
         })
dev.off()

# Printing and saving measuring points
pts.abs
pts.abs %>%
  write.table(file = 'DoE/lh_26.csv', sep = ";", dec = ",")

################################################################################
# Augment DoE points
pts.rel.aug <- lhs.augment(
  pts.rel, m = 13, type = 'optAugment', seed = DoE.seed)

# Absolute coordinates for augmented points
pts.abs.aug <- pts.rel.aug %>%
  mutate(loc.y = round(loc.y * height.y, digits = 1),
         loc.x = sapply(loc.x, map_to_plate.x))

# Differentiate points
pts.abs.aug <- pts.abs.aug %>%
  mutate(orig = loc.x %in% pts.abs$loc.x)

# Plot augmented DoE
png(
  filename = 'DoE/lh_aug_39.png',
  units = 'in',
  width = 15,
  height = 10,
  pointsize = 12 * 300 / 72,
  res = 300,
  bg = 'white'
)
pts.abs.aug %$%
  xyplot(loc.y ~ loc.x, data = ., groups = orig,
         main = 'Prošireni DoE 39 mjernih točaka',
         sub = 'Optimized Latin Hypercube Design',
         xlab = 'Širina [mm]',
         ylab = 'Visina [mm]',
         par.settings = custom.colors.settings,
         scales = list(
           x = list(at = x.at, rot = 45),
           y = list(at = y.at)
         ),
         panel = function(...) {
           panel.abline(v = x.at, h = y.at, col.line = 'lightgrey', ...)
           panel.xyplot(...)
           panel.abline(v = rib.boundaries)
         })
dev.off()

# Printing and saving measuring points
row.names(pts.abs.aug)
pts.abs.aug %>%
  write.table(file = 'DoE/lh_aug_39.csv', sep = ";", dec = ",")

################################################################################
# L-CVT DoE
pts.lcvt.abs <- tibble::tribble(
  ~loc.x,   ~loc.y,
  125, 1345,
  166,  313,
  340, 2052,
  516,  839,
  650,  977,
  693, 2285,
  862, 1567,
  900,  469,
  1189, 1405,
  1273, 2411,
  1427, 1968,
  1600,  487,
  1750, 1681,
  1750,  921,
  2150, 1194,
  2200, 2119,
  2330, 1890,
  2350,  196,
  2550, 1445,
  2735,  122,
  2900, 2331,
  2950, 1059,
  3211,  697,
  3270, 1749,
  3530,  596
)

# Plot and save L-CVT DoE as bitmap image
png(
  filename = 'DoE/l_cvt_small.png',
  units = 'mm',
  width = 120,
  height = 60,
  pointsize = 7,
  res = 320,
  bg = 'white'
)
trellis.par.set(
  layout.widths = list(
    left.padding = 0,
    right.padding = 0,
    ylab.axis.padding = 0,
    key.ylab.padding = 0,
    axis.key.padding = 0,
    between = 0
  ),
  layout.heights = list(
    top.padding = 0.2,
    bottom.padding = 0.3,
    main.key.padding = 0.2,
    key.axis.padding = 0.2,
    axis.xlab.padding = 0.2,
    key.sub.padding = 0.2,
    axis.key.padding = 0.2
  ),
  axis.components = list(
    left = list(
      pad1 = 0.75, pad2 = 0.75
    ),
    top = list(
      pad1 = 0.7, pad2 = 0.7
    ),
    right = list(
      pad1 = 0.75, pad2 = 0.75
    ),
    bottom = list(
      pad1 = 0.7, pad2 = 0.7
    )
  )
)
pts.lcvt.abs %>%
  xyplot(loc.y ~ loc.x, data = .,
         # main = 'DoE L-CVT ',
         xlab = list(label = 'Width [mm]', cex = 1.1),
         ylab = list(label = 'Height [mm]', cex = 1.1),
         # xlab = '',
         # ylab = '',
         aspect ='iso',
         par.settings = custom.colors.settings,
         scales = list(
           cex = 1.1,
           x = list(at = c(0, 1200, 2400, 3600)), # x.at
           y = list(at = c(0, 1250, 2500)) # y.at
         ),
         panel = function(loc.x, loc.y, ...) {
           # panel.abline(v = x.at, h = y.at, col.line = 'lightgrey', ...)
           panel.abline(v = rib.boundaries, ...)
           panel.xyplot(fill = "dodgerblue4", col = "dodgerblue4", cex=1,...)
           # panel.text(..., label = row.names(.))
         })
dev.off()

# Plot and save L-CVT DoE as vector image
postscript(
  file = 'DoE/l_cvt.ps',
  onefile = T,
  width = 15,
  height = 10,
  pointsize = 12 * 300 / 72,
  bg = 'transparent'
)
pts.lcvt.abs %>%
  xyplot(loc.y ~ loc.x, data = .,
         # main = 'DoE LCVT ',
         xlab = '',
         ylab = '',
         par.settings = custom.colors.settings,
         scales = list(
           x = list(at = x.at, rot = 45),
           y = list(at = y.at)
         ),
         panel = function(loc.x, loc.y, ...) {
           panel.abline(v = x.at, h = y.at, col.line = 'lightgrey', ...)
           panel.xyplot(...)
           panel.abline(v = rib.boundaries, ...)
           panel.text(..., label = row.names(.))
         })
dev.off()

################################################################################
# L-CVT DoE analysis
pts.lcvt.abs %>%
  mutate(loc.x = loc.x / width.x,
         loc.y = loc.y / height.y) %$%
  tibble(coverage = coverage(.),
         !!!discrepancyCriteria(.),
         mesh.ratio = meshRatio(.),
         min.dist = mindist(.),
         S.optimality = Scalc(.)
         )

pts.abs %>%
  mutate(loc.x = loc.x / width.x,
         loc.y = loc.y / height.y) %$%
  tibble(coverage = coverage(.),
         !!!discrepancyCriteria(.),
         mesh.ratio = meshRatio(.),
         min.dist = mindist(.),
         S.optimality = Scalc(.)
         )

