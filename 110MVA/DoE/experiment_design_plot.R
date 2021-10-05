
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
# Saving data
library(readr)
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
width.x <- 3640
height.y <- 2800
# Power transformer rib dimensions
rib.flat.x <- 210
rib.rad.x <- 5
rib.tot.x <- rib.flat.x + 2 * rib.rad.x
# Power transformer dimensions between ribs
flat.x <- 350
element.no <- 13

generate_all_rib_coords <- function(element.no, rib.tot.x, flat.x) {
  coords <- c()
  for (i in 1:(element.no%/%2 + 1)) {
    print(i)
    coords <- c(coords, ((i-1)*(rib.tot.x+flat.x)):(i*rib.tot.x + (i-1)*flat.x))
    print(coords)
  }
  coords
}

all_rib_coords <- generate_all_rib_coords(element.no, rib.tot.x, flat.x)

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
# pts.rel <- lhs.design(
#   nruns = 26, nfactors = 2, type = 'optimum', seed = DoE.seed,
#   factor.names = list(
#     loc.x = c(0, 1),
#     loc.y = c(0, 1)
#     )
#   )

# Absolute coordinates
# pts.abs <- pts.rel %>%
#   mutate(loc.y = round(loc.y * height.y, digits = 1),
#          loc.x = sapply(loc.x, map_to_plate.x))

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

################################################################################
# L-CVT DoE
pts.lcvt.abs <- read.csv2('110MVA/DoE/Oplata_i_prigusenje/LCVT_oplata_80.csv',
                          header = T) %>%
  select(-1) %>%
  rename(loc.x=1, loc.y=2) %>%
  mutate(ID = row_number()) %>% select(ID, loc.x, loc.y)

# Determine if point od rib or not
pts.lcvt.abs <- pts.lcvt.abs %>% mutate(
  rib=case_when(
    loc.x %in% all_rib_coords ~ T,
    TRUE ~ F
  )
)

# 10 points for impact test
pts.lcvt.abs <- pts.lcvt.abs %>% mutate(
  impact = case_when(
    ID==1 | ID==5 | ID==40 | ID==78 | ID==80 ~ 'rib',
    ID==9 | ID==10 | ID==36 | ID==68 | ID==73 ~ 'plate',
    TRUE ~ 'NA'
  )
)

# Save clean data with rib info
write_excel_csv2(
  pts.lcvt.abs,
  path='110MVA/DoE/Oplata_i_prigusenje/l_cvt_oplata_prigusenje.csv',
  )

# Plot and save L-CVT hull DoE as bitmap image
png(
  filename = '110MVA/DoE/Oplata_i_prigusenje/l_cvt.png',
  units = 'in',
  width = 12,
  height = 10,
  pointsize = 12,
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
         main = list(label = 'L-CVT oplata kotla', cex = 1.6),
         xlab = list(label = expression("Širina " * italic("x") * " [mm]"), cex = 1.6),
         ylab = list(label = expression('Visina ' * italic('y') * ' [mm]'), cex = 1.6),
         # xlab = '',
         # ylab = '',
         aspect ='iso',
         par.settings = custom.colors.settings,
         xlim = c(-100, 3700),
         ylim = c(-100, 2900),
         scales = list(
           cex = 1.1,
           x = list(at = x.at, rot = 45), # list(at = c(0, 1200, 2400, 3600)),
           y = list(at = y.at) #list(at = c(0, 1250, 2500))
         ),
         panel = function(loc.x, loc.y, ...) {
           panel.abline(v = x.at, h = y.at, col.line = 'lightgrey', ...)
           panel.abline(v = rib.boundaries, ...)
           panel.xyplot(fill = rgb(1, 1, 1, 1), col = "red2", cex=4.0, ...)
           panel.text(..., label = row.names(.), cex=1.3, col = "purple4")
         })
dev.off()

# Plot and save L-CVT hull DoE as vector image
pdf(
  file = '110MVA/DoE/Oplata_i_prigusenje/l_cvt.pdf',
  onefile = T,
  width = 15,
  height = 10,
  pointsize = 12 * 300 / 72,
  bg = 'transparent'
)
pts.lcvt.abs %>%
  xyplot(loc.y ~ loc.x, data = .,
         # main = 'DoE L-CVT ',
         # xlab = list(label = 'Width [mm]', cex = 1.1),
         # ylab = list(label = 'Height [mm]', cex = 1.1),
         xlab = '',
         ylab = '',
         aspect ='iso',
         par.settings = custom.colors.settings,
         xlim = c(-100, 3700),
         ylim = c(-100, 2900),
         scales = list(
           cex = 1.1,
           x = list(at = x.at, rot = 45), # list(at = c(0, 1200, 2400, 3600)),
           y = list(at = y.at) #list(at = c(0, 1250, 2500))
         ),
         panel = function(loc.x, loc.y, ...) {
           panel.abline(v = x.at, h = y.at, col.line = 'lightgrey', ...)
           panel.abline(v = rib.boundaries, ...)
           panel.xyplot(fill = rgb(1, 1, 1, 1), col = "red2", cex=4.0, ...)
           panel.text(..., label = row.names(.), cex=1.3, col = "purple4")
         })
dev.off()

# Plot and save impact test DoE as bitmap image
png(
  filename = '110MVA/DoE/Oplata_i_prigusenje/impact_doe.png',
  units = 'in',
  width = 13,
  height = 10,
  pointsize = 12,
  res = 320,
  bg = 'white'
)
trellis.par.set(
  superpose.symbol = list(
    pch= rep(c(22, 23), length.out=7),
    fill = rep(c('beige', 'darkolivegreen2'), length.out=7),
    col = rep(c('red2'), length.out=7)
  ),
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
pts.lcvt.abs %>% filter(impact != 'NA') %>%
  xyplot(loc.y ~ loc.x, data = .,
         groups = as.factor(impact),
         par.settings = list(
           superpose.symbol = list(
             pch= rep(c(22, 23), length.out=7),
             fill = rep(c('beige', 'darkolivegreen2'), length.out=7),
             col = rep(c('red2'), length.out=7),
             cex= rep(2, length.out=7)
           )
         ),
         auto.key = list(
           text=c('NE', 'DA'),
           space = 'right',
           # x = -0.15,
           # y = 0.4,
           # corner = c(1,1),
           cex = 1.4,
           title = 'Ukruta'
         ),
         # key = list(
         #   points = list(c(22, 23),
         #   text = c('rebro', 'oplata'),
         #   title = 'Legenda'
         # )),
         main = list(label = 'Udarni test - Oplata kotla', cex = 1.6),
         xlab = list(label = expression("Širina " * italic("x") * " [mm]"), cex = 1.6),
         ylab = list(label = expression('Visina ' * italic('y') * ' [mm]'), cex = 1.6),
         # xlab = '',
         # ylab = '',
         aspect ='iso',
         xlim = c(-100, 3700),
         ylim = c(-100, 2900),
         scales = list(
           cex = 1.1,
           x = list(at = x.at, rot = 45), # list(at = c(0, 1200, 2400, 3600)),
           y = list(at = y.at) #list(at = c(0, 1250, 2500))
         ),
         panel = function(loc.x, loc.y, groups, ...) {
           panel.abline(v = x.at, h = y.at, col.line = 'lightgrey', ...)
           panel.abline(v = rib.boundaries, ...)
           panel.xyplot(groups = groups, pch = c(22, 23),
                        fill = c('beige', 'darkolivegreen2'),
                        col = 'red2', cex=4.0, ...)
           panel.text(..., label = .$ID, cex=1.3, col = "purple4")
         })
dev.off()
