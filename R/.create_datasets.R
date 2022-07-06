##### Chapter 1 ####
# Install required packages
pcr <- array(c(302, 21, 14, 91), dim = c(2, 2),
            dimnames = list('Alternate Test' = c("+", "-"),
                            'Gold Standard Test' = c("+", "-")))
usethis::use_data(pcr, overwrite = T)

#### Chapter 3 ####
canola <- read.csv('raw/canola.csv')
usethis::use_data(canola)

maize <- read.csv('raw/maize.csv')
maize$method <- factor(maize$method)
usethis::use_data(maize)

#### Chapter 4 ####
pea_color <- read.csv('raw/colour.csv')
pea_color <- array(pea_color$n, dim = c(1, 2), dimnames = list('counts', color = pea_color$colour))
usethis::use_data(pea_color, overwrite = T)

transg <- read.csv('raw/transg.csv')
transg <- array(transg$n, dim = c(1, 4), dimnames = list('counts', trait = transg$trait))
usethis::use_data(transg, overwrite = T)

adh <- read.csv('raw/adh.csv')
adh[, 1:2] <- lapply(adh[, 1:2], factor)
usethis::use_data(adh)

pairing <- read.csv('raw/pairing.csv')
pairing <- pairing[order(pairing$pairing, pairing$geno), ]
pairing <- array(pairing$number, dim = c(4, 2), dimnames = list(geno = unique(pairing$geno), pairing = unique(pairing$pairing)))
usethis::use_data(pairing, overwrite = T)

cotton <- read.csv('raw/cotton.csv')
cotton$study <- factor(cotton$study)
usethis::use_data(cotton)

#### Chapter 5 ####
ols_example = data.frame(Y = c(60, 90, 10, 20, 50, 80),
                         Treatment = rep(c('1', '2', '3'), 2),
                         Block = rep(c('1', '2'), each = 3))
ols_example[, 2:3] = lapply(ols_example[, 2:3], factor)
usethis::use_data(ols_example)

peas <- read.csv('raw/peas.csv')
peas$block <- factor(peas$block)
peas$trt <- factor(peas$trt, levels = c('Control', '2%fru', '2%glu', '1%g+f', '2%suc'))
usethis::use_data(peas, overwrite = T)

#### Chapter 6 ####
xy_pairs <- read.csv('raw/xy_pairs.csv')
usethis::use_data(xy_pairs)

stalks <- read.csv('raw/stalks.csv')
usethis::use_data(stalks)

standard <- read.csv('raw/standard.csv')
standard$block <- factor(standard$block)
usethis::use_data(standard)

clethodim <- read.csv('raw/clethodim.csv')
usethis::use_data(clethodim)

leafarea <- read.csv('raw/leafarea.csv')
usethis::use_data(leafarea)

carbon_dioxide <- read.csv('raw/co2.csv')
carbon_dioxide$plant <- factor(carbon_dioxide$plant)
usethis::use_data(carbon_dioxide)

pigweed <- read.csv('raw/pigweed.csv')
pigweed[, c(1, 3)] <- lapply(pigweed[, c(1, 3)], factor)
pigweed$pct <- as.numeric(pigweed$pct)
usethis::use_data(pigweed)

lily5c <- read.csv('raw/lily5c.csv')
usethis::use_data(lily5c)

apple <- read.csv('raw/apple.csv')
apple$sample <- factor(apple$sample)
apple$bacteria <- factor(apple$bacteria, labels = c('absent', 'present'))
usethis::use_data(apple, overwrite = T)

#### Chapter 7 ####
forage1 <- read.csv('raw/forage1.csv')
forage1$block <- factor(forage1$block)
usethis::use_data(forage1, overwrite = T)

forage2 <- read.csv('raw/forage2.csv')
forage2[, c(1:2)] <- lapply(forage2[, c(1:2)], factor)
colnames(forage2)[1] <- 'cultivar'
usethis::use_data(forage2, overwrite = T)

#### Chapter 8 ####

# uses peas data from chapter 5

#### Chapter 9 ####
aba <- read.csv('raw/aba.csv')
aba$block <- factor(aba$block)
aba$aba <- factor(aba$aba)
usethis::use_data(aba, overwrite = T)

ryegrass <- read.csv('raw/ryegrass.csv')
ryegrass$entry <- factor(ryegrass$entry)
ryegrass$block <- factor(ryegrass$block)
colnames(ryegrass)[1] <- 'cultivar'
usethis::use_data(ryegrass, overwrite = T)

bap <- read.csv('raw/bap.csv')
bap[, c(1:3)] <- lapply(bap[, c(1:3)], factor)
colnames(bap)[1] <- 'cultivar'
usethis::use_data(bap, overwrite = T)

asi <- read.csv('raw/asi.csv')
asi[, 1:2] <- lapply(asi[, 1:2], factor)
usethis::use_data(asi)

wilt <- read.csv('raw/wilt.csv')
wilt[, 1:2] <- lapply(wilt[, 1:2], factor)
wilt$category <- ordered(wilt$category)
usethis::use_data(wilt, overwrite = T)

flowers <- read.csv('raw/flowers.csv')
flowers$colour <- factor(flowers$colour, levels = unique(flowers$colour))
flowers[, 2:3] <- lapply(flowers[, 2:3], factor)
colnames(flowers)[1] <- 'color'
usethis::use_data(flowers, overwrite = T)

#### Chapter 10 ####
manure <- read.csv('raw/manure.csv')
manure[, 1:2] <- lapply(manure[, 1:2], factor)
usethis::use_data(manure)

forage3 <- read.csv('raw/forage3.csv')
forage3[, 1:5] <- lapply(forage3[, 1:5], factor)
colnames(forage3)[5] <- 'cultivar'
usethis::use_data(forage3, overwrite = T)

soybean25 <- read.csv('raw/soybean25.csv')
soybean25[, 1:7] <- lapply(soybean25[, 1:7], factor)
colnames(soybean25)[7] <- "cultivar"
usethis::use_data(soybean25, overwrite = T)

soy2000 <- read.csv('raw/soy2000.csv')
soy2000[, 1:3] <- lapply(soy2000[, 1:3], factor)
colnames(soy2000)[1] <- 'cultivar'
usethis::use_data(soy2000, overwrite = T)

#### Chapter 11 ####
trefoil <- read.csv('raw/trefoil.csv')
trefoil[, 1:3] <- lapply(trefoil[, 1:3], factor)
usethis::use_data(trefoil)

density <- read.csv('raw/density.csv')
density[, 1:2] <- lapply(density[, 1:2], factor)
colnames(density)[1] <- 'hybrid'
usethis::use_data(density, overwrite = T)

orchard <- read.csv('raw/orchardWide.csv')
orchard[, 1:3] <- lapply(orchard[, 1:3], factor)
colnames(orchard)[2] <- 'cultivar'
usethis::use_data(orchard, overwrite = T)

clover <- read.csv('raw/cloverWide.csv')
clover[, 1:3] <- lapply(clover[, 1:3], factor)
colnames(clover)[3] <- 'cultivar'
usethis::use_data(clover, overwrite = T)

cold <- read.csv('raw/coldWide.csv')
cold[, 1:3] <- lapply(cold[, 1:3], factor)
colnames(cold)[3] <- 'cultivar'
usethis::use_data(cold, overwrite = T)

tbay <- read.csv('raw/tbay.csv')
tbay[, 1:3] <- lapply(tbay[, 1:3], factor)
colnames(tbay)[1] <- 'cultivar'
usethis::use_data(tbay, overwrite = T)

elora <- read.csv('raw/elora.csv')
elora[, 1:3] <- lapply(elora[, 1:3], factor)
colnames(elora)[1] <- 'cultivar'
usethis::use_data(elora, overwrite = T)

ndf <- read.csv('raw/ndf.csv', stringsAsFactors = T)
ndf$block <- factor(ndf$block)
colnames(ndf)[2] <- 'cultivar'
usethis::use_data(ndf, overwrite = T)

#### Chapter 12 ####
callus <- read.csv('raw/callus.csv')
usethis::use_data(callus)

dicbap <- read.csv('raw/dicbap.csv')
dicbap$callus_pct <- as.numeric(dicbap$callus_pct)
dicbap$block <- factor(dicbap$block)
usethis::use_data(dicbap)

#### Chapter 13 ####
nickel <- read.csv('raw/nickel.csv')
nickel$field <- factor(nickel$field)
usethis::use_data(nickel)

collinear <- read.csv('raw/collinear.csv')
usethis::use_data(collinear)

protrain <- read.csv('raw/protrain.csv')
protrain[, 1:2] <- lapply(protrain[, 1:2], factor)
usethis::use_data(protrain)

provalid <- read.csv('raw/provalid.csv')
provalid[, 1:2] <- lapply(provalid[, 1:2], factor)
usethis::use_data(provalid)

#### Chapter 14 ####
soywide <- read.csv('raw/soywide.csv')
soywide[, 1:3] <- lapply(soywide[, 1:3], factor)
colnames(soywide)[2] <- 'cultivar'
usethis::use_data(soywide, overwrite = T)

grape_cor <- c(0.7232, -0.6802, -0.6625, -0.5436, -0.5463)
names(grape_cor) <- c('Starch', 'Fructose', 'Glucose', 'Sucrose', 'Raffinose')
usethis::use_data(grape_cor, overwrite = T)

grape_among <- matrix(c( 1.0000, -0.6984, -0.6831, -0.5008,	-0.3421,
                      -0.6984,  1.0000,  0.9710,	0.3745,  0.6171,
                      -0.6831,  0.9710,	 1.0000,	0.2886,	 0.6645,
                      -0.5008,  0.3745,	 0.2886,	1.0000,	 0.3748,
                      -0.3421,  0.6171,	 0.6644,	0.3748,	 1.0000),
                    byrow = T, nrow = 5, dimnames = list(names(grape_cor), names(grape_cor)))

usethis::use_data(grape_among, overwrite = T)

grin <- read.csv('raw/grin.csv')
grin[, 1:2] <- lapply(grin[, 1:2], factor)
usethis::use_data(grin, overwrite = T)

#### Chapter 15 #####
silage <- read.csv('raw/silage.csv')
silage[, 1:3] <- lapply(silage[, 1:3], factor)
usethis::use_data(silage, overwrite = T)

#### Other ####
fitness <- read.csv('raw/fitness.csv')
usethis::use_data(fitness, overwrite = T)
