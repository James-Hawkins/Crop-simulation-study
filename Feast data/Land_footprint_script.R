
library(readxl)
library(ggplot2)
library(ggpubr)
library(cowplot)

# Automatically define working directory based on where the file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

save.image('All.data.RData')


run.all <- function(){
  
sarid <- read_excel("Land_use_parameters.xlsx" , sheet = 'Sarid')
thsh <- read_excel("Land_use_parameters.xlsx" , sheet = 'THSH')
sh <- read_excel("Land_use_parameters.xlsx" , sheet = 'SH')
thh <- read_excel("Land_use_parameters.xlsx" , sheet = 'THH')

sarid <- as.data.frame(sarid)
thsh <- as.data.frame(thsh)
sh <- as.data.frame(sh)
thh <- as.data.frame(thh)

sarid.dmi <- sarid[1:5,c(1,2)]
thsh.dmi <- thsh[1:5,c(1,2)]
sh.dmi <- sh[1:5,c(1,2)]
thh.dmi <- thh[1:5,c(1,2)]

sarid.lf <- sarid[12:15,c(5 , 10 , 11)]
thsh.lf <- thsh[12:15,c(5 , 10 , 11)]
sh.lf <- sh[12:15,c(5 , 10 , 11)]
thh.lf <- thh[12:15,c(5 , 10 , 11)]


sarid.dmi$aez <- 'SA'
thsh.dmi$aez <- 'THSH'
sh.dmi$aez <-  'SH'
thh.dmi$aez <-  'THH'

sarid.lf$aez <- 'SA'
thsh.lf$aez <- 'THSH'
sh.lf$aez <-  'SH'
thh.lf$aez <-  'THH'


dmi.d.0 <- rbind(sarid.dmi , thsh.dmi , sh.dmi , thh.dmi)
lf.d.0 <- rbind(sarid.lf , thsh.lf , sh.lf , thh.lf)
#View(dmi.d.0)

names(dmi.d.0)[1] <- 'feed'
names(dmi.d.0)[2] <- 'dmi'

# Replace grazing + collected fodder with 'native grass'

dmi.d.0$dmi.n <- NA

for (aez in unique(dmi.d.0$aez)) {
 dmi.d.0[dmi.d.0$aez == aez & dmi.d.0$feed == 'Grazing', 'dmi.n'] <- (
   dmi.d.0[dmi.d.0$aez == aez & dmi.d.0$feed == 'Grazing', 'dmi'] +
     dmi.d.0[dmi.d.0$aez == aez & dmi.d.0$feed == "Collected fodder", 'dmi' ]
 )
  
}

dmi.d.0[dmi.d.0$feed != 'Grazing' , 'dmi.n'] <-  dmi.d.0[dmi.d.0$feed != 'Grazing' , 'dmi']
dmi.d.0 <- dmi.d.0[ !(dmi.d.0$feed == 'Collected fodder') , ] 
dmi.d.0$dmi <- dmi.d.0$dmi.n
dmi.d.0 <- dmi.d.0[ , -4]
#View(dmi.d.0)

# Rename columns of dataframes
names(dmi.d.0)[1] <- 'feed'
names(dmi.d.0)[2] <- 'dmi.pct'

names(lf.d.0)[1] <- 'land'
names(lf.d.0)[2] <- 'lf.pct'
names(lf.d.0)[3] <- 'lf.act'

lf.d.0$lf.pct <- as.numeric(lf.d.0$lf.pct)
lf.d.0$lf.act <- as.numeric(lf.d.0$lf.act)

# Rename names for feed types to correspond with land types
uniq.fds <- unique(dmi.d.0$feed)

new.feed.nms <- c("Native grassland", "Rhodes grass" , "Maize residue" , "Maize concentrate")

dmi.d.0[dmi.d.0$feed == uniq.fds[1] , 'feed'] <- new.feed.nms[1]
dmi.d.0[dmi.d.0$feed == uniq.fds[2] , 'feed'] <- new.feed.nms[2]
dmi.d.0[dmi.d.0$feed == uniq.fds[3] , 'feed'] <- new.feed.nms[3]
dmi.d.0[dmi.d.0$feed == uniq.fds[4] , 'feed'] <- new.feed.nms[4]

# Add additional columns for strings of aez names
unique.aez <- unique(lf.d.0$aez)

lf.d.0$aez.long <- NA
dmi.d.0$aez.long <- NA

long.aez.strings <- c("Semi-arid" , "Trop. High. Sub-humid" , "Sub-humid" , "Trop. High. Humid")

lf.d.0[lf.d.0$aez == unique.aez[1] , 'aez.long'] <- long.aez.strings[1]
lf.d.0[lf.d.0$aez == unique.aez[2] , 'aez.long'] <- long.aez.strings[2]
lf.d.0[lf.d.0$aez == unique.aez[3] , 'aez.long'] <- long.aez.strings[3]
lf.d.0[lf.d.0$aez == unique.aez[4] , 'aez.long'] <- long.aez.strings[4]

dmi.d.0[dmi.d.0$aez == unique.aez[1] , 'aez.long'] <- long.aez.strings[1]
dmi.d.0[dmi.d.0$aez == unique.aez[2] , 'aez.long'] <- long.aez.strings[2]
dmi.d.0[dmi.d.0$aez == unique.aez[3] , 'aez.long'] <- long.aez.strings[3]
dmi.d.0[dmi.d.0$aez == unique.aez[4] , 'aez.long'] <- long.aez.strings[4]

# Add additional rows for scenario parameters here
lf.d.0$scen <- 'Baseline'
dmi.d.0$scen <- 'Baseline'

lf.d.0$scen.lab <- 'Baseline'
dmi.d.0$scen.lab <- 'Baseline'


lf.d.s1 <- lf.d.0
lf.d.s2 <- lf.d.0
lf.d.s3 <- lf.d.0
lf.d.s4 <- lf.d.0
lf.d.s5 <- lf.d.0

dmi.d.s1 <- dmi.d.0
dmi.d.s2 <- dmi.d.0
dmi.d.s3 <- dmi.d.0
dmi.d.s4 <- dmi.d.0
dmi.d.s5 <- dmi.d.0

scen.names <<- c(
  "Maize expansion"
  , "Maize intensification"
  , "Rhodes expansion"
  , "Rhodes intensification"
  , "Maize intensification + Rhodes expansion-intensification"
)

scen.names.lab <<- c(
  #"Baseline"
   "S1: Maize\nexpansion"
  , "S2: Maize\nintensification"
  , "S3: Rhodes\nexpansion"
  , "S4: Rhodes\nintensification"
  , "S5: S2 + S3\n+ S4"
)

scen.names.plus.bl.lab <<- c(
  "Baseline"
  , "S1: Maize\nexpansion"
  , "S2: Maize\nintensification"
  , "S3: Rhodes\nexpansion"
  , "S4: Rhodes\nintensification"
  , "S5: S2 + S3\n+ S4"
)

lf.d.s1$scen.lab <- scen.names.lab [1]
lf.d.s2$scen.lab <- scen.names.lab [2]
lf.d.s3$scen.lab <- scen.names.lab [3]
lf.d.s4$scen.lab <- scen.names.lab [4]
lf.d.s5$scen.lab <- scen.names.lab [5]

dmi.d.s1$scen.lab <- scen.names.lab [1]
dmi.d.s2$scen.lab <- scen.names.lab [2]
dmi.d.s3$scen.lab <- scen.names.lab [3]
dmi.d.s4$scen.lab <- scen.names.lab [4]
dmi.d.s5$scen.lab <- scen.names.lab [5]

lf.d.s1$scen <- scen.names[1]
lf.d.s2$scen <- scen.names[2]
lf.d.s3$scen <- scen.names[3]
lf.d.s4$scen <- scen.names[4]
lf.d.s5$scen <- scen.names[5]

dmi.d.s1$scen <- scen.names[1]
dmi.d.s2$scen <- scen.names[2]
dmi.d.s3$scen <- scen.names[3]
dmi.d.s4$scen <- scen.names[4]
dmi.d.s5$scen <- scen.names[5]



lf.d <- rbind( lf.d.0 , lf.d.s1 , lf.d.s2 , lf.d.s3 , lf.d.s4 , lf.d.s5 )
dmi.d <- rbind( dmi.d.0 , dmi.d.s1 , dmi.d.s2 , dmi.d.s3 , dmi.d.s4 , dmi.d.s5 )

#View(lf.d)
#View(dmi.d)

colnames(dmi.d)

# Define scenarios as % change from baseline parameters (feed, land footprint)
scen.prms.dm <- read_excel("Scenario_params.xlsx" , sheet = 'Feeding')
scen.prms.lf <- read_excel("Scenario_params.xlsx" , sheet = 'Land.footprint')

scen.prms.dm <- as.data.frame(scen.prms.dm)
scen.prms.lf <- as.data.frame(scen.prms.lf)

unique.scens <- unique(scen.prms.dm$scen)
unique.feeds <- unique(scen.prms.dm$feed)
unique.lands <- unique(scen.prms.lf$land)
unique.aezs <- unique(scen.prms.lf$aez)

for (s in unique.scens) {
for (f in unique.feeds) {
for (aez in unique.aezs) {
  
dmi.d[dmi.d$feed == f & dmi.d$scen == s & dmi.d$aez == aez, 'dmi.pct'] <- (
  dmi.d[dmi.d$feed == f & dmi.d$scen == 'Baseline' & dmi.d$aez == aez, 'dmi.pct'] 
     * scen.prms.dm[scen.prms.dm$feed == f & scen.prms.dm$scen == s & scen.prms.dm$aez == aez , 'delta.feed.dmi']
    )

}
}
}

for (s in unique(scen.prms.dm$scen)) {
  for (f in unique.lands) {
    for (aez in unique.aezs) {
      
    #  lf.d[lf.d$land == f & lf.d$scen == s & lf.d$aez == aez, 'lf.pct'] <- (
      # lf.d[lf.d$land == f & lf.d$scen == 'Baseline' & lf.d$aez == aez, 'lf.pct'] 
      # * scen.prms.lf[scen.prms.lf$land == f & scen.prms.lf$scen == s & scen.prms.lf$aez == aez , 'delta.lf.pct']
      # )
      
      lf.d[lf.d$land == f & lf.d$scen == s & lf.d$aez == aez, 'lf.act'] <- (
        lf.d[lf.d$land == f & lf.d$scen == 'Baseline' & lf.d$aez == aez, 'lf.act'] 
        * scen.prms.lf[scen.prms.lf$land == f & scen.prms.lf$scen == s & scen.prms.lf$aez == aez , 'delta.lf.act']
      )
      
    }
  }
}

# View(dmi.d)
# View(lf.d)
dmi.d <<- dmi.d
lf.d <<-  lf.d


plots <- function(){
  
feed.levels <- c(
  "Maize residue"
  , "Maize concentrate"
  ,"Rhodes grass" 
  ,"Native grassland"
 # ,  "Collected fodder" 
)


color.scale.feed <-  c(
  '#e3a86e'
  , '#F2800E'
  , '#07B33B'
  , '#CDF09E'
)

land.levels <- c(
         'Maize as residue'
         ,'Maize as concentrate'
         ,'Improved forage'
         ,'Native grass'
  )

color.scale.land <-  c(
  '#e3a86e'
  , '#F2800E'
  , '#07B33B'
  , '#CDF09E'
  #,'pink'
)

aez.long.levels <- c('Trop. High. Sub-humid'
                     ,'Trop. High. Humid'
                     ,'Sub-humid'
                     ,'Semi-arid'
                     )

dmi.d$feed <- factor (dmi.d$feed  , levels = feed.levels)
lf.d$land <- factor (lf.d$land  , levels = land.levels)

dmi.d$aez.long <- factor (dmi.d$aez.long , levels = aez.long.levels)
lf.d$aez.long <- factor (lf.d$aez.long  , levels = aez.long.levels)

dmi.d$scen.lab <- factor (dmi.d$scen.lab , levels = scen.names.plus.bl )
lf.d$scen.lab  <- factor (lf.d$scen.lab  , levels = scen.names.plus.bl )


# Plot theme parameters
bar.width <- 0.7
y.tit.fs <- 6.25
y.tick.fs <- 4.5
x.ax.text.fs <- 6.5

# figure margins
p1.mg.top <- 0.15
p1.mg.right <- 0.15
p1.mg.bottom <- 0.05
p1.mg.left <- 0.15

p2.mg.top <- 0.05
p2.mg.right <- p1.mg.right
p2.mg.bottom <- 0.05
p2.mg.left <- 0.15

p3.mg.top <- 0.05
p3.mg.right <- p1.mg.right
p3.mg.bottom <- 0.8
p3.mg.left <- 0.15

# Facets
strip.fs <- 7

# Legend
n.cols <- 4
leg.x.crd <- 0.5
leg.y.crd <- -1.18
leg.key.h <- 0.35
leg.key.w <- 0.35
leg.txt.fs <- 6.8


lf.act.0 <<- ggplot( lf.d  , aes( fill = land , x = aez.long,  y = lf.act ) ) +
  geom_bar( stat = 'identity' , width = bar.width ) +
  ylab('  Land footprint (ha/TLU)') +
  scale_fill_manual(values = color.scale.land) +
  facet_grid( ~ scen.lab) +
  theme(
    plot.margin = unit(c( p1.mg.top ,  p1.mg.right,  p1.mg.bottom, p1.mg.left), "cm"),
    axis.text.x = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_text(size = y.tit.fs ) 
    , axis.text.y = element_text(size = y.tick.fs)
    , axis.ticks.x = element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    # Strip
    ,strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    , strip.text.x =   element_text( size = strip.fs )
    # Legend
    ,  legend.position="none"
 
    ) 


lf.pct.p.0 <<- ggplot( lf.d  , aes(x = aez.long , y = lf.act, fill = land) ) +
  geom_bar(position="fill" , stat = "identity" , width = bar.width ) +
  ylab('Land footprint (%)  ') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale.land) +
  facet_grid( ~ scen.lab) +
  theme(
    plot.margin = unit(c( p2.mg.top ,  p2.mg.right,  p2.mg.bottom, p2.mg.left), "cm"),
    , axis.title.x = element_blank()
    , axis.title.y = element_text(size = y.tit.fs ) 
    , axis.ticks.x = element_blank()
    , axis.text.y = element_text(size = y.tick.fs)
    , axis.text.x = element_blank()
    # Strip
    ,strip.background.y =  element_blank(),
    ,strip.text.x =   element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    ,  legend.position="none" 
    )
    
dmi.p.0 <<- ggplot( dmi.d  , aes(x = aez.long , y = dmi.pct , fill = feed) ) +
  geom_bar(position="fill", stat="identity"  , width = bar.width) +
  ylab('Dry matter intake (%)     ') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale.feed) +
  facet_grid( ~ scen.lab) +
  guides(fill=guide_legend(ncol=n.cols)) +
  theme(
    plot.margin = unit(c( p3.mg.top ,  p3.mg.right,  p3.mg.bottom, p3.mg.left), "cm"),
    , axis.title.x = element_blank()
    , axis.title.y = element_text(size = y.tit.fs) 
    , axis.ticks.x = element_blank()
    , axis.text.y = element_text(size = y.tick.fs)
    , axis.text.x = element_text(angle = 90, face = 'italic' , vjust = 0.5, hjust=1, size = x.ax.text.fs )
    #Strip
    , strip.text.x =   element_blank()
    , strip.background.y =  element_blank(),
    , strip.text.y =   element_blank()
    # Panel
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , panel.background = element_blank()
    , panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    # Legend
    #,  legend.position="bottom" 
    , legend.position = c(leg.x.crd , leg.y.crd)
    #, legend.position.inside = c(.25,-.5)
    ,  legend.title=element_blank()
    ,  legend.key.height = unit(leg.key.h , 'cm')
    ,  legend.key.width = unit(leg.key.w , 'cm')
    ,  legend.text = element_text(size = leg.txt.fs ) 
    )

label.fs <- 8

lf.act  <<- annotate_figure( lf.act.0 ,   fig.lab = " a  ", fig.lab.pos ="top.left", fig.lab.size = label.fs)
lf.pct.p  <<- annotate_figure(lf.pct.p.0 ,   fig.lab = " b  ", fig.lab.pos ="top.left", fig.lab.size = label.fs)
dmi.p  <<- annotate_figure( dmi.p.0 ,   fig.lab = " c  ", fig.lab.pos ="top.left", fig.lab.size = label.fs)


fig.land.nexus   <<- plot_grid( 
                           lf.act , 
                           lf.pct.p  , 
                           dmi.p , 
                           align = "h", 
                           nrow = 3, 
                           ncol = 1 , 
                           rel_heights = c(24/100, 23/100 , 53/100))

fig.land.nexus


fig.land.nexus.pt.x.dim <- 640
fig.land.nexus.pt.y.dim <- 540
ggsave("fig.land.nexus.jpeg",     fig.land.nexus   ,  width =   fig.land.nexus.pt.x.dim, height =   fig.land.nexus.pt.y.dim , units="px", scale=2.5  )

}
plots()

}
run.all()
