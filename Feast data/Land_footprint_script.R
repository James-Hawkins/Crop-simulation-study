
library(readxl)
library(ggplot2)
library(ggpubr)
library(cowplot)

# Automatically define working directory based on where the file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

run <- function(){
  
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


# Add additional rows for scenario parameters here
lf.d.0$scen <- 'Baseline'
dmi.d.0$scen <- 'Baseline'

lf.d.s1 <- lf.d.0
lf.d.s2 <- lf.d.0
lf.d.s3 <- lf.d.0
lf.d.s4 <- lf.d.0

dmi.d.s1 <- dmi.d.0
dmi.d.s2 <- dmi.d.0
dmi.d.s3 <- dmi.d.0
dmi.d.s4 <- dmi.d.0

lf.d.s1$scen <- 'scen 1'
lf.d.s2$scen <- 'scen 2'
lf.d.s3$scen <- 'scen 3'
lf.d.s4$scen <- 'scen 4'

dmi.d.s1$scen <- 'scen 1'
dmi.d.s2$scen <- 'scen 2'
dmi.d.s3$scen <- 'scen 3'
dmi.d.s4$scen <- 'scen 4'

lf.d <- rbind( lf.d.0 , lf.d.s1 , lf.d.s2 , lf.d.s3 , lf.d.s4 )
dmi.d <- rbind( dmi.d.0 , dmi.d.s1 , dmi.d.s2 , dmi.d.s3 , dmi.d.s4 )

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

for (s in unique.scens) {
  for (f in unique.lands) {
    for (aez in unique.aezs) {
      
      lf.d[lf.d$land == f & lf.d$scen == s & lf.d$aez == aez, 'lf.pct'] <- (
       lf.d[lf.d$land == f & lf.d$scen == 'Baseline' & lf.d$aez == aez, 'lf.pct'] 
        * scen.prms.lf[scen.prms.lf$land == f & scen.prms.lf$scen == s & scen.prms.lf$aez == aez , 'delta.lf.pct']
      )
      
      lf.d[lf.d$land == f & lf.d$scen == s & lf.d$aez == aez, 'lf.act'] <- (
        lf.d[lf.d$land == f & lf.d$scen == 'Baseline' & lf.d$aez == aez, 'lf.act'] 
        * scen.prms.lf[scen.prms.lf$land == f & scen.prms.lf$scen == s & scen.prms.lf$aez == aez , 'delta.lf.act']
      )
      
    }
  }
}

View(dmi.d)
View(lf.d)

feed.levels <- c(
  "Crop residue"
  , "Concentrate/purchased"
  ,"Cultivated fodder" 
  ,"Grazing"
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

dmi.d$feed <- factor (dmi.d$feed  , levels = feed.levels)
lf.d$land <- factor (lf.d$land  , levels = land.levels)

# Plot theme parameters
bar.width <- 0.7
y.tit.fs <- 8.5

p.mg.top <- .15
p.mg.right <- .3
p.mg.bottom <- .15
p.mg.left <- .15

# Legend
leg.key.h <- 0.35
leg.key.w <- 0.35






lf.act.0 <- ggplot( lf.d  , aes( fill = land , x = aez ,  y = lf.act ) ) +
  geom_bar( stat = 'identity' , width = bar.width ) +
  ylab('Land footprint (ha/TLU)') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale.land) +
  facet_grid( ~ scen) +
  theme(
    plot.margin = unit(c( p.mg.top ,  p.mg.right,  p.mg.bottom, p.mg.left), "cm"),
    axis.text.x = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_text(size = y.tit.fs ) 
    , axis.ticks.x = element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    # Strip
    ,strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    # Legend
    ,  legend.position="none"
    ) 


lf.pct.p.0 <- ggplot( lf.d  , aes(x = aez , y = lf.pct , fill = land) ) +
  geom_bar(position="fill" , stat = "identity" , width = bar.width ) +
  ylab('Land footprint (%)') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale.land) +
  facet_grid( ~ scen) +
  theme(
    plot.margin = unit(c( p.mg.top ,  p.mg.right,  p.mg.bottom, p.mg.left), "cm"),
    , axis.title.x = element_blank()
    , axis.title.y = element_text(size = y.tit.fs ) 
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
    # Strip
    ,strip.background.y =  element_blank(),
    ,strip.text.y =   element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    ,  legend.position="none" )
    
dmi.p.0 <- ggplot( dmi.d  , aes(x = aez , y = dmi.pct , fill = feed) ) +
  geom_bar(position="fill", stat="identity"  , width = bar.width) +
  ylab('Dry matter intake (%)') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale.feed) +
  facet_grid( ~ scen) +
  theme(
    plot.margin = unit(c( p.mg.top ,  p.mg.right,  p.mg.bottom, p.mg.left), "cm"),
    , axis.title.x = element_text(size = 9.5 ) 
    , axis.title.y = element_text(size = y.tit.fs) 
    , axis.ticks.x = element_blank()
    , axis.text.x = element_text(angle = 90, face = 'italic' , vjust = 0.5, hjust=1, size = 9.5 )
    #Strip
    ,strip.background.y =  element_blank(),
    ,strip.text.y =   element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    # Legend
    ,  legend.position="bottom" 
    ,  legend.key.height = unit(leg.key.h , 'cm')
    , legend.key.width = unit(leg.key.w , 'cm'))

label.fs <- 10

lf.act  <<- annotate_figure( lf.act.0 ,   fig.lab = "a", fig.lab.pos ="top.right", fig.lab.size = label.fs)

lf.pct.p  <<- annotate_figure(lf.pct.p.0 ,   fig.lab = "b", fig.lab.pos ="top.right", fig.lab.size = label.fs)
dmi.p  <<- annotate_figure( dmi.p.0 ,   fig.lab = "c", fig.lab.pos ="top.right", fig.lab.size = label.fs)


fig.land.nexus   <<- plot_grid( 
                           lf.act , 
                           lf.pct.p  , 
                           dmi.p , 
                           align = "h", 
                           nrow = 3, 
                           ncol = 1 , 
                           rel_heights = c(27.5/100, 27.5/100 , 45/100))

fig.land.nexus


fig.land.nexus.pt.x.dim <- 500
fig.land.nexus.pt.y.dim <- 580
ggsave("fig.land.nexus.jpeg",     fig.land.nexus   ,  width =   fig.land.nexus.pt.x.dim, height =   fig.land.nexus.pt.y.dim , units="px", scale=2.5  )

