
library(readxl)
library(ggplot2)
library(ggpubr)

# Automatically define working directory based on where the file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


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


sarid.dmi$aez <- 'Semi-arid'
thsh.dmi$aez <- 'THSH'
sh.dmi$aez <-  'SH'
thh.dmi$aez <-  'THH'

sarid.lf$aez <- 'Semi-arid'
thsh.lf$aez <- 'THSH'
sh.lf$aez <-  'SH'
thh.lf$aez <-  'THH'


dmi.d <- rbind(sarid.dmi , thsh.dmi , sh.dmi , thh.dmi)
lf.d <- rbind(sarid.lf , thsh.lf , sh.lf , thh.lf)
  
# Rename columns of dataframes
names(dmi.d)[1] <- 'feed'
names(dmi.d)[2] <- 'dmi.pct'

names(lf.d)[1] <- 'land'
names(lf.d)[2] <- 'lf.pct'
names(lf.d)[3] <- 'lf.act'

lf.d$lf.pct <- as.numeric(lf.d$lf.pct)
lf.d$lf.act <- as.numeric(lf.d$lf.act)


# Add additional rows for scenario parameters here


feed.levels <- c(
  "Crop residue"
  , "Concentrate/purchased"
  ,"Cultivated fodder" 
  ,"Grazing"
  ,  "Collected fodder" 
)

land.levels <- c(
         'Maize as residue'
         ,'Maize as concentrate'
         ,'Improved forage'
         ,'Native grass'
  )

dmi.d$feed <- factor (dmi.d$feed  , levels = feed.levels)
  lf.d$land <- factor (lf.d$land  , levels = land.levels)

color.scale <-  c(
  'black'
  , 'blue'
  , 'red'
  , 'green'
  ,'pink'
  )


dmi.p.0 <- ggplot( dmi.d  , aes(x = aez , y = dmi.pct , fill = feed) ) +
  geom_bar(position="stack", stat="identity") +
  ylab('Dry matter intake (%)') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale) +
  theme(
   , axis.title.x = element_blank()
    , axis.title.y = element_text(size = 9.5 ) 
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    ,  legend.position="none" )

lf.pct.p.0 <- ggplot( lf.d  , aes(x = aez , y = lf.pct , fill = land) ) +
  geom_bar(position="fill" , stat = "identity") +
  ylab('Land footprint (%)') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale) +
  theme(
   , axis.title.x = element_blank()
    , axis.title.y = element_text(size = 9.5 ) 
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    ,  legend.position="none" )



lf.act.0 <- ggplot( lf.d  , aes( fill = land , x = aez ,  y = lf.act ) ) +
  geom_bar( stat = 'identity') +
  ylab('Land footprint (ha/TLU') +
  xlab('Agro-ecolocical zone') +
  scale_fill_manual(values = color.scale) +
  theme(
    axis.text.x = element_text(angle = 90, face = 'italic' , vjust = 0.5, hjust=1, size = 9.5 )
    , axis.title.x = element_text(size = 9.5 ) 
    , axis.title.y = element_text(size = 9.5 ) 
    , axis.ticks.x = element_blank()
    # Panel
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,panel.border = element_rect(colour = "black",
                                 fill=NA, size=1)
    ,  legend.position="bottom" )


label.fs <- 12


dmi.p  <<- annotate_figure( dmi.p.0 ,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs)
lf.pct.p  <<- annotate_figure(lf.pct.p.0 ,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = label.fs)
lf.act  <<- annotate_figure( lf.act.0 ,   fig.lab = "c", fig.lab.pos ="top.left", fig.lab.size = label.fs)


fig.vop   <<- plot_grid(   dmi.p , 
                           lf.pct.p  , 
                           lf.act , 
                           align = "h", 
                           nrow = 3, 
                           ncol = 1 , 
                           rel_heights = c(25/100, 25/100 , 50/100))

fig.vop 
