# visualizations
rm(list = ls())
graphics.off()

# load libraries
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

# functions:
subsetByTime <- function(df,start_date,end_date){df[df$datetime >= start_date & df$datetime <= end_date,]}

# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(pl, ncol = length(pl), nrow = 1, position = c("bottom", "right")) {
  
  plots <- pl
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none") + theme(legend.title=element_blank()))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

# grid_arrange_shared_legend(pl = plot_list, ncol = 2, nrow = 2, position = "bottom") 







# load data
# load('plotdata/aireasdata.RData')
load('data/rotterdam/input/finput.Rdata')
feat_inp <-  feat_inp[,c(1,2,3,5,4,6,7,8,9,10,11,12)]
names(feat_inp) <- c("datetime",
                     "id",
                     "urban temperature",
                     "rural temperature",                     
                     "temperature difference",
                     "wind speed",
                     "wind direction",
                     "pressure",
                     "sky view factor",
                     "fraction impervious",
                     "solar heading",
                     "solar elevation")
ids <- unique(feat_inp$id)

# variables:                      IDs:
# 1. "datetime",                  1. "896336001"
# 2. "id",                        2. "914096001"
# 3. "urban temperature",         3. "1000000606"
# 4. "temperature difference",    4. "1000001176"    
# 5. "rural temperature",         5. "944506001"
# 6. "wind speed",                6. "43077310"
# 7. "wind direction",            7. "1000000888" 
# 8. "pressure",                  8. "Bolnes" 
# 9. "sky view factor",           9. "SpaansePolder"
# 10. "fraction impervious",      10. "Capelle" 
# 11. "solar heading",            11. "Oost"
# 12. "solar elevation"           12. "Ridderkerk" 
#                                 13. "Ommoord" 
#                                 14. "Rijnhaven" 
#                                 15. "Delfshaven"

# select variables
temperature <- c(1,3,4,5)
# select IDs:
# id = 8
plot_list <- list()
j <- 1
for (i in c(1,7,12,14)){
  id = i
  print(i)
# select date
start_date <- as.POSIXct("2017-05-27 00:00:00")
end_date <- as.POSIXct("2017-05-30 00:00:00")

data_plot <- feat_inp

# subset by id
data_plot <- subset(data_plot, id == ids[i])
# subset by time
data_plot <- subsetByTime(data_plot, start_date, end_date)
# subset by variables
data_plot <- data_plot[,temperature]

data_plot$datetime <- as.POSIXct(data_plot$datetime)

#### making plots
theme_update(plot.title = element_text(hjust = 0.5))

temp_obs <- ggplot(melt(data_plot, id = "datetime")) + 
  geom_step(aes(x=datetime, y = value, colour=variable), na.rm = T) +
  scale_colour_manual(values=c("orange","blue","green")) +
  labs(title = paste0("location id: ",ids[id]),
       x = "Time",
       y = "Temperature (Celsius)") +
  ylim(-5,34) +
  theme(legend.title=element_blank())#+
  # theme(axis.text=element_text(size=12),
  #       axis.title=element_text(size=14,face="bold"))
  
plot_list[[j]] <- temp_obs
j <- j+1
ggsave(paste0("data/rotterdam/data_plots/",id,"_temp_obs.png"),width = 4,height = 6)
}



#### (fancy) histograms 
























# all_temp_obs <- grid_arrange_shared_legend(pl = plot_list,ncol = 2, nrow = 2) + 
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=9,face="bold"))
# 
# ggsave(paste0("data/rotterdam/data_plots/all_temp_obs.png"),width = 12,height = 30)

all_temp_obs
## WINDS SPEED ##




grid.arrange(plot_list)






windsp <- ggplot(data = KNMI_1) + 
  geom_histogram(aes(x = ws),
                 binwidth = 0.5,
                 color = 'white') +
  labs(title = "Wind Speed Rotterdam 2016",
       x = "Wind Speed (m/s)",
       y = "Count") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
windsp

ggsave("windsp.png")


## wind direction ##
# http://personal.colby.edu/personal/m/mgimond/RIntro/06_Case_study_ocean_data.html
library(dplyr)

brks <- c(0,  45,  90, 135, 180, 225, 270, 315, 360)
lbs  <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

wind <- KNMI_1 %>%
  filter( is.na(wd) == FALSE, is.na(ws) == FALSE) %>%  
  mutate(WDIR2 = (wd + 360/8/2)%%360) %>%
  mutate( dirbin = cut(WDIR2, breaks=brks, labels=lbs) )  %>%
  group_by(dirbin) %>%
  summarise( dircnt =  n(), medspd = median(ws) )

windd <-ggplot(aes(x=dirbin, y=dircnt, fill=medspd), data=wind) + 
  geom_bar(width=1, stat = "identity") + 
  scale_fill_distiller(palette="Reds") + coord_polar(start = -(22.5 * pi/180))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
windd

ggsave("windd.png")


## Solar orientation ##
sunel$datetime <- as.POSIXct(sunel$datetime)

theme_update(plot.title = element_text(hjust = 0.5))

subdate <- function(df,x,y){df[df$datetime >= x & df$datetime <= y,]}

sub_july_sunel <- subdate(sunel, as.POSIXct("2017-05-10 00:00:00"), as.POSIXct("2017-05-12 00:00:00"))

# names(sub_july)
# [1] "datetime"   "s_azimuth"  "s_altitude"  
names(sub_july_sunel) <- c('Date','Heading (degrees)','Elevation (degrees)')

sun_obs <- ggplot(melt(sub_july_sunel, id = "Date")) + 
  geom_point(aes(x=Date, y = value, colour=variable), na.rm = T) +
  scale_colour_manual(values=c("orange","blue","green")) +
  labs(title = "Solar heading and elevation 10 and 11 May 2017") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

sun_obs

ggsave("sun_obs.png")



## Pressure ##
KNMI_1$datetime <- as.POSIXct(KNMI_1$datetime)

sub_july_knmi1 <- subdate(KNMI_1, as.POSIXct("2017-05-10 00:00:00"), as.POSIXct("2017-05-12 00:00:00"))


pres <- ggplot(data = KNMI_1) + 
  geom_histogram(aes(x = pr),
                 binwidth = 2,
                 color = 'white') +
  labs(title = "Pressure distribution Rotterdam spring 2017",
       x = "Pressure (hPa)",
       y = "Count") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
pres

ggsave("pres.png")


