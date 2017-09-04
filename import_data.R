#------------------
# August 2017, by Nadir Ahami
# 
# This sript is used to import data downloaded from the following to websites:
# 1. weather.tudelft.nl (tud data)
# 2. wow.knmi.nl (wow and aws data)
# 
#### Set up environment ####
rm(list=ls())

# Set working directory
dir <- "/usr/people/ahami/a_msc_thesis/clean_mt"
setwd(dir)

# packages
library(R.utils)
library(data.table)


#### files to be imported ####

# raw data dir
rw_dataloc <- "/usr/people/ahami/a_msc_thesis/R_dir/data_rdam/raw"

# csv names
doc_names <- paste0(rw_dataloc,"/csv_names.txt")
fl_names <- fread(doc_names, header = FALSE)
fieldnames_tud <- fread(paste0(rw_dataloc,"/tud_fields.txt"))
fieldnames_wow <- c("datetime","temperature")

tud <- fl_names[1:9]
wow <- fl_names[10:16]

#Import TUD data
tud_data_out <- data.frame()
for (e in 1:nrow(tud)){
  dl <- tud[e]
  fn <- paste0(rw_dataloc,"/",dl,".csv")
  lin_aptf <- 230200
  nl <- countLines(fn)
  if (nl <= lin_aptf) {
    l2keep <- nl
  } else {
    l2keep <- lin_aptf
    }

  data <- fread(fn, 
                skip=nl-l2keep,
                stringsAsFactors = FALSE, 
                na.strings = "NaN", 
                blank.lines.skip = TRUE,
                fill = TRUE,
                select = c(1,2,7))
  
  colnames(data) <- c('datetime','id','temp')
  data$datetime <- as.POSIXct(as.character(data$datetime), format = "%Y-%m-%d %H:%M:%S")
  
  data2 <- aggregate(list(temp = data$temp), by=list(datetime=cut(data$datetime,"hour")),mean)
  data2$datetime <- as.POSIXct(data2[,1], format = "%Y-%m-%d %H:%M:%S")
  data2$id <- data$id[1]
  data2 <- data2[,c(1,3,2)]
  tud_data_out <- rbind(tud_data_out,data2)
}
rm(data,data2,l2keep,nl,dl,fn,e,lin_aptf)

#Import WOW data
wow_data_out <- data.frame()
for (e in 1:nrow(wow)) {
  dl <- wow[e]
  fn <- paste0(rw_dataloc,"/",dl,".csv")
  
  data <- as.data.frame(read.csv(fn,sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-"))
  st_id = as.numeric(gsub("\\D", "", colnames(data)[2]))
  data$id <- as.numeric(st_id)
  colnames(data) <- c('datetime','temp','id')
  
  data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
  
  data2 <- aggregate(list(temp = data$temp), by=list(datetime=cut(data$datetime,"hour")),mean)
  data2$datetime <- as.POSIXct(data2$datetime, format = "%Y-%m-%d %H:%M:%S")
  data2$id <- data$id[1]
  data2 <- data2[,c(1,3,2)]
  
  wow_data_out <- rbind(wow_data_out,data2)
}
rm(data,data2,st_id,dl,fn,e)
urb_obs <- rbind(tud_data_out,wow_data_out)


#Import AWS data
fn_aws <- "/usr/people/ahami/a_msc_thesis/R_dir/data_rdam/raw/aws_knmi.csv"
KNMI_AWS_1 <- fread(file = fn_aws, sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-") 
KNMI_AWS_1$id <- as.numeric(gsub("\\D", "", colnames(KNMI_AWS_1)[2]))
KNMI_1 <- as.data.frame(strptime(KNMI_AWS_1$datum, format = "%Y-%m-%dT%H:%M:%S"))
KNMI_1 <- cbind(KNMI_1, KNMI_AWS_1[,c(7,2,3,4,6)])
colnames(KNMI_1) <- c('datetime','id','temp_aws','ws','wd','pr')
KNMI_1 <- aggregate(list(temp_aws = KNMI_1$temp_aws, ws = KNMI_1$ws, wd = KNMI_1$wd, pr = KNMI_1$pr), by=list(datetime=cut(KNMI_1$datetime,"hour")),mean)
KNMI_1$datetime <- as.character(KNMI_1$datetime) 

# Add cloud cover
cloudcover <- fread(paste0(rw_dataloc,"/cloudcoverdata.csv"))
cloudcover <-  cloudcover[,c(1,3)]
cloudcover$IT_DATETIME <- as.character(as.POSIXct(cloudcover$IT_DATETIME, format = "%Y%m%d_%H%M%S_000000"))
names(cloudcover) <- c("datetime","cl_cov")

KNMI_1 <- merge(KNMI_1,cloudcover,by = "datetime")

## Add radiaton hourly and daily
# daily solrad
sol_rad_daily <- fread(paste0(rw_dataloc,"/daily_solrad.csv"))
sol_rad_daily <-  sol_rad_daily[,c(1,3)]
sol_rad_daily$IT_DATETIME <- as.character(as.POSIXct(sol_rad_daily$IT_DATETIME, format = "%Y%m%d_%H%M%S_000000"))
names(sol_rad_daily) <- c("datetime","sol_rad_d")

sol_rad_daily$datetime <- as.POSIXct(sol_rad_daily$datetime)
sol_rad_daily$date <- as.character(as.Date(sol_rad_daily$datetime))
KNMI_1$datetime <-  as.POSIXct(KNMI_1$datetime)
KNMI_1$date <- as.character(as.Date(KNMI_1$datetime))

KNMI_1 <- merge(KNMI_1,sol_rad_daily[,2:3], by = "date")
KNMI_1 <-  KNMI_1[,2:8]

# hourly solrad
sol_rad_hourly <- fread(paste0(rw_dataloc,"/straling_dagelijks_uurlijks.csv"))
sol_rad_hourly <-  sol_rad_hourly[,c(1,5)]
sol_rad_hourly$IT_DATETIME <- as.character(as.POSIXct(sol_rad_hourly$IT_DATETIME, format = "%Y%m%d_%H%M%S_000000"))
names(sol_rad_hourly) <- c("datetime","sol_rad_h")
KNMI_1$datetime <- as.character(KNMI_1$datetime)

KNMI_1 <- merge(KNMI_1,sol_rad_hourly, by = "datetime")

#Import urban wow ref
fn_wowrur <- "/usr/people/ahami/a_msc_thesis/R_dir/data_rdam/raw/wow_rur.csv"
data <- read.csv(fn_wowrur,sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-")
st_id = as.numeric(gsub("\\D", "", colnames(data)[2]))
data$id <- as.numeric(st_id)
colnames(data) <- c('datetime','temp','id')

data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%dT%H:%M:%S")
data2 <- aggregate(list(temp = data$temp), by=list(datetime=cut(data$datetime,"hour")),mean)
data2$datetime <- as.POSIXct(data2$datetime, format = "%Y-%m-%d %H:%M:%S")
data2$id <- data$id[1]
data2 <- data2[,c(1,3,2)]

wow_rur <- data2
rm(data,data2)









# save data
save(urb_obs, file = 'data/rotterdam/input/urb_obs.RData')
save(KNMI_1, wow_rur, file = 'data/rotterdam/input/rur_obs.RData')
