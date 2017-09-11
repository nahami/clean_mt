#------------------
# Thesis: Statistical modeling of the Urban Heat Island
# May 2017, by Nadir Ahami
# 
# 
#### Set up environment ####
rm(list = ls())
source("R/statistics.R")
source("R/plots.R")

# Set working directory
dir <- "/usr/people/ahami/a_msc_thesis/clean_mt"
setwd(dir)

# packages
library(lintr)
library(xgboost)   # ensemble method: extreme gradient boosted trees
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(grid)
library(gridExtra)
library(gsw)
library(oce)
library(randomForest)

#### Load data ####
load("data/rotterdam/input/urb_obs.RData")
load("data/rotterdam/input/rur_obs.RData")
ancdat <- read.csv("data/rotterdam/input/rdam_anc", header = TRUE)
KNMI_1[,7] <- as.numeric(KNMI_1[,7])
KNMI_1[,8] <- as.numeric(KNMI_1[,8])
knmi_aws <- KNMI_1
wow_rur$datetime <- as.character(wow_rur$datetime)
knmi_aws2 <- merge(knmi_aws[,c(1,3,4,5,6,7,8)],
                   wow_rur, by = "datetime", all = TRUE)
knmi_aws2 <- knmi_aws2[,c(1,8,2,3,4,5,6,7)]

# Use wow as rural ref?? set wow_ref to TRUE
wow_ref <- FALSE
if (wow_ref == T){
  knmi_aws <- knmi_aws2
  print("now the wow station is the rural reference")
}
names(knmi_aws2) <- names(knmi_aws)
urb_obs$datetime <-  as.character(urb_obs$datetime)


# generate data sun elevation and heading
m_lat <- 51.93
m_lon <- 4.509
t0 <- as.POSIXct(knmi_aws[1,1], tz = "UTC")
tn <- as.POSIXct(knmi_aws[nrow(knmi_aws),1], tz = "UTC")
t <- seq(from = t0, to = tn, by = "hour")
sunel <- as.data.frame(sunAngle(t, longitude = m_lon, latitude = m_lat))[,1:3]
colnames(sunel) <- c("datetime", "s_azimuth", "s_altitude")
sunel$datetime <- as.character(sunel$datetime)

# Merge dfs
feat_inp <- merge(urb_obs, knmi_aws, by = "datetime") #KNMI AWS
feat_inp$tempdif <- feat_inp$temp - feat_inp$temp_aws
feat_inp <- feat_inp[,c(1,2,3,11,4,5,6,7,8,9,10)]

feat_inp <- merge(feat_inp, ancdat[,c(2,6,7)], by = "id", all = TRUE) #SVF, LC
#Removing stations outside area of interest (bernisse lansingerland):
feat_inp <- feat_inp[!(feat_inp$id == "Bernisse" | feat_inp$id == "Lansingerland"),]
feat_inp <- merge(feat_inp, sunel, by = "datetime")

# save(feat_inp,file = "data/rotterdam/input/finput.Rdata")
# remove rows with NA

load(file = "data/rotterdam/input/finput.Rdata")
feat_inp <- na.omit(feat_inp)



feat_inp_p <- preProcess(feat_inp[,-(1:4)], method = c("YeoJohnson","center","scale"))
feat_inp_pp <- predict(feat_inp_p,feat_inp[,5:15])
feat_inp[,5:15] <- feat_inp_pp


#### Split into train and test set #####
# Random row IDs
set.seed(123)
randomizedRowIDs <- sample(1:length(feat_inp[,1]), length(feat_inp[,1]),
                           replace = FALSE)

# Create train and test row IDs
train_fr <- 0.5
cut_off <- floor(train_fr * length(feat_inp[,1]))
trainRowIDs <- randomizedRowIDs[1:cut_off]
testRowIDs <- randomizedRowIDs[(cut_off + 1):length(feat_inp[,1])]

# Column names targets and predictors
tn <- 3 # 3 = temp, 4 = tempdif
target <- names(feat_inp)[tn]
predictors <- setdiff(names(feat_inp), names(feat_inp)[1:4])

# Split total set into train and test sets
trainX <- feat_inp[trainRowIDs, predictors]
trainY <- feat_inp[trainRowIDs, target]
testX <- feat_inp[testRowIDs, predictors]
testY <- feat_inp[testRowIDs, target]

# Convert dataframes to matrices
matrixTrainX <- as.matrix(trainX)
matrixTrainY <- as.matrix(trainY)
matrixTestX <- as.matrix(testX)
matrixTestY <- as.matrix(testY)

matrixTrainxy <- as.data.frame(cbind("y" = matrixTrainY, matrixTrainX))

##### Train model #####
# Train model boosted tree
bst.time <- system.time(
bst <- xgboost(objective = "reg:linear",
               data = matrixTrainX,
               label = matrixTrainY,
               nround = 200,
               lambda = seq(0,2,0.2),
               lambda_bias = seq(0,2,0.2),
               eta = seq(0.1,1,by = 0.1),
               max_depth = seq(1,15,by = 1),
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 20,
               save_name = "output/models/xgboost.model"
))

# rf_fit <- train(V1 ~ .,
#                 data = matrixTrainxy,
#                 # preProcess = c("center","scale","YeoJohnson"),
#                 method = "rf")

lm_fit <- train(V1 ~ .,
               data = matrixTrainxy,
               # preProcess = c("center","scale","YeoJohnson"),
               method = 'lm')



linmse <- rmse(matrixTestY,matrixTestX,lm_fit)
bstmse <- rmse(matrixTestY,matrixTestX,bst)

linrsq <-  rsquared(matrixTestY,matrixTestX,lm_fit)
bstrsq <-  rsquared(matrixTestY,matrixTestX,bst)
print(c(linmse,bstmse,linrsq,bstrsq))









xgb.importance(model = bst)
summary(lm_fit)

output_tbl <- feat_inp
load(file = "data/rotterdam/input/finput.Rdata")
feat_inp <- na.omit(feat_inp)
output_tbl$temp_aws <-  feat_inp$temp_aws
output_tbl$y_model <- predict(object = lm_fit,newdata = output_tbl[,5:15])
output_tbl$residuals <- output_tbl$temp - output_tbl$y_model
output_tbl$up_bnd <- output_tbl$y_model+linmse 
output_tbl$lo_bnd <- output_tbl$y_model-linmse

ancdat <- read.csv("data/rotterdam/input/rdam_anc", header = TRUE)
output_tbl <- merge(output_tbl,ancdat[,c(1,2)], by = "id", all = F)

output_tbl <- output_tbl[,c(2,3,5,16,17,18,19)]
output_tbl$datetime <-  as.POSIXct(output_tbl$datetime)













library(reshape2)
temp_obs <- ggplot(melt(output_tbl, id = "datetime")) + 
  geom_step(aes(x=datetime, y = value, colour=variable), na.rm = T) +
  # scale_colour_manual(values=c("orange","blue","green")) +
  labs(#title = paste0("location id: ",ids[id]),
       x = "Time",
       y = "Temperature (Celsius)") +
  theme(legend.title=element_blank())

