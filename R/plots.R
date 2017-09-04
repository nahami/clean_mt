subdate <- function(df,beginTime,y){df[df$datum >= beginTime & df$endTime <= y,]}

#' plotTempobs
#' @desciption makes a nice plot of df with following columns: datetime, rur_obs,urb_obs,dif
#' @author Nadir
#' @param df data frame
#' @param beginTime begin time in posixct class
#' @param endeTime end time in posixct class
#'  
#' @export

plotTempObs <- function(df,beginTime,endTime, drops){
# if (!inherits(df=="data.frame")){
#   message("df is not a data.frame")
#   return(FALSE)
# }

  theme_update(plot.title = element_text(hjust = 0.5))
  sub_set <- subdate(df, beginTime, endTime)
  
  if (drops != 0) {
    drops <- names(sub_set)[drops]
    sub_set <- sub_set[, !(names(sub_set) %in% drops)]
  }

  temp_obs <- ggplot(melt(sub_set, id = "datum")) + 
    geom_step(aes(x=datum, y = value, colour=variable), na.rm = T) +
    scale_colour_manual(values=c("orange","blue","green")) +
    labs(title = "temp_obs",
         x = "Wind Speed (m/s)",
         y = "Count") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  ggsave("temp_obs.png")
  message("saved plot")
  return(TRUE)
}

