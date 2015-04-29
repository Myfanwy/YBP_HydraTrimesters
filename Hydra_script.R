#--------------------------
# Hydra .csv detection file preparation
# Author: Myfanwy Johnston
# Date: 4/24/2015
#---------------------------

library(readr)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)

namesR <- c("DateTimeUTC", "Receiver", "Transmitter" , "SensorValue" , "SensorUnit" ,
                  "NA", "NA2" ,"Station", "Lat" ,"Lon") # with no spaces, easier in R

d <- read_csv("Yolo_Bypass_69khz_2014_Tri1.csv", col_names = namesR)
d <- d[-1,] #getting rid of old column headers
d$DateTimeUTC <- ymd_hms(as.character(d$DateTimeUTC)) #convert to character before using lubridate
d
str(d)
unique(d$Station)
# Finding orphan detections

# dplyr solution:

  d %>%
  group_by(Station) %>%
  summarize(min = min(DateTimeUTC), max = max(DateTimeUTC))

# writing a function to return max/min date of detections for a given station, where x is the df
det_minmax <- function(x) {
  x %>%
    group_by(Station, Receiver) %>%
    summarize(min = min(DateTimeUTC), max = max(DateTimeUTC))
}

det_minmax(d)


# clipping d to match the deployment start date:

d2 <- filter(d, DateTimeUTC >= "2014-02-01 8:01:00") 
min(d2$DateTimeUTC) > "2014-02-01 8:01:00"
d2

## read deployment file in and create df
dep <- read_excel("Yolo_Bypass_69khz_2014_Tri1_Deployments.xls")
str(dep) #make sure R picked up date formats
unique(dep$Location)

## Next Step: Figure out how to trim d to match deployment start_ and end_ times.

minmaxes <- det_minmax(d2) # create df of min and max detections by station
minmaxes <- rename(minmaxes, Location=Station) # rename to be able to merge with deployment df
end <- merge(minmaxes, dep, by="Location") # merge the dfs
end

end$Start_Date < end$min # Should all be true; the "false" result is for the deployment with dual start times
end$End_Date > end$max # Should all be true, again unless you have a deployment with a dual start time

select(end, Location, max, End_Date) # visually Verify that end_date is later than max detection
select(end, Location, Start_Date, min) # visually verity that start_date is earlier than min detection

#=======================================================================
#
# Format Detections dataframe for export as a .csv and upload into Hydra
#
#=======================================================================
str(d2)
d2 <- select(d2, DateTimeUTC, Receiver, Transmitter, Station) # these are the only columns I need - feel free to keep all of yours
names_export <- c("Date and Time (UTC)", "Receiver", "Transmitter","Station") # change colnames before exporting to conform to Hydra
colnames(d2) <- names_export

write.csv(d2, "Yolo_Bypass_69khz_2014_Tri1_Detections.csv")


