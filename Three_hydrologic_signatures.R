
setwd("D:/NHD+Data/R_CODES/USGSCodes")

# rm(list=setdiff(ls(), "nc"))

# Importing Gaging Stations -----------------------------------------------

library(tidyverse)
library(readxl)
GagesStations <- read_excel("GagesStations.xlsx")

# View(GagesStations)

stations <- GagesStations$STAID

# View(stations)

# DataRetrieval -----------------------------------------------------------

library(dataRetrieval)
site <- stations
parameter <- "00060"    # Discharge Parameter

start <- "2001-01-01"

end <- "2019-12-30"

## Never delete (remove) this stuff, never 

# nc <-readNWISdv(site, parameter, start, end) # Downloading the data takes longer than 3 hours

## Never never 


nc_mean <- renameNWISColumns(nc)

nc_mean$site_no <- as.factor(nc_mean$site_no)

str(nc_mean)

nc_1 <- nc


# View(nc_mean)

# write.csv(nc_mean, "nc_dv_mean1.csv")

nc_mean <- read_csv("nc_dv_mean1.csv", 
                    col_types = cols(`03` = col_character())) # Mean daily Discharge data from 2000 to 2019.


# str(nc_mean)

# The previous code was run earlier to generate the daily mean discharge of 514 Gaging stations.
# The file was large and it took more than 3 hours to run the code. 
# Hence, the file was stored as .txt format in the computer.


# View(nc_mean)


# Baseflow Index ----------------------------------------------------------

install.packages("EflowStats", repos=c("https://owi.usgs.gov/R",getOption("repos")))
 
remotes::install_github("USGS-R/EflowStats")

library(EflowStats)

base.flow.index <- nc_mean %>%
  group_by(site_no) %>%
  summarise ( bfi.index = calc_bfi(Flow))

# The total Gaging Stations was 514, the baseflow index calcualted the gaging index for only 409.

# Let's find out what happened to the rest of the gaging stations.


# Identifying the disappearing gaging stations  ---------------------------

# siteInfo <- attr(bas, "siteInfo")   
# This function gives you information about the gaging stations
# But it won't work here because we exported the data into txt file and brining back to R, loosing
# all the behind the scene information

Stations.not.listed <- setdiff(GagesStations$STAID, nc_mean$site_no)


Information.Stations.not.listed <- whatNWISdata(siteNumber = Stations.not.listed) %>%
  filter(parm_cd == "00060") %>% 
  select (site_no, parm_cd, begin_date, end_date, count_nu)

# There are few null values in the table. The Null values are the result of missing data 
# in the discharge table. We need to remove those Stations as well as they don't have continuous
# discharge data from 2000 to 2019.

# Baseflow index after removing null values. 
base.flow.index.removeNull <- na.omit(base.flow.index)

# Stations with continuous data
GagesStations.ContinuousData <- base.flow.index.removeNull$site_no



#Selecting only those mean daily hydrologic discharge value of stations that have continuous discharge data
nc_mean.ContinuiousData.for.stations <- nc_mean %>%
  filter(site_no %in% GagesStations.ContinuousData) 


# Calculating Slope -------------------------------------------------------

Slope <- nc_mean.ContinuiousData.for.stations %>% 
  group_by(site_no) %>% 
  summarise ( ln.ThirtythreeQ = log (quantile (Flow, probs = c(0.33))), 
              ln.SixtysixQ = log (quantile (Flow, probs = c(0.66)))) %>% 
  mutate (slope = (ln.ThirtythreeQ - ln.SixtysixQ)/ (0.66 - 0.33)) 


# Flashiness Index --------------------------------------------------------
# install.packages("devtools")
library(devtools) 
# Sys.setenv("TAR" = "internal") # needed if using R v3.6.0 or later
# install_github("leppott/ContDataQC")
library(ContDataQC)

Flashiness.Index = nc_mean.ContinuiousData.for.stations %>% 
  group_by (site_no) %>% 
  summarise (Flashiness.Index = RBIcalc(Flow))

# We have Calcualted three hydrologic signatures: They are

# 1. Baseflow Index as: base.flow.index.removalNull
# 2. Slope Index as: Slope
# 3. Flashiness Index as: Flashiness.Index


# Join (merge) the Files --------------------------------------------------

Three.hydrologic.signatures <- 
  select (Slope, site_no, slope) %>% 
  merge  (base.flow.index.removeNull, by = "site_no") %>% 
  merge  (Flashiness.Index, by = "site_no")


Three.hydrologic.signaturesCleaned <- 
  Three.hydrologic.signatures %>% 
  filter(bfi.index >= 0)

str(Three.hydrologic.signaturesCleaned)



# View(Three.hydrologic.signaturesCleaned)

# write.csv(Three.hydrologic.signaturesCleaned, "Signatures123.csv")


nc_mean %>% filter(site_no == "02174250") %>% ggplot(aes (x = Date, y = Flow))+
  geom_line()+
  scale_x_date(date_breaks = "1 year", date_labels =  "%b %Y") 




  

