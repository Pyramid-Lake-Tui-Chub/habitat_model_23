#########################################################################################################
#### INSTALL PACKAGES ----
library(tidyverse)
library(tidyr)
library(reshape2)
library(knitr)
library(dplyr)
library(lubridate)
library(data.table)
library(plotrix)

#########################################################################################################
#### READ IN DATA INSTEAD OF IMPORT FROM PROJECT FILES ----

# 2022 & 2023
hydro_data <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\bottom_type_3clus.csv")
hydro_match <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\hydracoustic_match_files.csv")
scuba_data <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\scuba_master_2.csv")

# 2022 ONLY
fish_data22 <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\fecund_fish.csv")
gill_data22 <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\gear_shp.csv")
temp_data22 <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\master_fromGPT.csv")

# 2023 ONLY
temp_data23 <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\temp_logger_master_2023.csv")
fish_data23 <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\allFish_nogear_2023.csv")
gill_data23 <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\gill_gear_2023.csv")

# Light Trap and Larval Tow Gear Data
# LightT_data <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\LightT_gear_2023.csv")
# LT_data <- read.csv("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023\\LT_gear_2023.csv")

##########################################################################################################
#### 2022 FISH ----

# eliminate all gear types except gill nets
fish_data22 <- subset(fish_data22, net != "HOPS")

# change from 2022 syntax to 2023 syntax, add net, net_num, net_type, species, & expressing status
# to be comparable to 2023
fish_data22 <- fish_data22 %>% mutate(net = case_when(net == "GIL10" ~ "CORE10",
                                      net == "GIL20" ~ "CORE20",
                                      net == "GIL30" ~ "CORE30"))
fish_data22 <- fish_data22 %>% mutate(net_num = case_when(net == "CORE10" ~ 10,
                                                          net == "CORE20" ~ 20,
                                                          net == "CORE30" ~ 30))
fish_data22 <- fish_data22 %>% add_column(species="TUI", expressing = "Y", panel = 0, net_type = "CORE")

# I calculated cpue by sex in Excel in 2022 
# To calculate overall CPUE by net in 2022,I add the sexes together 
# Here I eliminate sex data to make it match with 2023
fish_data22 <- fish_data22 %>% mutate(cpue = matureFCpue + matureMCpue)
fish_data22 <- subset(fish_data22, select = -c(matureF., matureM., matureFCpue, matureMCpue))

##########################################################################################################
#### 2022 GEAR ----

# delete excess habitat variables
gill_data22 <- subset(gill_data22, select = -c(surface_temp, conductivity, clarity, lat, long))

# filter down to just gill nets
gill_data22 <- filter(gill_data22, net == "GIL10" | net == "GIL20" | net == "GIL30")

# change names to be uniform with 2022 fish data for merge
gill_data22 <- gill_data22 %>% mutate(net = case_when(net == "GIL10" ~ "CORE10",
                                                      net == "GIL20" ~ "CORE20",
                                                      net == "GIL30" ~ "CORE30")) 
names(gill_data22)[5:6] <- c("shallow_m", "deep_m")

# add median depth column
gill_data22 <- gill_data22 %>% mutate(med_depth_m = rowMeans(gill_data22[,c("shallow_m", "deep_m")]))

# convert date/time data into POSIX form 
# necessary for calculations
gill_data22$date_time_set <- mdy_hm(gill_data22$date_time_set)
gill_data22$date_time_pull <- mdy_hm(gill_data22$date_time_pull)

# add effort to back calculate effort
# for poisson offset
gill_data22 <- gill_data22 %>% mutate(effort = (date_time_pull-date_time_set))

#################################################################################################################
#### 2022 COMBINE FISH & GEAR ----

# outer join gear + fish on site, net and month
# the incoming data was just fecund tui chub, no other fish types
combo22 <- merge(gill_data22, 
                   fish_data22, 
                   .by.x = c("site", "net", "month"),
                   .by.y = c("site", "net", "month"), 
                   all.x=TRUE)

# should have 73 rows
# 81 with a perfect study design
# minus 3 for not setting DAG in June
# minus 5 for dropping CORE30

# calculate count
# for poisson offset
combo22 <- combo22 %>% mutate(count = round(cpue*as.double(effort, units='hours')))

#################################################################################################################
#### 2022 TEMPERATURE ----

# eliminate depth_bin column
temp_data22 <- subset(temp_data22, select = -depth_bin)

# turn date time from character to POSIXct
temp_data22$date_time <- mdy_hm(temp_data22$date_time)

# force dataframes to data tables for join
setDT(combo22)
setDT(temp_data22)

# use a non-equi join in data.table to join temp_data22 and combo 22(fish and gear data)
# by month (will be a giant table hence allow cartesian)
temp <- combo22[temp_data22, on=.(month), nomatch=0, allow.cartesian = TRUE]

# cut temp logger date time by gear date time
# since the loggers are not site specific this will
# assign them to the proper sites
# I have not accounted for dpeth yet so it will still be large
temp2 <- temp[date_time_set <= date_time & date_time <= date_time_pull]

# take the absolute difference between the median depth of the net and the logger depth
temp3 <- temp2 %>% mutate(depth_differential = abs(med_depth_m-depth))

# determine the minimum depth_differential by net, site and month
temp4 <- temp3 %>% group_by(net, site, month) %>% mutate(min = min(depth_differential))

# filter to only rows where the depth_differential is minimized
# aka take data from the closest logger in depth
temp5 <- filter(temp4, depth_differential == min)

# take the mean for each net
temp6 <- aggregate(data= temp5, temp ~ net + month + site, FUN=mean)

#add average net temp to combo22
combo22 <- merge(x=temp6, y=combo22, by = c('month', 'net', 'site'))

# reduce dataset to unique values 
# and rename temp to match with other datasets
combo22 <- combo22 %>% distinct(.keep_all= TRUE)
names(combo22)[4] <- c("temp_avg")

### IN CASE NECESSARY ###
### FOR GPT TEMP DATA: ###
#  bin 0 = pendant @ 3 m 
#  bin 1 = pendant @ 5 m
#  bin 2 = pendant @ 13 and 15 m
#  bin 3 = pendant @ 25, 27 and 30 m

##########################################################################################################################################
#####                                                                                                                              ######
##########################################################################################################################################
#### 2023 FISH ----

# eliminate larval data
fish_data23 <- subset(fish_data23, panel != "NA")

# make a count summary table for the fish data
# this includes all species, not just Tui
fish_counts23 <- fish_data23 %>% group_by(site,date,net_num,panel,expressing) %>% count(species)

##########################################################################################################################################
#### 2023 GEAR ----

# convert date/time data into POSIX form
gill_data23$date_time_set <- mdy_hm(gill_data23$date_time_set)
gill_data23$date_time_pull <- mdy_hm(gill_data23$date_time_pull)

# calculate effort
gill_data23 <- gill_data23 %>% mutate(effort = (date_time_pull-date_time_set))

# delete spatial data and notes
gill_data23 <- subset(gill_data23, select = -c(x, y, notes))

# make net_type and net_num columns
gill_data23 <- gill_data23 %>% mutate(net_num = case_when(net == "1" ~ "1",
                                                      net == "2" ~ "2",
                                                      net == "3" ~ "3",
                                                      net == "4" ~ "4",
                                                      net == "5" ~ "5",
                                                      net == "6" ~ "6",
                                                      net == "7" ~ "7",
                                                      net == "8" ~ "8",
                                                      net == "9" ~ "9",
                                                      net == "10" ~ "10",
                                                      net == "11" ~ "11",
                                                      net == "12" ~ "12",
                                                      net == "CORE10" ~ "10",
                                                      net == "CORE20" ~ "20")) 
gill_data23 <- gill_data23 %>% mutate(net_type = case_when(net == "1" ~ "EXP",
                                                          net == "2" ~ "EXP",
                                                          net == "3" ~ "EXP",
                                                          net == "4" ~ "EXP",
                                                          net == "5" ~ "EXP",
                                                          net == "6" ~ "EXP",
                                                          net == "7" ~ "EXP",
                                                          net == "8" ~ "EXP",
                                                          net == "9" ~ "EXP",
                                                          net == "10" ~ "EXP",
                                                          net == "11" ~ "EXP",
                                                          net == "12" ~ "EXP",
                                                          net == "CORE10" ~ "CORE",
                                                          net == "CORE20" ~ "CORE")) 

####################################################################################################
#### 2023 COMBINE FISH & GEAR ----

# change net_num to net
names(fish_counts23)[3] <- "net"

# outer join gear + tui fish on site, date, net_num, panel to retain 0s
# TOO LOOK AT ANOTHER PART OF THE POPULATION:
# change the input values for species or expressing
combo23 <- merge(gill_data23, 
                   subset(fish_counts23, species == "TUI" & expressing == "Y"), 
                   .by.x = c("site", "net", "panel"),
                   .by.y = c("site", "net", "panel"), 
                   all.x=TRUE)

# replace NAs with 0s or values
# this makes 0s for expressing TUI in all nets
combo23["n"][is.na(combo23["n"])] <- 0
combo23["species"][is.na(combo23["species"])] <- "TUI"
combo23["expressing"][is.na(combo23["expressing"])] <- "Y"

# delete date column
combo23 <- subset(combo23, select = -date)

# add a column for median depth
combo23 <- combo23 %>% mutate(med_depth_m = rowMeans(combo23[,c("shallow_m", "deep_m")]))

# calculate CPUE
combo23 <- combo23 %>% mutate(cpue = n/as.double(effort, units='hours'))

# change n to count to match combo22
names(combo23)[15] <- "count"
#####################################################################################################################################
#### 2023 COMBINE FISH, GEAR & TEMP ----

# turn date time from character to POSIXct
temp_data23$date_time <- mdy_hm(temp_data23$date_time)

# change net_num to net
names(temp_data23)[3] <- "net"

# force dataframes to datatables for join
setDT(combo23)
setDT(temp_data23)

# use a non-equi join in data.table to filter temp_data23 by site, and net
# for the time intervals of the net set specified in gill_cpue
temp <- combo23[temp_data23, on=.(site, net), nomatch=0, allow.cartesian = TRUE]
temp2 <- temp[date_time_set <= date_time & date_time <= date_time_pull]
temp3 <- aggregate(data= temp2, temp_c ~ net + site, FUN=mean)
temp4 <- merge(x=temp3, y=temp2, by = c('site', 'net'))
temp5 <- subset(temp4, select = c(-temp_c.y, -date_time, -logger)) 
combo23 <-temp5 %>% distinct(.keep_all= TRUE)
## important to delete date_time and depth to reduce the dataset to unique values
## HOWEVER you retain date_time_set and pull

# change temp_c to temp_avg
names(combo23)[3] <- c("temp_avg")

###########################################################################################################################################
#### 2022 & 2023 COMBINE FISH, GEAR, TEMP ----

# add years column to make them easily seperated
combo22 <- combo22 %>% add_column(year = "2022")
combo23 <- combo23 %>% add_column(year = "2023")

# add month column in 2023 to match 2022
combo23$month <- format(as.Date(combo23$date_time_pull, format="%y-%m-%d"),"%m")
combo23$month <- as.integer(combo23$month)

## COMBINE THE DATASETS
fish_gear_all <- merge(combo22, combo23, all=TRUE)

# make set order for 2022, 0
# makes toggle for 2022, 1
fish_gear_all["set_order"][is.na(fish_gear_all["set_order"])] <- 0
fish_gear_all["toggle"][is.na(fish_gear_all["toggle"])] <- 1

# year to integer for later merges
fish_gear_all$year <- as.integer(fish_gear_all$year)

# clean up environment
rm(list=c("temp", "temp2", "temp3", "temp4", "temp5", "temp6"))

###########################################################################################################################################
##                                                                                                                                       ##
###########################################################################################################################################
#### 2022 & 2023 HYDROACOUSTICS: MATCH TO SITE/NET/YEAR ----

# delete unnecessary files, only in some of them :)
hydro_data <- hydro_data[,c(6:7, 13:22)]

# you will have to change # of columns 
# if you use a different number of clusters

# change names
names(hydro_data)[1:12] <- c("date", "file_name", "bottom_status", "depth", "veg_status", "veg_height_m", "veg_cover", "bottom_type_ping_status",
                             "bottom_type_num", "bottom_1_load", "bottom_2_load", "bottom_3_load")

# break date/time column into just date column
hydro_data$date <- ymd_hms(hydro_data$date)
hydro_data$date <- as_date(hydro_data$date)

# make a year column
hydro_data$year <- format(as.Date(hydro_data$date, format="%y-%m-%d"),"%Y")
hydro_data$year <- as.integer(hydro_data$year)

# makes depth positive
hydro_data$depth <- hydro_data$depth * -1

# subset hydroacoustics match file to only necessary columns
hydro_match <- subset(hydro_match, select = -c(date, nets_surveyed, not_surveyed, year, distance_to_net_2022_m, notes))

# merge hydroacoustics data
# with match file
# to assign files to sites
hydro_data<- merge(x = hydro_data,
                    y = hydro_match,
                    by = "file_name")

# filter out files where BottomTypeStatus is invalid
hydro_data <- hydro_data %>%
  filter(bottom_type_ping_status == "Valid")

# change net type to uppercase to match with other datasets
hydro_data$net_type <- toupper(hydro_data$net_type)

# cut hydro_data by net_type, site, year and toggle (for sites with 2 EXP transects)
# create a data frame that searches for hydro depths WITHIN net depths and return good or bad in a dummy column
hydro1 <- fish_gear_all %>% left_join(hydro_data, by = c("site", "year", "net_type", "toggle"), relationship = "many-to-many")%>% 
                                              mutate(dummy = if_else(depth >= shallow_m & depth <= deep_m, "good", "bad?"))

# cut that dataframe into two dataframes with one being all good and one being all bad
hydro2 <- subset (hydro1, dummy == "bad?")
hydro3 <- subset(hydro1, dummy == "good")

# use an anti-join to eliminate from the bad? dataset any site, net, year combination
# that has a "legitimate match"
# such that the bad dataset now only has site/net/year combos
# that have NO legitimate match
hydro4 <- hydro2 %>% anti_join(hydro3, by = c("site","net", "year"))

# within the bad? dataset 
# use a second ifelse for nets that have a small depth change and don't span a hydroacoustic depth
# to alter the dummy variable to return pings within .2 meters of either the deep or shallow end of the net
hydro4 <- hydro4 %>% mutate(dummy = ifelse((deep_m + 0.2 > depth & depth > deep_m) | (shallow_m > depth & depth >= shallow_m - 0.2), "good", "SHIT"))

# subset to just NTS2 and NTS3 CORE
# because they have garbage CORE20 data (very soft substrate)
hydro5 <- subset(hydro4, net == "CORE20" & site == "NTS2" & depth>16 & depth <17.5 |
                         net == "CORE20" & site == "NTS3" & depth>14.4 & depth <17)

# cut the "roundabout legitimate" dataset to only good matches
hydro4 <- subset(hydro4, dummy == "good")

# use rbind to combine the dataset of all legitimate matches
# and roundabouts legitimate matches
hydro_fish_gear <- rbind(hydro3,hydro4, hydro5)

# clean-up
hydro_fish_gear <- subset(hydro_fish_gear, select = -dummy)
names(hydro_fish_gear)[c(1,22,24)] <- c("month_fish", "date_hydro", "depth_hydro")
rm(list=c("hydro1", "hydro2", "hydro3", "hydro4", "hydro5"))

##########################################################################################################################################
#### HYDROACOUSTICS: VEGETATION & SUBSTRATE AVERAGING ----

## Vegetation

# average vegetation parameters by site, year, net, panel
# NOTE: should not need to gorup_by toggle
# because in the earlier join the nets were assigned by toggle
sum_hydro_veg <- hydro_fish_gear %>% group_by(site, year, net, panel) %>% summarise(
  veg_height_avg = mean(veg_height_m),
  veg_cover_avg = mean(veg_cover))

# merge veg avg back to full hydro dataset
hydro_fish_gear <- merge(sum_hydro_veg,
                         hydro_fish_gear,
                         .by = c("year", "site", "net", "panel"))

# lump bottom types 2 & 3 together
hydro_fish_gear$bottom_type_num <- replace(hydro_fish_gear$bottom_type_num, hydro_fish_gear$bottom_type_num == 3, 2)

# reclassify bottom type 2 to silt
# reclassify bottom type 1 to sand
hydro_fish_gear <- hydro_fish_gear %>% mutate(bottom_type_num = 
                                                case_when(bottom_type_num == 2 ~ "silt",
                                                          bottom_type_num == 1 ~ "sand"))

# count bottom type number by site, year and panel
hydro_fish_gear <- hydro_fish_gear %>% group_by(site, year, net, panel) %>% add_count(bottom_type_num, name = "bottom_num_count")

# count total by site, year, net and panel
hydro_fish_gear <- hydro_fish_gear %>% group_by(site,year, net, panel) %>% add_count(bottom_type_ping_status, name = "total_count")

# calculate percent bottom type by site, year, net and panel
hydro_fish_gear <- hydro_fish_gear %>% mutate(sub_perc_net = (bottom_num_count/total_count) * 100)

# pivot wider to have seperate columns with percents
# for sand and silt
hydro_fish_gear <- hydro_fish_gear %>% group_by(site, net) %>% spread(key = "bottom_type_num",
                                                              value = "sub_perc_net")

# turn NAs into 0s
hydro_fish_gear["sand"][is.na(hydro_fish_gear["sand"])] <- 0
hydro_fish_gear["silt"][is.na(hydro_fish_gear["silt"])] <- 0

# eliminate columns to reduce dataset to unique values
# and columns that will merge with scuba data
hydro_fish_gear <- subset(hydro_fish_gear, select = c("year", "site", "net", "net_type", "net_num", "panel", "cpue", "deep_m", "shallow_m", "med_depth_m",
                                                  "temp_avg", "date_hydro", "date_time_pull", "sand", "silt", "veg_height_avg",
                                                  "veg_cover_avg", "count", "effort"))

# find max value for each site, year, net, panel combo
# there should always be one that has a value and one that is 0
hydro_fish_gear <- hydro_fish_gear %>% group_by(year, site, net, panel) %>% mutate(silt = max(silt), sand = max(sand))

# slim dataset to a single row for each panel   
hydro_final <- distinct_all(hydro_fish_gear)

##########################################################################################################################################
###                                                                                                                                    ###
##########################################################################################################################################
#### SCUBA: MATCH TO SITE/NET ----

# transform date
scuba_data$date <- mdy(scuba_data$date)

# get rid of 2022
scuba_data <- subset(scuba_data, date >= "2023-04-01")

# delete unnecessary columns
scuba_data <- scuba_data[,c(1,2,3,7,8,10,13:20,25:30)]

# change bedrock and boulder to integer
scuba_data$bedrock <- as.integer(scuba_data$bedrock)
scuba_data$boulder <- as.integer(scuba_data$boulder)

# change scuba names for merge
names(scuba_data)[c(4:5)] <- c("deep_m_scuba", "shallow_m_scuba")

# cut SCUBA data by site, net-type and toggle and various forms of overlapping transect
# create a column with a dummy variable that indicates whether the transects
# meet the criteria
scuba1 <- gill_data23 %>% 
  full_join(scuba_data, by = c("site", "net_type", "toggle"), relationship = "many-to-many") %>% 
  mutate(dummy = if_else(((deep_m_scuba >= deep_m & shallow_m >= shallow_m_scuba) | 
                            (deep_m > deep_m_scuba & shallow_m < deep_m_scuba) | 
                            (deep_m > shallow_m_scuba & shallow_m_scuba > shallow_m) |
                            (deep_m > deep_m_scuba & shallow_m_scuba > shallow_m)), 
                         "good", "bad?")) 

# create a dataset of just the transects that don't match
# and one of the transects that do
scuba2 <- subset (scuba1, dummy == "bad?")
scuba3 <- subset(scuba1, dummy == "good")

## NOTE: with the full join, all objects that match on site, net type and toggle
## are retained, even if they don't meet the conditional
## so you can still select based on those features

# keep the CORE10 data for sites where we did a CORE10 transect but it doesn't
# precisely align depth wise
scuba2 <- subset(scuba2, net == "CORE10" & site == "BLH2" | 
                   net == "CORE10" & site == "PLQ3"|
                   net == "CORE10" & site == "NTS3"|
                   net == "CORE10" & site == "NTS")

# combine the "legitimate match" dataset
# and the forced CORE10 dataset
scuba_gear <- rbind(scuba3,scuba2)

# clean-up
scuba_gear <- subset(scuba_gear, select = -dummy)
rm(list=c("scuba1", "scuba2", "scuba3"))

#############################################################################################################
#### SCUBA: VEGETATION & SUBSTRATE AVERAGING ----

# flip scuba sub_classes to long format
scuba_gear <- scuba_gear %>% pivot_longer(cols = "bedrock":"hard_clay",
                                          names_to = "sub_class",
                                          values_to = "sub_class_perc")

# average substrate class percent by site, net, panel 
sum_scuba_sub <- scuba_gear %>% group_by(site, net, panel,sub_class) %>% 
  summarise(substrate_avg = mean(sub_class_perc))

# pivot back to wide
sum_scuba_sub <- sum_scuba_sub %>% pivot_wider(names_from = "sub_class",
                                               values_from = "substrate_avg")

# average vegetation height and vegetation cover by site, net, panel
sum_scuba_veg <- scuba_gear %>% group_by(site, net, panel) %>% 
  summarise(veg_height_avg = mean(c(veg_1,veg_2,veg_3,veg_4,veg_5)),
            veg_cover_avg = mean(veg_cover))

# merge averages
sum_scuba <- merge(x = sum_scuba_veg,
                    y = sum_scuba_sub,
                    by = c("site", "net", "panel"))

# add sum_scuba to fish and temp data for merge
scuba_fish_gear <- merge(x = sum_scuba,
                         y = combo23,
                         .by = c("site", "net", "panel"))

# change year to integer for later joins
scuba_fish_gear$year <- as.integer(scuba_fish_gear$year)

# eliminate columns that won't match with scuba data from hydro data
scuba_final <- subset(scuba_fish_gear, select = c("site", "net", "net_num", "net_type", "panel", "veg_height_avg", "veg_cover_avg", "cpue", "deep_m", "shallow_m",
                                                  "med_depth_m", "bedrock", "boulder", "gravel", "small_gravel", "cobble", "sand",
                                                  "silt", "hard_clay", "temp_avg", "year", "date_time_pull", "count", "effort"))

##############################################################################################################
###                                                                                                        ###
##############################################################################################################
#### 2022 & 2023 FULL HABITAT, GEAR & FISH COMBINE ----

# anti-join hydros with scuba such that only nets NOT found in scuba are retained in hydros
# aka use SCUBA over hydros whenever possible
hydro_anti <- hydro_final %>% anti_join(scuba_final, by = c("site","net", "year"))

# merge using SCUBA data when available and hydroacoustics data if not available
habData_master <- merge(scuba_final,
                        hydro_anti,
                        .by = c("site", "year", "net", "panel"),
                        all.x=TRUE,
                        all.y=TRUE)

# force NAs for complex substrate in hydro only substrate data to 0 (need this for reclassify)
habData_master["bedrock"][is.na(habData_master["bedrock"])] <- 0
habData_master["boulder"][is.na(habData_master["boulder"])] <- 0
habData_master["cobble"][is.na(habData_master["cobble"])] <- 0
habData_master["gravel"][is.na(habData_master["gravel"])] <- 0
habData_master["small_gravel"][is.na(habData_master["small_gravel"])] <- 0
habData_master["hard_clay"][is.na(habData_master["hard_clay"])] <- 0

##############################################################################################################
#### RE-CLASSIFY SUBSTRATE TO CMECS CLASSIFICATIONS ----

# case_when to classify substrate to the subgroup level based on cmecs classifications
habData_master <- habData_master %>% mutate(cmecs_subgroup = case_when(bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand < 10 & (hard_clay/(hard_clay + silt)) * 100 >= 67 ~ "clay",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand < 10 & (silt/(hard_clay + silt)) * 100 >= 33 & (silt/(hard_clay + silt)) * 100 < 67 ~ "silt_clay",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand < 10 & (silt/(silt + hard_clay)) * 100 >= 67 ~ "silt",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & (hard_clay/(hard_clay + silt)) * 100 >= 67 & sand >= 10 & sand < 50 ~ "sandy_clay",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand >= 10 & sand < 50 & (silt/(hard_clay + silt)) * 100 >= 33 & (silt/(hard_clay + silt)) * 100 < 67 ~ "sandy_silt_clay",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand >= 10 & sand < 50 & (silt/(hard_clay + silt)) * 100 >= 67 ~ "sandy_silt",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand >= 50 & sand < 90 & (hard_clay/(hard_clay + silt)) * 100 >= 67 ~ "clayey_sand",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand >= 50 & sand < 90 & (silt/(hard_clay + silt)) * 100 >= 33 & (silt/(hard_clay + silt)) * 100 < 67 ~ "silty_clayey_sand",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand >= 50 & sand < 90 & (silt/(hard_clay + silt)) * 100 >= 67 ~ "silty_sand",
                                                                    bedrock == 0 & boulder == 0 & cobble == 0 & gravel == 0 & small_gravel == 0 & sand >= 90 ~ "sand",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) < 5 & ((hard_clay + silt)/(hard_clay + silt + sand)) * 100 >= 90 ~ "slightly_gravelly_mud",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) < 5 & ((hard_clay + silt)/(hard_clay + silt + sand)) * 100 >= 50 & ((hard_clay + silt)/(hard_clay + silt + sand)) * 100 < 90 ~ "slightly_gravelly_sandy_mud",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) < 5 & ((sand)/(hard_clay + silt + sand)) * 100 >= 50 & ((sand)/(hard_clay + silt + sand)) * 100 < 90 ~ "slightly_gravelly_muddy_sand",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) < 5 & ((sand)/(hard_clay + silt + sand)) * 100 >= 90 ~ "slightly_gravelly_sand",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 5 & (bedrock + boulder + cobble + gravel + small_gravel) < 30 & ((hard_clay + silt)/(hard_clay + silt + sand)) * 100 >= 50 ~ "gravelly_mud",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 5 & (bedrock + boulder + cobble + gravel + small_gravel) < 30 & ((sand)/(hard_clay + silt + sand)) * 100 >= 50 & ((sand)/(hard_clay + silt + sand)) * 100 < 90 ~ "gravelly_muddy_sand",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 5 & (bedrock + boulder + cobble + gravel + small_gravel) < 30 & ((sand)/(hard_clay + silt + sand)) * 100 >= 90 ~ "gravelly_sand",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 30 & (bedrock + boulder + cobble + gravel + small_gravel) < 80 & ((hard_clay + silt)/(hard_clay + silt + sand)) * 100 >= 50 ~ "muddy_gravel",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 30 & (bedrock + boulder + cobble + gravel + small_gravel) < 80 & ((sand)/(hard_clay + silt + sand)) * 100 >= 50 & ((sand)/(hard_clay + silt + sand)) * 100 < 90 ~ "muddy_sandy_gravel",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 30 & (bedrock + boulder + cobble + gravel + small_gravel) < 80 & ((sand)/(hard_clay + silt + sand)) * 100 >= 90 ~ "sandy_gravel",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 80 & ((bedrock + boulder)/(bedrock + boulder + cobble + gravel + small_gravel) * 100) >= 80 ~ "boulder",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 80 & ((cobble)/(bedrock + boulder + cobble + gravel + small_gravel) * 100) >= 80 ~ "cobble",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 80 & ((gravel + small_gravel)/(bedrock + boulder + cobble + gravel + small_gravel) * 100) >= 80 ~ "pebble",
                                                                    (bedrock + boulder + cobble + gravel + small_gravel) >= 80 ~ "gravel"))

# case_when to classify substrate to the group level based on cmecs classifications
habData_master <- habData_master %>% mutate(cmecs_group = case_when(cmecs_subgroup == "clay" | cmecs_subgroup == "silt_clay" | cmecs_subgroup == "silt" ~ "mud",
                                                                    cmecs_subgroup == "sandy_clay" | cmecs_subgroup == "sandy_silt_clay" | cmecs_subgroup == "sandy_silt" ~ "sandy_mud",
                                                                    cmecs_subgroup == "clayey_sand" | cmecs_subgroup == "silty_clayey_sand" | cmecs_subgroup == "silt_sand" | cmecs_subgroup == "silty_sand"~ "muddy_sand",
                                                                    cmecs_subgroup == "sand" ~ "sand",
                                                                    cmecs_subgroup == "slightly_gravelly_mud" | cmecs_subgroup == "slightly_gravelly_sandy_mud" | cmecs_subgroup == "slightly_gravelly_muddy_sand" | cmecs_subgroup == "slightly_gravelly_sand"~ "slightly_gravelly",
                                                                    cmecs_subgroup == "gravelly_mud" | cmecs_subgroup == "gravelly_muddy_sand" | cmecs_subgroup == "gravelly_sand" ~ "gravelly",
                                                                    cmecs_subgroup == "muddy_gravel" | cmecs_subgroup == "muddy_sandy_gravel" | cmecs_subgroup == "sandy_gravel" ~ "gravel_mixes",
                                                                    cmecs_subgroup == "gravel" ~ "gravel"))

habData_master <- habData_master %>% mutate(cmecs_reduce = case_when(cmecs_group == "gravel" | cmecs_group == "gravel_mixes" ~ "gravel_mixes",
                                                                     cmecs_group == "gravelly" ~ "gravelly",
                                                                     cmecs_group == "sand" | cmecs_subgroup == "slightly_gravelly_muddy_sand" | cmecs_subgroup == "slightly_gravelly_sand" ~ "sand",
                                                                     cmecs_group == "muddy_sand" | cmecs_group == "sandy_mud" ~ "sand_mud",
                                                                     cmecs_group == "mud" | cmecs_subgroup == "slightly_gravelly_mud" | cmecs_subgroup == "slightly_gravelly_sandy_mud" ~ "mud"))
# makes cmecs_reduce an ordinal variable
habData_master <- habData_master %>% mutate(cmecs_reduce_ord = case_when(cmecs_group == "gravel" | cmecs_group == "gravel_mixes" ~ 5,
                                                                     cmecs_group == "gravelly" ~ 4,
                                                                     cmecs_group == "mud" | cmecs_subgroup == "slightly_gravelly_mud" | cmecs_subgroup == "slightly_gravelly_sandy_mud" ~ 1,
                                                                     cmecs_group == "muddy_sand" | cmecs_group == "sandy_mud" ~ 2,
                                                                     cmecs_group == "sand" | cmecs_subgroup == "slightly_gravelly_muddy_sand" | cmecs_subgroup == "slightly_gravelly_sand" ~ 3))
# add month column
habData_master$month <- month(habData_master$date_time_pull)

# add slope variable
habData_master <- habData_master %>% mutate(slope = case_when(net_type == "CORE" ~ ((shallow_m - deep_m)/ 24.38) * -1,
                                                              net_type == "EXP" ~ ((shallow_m - deep_m)/ 3.05) * -1))

# force effort to numeric
habData_master$effort <- as.numeric(habData_master$effort)

# transform continuous input variables
habData_master$veg_height_avg_log <- log10(habData_master$veg_height_avg + 0.08)
habData_master$veg_cover_avg_log <- log10(habData_master$veg_cover_avg + 0.3)
habData_master$temp_avg_log <- log10(habData_master$temp_avg + 0.08)
habData_master$slope_log <- log10(habData_master$slope + 0.9)

#### SPLIT BY MODEL ####

## 2022 CORE ##
core22 <- habData_master %>% filter(year == "2022")

#@ 2022 CORE: all months, only core10 & core20 #@
# model everything except vegetation
fullnets_22 <- subset(core22, net != "CORE30" & site != "DAG")

## 2022 CORE: subset to July for vegetation (only complete dataset) ##
# model all habitat variables including vegetation
# complete22 <- subset(exp23, date_time_pull > "2022-07-20")

## 2023 CORE ##
core23 <- habData_master %>% filter(year == "2023" & net_type == "CORE")

## 2023 EXP ##
exp23 <- habData_master %>% filter(year == "2023" & net_type == "EXP")

# case when to assign net/panels to mesh (1-6)
exp23 <- exp23 %>% mutate(mesh_num = case_when((net == 1 & panel == 1)| 
                                                 (net == 5 & panel == 2)|
                                                 (net == 7 & panel == 2)|
                                                 (net == 11 & panel == 1) ~ 1,
                                               (net == 2 & panel == 1)| 
                                                 (net == 6 & panel == 1)|
                                                 (net == 8 & panel == 2)|
                                                 (net == 12 & panel == 1) ~ 2,
                                               (net == 3 & panel == 2)| 
                                                 (net == 4 & panel == 2)|
                                                 (net == 9 & panel == 2)|
                                                 (net == 10 & panel == 1) ~ 3,
                                               (net == 1 & panel == 2)| 
                                                 (net == 4 & panel == 1)|
                                                 (net == 7 & panel == 1)|
                                                 (net == 10 & panel == 2) ~ 4,
                                               (net == 3 & panel == 1)| 
                                                 (net == 5 & panel == 1)|
                                                 (net == 9 & panel == 1)|
                                                 (net == 11 & panel == 2) ~ 5,
                                               (net == 2 & panel == 2)| 
                                                 (net == 6 & panel == 2)|
                                                 (net == 8 & panel == 1)|
                                                 (net == 12 & panel == 2) ~ 6))

#### WRITE FINAL DATAFRAME TO A CSV ----
write.csv(habData_master, "C:\\Documents\\Pyramid_Lake\\RCreations\\csv_files\\habData_23.csv", row.names=FALSE)
write.csv(core23, "C:\\Documents\\Pyramid_Lake\\RCreations\\csv_files\\core23.csv", row.names=FALSE)
