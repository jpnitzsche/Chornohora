################################################
## Read and Explore Chornohora Inventory Data ##                    
## J. Nitzsche                                ##                            
## 27.02.2019                                 ##                            
################################################


###### Preparation ######

rm(list=ls())

#Working Directory
workingDirectory <- "H:/R/chornohora"

#Required packages
requiredPackages <- c("rgdal", "dplyr", "plyr", "ggplot2", "data.table")

#Install required packages
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) 
  install.packages(new.packages)

#Load required packages
lapply(requiredPackages, library, character.only=T)

##### Import Data #####

# Read stand inventory data 2018
filenames <- list.files(path = "H:/R/chornohora/data/csv", pattern = "*", full.names=TRUE)
stand_inv_2018 <- ldply(filenames, read.csv, sep =";")
# Select relevant columns
stand_inv_2018 <- stand_inv_2018 %>%
  select(f_id, aj, bnr, ba, ach, dec, d1, d2, d.mit, P, ic1, ic2, ic3, ic4, ic5, ic6, H, kag, d7,
         bem1, bem2, bem3, nach.bnr)

#Read stand data from 2006 and 2012 inventories (source: GitHub "spruce_transition/data/analysis/data_cleaned.csv")
stand_inv_06_12 <- read.csv('H:/R/chornohora/data/data_cleaned.csv', sep = ";", header = TRUE)




##### Clean Files #####

###Inventory 2018
# Drop column d.mit, nach.bnr, P
stand_inv_2018 <- subset(stand_inv_2018, select = -c(d.mit, nach.bnr, P))
#Rename columns
colnames(stand_inv_2018)[colnames(stand_inv_2018)=="H"] <- "htot"
colnames(stand_inv_2018)[colnames(stand_inv_2018)=="ach"] <- "ahc"
colnames(stand_inv_2018)[colnames(stand_inv_2018)=="d7"] <- "dh2"
#Reorder columns
stand_inv_2018 <- stand_inv_2018[c("f_id","aj","bnr","ba","ahc","dec","d1","d2","dh2","htot", 
                                   "kag","ic1","ic2","ic3","ic4","ic5","ic6","bem1","bem2","bem3")]

###Inventory 2006 and 2012
#Drop column fnum, koorx, koory, cond, fid_bid, fid_bid_aj. ahc_dec
stand_inv_06_12 <- subset(stand_inv_06_12, select = -c(fnum, koorx, koory, cond, fid_bid, fid_bid_aj, ahc_dec))
#Change name of column bem to bem1
setnames(stand_inv_06_12, "bem", "bem1")
#Add columns bem2, bem3
stand_inv_06_12$bem2<- NA
stand_inv_06_12$bem3<- NA

### Merge Data ###
dat <- rbind(stand_inv_06_12, stand_inv_2018)

################################# Checks ############################

#Check aj
table(dat$aj)
aj <- subset(dat, dat$aj == c(11, 36, 37, 38))

#Drop rows with aj 11, 36, 37, 38
dat_clean <- dat[ !(dat$aj %in% c(11, 36, 37, 38)),]
table(dat_clean$aj)


### Plot

ggplot(dat = dat_clean) +
  geom_point(mapping = aes(x = d1, y = htot, color = ba)) + 
  facet_wrap(~ aj, nrow = 2)
