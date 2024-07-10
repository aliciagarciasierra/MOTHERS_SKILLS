
#################################################
############## CLEANING THE DATA ################
#################################################

load("datareadytoclean.RData")


#################################################
#### MOTHERS OCCUPATION SPECIFIC SKILLS ########
#################################################

####### CONSTRUCT INDEX FOR MATHEMATICAL SKILLS ########


# Select variables of interest 
myvars <- c("Science_LV", "Mathematics_LV", 
            "Programming_LV", "Analysis_LV", 
            "Systems_Analysis_LV", 
            "Systems_Evaluation_LV",
            "Technology_Design_LV")
mathematical<-data[myvars]

# Calculate internal consistency
psych::alpha(mathematical[,c("Science_LV", "Mathematics_LV", 
                             "Programming_LV", "Analysis_LV", 
                             "Systems_Analysis_LV", 
                             "Systems_Evaluation_LV",
                             "Technology_Design_LV")])


#Given alpha of 0.9, the internal consistency of the composite seems very high 

# Compute average index
data$avmathematicalskills<-rowMeans(mathematical[,c("Science_LV", "Mathematics_LV", 
                                                    "Programming_LV", "Analysis_LV", 
                                                    "Systems_Analysis_LV", 
                                                    "Systems_Evaluation_LV",
                                                    "Technology_Design_LV")], na.rm=TRUE)


####### CONSTRUCT INDEX FOR READING SKILLS ########

# Select the variables
myvars <- c("Reading_Comprehension_LV", "Speaking_LV",
            "Writing_LV", "Active_Listening_LV")
verbal<-data[myvars]

# Compute alpha
psych::alpha(verbal[,c("Reading_Comprehension_LV", "Speaking_LV",
                       "Writing_LV", "Active_Listening_LV")])


#Given alpha of 0.98, we can claim that the internal consistency of the composite is very high. 

# Compute average index

data$avverbalskills<-rowMeans(verbal[,c("Reading_Comprehension_LV", "Speaking_LV",
                                        "Writing_LV", "Active_Listening_LV")], na.rm=TRUE)



#################################################
############## OUTCOME VARIABLES ##############
#################################################

#Center and scale variables

data$mathz<-scale(data$mathz, center=TRUE, scale=TRUE)
data$recoz<-scale(data$recoz, center=TRUE, scale=TRUE)


#################################################
##############  CONTROLS VARIABLES ##############
#################################################


# MOTHEREDUD #
table(data$motheredu)
data$motheredud<-data$motheredu
data$motheredud[data$motheredud==0 |data$motheredud==1 | data$motheredud==2]<-0 # Less than Tertiary
data$motheredud[data$motheredud>=3]<-1 # Tertiary Education
table(data$motheredud)

# RACE # 
data$black<-data$race
data$black[data$black==1 | data$black==3]<-0
data$black[data$black==2]<-1
table(data$black)

data$hisp<-data$race
data$hisp[data$hisp==2 | data$hisp==3]<-0
data$hisp[data$hisp==1]<-1
table(data$hisp)

data$white<-data$race
data$white[data$white==1 | data$white==2]<-0
data$white[data$white==3]<-1 
table(data$white)

# AGE OF THE MOTHER AT BIRTH OF THE CHILD #
table(data$birthage)
data$birthage[data$birthage<14]<-NA # exclude those who were mothers before age 14

# RESIDENCE #
table(data$residence) # takes value 1 if the child grew up with her mother
data$residence[data$residence!=1]<-0 # different residence than "in household with mother"

#BIRTH ORDER#
table(data$birthorder)

#DATE of BIRTH#
table(data$datebirth)

# AGE #
table(data$year)
table(data$datebirth)
data$cage<-data$datebirth-data$year
table(data$cage)   

# FAMILY SIZE #
data$famsize<-data$FAMSIZE
table(data$FAMSIZE)

# UNEMPLOYED COMPENSATION RECEIVED #
table(data$unemp)

# HOURS WORKED PARTNER #
table(data$hoursworkpartner)

# AGE YOUNGEST MEMBER IN HOUSEHOLD #
table(data$ageyoungest)

# MARITAL STATUS 
table(data$marstatus) # 0 never married, 1 married, 2 separated, 3 divorced, 5 remarried, 6 widowed
data$married<-NA
data$married[data$marstatus== 0 | data$marstatus== 2 | data$marstatus== 3 | data$marstatus== 5 | data$marstatus== 6] <- 0
data$married[data$marstatus== 1]<-1 # married vs. the rest
table(data$married)

# HEALTH LIMIT #
table(data$healthlimit)

# CHILD GENDER #
table(data$sex)
data$cmale<-data$sex
data$cmale[data$sex==2]<-0
table(data$cmale) # value 1 if child male

# NEWBORN # 
data$twoyold<-data$ageyoungest
data$twoyold[data$twoyold<=2]<-1
data$twoyold[data$twoyold>=3]<-0
table(data$twoyold) # value 1 if the youngest child is new born (younger than 2)


# INDUSTRY #

data <- data %>%
  mutate(findustry = case_when(
    industry >= 10 & industry <= 31 ~ 0,  # Agriculture
    industry >= 40 & industry <= 50 ~ 1,  # Mining
    industry >= 60 & industry <= 100 ~ 2,  # Construction
    industry >= 100 & industry <= 392 ~ 3,  # Manufacturing
    industry >= 400 & industry <= 472 ~ 4,  # Transportation
    industry >= 500 & industry <= 698 ~ 5,  # Trade
    industry >= 700 & industry <= 719 ~ 6,  # Finance, Insurance and real Estate
    industry >= 721 & industry <= 760 ~ 7,  # Business and repair services
    industry >= 761 & industry <= 792 ~ 8,  # Personal Services
    industry >= 800 & industry <= 802 ~ 9,  # Entertainment and recreation
    industry >= 812 & industry <= 892 ~ 10,  # Professional and related services
    industry >= 900 & industry <= 932 ~ 11,  # Public Administration
    industry >= 990 ~ NA_real_,  # Not reported
    TRUE ~ NA_real_  # Default case to handle any unexpected values
  ))

data$findustry <- ordered(data$findustry,
                          levels = c(0 , 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                          labels = c("Agriculture", "Mining", "Construction",
                                     "Manufacturing", "Transportation", "Trade",
                                     "Finance and isurance", "Buiseness", "Personal services",
                                     "Entertainment", "Professional services", "Public administration"))

table(data$findustry)

# BEHAVIOURAL PROBLEMS DICHOTOMOUS # 

data$dbehav<-data$behav
data$dbehav[data$dbehav<=585.7 ]<-0
data$dbehav[data$dbehav>=585.7 ]<-1 # takes value 1 if above the mean of behavioural problems 
table(data$dbehav)


#################################################
############## GENERAL STRUCTURE DATA ###########
#################################################

data <- data  %>%  # arrange order
  dplyr::rename(motherID=ID) %>%  
  arrange(motherID, childID, year) %>%  
  relocate(childID, year,.after=motherID) # Rearrange structure 


################################################################
####### CONSTRUCT THE TIME TO TREATMENT  ##################
################################################################

# To explore the mechanism, I look at the effect of mothers' skills
# variations on chidren's skills by time.
# First, I construct four different treatments: 
# downgrade in maths, downgrade in reading, updgrade in maths, upgrade in reading
# Second, I compute the time within each individual to this change or treatment.


############ DOWNGRADE MATHEMATICS ###########

# Create treatment
data<-data%>%
  group_by(childID)%>%
  filter(!is.na(avmathematicalskills))%>%
  mutate(mathdecrease=if_else(avmathematicalskills<lag(avmathematicalskills), "1", "0")) 

# Mark the year in which treatment occurs
data$yearmathdecrease <- as.numeric(ifelse(data$mathdecrease=="1", data$year, "NA"))
class(data$yearmathdecrease)

# Select the first year in which treatment occurs for each child
idlist<-unique(data$childID)
data$groupmathdecrease<-NA
ticker=0
for (i in (idlist)){
  ticker=ticker+1
  reldata<-data[which(data$childID==i),]
  min_value<-min(reldata$yearmathdecrease, na.rm=TRUE)
  data$groupmathdecrease<-ifelse(data$childID==i, min_value, data$groupmathdecrease)
  print(ticker)
}

# Compute the years to (before or after) the treatment for each observation within each individual
data<-data%>%
  group_by(childID)%>%
  dplyr::mutate(timetomathdecrease=as.numeric(year)-as.numeric(groupmathdecrease))


############ DOWNGRADE READING ###########

# Create treatment
data <- data %>%
  group_by(childID)%>%
  filter(!is.na(avverbalskills))%>%
  mutate(verbaldecrease=if_else(avverbalskills<lag(avverbalskills), "1", "0")) 

# Mark the year in which treatment occurs
data$yearverbaldecrease <- as.numeric(ifelse(data$verbaldecrease=="1", data$year, "NA"))
class(data$yearverbaldecrease)

# Select the first year in which treatment occurs for each child
idlist<-unique(data$childID)
data$groupverbaldecrease<-NA
ticker=0
for (i in (idlist)){
  ticker=ticker+1
  reldata<-data[which(data$childID==i),]
  min_value<-min(reldata$yearverbaldecrease, na.rm=TRUE)
  data$groupverbaldecrease<-ifelse(data$childID==i, min_value, data$groupverbaldecrease)
  print(ticker)
}

# Compute the years to (before or after) the treatment for each observation within each individual
data<-data%>%
  group_by(childID)%>%
  dplyr::mutate(timetoverbaldecrease=as.numeric(year)-as.numeric(groupverbaldecrease))


############ UPGRADE MATHEMATICS ###########

# Create treatment 
data<-data%>%
  group_by(childID)%>%
  filter(!is.na(avmathematicalskills))%>%
  mutate(mathincrease=if_else(avmathematicalskills>lag(avmathematicalskills), "1", "0")) 

# Mark the year in which treatment occurs
data$yearmathincrease <- as.numeric(ifelse(data$mathincrease=="1", data$year, "NA"))
class(data$yearmathincrease)

# Select the first year in which treatment occurs for each child
idlist<-unique(data$childID)
data$groupmathincrease<-NA
ticker=0
for (i in (idlist)){
  ticker=ticker+1
  reldata<-data[which(data$childID==i),]
  min_value<-min(reldata$yearmathincrease, na.rm=TRUE)
  data$groupmathincrease<-ifelse(data$childID==i, min_value, data$groupmathincrease)
  print(ticker)
}

# Compute the years to (before or after) the treatment for each observation within each individual
data<-data%>%
  group_by(childID)%>%
  dplyr::mutate(timetomathincrease=as.numeric(year)-as.numeric(groupmathincrease))


############ UPGRADE READING ###########

# Create treatment 
data <- data %>%
  group_by(childID)%>%
  filter(!is.na(avverbalskills))%>%
  mutate(verbalincrease=if_else(avverbalskills>lag(avverbalskills), "1", "0")) 

# Mark the year in which treatment occurs
data$yearverbalincrease <- as.numeric(ifelse(data$verbalincrease=="1", data$year, "NA"))
class(data$yearverbalincrease)

# Select the first year in which treatment occurs for each child
idlist<-unique(data$childID)
data$groupverbalincrease<-NA
ticker=0
for (i in (idlist)){
  ticker=ticker+1
  reldata<-data[which(data$childID==i),]
  min_value<-min(reldata$yearverbalincrease, na.rm=TRUE)
  data$groupverbalincrease<-ifelse(data$childID==i, min_value, data$groupverbalincrease)
  print(ticker)
}

# Compute the years to (before or after) the treatment for each observation within each individual
data<-data%>%
  group_by(childID)%>%
  dplyr::mutate(timetoverbalincrease=as.numeric(year)-as.numeric(groupverbalincrease))

#################################################
############  SAVE THE DATA ############ 
#################################################

save(data, file="dataanalysis.RData")
