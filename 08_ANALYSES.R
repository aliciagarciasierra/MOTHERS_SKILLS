#################################################
################ ANALYSES #######################
#################################################


#################################################
############## OLS MODELS #######################
#################################################

model1<- lm(mathz~ Lag(avmathematicalskills) + motheredud + birthage + residence 
            + factor(race) +famsize + factor(cmale) + factor(findustry),data)
summary(model1, cluster="motherID")

model2<- lm(recoz~ Lag(avverbalskills)+ motheredud + birthage + residence 
            + factor(race) +famsize + factor(cmale) + factor(findustry),data)
summary(model2, cluster="motherID")



#####################################################
############## TWFE MODELS #########################
#####################################################

model3<- plm(mathz~ Lag(avmathematicalskills) + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model3, cluster="motherID")

model4<- plm(recoz~ Lag(avverbalskills)+ factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model4, cluster="motherID")


################################################################
##################### FE ADJUSTED OTHER SKILLS #################
################################################################


model5<- plm(mathz~Lag(avmathematicalskills) + avverbalskills + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model5,  cluster="motherID")


model6<- plm(recoz~ Lag(avverbalskills) + avmathematicalskills + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model6,  cluster="motherID")



################################################################
##################### IPW TWFE MODELS ##########################  
################################################################

# Select variables of interest 

myvars<-c("childID","motherID", "year", "occ")
ipw<-data[myvars]

# Create treatment variable 

ipw<- ipw %>% 
  filter(!is.na(occ)) %>%
  group_by(motherID, childID)%>%
  arrange(motherID,childID, year) %>%   
  mutate(dummy = if_else(occ!=Lag(occ), "1", "0"))

# Merge with existent database 

data_ipw<-merge(data, ipw, by=c("motherID","childID", "year"),all=TRUE)


# Get the weights using the confounders

data_ipw<-data_ipw[complete.cases(data_ipw[,c("dummy")]),]

# Using cross-sectional confounders
weights_weightit_cross<-weightit(dummy ~ motheredud + factor(race) + 
                                   + datebirth + famsize + findustry+
                                   ageyoungest + marstatus + healthlimit + stress+ factor(cmale),
                                 data=data_ipw)

# Using ongitudinal confounders
weights_weightit_long<-weightit(dummy ~ motheredud + factor(race) + 
                                  + datebirth + famsize + 
                                  ageyoungest + marstatus + healthlimit + factor(cmale)+
                                  income + workinghours+ hoursworkpartner + 
                                  stress + jobquality +  reasontoleave + flexible,
                                data=data_ipw)

# Check 
head(weights_weightit_cross$weights)
head(weights_weightit_long$weights)

# Merge the weights to the existent data (both for cross-sectional and longitudinal)
data_weights_cross <- data_ipw %>% 
  mutate(ipw = weights_weightit_cross$weights)

data_weights_long <- data_ipw %>% 
  mutate(ipw = weights_weightit_long$weights)


# Models with cross-sectional and longitudinal weights

model7<- plm(mathz~ Lag(avmathematicalskills) + factor(findustry),
             data=data_weights_cross, # cross-sectional weights
             weights=ipw,
             index = c("childID", "year"), 
             model = "within")
summary(model7,  cluster="motherID")

model8<- plm(mathz~ Lag(avmathematicalskills)  + factor(findustry),
             data=data_weights_long, # longitudinal weights
             weights=ipw,
             index = c("childID", "year"), 
             model = "within")
summary(model8, cluster="motherID")

model9<- plm(recoz~ Lag(avverbalskills) + factor(findustry),
             data=data_weights_cross, # cross-sectional weights
             weights=ipw,
             index = c("childID", "year"), 
             model = "within")
summary(model9,  cluster="motherID")

model10<- plm(recoz~ Lag(avverbalskills)  + factor(findustry),
              data=data_weights_long, # longitudinal weights
              weights=ipw,
              index = c("childID", "year"), 
              model = "within")
summary(model10,  cluster="motherID")



############################################################
##################### ASYMETRIC MODELS #####################
############################################################

#Establish a panel structure
panel_data <- panel_data(data, id = childID, wave = year)

#Get rid of missing cases (important for first differences)
panel_data<-panel_data %>%
  filter(!is.na(avmathematicalskills))%>%
  filter(!is.na(avverbalskills))

# Models

m11 <- asym(mathz~ Lag(avmathematicalskills), data = panel_data)
summary(m11, cluster="motherID")

m12 <- asym(recoz~ Lag(avverbalskills), data = panel_data)
summary(m12,  cluster="motherID")


############################################################
##################### TIME TO TREATMENT MODELS ################
############################################################

# First, contruct a variable that captures change in occupations

data<-data%>%
  filter(!is.na(occ))%>%
  arrange(childID, year)%>%
  group_by(childID)%>%
  mutate(diff=c(NA, diff(occ))) %>%
  filter(!is.na(diff))

# If the difference between two occupations is different from 0, that means there has been a change, so I assign value 1

data$change<-data$diff
data$change[data$change!=0]<-1
table(data$change)

# Year of the treatment

data$yearevent <- ifelse(data$change == 1, data$year , "NO")

# Time treated (first time)

data <- data %>%
  dplyr::group_by(childID)%>%
  dplyr::mutate(last_treated = min(ifelse(change== "1", yearevent, "NA"),
                                   na.rm = TRUE))

# Time to treatment
data$timeaftertreat<-as.numeric(data$year)-as.numeric(data$last_treated)

# Reduce data to after treatment to get only those who have been in the same occupation for a while
filtered <- data  %>% 
  filter(timeaftertreat>= 0 & timeaftertreat<= 14) 

# Check the new variables
myvars<-c("occ", "year", "childID", "diff", "change", "yearevent", "last_treated", "timeaftertreat")
check<-filtered[myvars]


# Models

model13<- feols(mathz~ i(timeaftertreat, Lag(avmathematicalskills), 0) | childID + year, filtered)
summary(model13,  cluster="motherID")

model14<- feols(recoz~ i(timeaftertreat, Lag(avverbalskills), 0 ) | childID + year, filtered)
summary(model14,  cluster="motherID")


############################################################
##################### SES STRATIFIED MODELS ################
############################################################

# Split the sample by level of education of the mother

split<-split(data, f=data$motheredud)
tert<-split$`1` # tertiary educated
nontert<-split$`0` # non-tertiary educated

# Models

model15<- feols(mathz~  Lag(avmathematicalskills)| childID+ year, data = tert)
etable(model15,  cluster="motherID")

model16<- feols(mathz~ Lag(avmathematicalskills) | childID+ year, data = nontert)
etable(model16,  cluster="motherID")

model17<- feols(recoz~  Lag(avverbalskills) | childID+ year, data = tert)
etable(model17,  cluster="motherID")

model18<- feols(recoz~ Lag(avverbalskills)  | childID+ year, data = nontert)
etable(model18,  cluster="motherID")


# Interaction with the industry (i.e., job sector) of the mother

model19<- feols(mathz~ i(findustry, Lag(avmathematicalskills)) | childID + year, data = data)
etable(model19,  cluster="motherID")

model20<- feols(recoz~ i(findustry, Lag(avverbalskills)) | childID + year, data = data)
etable(model20,  cluster="motherID")



