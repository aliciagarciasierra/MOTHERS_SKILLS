#######################################################
##################   ROBUSTNESS ######################
######################################################


#############################################################
#######  FAMILY-RELATED TIME VARYING CONTROLS ##############
############################################################


#-------------  MOTHERS EXTRACURRICULAR TRAINING


model1<- plm(mathz~ Lag(avmathematicalskills) +  factor(training), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model1,  cluster="motherID")


model2<- plm(recoz~ Lag(avverbalskills)+  factor(training), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model2,  cluster="motherID")


#------- NEW RELATIONSHIP/MARITAL STATUS CHANGE

model3<- plm(mathz~ Lag(avmathematicalskills) + factor(marstatus) + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model3,  cluster="motherID")

model4<- plm(recoz~ Lag(avverbalskills) + factor(marstatus) + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model4,  cluster="motherID")


#-------- CHANGES IN FAMILY SIZE

model5<- plm(mathz~ Lag(avmathematicalskills) + famsize + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model5,  cluster="motherID")


model6<- plm(recoz~ Lag(avverbalskills) + famsize + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model6,  cluster="motherID")


#--------- NEWBORN AT HOME

model7<- plm(mathz~ Lag(avmathematicalskills) + twoyold + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model7,  cluster="motherID")


model8<- plm(recoz~ Lag(avverbalskills) + twoyold + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model8,  cluster="motherID")


#------------  CHANGES IN REGION


model9<- plm(mathz~ Lag(avmathematicalskills) + REGION + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model9,  cluster="motherID")


model10<- plm(recoz~ Lag(avverbalskills) + REGION + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model10,  cluster="motherID")



################################################################
######################## JOB-RELATED TIME VARYING CONTROLS
############################################################


#-------  WORKING HOURS

# Some clearning of the variable
data$workinghours[data$workinghours<=1 | data$workinghours>= 90]<-NA #remove outliers
table(data$workinghours)

# Models
model11<- plm(mathz~ Lag(avmathematicalskills) + workinghours + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model11,  cluster="motherID")


model12<- plm(recoz~ Lag(avverbalskills) + workinghours + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model12,  cluster="motherID")

#-------  WORKING HOURS of the PARTNER OF THE MOTHER

model13<- plm(mathz~ Lag(avmathematicalskills) +hoursworkpartner  + factor(findustry), 
               data = data,
               index = c("childID", "year"), 
               model = "within")
summary(model13,  cluster="motherID")


model14<- plm(recoz~ Lag(avverbalskills) + hoursworkpartner+ factor(findustry), 
               data = data,
               index = c("childID", "year"), 
               model = "within")
summary(model14,  cluster="motherID")

#-------  SUPERVISORY RESPONSIBILITIES

model15<- plm(mathz~ Lag(avmathematicalskills) + supervisory + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model15,  cluster="motherID")


model16<- plm(recoz~ Lag(avverbalskills) + supervisory + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model16,  cluster="motherID")


#-------  JOB QUALITY

model17<- plm(mathz~ Lag(avmathematicalskills) + jobquality + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model17,  cluster="motherID")


model18<- plm(recoz~ Lag(avverbalskills) + jobquality + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model18,  cluster="motherID")


#----------  FIRM SIZE

model19<- plm(mathz~ Lag(avmathematicalskills)+ size + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model19,  cluster="motherID")


model20<- plm(recoz~ Lag(avverbalskills) + size + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")

summary(model20,  cluster="motherID")


#---------- INCOME

model21<- plm(mathz~ Lag(avmathematicalskills)+ income + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")
summary(model21,  cluster="motherID")


model22<- plm(recoz~ Lag(avverbalskills) + income + factor(findustry), 
              data = data,
              index = c("childID", "year"), 
              model = "within")

summary(model22,  cluster="motherID")



##################################################################
#################### CHILD-RELATED CONTROLS #####################
#################################################################

#---------- AGE OF THE CHILDREN (in months)


# Create age groups for an easier interpretation
table(data$ageatin)
data$agegroups<-as.factor(ntile(data$ageatin, 5))
table(data$agegroups)

# Models
model23<- plm(mathz~ Lag(avmathematicalskills)+ ageatin + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model23,  cluster="motherID")


model24<- plm(recoz~ Lag(avverbalskills) +ageatin+ factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model24,  cluster="motherID")


#----------  HEALTH LIMITATION


model25<- plm(mathz~ Lag(avmathematicalskills) + healthlimit + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(mode25,  cluster="motherID")


model26<- plm(recoz~ Lag(avverbalskills) + healthlimit + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model26,  cluster="motherID")

#---------- COGNITIVE DELAYS

# Exclude those with cognitive delays

data$disab[is.na(data$disab)]<-0
table(data$disab)
datadisab <- subset(data, disab!=1)

# Models 

model27<- plm(mathz~ Lag(avmathematicalskills) + factor(findustry), 
             data = datadisab,
             index = c("childID", "year"), 
             model = "within")
summary(model27,  cluster="motherID")


model28<- plm(recoz~ Lag(avverbalskills) +factor(findustry), 
             data =datadisab,
             index = c("childID", "year"), 
             model = "within")
summary(model28,  cluster="motherID")


#---------- CHILD-CARE ARRANGEMENTS


model29<- plm(mathz~ Lag(avmathematicalskills) + childcare + factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model29,  cluster="motherID")


model30<- plm(recoz~ Lag(avverbalskills) + childcare+ factor(findustry), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model30,  cluster="motherID")



##################################################################
################# STRATIFY CHILD'S GENDER #####################
#################################################################

#---------- CHILD'S GENDER.

# Split the sample by child's gender
split<-split(data, f=data$cmale)
boys<-split$`1`
girls<-split$`0`

# Models for boys
model31<- plm(mathz~ Lag(avmathematicalskills) + factor(findustry), 
              data =boys,
              index = c("childID", "year"), 
              model = "within")
summary(model31, cluster="motherID")

model32<- plm(recoz~ Lag(avverbalskills) + factor(findustry), 
              data = boys,
              index = c("childID", "year"), 
              model = "within")
summary(model32, cluster="motherID")

# Models for girls
model33<- plm(mathz~ Lag(avmathematicalskills) + factor(findustry), 
              data =girls,
              index = c("childID", "year"), 
              model = "within")
summary(model33, cluster="motherID")

model34<- plm(recoz~ Lag(avverbalskills) + factor(findustry), 
              data = girls,
              index = c("childID", "year"), 
              model = "within")
summary(model34, cluster="motherID")




##################################################################
################# CAUSALITY THREADS #############################
#################################################################

#-------  REVERSE CAUSALITY: Does changes in previous math predict changing occupation?

model35<- plm(avmathematicalskills~Lag(mathz) , 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model35,  cluster="motherID")

model36<- plm(avverbalskills~Lag(recoz) , 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model36,  cluster="motherID")

#------- REVERSE CAUSALITY: health limit, behavioral and learning disabilities impact on mothers' skills


#---- behavioral problems

model37<- plm(avmathematicalskills~Lag(dbehav), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(mode37,  cluster="motherID")

model38<- plm(avverbalskills~Lag(dbehav), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model38,  cluster="motherID")

#---- health limitations

model39<- plm(avmathematicalskills~Lag(healthlimit), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model39,  cluster="motherID")

model40<- plm(avverbalskills~Lag(healthlimit), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model40,  cluster="motherID")

#----- disability


model41<- plm(avmathematicalskills~Lag(disab), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model41,  cluster="motherID")

model42<- plm(avverbalskills~Lag(disab), 
             data = data,
             index = c("childID", "year"), 
             model = "within")
summary(model42,  cluster="motherID")

