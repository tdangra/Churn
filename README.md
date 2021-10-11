# Churn

library(readxl) 
library(corrplot)
library(ggplot2)
library(lattice)
library(caret)
library(plyr)
library(dplyr)
library(car)
library(pROC)
library(MASS) 

c744 <- read_excel("C:/Users/teh_a/Downloads/c744.xlsx")


# To explore the data set
str(c744)

# To check for missing values
sapply(c744, function(x) sum(is.na(x)))

# To drop the 11 missing values of TotalCharges
c744<-c744[complete.cases(c744),]



# To remove the extra space
c744 %>%
   mutate_all(trimws)

# To check for case errors and categorical levels
unique(c744$Gender)
unique(c744$SeniorCitizen)
unique(c744$Partner)
unique(c744$Dependents)
unique(c744$Tenure)
unique(c744$PhoneService)
unique(c744$MultipleLines)
unique(c744$InternetService)
unique(c744$DeviceProtection)
unique(c744$TechSupport)
unique(c744$StreamingMovies)
unique(c744$StreamingTV)
unique(c744$Contract)
unique(c744$PaperlessBilling)
unique(c744$PaymentMethod)
unique(c744$MonthlyCharges)
unique(c744$TotalCharges)
unique(c744$Churn)

# To change the sentence case of columns gender and tenure to align with other columns
names(c744)[names(c744)=="gender"]<-"Gender"
names(c744)[names(c744)=="tenure"]<-"Tenure"

# To recode SeniorCitizen column as Yes, No to be aligned with other variable
c744$SeniorCitizen <- ifelse(c744$SeniorCitizen == 0, "No", ifelse(c744$SeniorCitizen == 1, "Yes","Unknown"))
c744$SeniorCitizen <- as.factor(c744$SeniorCitizen)

# Exploring Numerical Variables
Corrc744<-cor(c744[sapply(c744, is.numeric)])
corrplot(Corrc744, method = "number", mar=c(1,1,1,1))


# Dropping Totalcharges because Totalcharges and Tenure are highly correlated and basically show the same things
dropT <- c("TotalCharges")
c744 <- c744[,!(names(c744) %in% dropT)]
rm(dropT)

# Verifying the collinearity again
Corrc744<-cor(c744[sapply(c744, is.numeric)])
corrplot(Corrc744, method = "number", mar=c(1,1,1,1))

#To recode Multiple Lines because "No" and "No phone service" means the same thing
c744$MultipleLines <- as.factor(mapvalues(c744$MultipleLines,from=c("No phone service"),to=c("No")))

# To recode "No internet service" and "No" in multiple variables
c744$OnlineSecurity <- as.factor(mapvalues(c744$OnlineSecurity,from=c("No internet service"),to=c("No")))
c744$OnlineBackup <- as.factor(mapvalues(c744$OnlineBackup,from=c("No internet service"),to=c("No")))
c744$DeviceProtection <- as.factor(mapvalues(c744$DeviceProtection,from=c("No internet service"),to=c("No")))
c744$TechSupport <- as.factor(mapvalues(c744$TechSupport,from=c("No internet service"),to=c("No")))
c744$StreamingTV <- as.factor(mapvalues(c744$StreamingTV,from=c("No internet service"),to=c("No")))
c744$StreamingMovies <- as.factor(mapvalues(c744$StreamingMovies,from=c("No internet service"),to=c("No")))

# To clean up the Tenure
min(c744$Tenure); max(c744$Tenure)
 
c744$Tenure <- ifelse(c744$Tenure < 12, "New", ifelse(c744$Tenure >12 & c744$Tenure > 36, "Moderatly Loyal","Loyal"))

c744$Tenure <- as.factor(c744$Tenure)

summary(c744$Tenure)
                     
# To clean up MonthlyCharges                                                                 
summary(c744$MonthlyCharges)

hist(c744$MonthlyCharges,prob=TRUE,breaks=10,main="Histogram for Monthly Charges",
    xlab="Monthly Charges",cex.axis=0.60, col=6, col.main=8, col.lab=4, font.axis=4, 
    font.lab=4,font.main=4)

c744$MonthlyCharges <- ifelse(c744$MonthlyCharges < 39.51, "Low", ifelse(c744$MonthlyCharges >39.51 & c744$MonthlyCharges > 70.35, "Medium","High"))

c744$MonthlyCharges <- as.factor(c744$MonthlyCharges)

summary(c744$MonthlyCharges)


 # Data split
 
set.seed(1)
 sample_size <- floor(0.7 * nrow(c744))
 separate <- sample(seq_len(nrow(c744)), size = sample_size)
 trainc744 <- c744[separate,]
 Validc744 <- c744[-separate,]
 
 # Explorarotry Analysis
 # Churn
 ggplot(trainc744, aes(x=Churn, fill=Churn)) + 
    ggtitle("Percentage of Churn") + xlab("Churn") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3)+ 
    ylab("Percentage") 
 
  trainc744 %>%
   group_by(Churn) %>%
   summarise(n=n())
  
 #Gender
  ggplot(trainc744, aes(x=Gender, fill=Gender)) + 
     ggtitle("Percentage of Gender")+ xlab("Gender") + 
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3)+ ylab("Percentage")
 
  ggplot(trainc744, aes(x=Gender, fill=Churn), position="dodge") + 
     ggtitle("Percentage of Churn accross Gender")+ xlab("Gender") + 
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position = "dodge")+ 
     ylab("Percentage") 
 
   trainc744 %>%
   group_by(Gender, Churn) %>%
   summarise(n=n())
  
  # SeniorCitizen
  ggplot(trainc744, aes(x=SeniorCitizen, fill=SeniorCitizen)) + 
     ggtitle("Percentage of SeniorCitizen") + xlab("SeniorCitizen") + 
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3)+ ylab("Percentage")
 
  ggplot(trainc744, aes(x=SeniorCitizen, fill=Churn)) + 
     ggtitle("Percentage of Churn Vs SeniorCitizen") + xlab("SeniorCitizen") + 
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge")+ 
     ylab("Percentage")
  
 trainc744 %>%
   group_by(SeniorCitizen, Churn) %>%
   summarise(n=n())

# Partner
  ggplot(trainc744, aes(x=Partner, fill=Partner)) + 
    ggtitle("Percentage of Partner") + xlab("Partner") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3)+ ylab("Percentage") 
 
 ggplot(trainc744, aes(x=Partner, fill=Churn)) + 
    ggtitle("Percentage of Churn Vs Partner") + xlab("Partner") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge")+
    ylab("Percentage") 
 
 trainc744 %>%
   group_by(Partner, Churn) %>%
   summarise(n=n())
 
#Dependents
 ggplot(trainc744, aes(x=Dependents, fill=Dependents)) + 
    ggtitle("Percentage of Dependents") + xlab("Dependents") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3)+ ylab("Percentage") 
 
 ggplot(trainc744, aes(x=Dependents, fill=Churn)) + 
    ggtitle("Percentage of Churn Vs Dependents") + xlab("Dependents") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge")+ 
    ylab("Percentage") 
 
 trainc744 %>%
   group_by(Dependents, Churn) %>%
   summarise(n=n())
 
 # Phoneservice
  ggplot(trainc744, aes(x=PhoneService, fill=PhoneService)) + 
    ggtitle("Percentage of PhoneService") + xlab("PhoneService") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3)+ ylab("Percentage") 
 
 ggplot(trainc744, aes(x=PhoneService, fill=Churn)) + 
    ggtitle("Percentage of Churn Vs PhoneService") + xlab("PhoneService") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge")+
    ylab("Percentage")

  trainc744%>%
   group_by(PhoneService, Churn) %>%
   summarise(n=n())
 
 # PaperlessBilling
 ggplot(trainc744, aes(x=PaperlessBilling, fill=PaperlessBilling)) +
    ggtitle("Percentage of PaperlessBilling") + xlab("PaperlessBilling") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 

  ggplot(trainc744, aes(x=PaperlessBilling, fill=Churn)) +
    ggtitle("Percentage of Churn Vs PaperlessBilling") + xlab("PaperlessBilling") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
    ylab("Percentage")
 
 trainc744 %>%
   group_by(PaperlessBilling, Churn) %>%
   summarise(n=n())

  #MultipleLines
  ggplot(trainc744, aes(x=MultipleLines, fill=MultipleLines)) +
    ggtitle("Percentage of Multiple Lines") + xlab("Multiple Lines") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
 ggplot(trainc744, aes(x=MultipleLines, fill=Churn)) +
    ggtitle("Percentage of Churn Vs Multiple Lines") + xlab("Multiple Lines") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
    ylab("Percentage") 
 
 trainc744 %>%
   group_by(MultipleLines, Churn) %>%
   summarise(n=n())
 
 #InternetService
  ggplot(trainc744, aes(x=InternetService, fill=InternetService)) +
    ggtitle("Percentage of InternetService") + xlab("InternetService") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
 ggplot(trainc744, aes(x=InternetService, fill=Churn)) +
    ggtitle("Percentage of Churn Vs InternetService") + xlab("InternetService") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
    ylab("Percentage")
 
 trainc744 %>%
   group_by(InternetService, Churn) %>%
   summarise(n=n())
 
 #OnlineSecurity
 ggplot(trainc744, aes(x=OnlineSecurity, fill=OnlineSecurity)) +
    ggtitle("Percentage of OnlineSecurity") + xlab("OnlineSecurity") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage")
 
 ggplot(trainc744, aes(x=OnlineSecurity, fill=Churn)) +
    ggtitle("Percentage of Churn Vs OnlineSecurity") + xlab("OnlineSecurity") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
    ylab("Percentage")
 
 trainc744 %>%
   group_by(OnlineSecurity, Churn) %>%
   summarise(n=n())
 
#OnlineBackup 
 ggplot(trainc744, aes(x=OnlineBackup, fill=OnlineBackup)) +
    ggtitle("Percentage of OnlineBackup") + xlab("OnlineBackup") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
 ggplot(trainc744, aes(x=OnlineBackup, fill=Churn)) +
    ggtitle("Percentage of Churn Vs OnlineBackup") + xlab("OnlineBackup") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
    ylab("Percentage")
 
 trainc744 %>%
   group_by(OnlineBackup, Churn) %>%
   summarise(n=n())
 
 #DeviceProtection
  ggplot(trainc744, aes(x=DeviceProtection, fill=DeviceProtection)) +
    ggtitle("Percentage of DeviceProtection") + xlab("DeviceProtection") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
 ggplot(trainc744, aes(x=DeviceProtection, fill=Churn)) +
    ggtitle("Percentage of Churn Vs DeviceProtection") + xlab("DeviceProtection") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
    ylab("Percentage") 
 
  trainc744 %>%
   group_by(DeviceProtection, Churn) %>%
   summarise(n=n())
 
 # TechSupport
  ggplot(trainc744, aes(x=TechSupport, fill=TechSupport)) +
     ggtitle("Percentage of TechSupport") + xlab("TechSupport") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
  ggplot(trainc744, aes(x=TechSupport, fill=Churn)) +
     ggtitle("Percentage of Churn Vs TechSupport") + xlab("TechSupport") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
     ylab("Percentage") 
  
  
  trainc744 %>%
   group_by(TechSupport, Churn) %>%
   summarise(n=n())
  
 #StreamingTV
  ggplot(trainc744, aes(x=StreamingTV, fill=StreamingTV)) +
     ggtitle("Percentage of StreamingTV") + xlab("StreamingTV") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
  
  ggplot(trainc744, aes(x=StreamingTV, fill=Churn)) +
     ggtitle("Percentage of Churn Vs StreamingTV") + xlab("StreamingTV") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
     ylab("Percentage") 
  
  trainc744 %>%
   group_by(StreamingTV, Churn) %>%
   summarise(n=n())
 
  #StreamingMovies
  ggplot(trainc744, aes(x=StreamingMovies, fill=StreamingMovies)) +
     ggtitle("Percentage of StreamingMovies") + xlab("StreamingMovies") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage")
  
  ggplot(trainc744, aes(x=StreamingMovies, fill=Churn)) +
     ggtitle("Percentage of Churn Vs StreamingMovies") + xlab("StreamingMovies") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
     ylab("Percentage")                                                                        
   
  trainc744 %>%
   group_by(StreamingMovies, Churn) %>%
   summarise(n=n())
 
  #Contract
  ggplot(trainc744, aes(x=Contract, fill=Contract)) +
     ggtitle("Percentage of Contract") + xlab("Contract") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
  
  ggplot(trainc744, aes(x=Contract, fill=Churn)) +
     ggtitle("Percentage of Churn Vs Contract") + xlab("Contract") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
     ylab("Percentage")
  
  trainc744%>%
   group_by(Contract, Churn) %>%
   summarise(n=n())
 
 #PaymentMethod
  ggplot(trainc744, aes(x=PaymentMethod, fill=PaymentMethod)) +
     ggtitle("Percentage of PaymentMethod") + xlab("PaymentMethod") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
  ggplot(trainc744, aes(x=PaymentMethod, fill=Churn)) +
     ggtitle("Percentage of Churn Vs PaymentMethod") + xlab("PaymentMethod") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
     ylab("Percentage") 
  
  trainc744 %>%
   group_by(PaymentMethod, Churn) %>%
   summarise(n=n())
 
  #MonthlyCharges
  ggplot(trainc744, aes(x=MonthlyCharges , fill=MonthlyCharges )) +
     ggtitle("Percentage of MonthlyCharges ") + xlab("MonthlyCharges ") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
  ggplot(trainc744, aes(x=MonthlyCharges , fill=Churn )) +
     ggtitle("Percentage of Churn Vs MonthlyCharges ") + xlab("MonthlyCharges ") +
     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
     ylab("Percentage")
  
  trainc744 %>%
   group_by(MonthlyCharges, Churn) %>%
   summarise(n=n())
 
 
  #Tenure
  ggplot(trainc744, aes(x=Tenure, fill=Tenure)) +
   ggtitle("Percentage of Tenure") + xlab("Tenure") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3) + ylab("Percentage") 
 
  ggplot(trainc744, aes(x=Tenure, fill=Churn)) +
   ggtitle("Percentage of Churn Vs Tenure") + xlab("Tenure") +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)), width=0.3, position="dodge") + 
   ylab("Percentage") 
 
  trainc744 %>%
    group_by(Tenure, Churn) %>%
    summarise(n=n())
 
 # Intercept Model
 InterceptModelc744 <- glm(as.factor(Churn) ~ 1, family = binomial(link = "logit"), 
                data = trainc744)
 print(summary(InterceptModelc744))
 
 # Full Model
 FullModelc744<- glm(as.factor(Churn)  ~ ., family = binomial(link = "logit"), data = trainc744) 
 print(summary(FullModelc744))
 
 # To check the VIF 
 vif(FullModelc744)
 
 # To drop MonthlyVharges due to high VIF
 dropM<-c("MonthlyCharges")
 trainc744<- trainc744[,!(names(trainc744) %in% dropM)]
 rm(dropM)
 # To create another model and to verufy VIF after dropping Monthly Charges
 FullModelc7442<- glm(as.factor(Churn)  ~ ., family = binomial(link = "logit"), data = trainc744) 
 print(summary(FullModelc7442))
 vif(FullModelc7442)
 
  # To create the AIC Model
 AICModelc744 <- stepAIC(FullModelc7442, confsetsize= 1)
 print(summary(AICModelc744))
 
 # To check the Accuracy and ROC
 #Drop the MonthlyCharges in validc744
 dropMV<-c("MonthlyCharges")
 Validc744<- Validc744[,!(names(Validc744) %in% dropMV)]
 rm(dropMV)
 
 Churnpredict1 <- predict(AICModelc744, newdata=Validc744, type = "response", positive = "No")
 
 roc.plot <- plot.roc(Validc744$Churn, Churnpredict1,
                      identity.col="black",
                      print.auc=TRUE, auc.polygon = TRUE,
                      main = "ROC Curve for Logistic Regression Churn", xlab = "Specificit(1-False Positive Rate)",
                      ylab= "True Positive Rate", col = 6, col.main=8, col.lab = 4, font.axis=4,
                      font.lab=4, font.main=4) 
Churnpredict1 <- predict(AICModelc744, newdata=Validc744, type = "response", positive = "No")
Validc744$Churn<- as.factor(Validc744$Churn)
Chrunpredict1 <- ifelse(Churnpredict1 >0.5, "Yes", "No")
Churnpredict1 <- as.factor(Churnpredict1)
confusionMatrix((Churnpredict1), Validc744$Churn)
