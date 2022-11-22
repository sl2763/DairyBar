#combining the sales data from each week
library(dplyr)

##setwd
setwd("Z:/sl2763_Samantha Lau/Projects/2022 Cornell Dairy Bar Study/QRcodeData")

##read in each week
QRCodeData<-read.csv("FullQRCodeData.csv")

#remove Sam and Eric's cookies for when they were playing around with testing the QR codes
#Erics are iTlyEaZMgLBP0o_XfLVu9lU_F9S1a4Nu and KI91KEqZ5T-jquwsj3wuem7Rc7Tfil9r
#Erics IP address is 69.203.92.62
#Sams is 1iSc7YNAcJgpAtf1mbekIOBS3LvcqUUZ

#check if Transaction is in the first column string
#remove row if it is
QRCodeData2<- QRCodeData[!grepl("iTlyEaZMgLBP0o_XfLVu9lU_F9S1a4Nu", QRCodeData$cookie),]
QRCodeData2<- QRCodeData2[!grepl("KI91KEqZ5T-jquwsj3wuem7Rc7Tfil9r", QRCodeData2$cookie),]
QRCodeData2<- QRCodeData2[!grepl("1iSc7YNAcJgpAtf1mbekIOBS3LvcqUUZ", QRCodeData2$cookie),]
QRCodeData2<- QRCodeData2[!grepl("69.203.92.62", QRCodeData2$x.forwarded.for),]


##keep columns 1-6
QRCodeData3 <- QRCodeData2[c(1:6)]

#split created at column into 2
library(stringr)
QRCodeData3 <- data.frame(QRCodeData3,do.call(rbind,str_split(QRCodeData3$createdAt," ")))

QRCodeData4 <- QRCodeData3[c(1:5,7:8)]

#make column with Week Number
QRCodeData4$Week[QRCodeData4$X1 == "2/7/2022"] <- "2" 
QRCodeData4$Week[QRCodeData4$X1 == "2/8/2022"] <- "2" 
QRCodeData4$Week[QRCodeData4$X1 == "2/10/2022"] <- "2" 
#2/14-2/18 = WEEK3
QRCodeData4$Week[QRCodeData4$X1 == "2/14/2022"] <- "3" 
QRCodeData4$Week[QRCodeData4$X1 == "2/15/2022"] <- "3" 
QRCodeData4$Week[QRCodeData4$X1 == "2/16/2022"] <- "3" 
QRCodeData4$Week[QRCodeData4$X1 == "2/17/2022"] <- "3" 
QRCodeData4$Week[QRCodeData4$X1 == "2/18/2022"] <- "3" 
#2/21-2/25 = WEEK4
QRCodeData4$Week[QRCodeData4$X1 == "2/21/2022"] <- "4" 
QRCodeData4$Week[QRCodeData4$X1 == "2/22/2022"] <- "4" 
QRCodeData4$Week[QRCodeData4$X1 == "2/23/2022"] <- "4" 
QRCodeData4$Week[QRCodeData4$X1 == "2/24/2022"] <- "4" 
QRCodeData4$Week[QRCodeData4$X1 == "2/25/2022"] <- "4" 
#2/28-3/4 = WEEK5
QRCodeData4$Week[QRCodeData4$X1 == "2/28/2022"] <- "5" 
QRCodeData4$Week[QRCodeData4$X1 == "3/1/2022"] <- "5" 
QRCodeData4$Week[QRCodeData4$X1 == "3/2/2022"] <- "5" 
QRCodeData4$Week[QRCodeData4$X1 == "3/3/2022"] <- "5" 
QRCodeData4$Week[QRCodeData4$X1 == "3/4/2022"] <- "5" 
#3/7-3/11 = WEEK6
QRCodeData4$Week[QRCodeData4$X1 == "3/7/2022"] <- "6" 
QRCodeData4$Week[QRCodeData4$X1 == "3/8/2022"] <- "6" 
QRCodeData4$Week[QRCodeData4$X1 == "3/9/2022"] <- "6" 
QRCodeData4$Week[QRCodeData4$X1 == "3/10/2022"] <- "6" 
QRCodeData4$Week[QRCodeData4$X1 == "3/11/2022"] <- "6" 
#3/14-3/18 = WEEK7
QRCodeData4$Week[QRCodeData4$X1 == "3/14/2022"] <- "7" 
QRCodeData4$Week[QRCodeData4$X1 == "3/15/2022"] <- "7" 
QRCodeData4$Week[QRCodeData4$X1 == "3/16/2022"] <- "7" 
QRCodeData4$Week[QRCodeData4$X1 == "3/17/2022"] <- "7" 
QRCodeData4$Week[QRCodeData4$X1 == "3/18/2022"] <- "7" 
#3/21-3/25 = WEEK8
QRCodeData4$Week[QRCodeData4$X1 == "3/21/2022"] <- "8" 
QRCodeData4$Week[QRCodeData4$X1 == "3/22/2022"] <- "8" 
QRCodeData4$Week[QRCodeData4$X1 == "3/23/2022"] <- "8" 
QRCodeData4$Week[QRCodeData4$X1 == "3/24/2022"] <- "8" 
QRCodeData4$Week[QRCodeData4$X1 == "3/25/2022"] <- "8" 

#figure out how many milks were scanned each week
MilkScanned <- QRCodeData4 %>% 
  group_by(Week) %>% 
  summarise(sumMilkScanned = n())

#export into csv
write.csv(MilkScanned, "MilkScanned_040822.csv")



#best by date identifiers
BestByDate<-read.csv("BestbydateIdentifiers.csv")
#only want the first 12 rows
BestByDate2<-BestByDate[1:12, ]

#match columns and then print the other column into the QRCode dataframe
#rename column
names(BestByDate2)[1:2] <- c("BestByDate", "qr")

#something wrong happened here
QRCodeData5<- merge(QRCodeData4, BestByDate2, by="qr")


#rename columns
names(QRCodeData5)[6:7] <- c("PurchaseDate", "PurchaseTime")

#export into csv
write.csv(QRCodeData5, "QRCodeData5_041222.csv")

#figure out how many milks were scanned each week
MilkScanned2 <- QRCodeData5 %>% 
  group_by(Week) %>% 
  summarise(sumMilkScanned = n())

#export into csv
write.csv(MilkScanned2, "MilkScanned_041222.csv")


###### IGNORE EVERYTHING BELOW THIS
##combine all csv into one file
combineddata<- rbind(week1, week2, week3, week4, week5, week6, week7, week8)

##get rid of rows if they have an empty columns
#set empty rows to NA
combineddata[combineddata==""]<-NA
#get rid of all NAs
combineddata <- na.omit(combineddata)

##keep columns 1, 7, 9, 10
newcombined <- combineddata[c(4,10:12)]

#check if Transaction is in the first column string
#remove row if it is
newcombined$REMOVE<-grepl('Transaction', newcombined$X.2)

#if true, remove row
newcombined2<- newcombined[!grepl("TRUE", newcombined$REMOVE),]

#Organize columns
newcombined2<-newcombined2[c(1:4)]


#pull transaction times
library(stringr)
newcombined3 <- data.frame(newcombined2,do.call(rbind,str_split(newcombined2$X.2," ")))

newcombined4<-newcombined3[c(2,3,6,4)]
#rename columns
names(newcombined4)[1:4] <- c("Item", "DateSold", "TimeSold", "Week")

#make columns with the type of milk
newcombined4$MilkFat[newcombined4$Item == "CornelMilk1/2Gal"] <- "Whole" 
newcombined4$MilkFat[newcombined4$Item == "MlkWhlProjec3.19/ITH"] <- "Whole"   
newcombined4$MilkFat[newcombined4$Item == "MlkWhlDisc2.87/ITH"] <- "Whole"   
newcombined4$MilkFat[newcombined4$Item == "MlkWhlDisc2.55/ITH"] <- "Whole"   
newcombined4$MilkFat[newcombined4$Item == "MlkWhlDisc1.91/ITH"] <- "Whole"   
newcombined4$MilkFat[newcombined4$Item == "Corn2%Milk1/2Gal"] <- "ReducedFat" 
newcombined4$MilkFat[newcombined4$Item == "Mlk2%Project3.19/ITH"] <- "ReducedFat"  
newcombined4$MilkFat[newcombined4$Item == "Milk2%Disc2.87/ITH"] <- "ReducedFat" 
newcombined4$MilkFat[newcombined4$Item == "Milk2%Disc2.55/ITH"] <- "ReducedFat" 
newcombined4$MilkFat[newcombined4$Item == "Milk2%Disc1.99/ITH"] <- "ReducedFat"  
newcombined4$MilkFat[newcombined4$Item == "CornSkMilk1/2Gal"] <- "Skim" 
newcombined4$MilkFat[newcombined4$Item == "MlkSkmProjec3.19/ITH"] <- "Skim" 
newcombined4$MilkFat[newcombined4$Item == "MlkSkmDisc2.55/ITH"] <- "Skim" 
newcombined4$MilkFat[newcombined4$Item == "MlkSkmDisc2.87/ITH"] <- "Skim" 
newcombined4$MilkFat[newcombined4$Item == "MlkSkmDisc1.91/ITH"] <- "Skim" 

#create a column with the price
newcombined4$Price <- ifelse(grepl("3.19", newcombined4$Item), "3.19",
                             ifelse(grepl("2.87", newcombined4$Item), "2.87", 
                                    ifelse(grepl("2.55", newcombined4$Item),"2.55",
                                           ifelse(grepl("1.91", newcombined4$Item), "1.91",
                                                  ifelse(grepl("1.99", newcombined4$Item), "1.91","3.19")))))

#create a month column
#if weeks 1-4, month 1
#if weeks 5-8, month 2
newcombined4$Month <- ifelse(grepl("1", newcombined4$Week), "1",
                             ifelse(grepl("2", newcombined4$Week), "1", 
                                    ifelse(grepl("3", newcombined4$Week), "1",
                                           ifelse(grepl("4", newcombined4$Week), "1", "2"))))
                            


write.csv(newcombined4, "TimestampSalesData040522.csv")



