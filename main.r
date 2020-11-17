#--------------------------------------
#Author: Lei Chen
#Date: November 17,2020
#Comp321 final
#--------------------------------------
#Import table
myData <- read_excel("/Users/leianna/Documents/321F20/final/catsvdogs.xlsx")

#--------------------------------------
#print list of headers
list_header<-names(myData)
for(val in 1:length(list_header)){
  print(list_header[val])
}

#--------------------------------------
#Find the min except location
a<-myData[myData$`Number of Households (in 1000)`==min(myData$`Number of Households (in 1000)`),]
b<-myData[myData$`Percentage of households with pets`==min(myData$`Percentage of households with pets`),]
c<-myData[myData$`Number of Pet Households (in 1000)`==min(myData$`Number of Pet Households (in 1000)`),]
d<-myData[myData$`Percentage of Dog Owners`==min(myData$`Percentage of Dog Owners`),]
e<-myData[myData$`Dog Owning Households (1000s)`==min(myData$`Dog Owning Households (1000s)`),]
f<-myData[myData$`Mean Number of Dogs per household`==min(myData$`Mean Number of Dogs per household`),]
g<-myData[myData$`Dog Population (in 1000)`==min(myData$`Dog Population (in 1000)`),]
h<-myData[myData$`Percentage of Cat Owners`==min(myData$`Percentage of Cat Owners`),]
i<-myData[myData$`Cat Owning Households`==min(myData$`Cat Owning Households`),]
j<-myData[myData$`Mean Number of Cats`==min(myData$`Mean Number of Cats`),]
k<-myData[myData$`Cat Population`==min(myData$`Cat Population`),]

#Merge all the table
minTable <- rbind(a,b,c,d,e,f,g,h,i,j,k) %>% distinct()

#--------------------------------------
#Find the max except location
a<-myData[myData$`Number of Households (in 1000)`==max(myData$`Number of Households (in 1000)`),]
b<-myData[myData$`Percentage of households with pets`==max(myData$`Percentage of households with pets`),]
c<-myData[myData$`Number of Pet Households (in 1000)`==max(myData$`Number of Pet Households (in 1000)`),]
d<-myData[myData$`Percentage of Dog Owners`==max(myData$`Percentage of Dog Owners`),]
e<-myData[myData$`Dog Owning Households (1000s)`==max(myData$`Dog Owning Households (1000s)`),]
f<-myData[myData$`Mean Number of Dogs per household`==min(myData$`Mean Number of Dogs per household`),]
g<-myData[myData$`Dog Population (in 1000)`==max(myData$`Dog Population (in 1000)`),]
h<-myData[myData$`Percentage of Cat Owners`==max(myData$`Percentage of Cat Owners`),]
i<-myData[myData$`Cat Owning Households`==max(myData$`Cat Owning Households`),]
j<-myData[myData$`Mean Number of Cats`==max(myData$`Mean Number of Cats`),]
k<-myData[myData$`Cat Population`==max(myData$`Cat Population`),]

#Merge all the table
maxTable <- rbind(a,b,c,d,e,f,g,h,i,j,k) %>% distinct()

