getwd()
setwd("X:/SENCO GOLD CONSULTANCY SERVICE LIMITED")
getwd()
library(readxl)
#install.packages("readxl")
library(readxl)

sg_16_17=read_excel("Sales Data_2016_2020.xlsx",sheet="Sales 16-17")
sg_17_18=read_excel("Sales Data_2016_2020.xlsx",sheet="Sales 17-18")
sg_18_19=read_excel("Sales Data_2016_2020.xlsx",sheet="Sales 18-19")
sg_19_20=read_excel("Sales Data_2016_2020.xlsx",sheet="Sales 19-20")


View(sg_16_17)


gold_amt=subset(sg_16_17,ITEMTYPECODE=="GOLD"&BOMLINETYPE=="Material"&Amount>0,select=c(2,7,8,15))
gold_amt
k=cbind(table(gold_amt$ORNAMENTCATEGORYCODE))
median(k)
summary(k)
max(gold_amt$Amount)
min(gold_amt$Amount)


library(lubridate)


#Month
date=month(gold_amt$INVOICEDATE)
date
a=table(date)
gold_amt1=cbind(gold_amt,date)
gold_amt1
a=0
for(i in 1:12)
{
  a[i]=sum(gold_amt1$Amount[gold_amt1$date==i])
}
a
amount=c(a[4:12],a[1:3])
amount

month=c("Apr '16","May '16","Jun '16","Jul '16","Aug '16","Sep '16","Oct '16","Nov '16","Dec '16","Jan '17","Feb '17","Mar '17")
barplot(amount/(10^5),names.arg=month,col="#ebae34",border=0,ylab="Amount  (in lakhs)",xlab="Months",ylim=c(0,10000),font.lab=2,main="Total Sales Amount of Gold \n Economic Year: 2016 - '17")
table(gold_amt1$ORNAMENTCATEGORYCODE)


table(gold_amt1$ORNAMENTCATEGORYCODE)
a=names(table(gold_amt1$ORNAMENTCATEGORYCODE))
a[1]
sort(table(gold_amt1$ORNAMENTCATEGORYCODE),decreasing=T)



#DIAMOND BAR PLOT
dia_amt=subset(sg_16_17,(ITEMTYPECODE=="DIAMOND"|ITEMTYPECODE=="diamond")&BOMLINETYPE=="Material"&Amount>0,select=c(2,7,8,15))
dia_amt
sort(table(dia_amt$ORNAMENTCATEGORYCODE))
date=month(dia_amt$INVOICEDATE)
date
a=table(date)
a
dia_amt1=cbind(dia_amt,date)
dia_amt1
a=0
for(i in 1:12)
{
  a[i]=sum(dia_amt1$Amount[dia_amt1$date==i])
}
a
amount=c(a[4:12],a[1:3])
amount
month=c("Apr '16","May '16","Jun '16","Jul '16","Aug '16","Sep '16","Oct '16","Nov '16","Dec '16","Jan '17","Feb '17","Mar '17")
barplot(amount/(10^5),names.arg=month,col="#ebae34",border=0,ylab="Amount  (in lakhs)",xlab="Months",ylim=c(0,1000),font.lab=2,main="Total Sales Amount of Diamond \n Economic Year: 2016 - '17")



#BARPLOT SILVER
sil_amt=subset(sg_16_17,ITEMTYPECODE=="SILVER"&BOMLINETYPE=="Material"&Amount>0,select=c(2,7,8,15))
sil_amt
date=month(sil_amt$INVOICEDATE)
date
a=table(date)
sil_amt1=cbind(sil_amt,date)
sil_amt1
a=0
for(i in 1:12)
{
  a[i]=sum(sil_amt1$Amount[sil_amt1$date==i])
}
a
amount=c(a[4:12],a[1:3])
amount
month=c("Apr '16","May '16","Jun '16","Jul '16","Aug '16","Sep '16","Oct '16","Nov '16","Dec '16","Jan '17","Feb '17","Mar '17")
barplot(amount/(10^5),names.arg=month,col="#ebae34",border=0,ylab="Amount  (in lakhs)",xlab="Months",ylim=c(0,250),font.lab=2,main="Total Sales Amount of Silver \n Economic Year: 2016 - '17")
axis(side=2,at=c(0:250,50))


#BARPLOT PLATINUM
plat_amt=subset(sg_16_17,ITEMTYPECODE=="PLATINUM"&BOMLINETYPE=="Material"&Amount>0,select=c(2,7,8,15))
plat_amt
p=cbind(table(plat_amt$ORNAMENTCATEGORYCODE))
sort(p)
date=month(plat_amt$INVOICEDATE)
date
a=table(date)
plat_amt1=cbind(plat_amt,date)
plat_amt1
a=0
for(i in 1:12)
{
  a[i]=sum(plat_amt1$Amount[plat_amt1$date==i])
}
a
amount=c(a[4:12],a[1:3])
amount
month=c("Apr '16","May '16","Jun '16","Jul '16","Aug '16","Sep '16","Oct '16","Nov '16","Dec '16","Jan '17","Feb '17","Mar '17")
barplot(amount/(10^5),names.arg=month,col="#ebae34",border=0,ylim=c(0,200),ylab="Amount  (in lakhs)",xlab="Months",font.lab=2,main="Total Sales Amount of Platinum  \n Economic Year: 2016 - '17")
