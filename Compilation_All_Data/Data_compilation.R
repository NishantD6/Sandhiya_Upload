library(lubridate)
library(vars)
library(dplyr)
sales <- read.csv("/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/IHS_Car Sales/Sales_Data_2015-2020.csv")
complaints <- read.csv("/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/Complaints Data/Complaints.csv")
perception <- read.csv('/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/YouGov Data/Index_Buzz.csv')
adspend <- read.csv("/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/AdSpend/AdSpend.csv")



sales$Make <- recode(sales$Make, 
                     'Mercedes-Benz' =  'mercedes')


complaints$Make <- recode(complaints$Make, 
                     'Mercedes-Benz' =  'mercedes')



sales$Date = as.Date(sales$Date)

sales <-sales%>%
  select(Make= Make, Date, Sales, Price_Avg)%>%
  mutate(month=month(Date), Year=year(Date), Make=tolower(Make))

complaints<-complaints%>%
  mutate(Make=tolower(Make))

new_df <- complaints%>%
  right_join(sales, by=c("Make","Year", "month"))


glimpse(new_df)

glimpse(perception)

c <- perception[, c(2,5, 6, 7, 8, 9, 10, 11, 12, 13, 14,15)]
c<- c%>%na.omit()
glimpse(c)

res<- cor(c)
d < -round(cor(c), 2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 70)

View(perception)

perception$date = as.Date(perception$date, format = "%d/%m/%Y" )

perception <- perception%>%
  select(brand.name, date, percep, buzz_negative, buzz_positive)%>%
  filter(date>"2015-01-01", date<'2020-01-01')%>%
  mutate(month=month(date), Year=year(date), Make=tolower(brand.name))%>%
  group_by(Make, month, Year)%>%
  summarise(avg_percep_score=mean(percep), avg_pos_buzz=mean(buzz_positive), avg_neg_buzz=mean(buzz_negative))

df <- new_df%>%
  right_join(perception, by=c("Make","Year", "month"))

df$Date <- as.Date(df$Date)
df$Make <- as.factor(df$Make)
adspend$Date <- as.Date(adspend$Date)

df1 <- df%>%
  left_join(adspend, by=c("Make","Date"))

df2 <- df1%>%
  filter(!Make %in% c('rivian', 'jeep', 'mini', 'land rover','general motors', 'mercury', 'ram', 'genesis'))%>%
  select(-LocalDiscountedAd, -X.x, -X.y)


df2$LocalAd[is.na(df2$LocalAd)] <- 0

glimpse(df2)
View(df2)

df3<- df2%>%
  #filter(month=='1', Year == '2015')%>%
  #na.omit()%>%
  #filter(Sales!=0)%>%
  group_by(Make, Date)%>%
  mutate(ComplaintsToSales = num_complaints/Sales)%>%
  ungroup()%>%
  group_by(Date)%>%
  mutate(totalComplaintsToSales = sum(ComplaintsToSales), CompCont = ComplaintsToSales/totalComplaintsToSales)%>%
  ungroup()

df4 <- df3%>%
  select(make='Make', date='Date', sales = "Sales", percep="avg_percep_score", ad="LocalAd", nbuzz='avg_neg_buzz', pbuz='avg_pos_buzz', compcont=CompCont)
write.csv(df4, '/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/VAR_data.csv')

glimpse(df4)

View(df3%>%
  group_by(Make)%>%
  summarise(percep=mean(avg_index_score))%>%
  arrange(desc(percep)))

write.dta(df3,"/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/VARData.dta" )

df3%>%
  select(CompCont, Sales, Date, Make)%>%
  ggplot(aes(x=Date, y=Sales))+
  geom_line()+
  facet_wrap('Make')

df3%>%
  select(CompCont, Sales, Date, Make)%>%
  ggplot(aes(x=Date, y=CompCont))+
  geom_line()+
  facet_wrap('Make')


df3%>%
  filter(LocalAd>140000000)
  
#####################################



summary(lm(formula= Sales~num_complaints+ Price_Avg+ avg_index_score+as.factor(Make)+Date+LocalAd+ CompCont, data=df3))

vdf <-df3%>%filter(Make=='acura')%>%select(Sales, Price_Avg, avg_index_score, LocalAd, CompCont)%>%na.omit()

var2 <- VAR(vdf, type='const', lag.max=2, ic="AIC")

a <- irf(var2, impluse='CompCont', response='Sales')
plot(a)

# correct dataset
cdf<-df%>%
  na.omit()%>%
  #filter(Make=="BMW" |Make=="Daimler", Year=="2015", month=="1"|month=="2")%>%
  #filter(Year=="2016", month=="1")%>%
  filter(Sales!=0, num_complaints!=0)%>%
  group_by(Date)%>%
  mutate(BCbyBS=num_complaints/Sales)%>%
  ungroup()%>%
  group_by(Year, month)%>%
  mutate(sBCbyBS=sum(BCbyBS),MSCC=BCbyBS*100/sBCbyBS)%>%
  #filter(Make =="Aston Martin Finance Gr.")%>%
  select(MSCC)%>%
  ndiffs(MSCC)
#filter(Year=="2018", month=="2")

cdf%>%
  ggplot(aes(x=Date, y= MSCC))+
  geom_line(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Sales adjusted Monthly Market Share of Complaints 2015 to 2020")

glimpse(cdf)

cdf%>%
  ungroup()%>%
  group_by(Year, month)%>%
  mutate(total_sales=sum(numSold), total_complaints=sum(num_complaints), complaintShare=100*num_complaints/total_complaints)%>%
  ungroup()%>%
  #filter(Year=="2016", month=="1")%>%
  ggplot(aes(x=Date, y= complaintShare))+
  geom_line(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Monthly Market Share of Complaints 2015 to 2020")

cdf%>%
  ungroup()%>%
  #group_by(Year, month)%>%
  #mutate(total_sales=sum(numSold), total_complaints=sum(num_complaints), complaintShare=100*num_complaints/total_complaints)%>%
  ungroup()%>%
  filter(Year=="2016", Make=="Toyota Group")%>%
  ggplot(aes(x=Date, y= num_complaints))+
  geom_line(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Total Complaints per day 2015 to 2020")


new_dataset1%>%
  ggplot(aes(x=Date, y= num_complaints))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Cars MAKE SOLD per Month from 2015 to 2020")

View(new_dataset1)
new_dataset1%>%
  filter(Make!="Suzuki")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  group_by(Make)%>%
  summarise(CSRatioMean=mean(CSRatio))

new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  ggplot(aes(x=Date, y= CSRatio))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Complaints to Cars Sold per Month from 2015 to 2020")

new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  group_by(month, Year)%>%
  mutate(totalCSRatio=sum(CSRatio))%>%
  ungroup()%>%
  mutate(complaintShare=CSRatio/totalCSRatio*100)%>%
  #filter( month==1, Year==2018)%>%
  #mutate(TotalMonthlyComplaints=sum(num_complaints), TotalMonthlySales=sum(numSold), ComplaintShare=CSRatio/(TotalMonthlyComplaints/TotalMonthlySales))%>%
  ggplot(aes(x=Date, y= complaintShare))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Sales Adjusted Share of Complaints for Cars Sold per Month from 2015 to 2020")


new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  group_by(month, Year)%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  #mutate(totalComplaints=sum(num_complaints), totalSold=sum(numSold))%>%
  ungroup()%>%
  group_by(Make, month, Year)%>%
  #mutate(complaints = sum(num_complaints), complaintShare=complaints/totalComplaints, salesProportion=numSold/totalSold, adjComplaintShare=complaintShare*salesProportion*10000)%>%
  #filter( month==1, Year==2018)%>%
  mutate(TotalMonthlyComplaints=sum(num_complaints), TotalMonthlySales=sum(numSold), ComplaintShare=CSRatio/(TotalMonthlyComplaints/TotalMonthlySales))%>%
  ggplot(aes(x=Date, y= ComplaintShare))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Share of Complaints for Cars Sold per Month from 2015 to 2020")

# Complaints Market Share = Number of Complaints/Total Number of Complaints in that month
new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  #mutate(CSRatio = num_complaints/numSold*100)%>%
  group_by(month, Year)%>%
  mutate(totalComplaints=sum(num_complaints), totalSold=sum(numSold))%>%
  ungroup()%>%
  group_by(Make, month, Year)%>%
  mutate(complaints = sum(num_complaints), complaintShare=complaints/totalComplaints, salesProportion=numSold/totalSold, adjComplaintShare=complaintShare*salesProportion*10000)%>%
  #filter( month==1, Year==2018)%>%
  #mutate(TotalMonthlyComplaints=sum(num_complaints), TotalMonthlySales=sum(numSold), ComplaintShare=CSRatio/(TotalMonthlyComplaints/TotalMonthlySales))%>%
  ggplot(aes(x=Date, y= complaintShare))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Share of Complaints for Cars Sold per Month from 2015 to 2020")


df2%>%
  group_by(MakeGroup, Date)%>%
  filter( Date<"2020-01-01")%>%
  summarise(N_sold_per_Brand_Month=sum(NumSold))%>%
  ggplot(aes(x=Date, y=N_sold_per_Brand_Month))+
  geom_line(se = F)+
  facet_wrap(~MakeGroup)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Cars MAKE SOLD per Month from 2015 to 2020")
