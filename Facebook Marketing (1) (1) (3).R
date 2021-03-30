#Group 15: Anisha Bhuiyan, Shermeen Khan, Sahil Rajapkar
#Assignment HW # 2

--------------------------------------
  
#Reading in the Facebook data through the CSV file.
csv_download_link = "http://guides.newman.baruch.cuny.edu/ld.php?content_id=39953204"

link_url = url(csv_download_link) 
fb_data = read.csv(link_url,sep=";")

#previewing the data 
head(fb_data)

#seeing the data structure
str(fb_data)

#examining the dimensions of the facebook data
dim(fb_data)

#We will be converting some variables to factor type
fb_data$Category = as.factor(fb_data$Category) #creating a categorical variable with defined values
fb_data$Post.Month = as.factor(fb_data$Post.Month) #creating a categorical variable with defined values
fb_data$Post.Weekday = as.factor(fb_data$Post.Weekday) #creating a categorical variable with defined values
fb_data$Post.Hour = as.factor(fb_data$Post.Hour) #creating a categorical variable with defined values
fb_data$Paid = as.factor(fb_data$Paid) #creating a categorical variable with defined values
fb_data$Type = as.factor(fb_data$Type) #creating a categorical variable with defined values

#seeing the reupdated data structure
str(fb_data)

#we are seeing the values within the Category variable  
levels(fb_data$Category)

#converting the characters to recode, we are renaming the categories 1 to 3 based on the necessary data post information
fb_data$Category = as.character(fb_data$Category)
fb_data[fb_data$Category=="1", "Category"] = "action"
fb_data[fb_data$Category=="2", "Category"] = "product"
fb_data[fb_data$Category=="3", "Category"] = "inspiration"

#converting data back to factor type 
fb_data$Category = as.factor(fb_data$Category)

#confirming levels was updated 
levels(fb_data$Category)

#we are doing the missing values information first because it results in an error in the paid and non-paid recoding
#Counts and removes the number of null values in the set
sum(is.na(fb_data))

fb_data = na.omit(fb_data)

#Confirms that there are 0 missing values
sum(is.na(fb_data))

# This step will be used to recode the Paid variable values of 0-1 to non-paid and paid and change the data type accordingly
levels(fb_data$Paid)
fb_data$Paid = as.character(fb_data$Paid)
fb_data[fb_data$Paid=="0", "Paid"] = "non_paid"
fb_data[fb_data$Paid=="1", "Paid"] = "paid"
fb_data$Paid = as.factor((fb_data$Paid))
levels(fb_data$Paid)

#Using descriptive statistics to see the 5- number summary of and the standard deviation
summary((fb_data$share))
sd(fb_data$share)

#Plotting a histogram to see the distribution of shares  
hist(fb_data$share)
hist(fb_data$share,xlab="Facebook Shares",ylab="# of shares",main="Histogram of Shares", breaks=100)

#Applying the xlim parameter to adjust the range and analyze the range of shares most posts fall under
hist(fb_data$share,xlab="Facebook Shares",ylab="# of shares",main="Histogram of Shares", breaks=100, xlim=c(0,200))

#Using descriptive statistics to examine the categorical variables to determine the trends in the data
categorical_variables = c("Category","Paid","Post.Month","Post.Weekday","Post.Hour")
summary(fb_data[,categorical_variables])

#Creating a bar plot for the monthly posting frequency, category of posts made, and number of paid posts 
plot(fb_data$Post.Month,main="Posts by Month",xlab="Month",ylab="# of posts",las=2)

plot(fb_data$Category,main="Category",xlab="Type",ylab="# of each category")

plot(fb_data$Paid,main="Paid vs Non-paid ads",xlab="Type",ylab="# of posts")

# Comparing the relationship between likes and shares with a scatterplot
x=fb_data$like 
y=fb_data$share 
plot(x,y,xlab="likes",ylab="shares",main="likes vs shares")
plot(x,y,xlab="likes",ylab="shares",main="likes vs shares", 
      xlim=c(0,1000), ylim=c(0,200))

#The correlation between likes and shares to measure the strength of the relationship
cor(fb_data$like, fb_data$share)

#Creating a box plot to provide insight on the relationship of shares received per month
x=fb_data$Post.Month
y=fb_data$share
plot(x,y,ylim=c(0,500),las=2,xlab="month",ylab="shares", main="Shares by Month")

#Comparing the paid vs non-paid information using x tabs and aggregate functions
xtabs(~fb_data$share + fb_data$Paid)
aggregate(share~Paid,fb_data, sum)

#Comparing the performance between paid and non-paid posts using boxplots
x=fb_data$Paid
y=fb_data$share
plot(x,y,main="Shares for Non-Paid vs Paid posts",xlab="Non-paid vs Paid",
     ylab="share",ylim=c(0,500))


#Creating 1 x 2 panel to compare shares on the non-paid and paid posts for each category. This will be used to provide insight about the current dataset.
par(mfrow=c(1,2))

x = fb_data[fb_data$Paid=="non_paid", "Category"]
y = fb_data[fb_data$Paid=="non_paid", "share"]
plot(x, y, las=2,ylab="share",ylim=c(0,500),main="Non-Paid")

x = fb_data[fb_data$Paid=="paid", "Category"]
y = fb_data[fb_data$Paid=="paid", "share"]
plot(x, y, las=2,ylab="share",ylim=c(0,500),main="Paid")




