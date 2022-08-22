#nhap du lieu
data <- read.csv("gia_nha.csv")
#trich lay cac cot can thiet
new_DF <- subset(data, select=c("price", "floors", "condition", "view", "sqft_living", "sqft_above", "sqft_basement"))
#lam sach du lieu
apply(is.na(new_DF), 2, sum)
new_DF <- na.omit(new_DF)
View(new_DF)

#doi cac bien ve log cua no, neu nha khong co tang ham -> sqft_basement = 0
new_DF$ price <- log(new_DF$ price )
new_DF$ sqft_living <- log(new_DF$ sqft_living)
new_DF$ sqft_above <- log(new_DF$ sqft_above)
new_DF$ sqft_basement <- log(new_DF$ sqft_basement)
for (i in 1:nrow(new_DF)){
  if (new_DF[i,7]==-Inf){
    new_DF[i,7]=0
  }
}
#thong ke mo ta
#1. cac bien lien tuc
thongke<-as.data.frame (
  rbind(
    apply(new_DF[c(1 ,5 ,6, 7)],MARGIN=2,mean),
    apply(new_DF[c(1 ,5 ,6, 7)],MARGIN=2,median),
    apply(new_DF[c(1 ,5 ,6, 7)],MARGIN=2,sd),
    apply(new_DF[c(1 ,5 ,6, 7)],MARGIN=2,min),
    apply(new_DF[c(1 ,5 ,6, 7)],MARGIN=2,max)
  ), 
  row.names=c("Trung binh","Trung vi","Do lech chuan","GTNN","GTLN")
)
#2. cac bien con lai
floors_table <- table(new_DF$floors, dnn = "floors")
View(floors_table)

condition_table <- table(new_DF$condition, dnn = "condition")
View(condition_table)

view_table <- table(new_DF$view, dnn = "view")
View(view_table)
#3. do thi
hist(new_DF$price, main="Histogram of price",xlab="price",ylab="Frequency",xlim=c(11,16), ylim=c(0,8000), labels = T, col = "skyblue")
boxplot(price~floors, new_DF, main="Boxplot of price for each category of floors", xlab="floors", ylab="price", col = "skyblue")
boxplot(price~condition, new_DF, main="Boxplot of price for each category of condition", xlab="condition", ylab="price", col = "skyblue")
boxplot(price~view, new_DF, main="Boxplot of price for each category of view", xlab="view", ylab="price", col = "skyblue")
pairs (price~sqft_living, main ="Pairs of price for sqft_living", new_DF, col="dodgerblue")
pairs (price~sqft_above, main ="Pairs of price for sqft_above", new_DF, col="dodgerblue")
pairs (price~sqft_basement, main ="Pairs of price for sqft_basement", new_DF, col="dodgerblue")
#phuong trinh hoi quy tuyen tinh

HoiQuy<-lm(price~floors+condition+view+sqft_living+sqft_above+sqft_basement,data=new_DF)
summary(HoiQuy)
#du bao gia nha
#testcase du bao
x <-data.frame(floors=2,condition=3,view=4,sqft_living=10,sqft_above=10,sqft_basement=10)
predict(HoiQuy,newdata=x)
# =>gia nha = 15.70491

plot(HoiQuy, which =1)
