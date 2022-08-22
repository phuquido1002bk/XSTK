#link data: https://www.kaggle.com/datasets/braydenrogowski/league-of-legends-worlds-2021-playin-group-stats
#nhap du lieu
data1<-read.csv("ChampionLOLdata.csv")
#trich lay cac cot can thiet
new_DF1 <- subset(data1, select=c("Gold.Earned", "Kills", 
                                  "Assists", "Creep.Score",
                                  "Wards.Destroyed", 
                                  "Position"))
#lam sach du lieu
apply(is.na(new_DF1), 2, sum)
new_DF1 <- na.omit(new_DF1)
View(new_DF1)
#chuyen doi bien Position sang [1,2,3,4,5]
for (i in 1:nrow(new_DF1)) {
  if (new_DF1$Position[i] == "Jungle"){
    new_DF1$Position[i] = "1"
  }
  if (new_DF1$Position[i] == "Support"){
    new_DF1$Position[i] = "2"
  }
  if (new_DF1$Position[i] == "Top"){
    new_DF1$Position[i] = "3"
  }
  if (new_DF1$Position[i] == "Mid"){
    new_DF1$Position[i] = "4"
  }
  if (new_DF1$Position[i] == "Adc"){
    new_DF1$Position[i] = "5"
  }
}
new_DF1$Position = as.numeric(new_DF1$Position)
#thong ke mo ta
#1. cac bien lien tuc
thongke1<-as.data.frame (
  rbind(
    apply(new_DF1[1:5],MARGIN=2,mean),
    apply(new_DF1[1:5],MARGIN=2,median),
    apply(new_DF1[1:5],MARGIN=2,sd),
    apply(new_DF1[1:5],MARGIN=2,min),
    apply(new_DF1[1:5],MARGIN=2,max)
  ), 
  row.names=c("Trung binh","Trung vi","Do lech chuan","GTNN","GTLN")
)
View(thongke1)
#2. cac bien con lai
Pos_table <- table(new_DF1$Position, dnn = "Position")
View(Pos_table)

#3. do thi
hist(new_DF1$Gold.Earned, main="Histogram of Gold.Earned",xlab="Gold.Earned",
     ylab="Frequency",ylim=c(0,60), labels = T, col = "skyblue")
boxplot(Gold.Earned~Position, new_DF1, main="Boxplot of Gold.Earned for each category of Pos", xlab="Gold.Earned", ylab="Gold.Earned", col = "skyblue")
pairs (Gold.Earned~Kills, main ="Pairs of Gold.Earned for Kills", 
       new_DF1, col="dodgerblue")
pairs (Gold.Earned~Assists , main ="Pairs of Gold.Earned for Assists", 
       new_DF1, col="dodgerblue")
pairs (Gold.Earned~Wards.Destroyed, main ="Pairs of Gold.Earned for Wards.Destroyed", 
       new_DF1, col="dodgerblue")
pairs (Gold.Earned~Creep.Score, main ="Pairs of Gold.Earned for Creep.Score", 
       new_DF1, col="dodgerblue")
#phuong trinh hoi quy tuyen tinh
HoiQuy1<-lm(Gold.Earned~Assists+Kills+Creep.Score+Wards.Destroyed+Position,data=new_DF1)
summary(HoiQuy1)
HoiQuy2<-lm(Gold.Earned~Assists+Kills+Creep.Score+Wards.Destroyed,data=new_DF1)
summary(HoiQuy2)
#du bao chi so vang trong 1 tran thi dau cua gamer
#testcase du bao
x <-data.frame(Assists=3,Kills=7,Creep.Score=4,Wards.Destroyed =20)
predict(HoiQuy2,newdata=x, interval = "confidence")
# =>so vang 8326.331