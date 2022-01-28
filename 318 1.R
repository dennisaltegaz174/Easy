x<-c(1,-1,3.5,2)
x+2
x^2
sum((x-mean(x)))^2
seq(1:10)
seq(1,10)
seq(from=2,to=6,by=0.4)
seq(10,0)
seq(10,0,by=5)
seq
seq(from=-1,to=1,length=6)
seq(from=-1,to=1,length.out=6)
seq(50,100,5)
seq(100,50,by=5)
rep(5,3)
rep(2:5,each=3)
rep(-1:3,length.out=10)
2^(0:10)
rep(seq(from=0,by=10,to=30),each=3)
1:10*c(-1,1)
matrix(1:12,nrow = 3,ncol = 4)
matrix(1:12,nrow = 3)
matrix(1:12,2)
matrix(1:12,3,byrow = T)
diag(3)
diag(4)
diag(1:4)
1:5 %0% 1:5
outer(1:3,1:4,"+")
outer(1:10,1:10,"+")
a<-matrix(c(1:8,10),3,3)
a
b<-c(1,2,3)
a*b
a%*%b
t(a)
det(a)
diag(a)
solve(a)
a[2,1]
a[,2]
a[,1:2]
a[2,2:ncol(a)]
a[c(),1:12]
k<-matrix(c(4,5,3,3,2,0,6,5,4),3,byrow = TRUE)
k
p<-c(1,4,3)
library(matlib)
showEqn(k,p)
c(R(k),R(cbind(k,p)))
solve(k,p,fractions=T)
cbind(k,t(k))
rbind(k,1,0)
cols<-c("blue","blue","red","red","yellow","green")
summary(cols)
col<-factor(cols)
summary(col)
str(col)
table(cols)
barplot(table(cols))
pie(table(cols))
pie(table(cols),col=c("blue","green","red","yellow"))
data<-data.frame(x1=c(1,2,2,3),
                 x2=c(4,5,6,7),
                 x3=c(8,9,10,11))
#write data as text file ro directory
write.table(data,file = "data.txt",row.names = FALSE)
#read txt file into list
data<-scan("data.txt",what=list("","",""))
data
str(data)
summary(data)
aa<-factor(scan(text = " 2 4 3 3 2 1 1 2 3 4 2 1 4 3 2 1 4 3 2 4"))
levels<-c("<14","15-24","25-35",">35")
levels
summary(aa)
str(aa)
table(aa)
barplot(table(aa),col="steelblue",main = "barplot")
#hypothesis testing for factor
prop.test(x=12,n=50,p=1/6,alternative = "greater")
#pvalue less than 0.05 reject null hypothesis
#Qualitative variables
mp3<-scan(text = "5.3 3.6 5.5 4.7 6.7 4.3 4.3 8.9 5.1 5.8 4.4 ")
mean(mp3)
mode(mp3)
var(mp3)
sd(mp3)
summary(mp3)
quantile(mp3,c(0.25,0.75))
quantile(mp3,c(0.18,0.36,0.54,0.72,0.9))
#hyposthesis testing
t<-mean(((mp3-4.5)/sd(mp3))/sqrt(length(mp3)))
pt(t,df=length(mp3)-1,lower.tail = F)*2        
t.test(mp3,mu=4.5)
#measure of spread
IQR(mp3)
median(abs(mp3-median(mp3)))*1.4826
mad(mp3)


#visualizing qualitative data
mp3<-c(mp3,scan(text = " 4.9 5 4.9 5.4 6.2 5.1 5.8 5.5 6.7 7"))
par(mfrow=c(1,2))
hist(mp3,col="darkred")
hist(mp3,prob=T,col="steelblue")
#kernel density estimate/densisty estimate
q<-hist(mp3,prob=T,col="darkgreen",ylim=c(0,0.6),lines(density(mp3),col="red",lwd=.9,lty=5))

par(mfrow=c(1,2))
aaa<-rnorm(n=40,mean=7,sd=2)
hist(aaa,probability = T,col = "khaki")
hist(aaa,probability = T,col="khaki",breaks = seq(0.5,14,1.5))
seq(0.5,14,1.5)
par(mfrow=c(1,2))
boxplot(mp3)
boxplot(mp3,col="cornsilk",notch = T)
stem(mp3)
stem(mp3,scale = 2)
#Converting quallitative data to quantitave
mr<-cut(mp3,breaks = c(3:9))
mr
levels(mr)<-c("tiny","small","medium","med-lary","large","huge")
table(mr)
plot(mr)
#fitting and modelling distributions
pexp(q=3,rate=1,lower.tail=F)
x.exp<-rexp(n=100,rate=1)
sum(x.exp>=3)
hist(x.exp,probability = T,ylim = c(0,0.8))
lines(density(x.exp),lty=2)
lines(density(rexp(10000,rate = 1)),col="red")
y<-x.exp,main="qq plot exponential"
data("faithful")
summary(faithful)
plot(faithful$eruptions,faithful$waiting)
data(cars)
mtcars
tab<-table(mtcars$cyl,mtcars$am)
tab
prop.table(tab,margin = 1)
prop.table(tab)
k<-matrix(c(1:6),ncol = 2,byrow = T)
k
prop.table(k)
prop.table(k,1)
prop.table(k,2)
as.data.frame(table(mtcars$cyl,mtcars$am))
par(mfrow=c(1,3))
barplot(tab)
barplot(table(mtcars$cyl,mtcars$am),legend.text = TRUE,main = "Transmission and cyliner")
mosaicplot(table(mtcars$cyl,mtcars$am),col=T,main="cylinder and transmission")
boxplot(mtcars$disp)
boxplot(mtcars$hp)
mtcars[which(mtcars$hp>250),]
plot(x=mtcars$disp,y=mtcars$hp)
cor(mtcars$disp,mtcars$hp,method = "spearman")
#rRegression
model=lm(mtcars$hp~mtcars$disp)
model
summary(model)
plot(density(model$residuals))
a<-1:25
b<-sort(rnorm(25))
mean(b)
plot(a,b,type="l",col="red",lwd=2)
dim(mtcars)
names(mtcars)
str(mtcars)
mtcars[1:3,]
mtcars[,4]
names(cars)
dim(cars)
dimnames(mtcars)
mtcars$hp[1]==mtcars$hp[1]
yy<-mtcars[[4]]
yy
zz<-mtcars[4]
zz
mtcars
nrow(mtcars)
mtcars[mtcars$wt>4000,]
row.names(mtcars[mtcars$wt>4000,])
p<-mtcars[mtcars$hp>=200,]
mean(p$disp)
apply(p,2,mean)
e<-row.names(max(mtcars$mpg))
e
row
mtcars$wt
#car with highest fuel economy
max(mtcars$mpg)
fuel<-sapply(mtcars,max)
fuel
fuel2<-lapply(mtcars$mpg,min )
fuel2
beans<-read.csv("C://Users//admin//Documents//DATASETS//BeansData.csv",header = T,comm="#")
View(beans)
apply(X=beans[,6:8],MARGIN = 2,FUN = mean,na.rm=T)
tapply(beans$rt.len,INDEX = list(beans$rt.len),FUN = mean,na.rm=T)
aggregate(x=beans[,6:8],by=list(beans$pot.size,beans$phos),FUN = mean,na.rm=T)
aggregate(x=beans[,6:8],beans[c(1,2)],FUN=sd,na.rm=T)
barplot(beans$ShtDM)
#Reformating data from wide  to long
data("PlantGrowth")
head(PlantGrowth)
unstack(PlantGrowth)
##
str(beans)
beans$P.lev<-as.factor(beans$P.lev)
beans$rep<-as.factor(beans$rep)
beans$trt<-as.factor(beans$trt)
str(beans)
#reshape
library(reshape2)
iriz<-data.frame(iris)
iriz
iriz$id<-row.names(iriz)
head(melt(iris))
meltiris<-melt(iris,id=c("id","species"))
#merging datasets
ml<-merge(authors,books,by.x="sirname",by.y="name")

library(MASS)
data("mammals")
model11<-lm(brain~body,data=mammals)
model11
plot(model11)
#
par(mfrow= c(2,2))
with(mammals,plot(body,brain))
with(mammals,plot(body,brain,log="xy"))
with(mammals,plot(log10(body),log10(brain)))

model22<-lm(log(brain)~log(body),data=mammals)
model22
#y=  2.1348 +  0.7517 log(body)
#each unit inccrease in log body yeilded an increase of 0.75 inlog brain
#the residuals are much nearer to  normals distribution than the raw data
op<-par(mfrow=c(1,2))
mar=c(4,3,2,1)
plot(density(model11$residuals),main = "model1")
plot(density(model22$residuals),main = "model2")
#apply
m1<-matrix(c<-(1:10),nrow = 5,ncol = 6)
m1
a_m1<-apply(m1,2,sum)
a_m1
#lapply
movies<-c("spiderman","oval","kissing","shameless")
movies_lower<-lapply(movies,tolower)
movies_lower
movies_upper<-lapply(movies,toupper)
movies_upper
str(movies_upper)
movies_better<-unlist(lapply(movies,toupper))
str(movies_better)
#sapply function
dt<-cars
lm_dt<-lapply(dt,min)
sm_cars<-sapply(dt,min)
lm_dt
sm_cars
avg<-function(x){(min(x)+max(x))/2}
fcars<-sapply(dt,avg)
fcars
#tapply function
data("iris")
tapply(iris$Sepal.Width,iris$Species,median)

seq(from=-1,to=1,length=6)
seq(from=-1,to=1,length.out=6)
seq(50,100,5)
seq(100,50,-5)
seq(100,50,by=5)
seq(50,100,-5)
seq(10,100,-10)
seq(100,10,-10)
k<-c(1,3,12,NA,33,7,NA,21)
k[is.na(k)]=11
k
p<-c(1,3,12,NA,33,7,NA,21)
sum(is.na(p))
a<-c(33,21,12,NA,7,8)
mean(a)
mean(a,na.rm = T)
mean(a,na.rm = F)
seq(5,11,by=2)
seq(5,11,by=2,length.out = 3)
x<-seq(4,12,4)
rep(x,each=2)
rep(c(1,2),3)
rep(c(1,2),each=3)
length(p)
length(a)
p<-c(1,3,12,NA,33,7,NA,21)
p[!is.na(p)]
l<-c("Nature's","Best")
summary(l)
str(l)
length(l)
nchar(l)
y<-c("a","b","c","d","e")
y[-2]
h<-factor(p)
str(h)
summary(p)
levels(h)
xx<-c(11,22,47,47,11,47,11)
factor(xx,levels =c(11, 22, 47) ,ordered = T)
xx[3]<-"b"
xx
dim(diag(3))
diag(10,3,4)
diag(1:4)
ff<-matrix(c(1:9),ncol=3,dimnames = list(c("a","b","c"),c("A","B","C")))
upper.tri(ff)
lower.tri(ff)
diag(4)
matrix(diag=4)
matrix(c(4,0,0,0,4,0,0,0,4),nrow = 3,byrow = T)
s<-matrix(c(1:9),3,3,byrow = T)
w<-matrix(c(1:9),3,3)
s%*%w
s*w
s
w
(s+w)^2
diag(4,3,3)
buildings<-data.frame(location=c(1,2,3),name=c("building1","building2","building1"))
da<-data.frame(survey=c(1,1,1,2,2,2),location=c(1,2,3,2,3,1),efficiency=c(51,64,70,71,80,58))
buildingstat<-merge(buildings,da)
buildingstat
v<-rnorm(-100:100,10,replace=T)
v
beans<-read.csv("C://Users//admin//Documents//DATASETS//BeansData.csv",header = T,comm="#")
View(beans)
dim(beans)
data(mtcars)
plot(mtcars$mpg,mtcars$drat,type ="l")
plot(mtcars$mpg,mtcars$drat,type = "b",col=2)
plot(mtcars$mpg,mtcars$drat,col=6)
#Add another varible to our plot
plot(mtcars$mpg,mtcars$drat,col=6);points(mtcars$mpg,mtcars$wt,col=5)
plot(mtcars$mpg,mtcars$drat,col=6,type = "l");lines(mtcars$mpg,mtcars$wt,col=5)
hist(mtcars$gear)
plot(mtcars$mpg,mtcars$drat,lwd=2);points(mtcars$mpg,mtcars$wt,lwd=1)
attach(mtcars)
plot(mpg,drat,col=2,lwd=2,ylim = c(0,30));points(mpg,wt,col=3,lwd=3);points(mpg,qsec,col=4)



Gender<-rep(c("Female","male"),each=2)
Restaurant<-rep(c("Yes","No"),each=2)
count<-c(220,780,400,600)
diningsurvey<-data.frame(Gender,Restaurant,count)
diningsurvey
table(diningsurvey$Gender)
table(diningsurvey$Restaurant)
table(diningsurvey$Gender,diningsurvey$Restaurant)
table(diningsurvey$count>650)

diningsurvey$Restaurant<-c("yes","no","yes",NA)   

#useNA
table(diningsurvey$Restaurant,useNA = "always")
is.na(diningsurvey)
#is.na
table(diningsurvey$Gender,is.na(diningsurvey$Restaurant))
table(diningsurvey$Gender,exclude = "male")
rentalunits<-matrix(c(45,37,34,10,15,12,24,18,19),ncol=3,byrow = T)
colnames(rentalunits)<-c("section 1","section 2","section 3")
rownames(rentalunits)<-c("rented","vacant","reserved")
as.table(rentalunits)
margin.table(rentalunits)
margin.table(rentalunits,1)
margin.table(rentalunits,2)
prop.table(rentalunits)
prop.table(rentalunits,1)
prop.table(rentalunits,2)
ftable(rentalunits)
summary(rentalunits)
chisq.test(rentalunits)
addmargins(rentalunits)
addmargins(rentalunits,1)
addmargins(rentalunits,2)
addmargins(prop.table(rentalunits,2))
as.data.frame(rentalunits)
seq(2,11,by=3)
seq(9,45,by=9)
seq(9,90,length.out = 10)
seq(-10,10,length.out = 5)
x<-5
1:x-1
seq(5,1,by=-1)
(10:20)*2
10:20*2
105:(30*3)
1+1:10/10
2^(0:5)
seq(from=10, to=-3, by=-2)
