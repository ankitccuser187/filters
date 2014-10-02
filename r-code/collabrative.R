#code to implement c-means and k-means clustering

#including library into development enviorment of code library e0171
#before running follwing code import 'e1071' first
library(e1071)

#enter the path of data-sets
dframe_user<-read.csv("C:\\Users\\ankit chaudhary\\Desktop\\dataset\\users.csv")
dframe_rating<-read.csv("C:\\Users\\ankit chaudhary\\Desktop\\dataset\\ratings.csv")

#code to join user and rating column on basis of user_id
df_userrating<-merge(dframe_rating,dframe_user,by="X1")

#write cloumn name to be more readable code and process
names(df_userrating)<-c("UserID","MovieID","Ratings","Timestamp","Gender","Age","Ocupation","Zipcode")

#time stamp will be useless until unless we considering to look
#into aditonal contextual information
df_mod<-subset(df_userrating,select=-c(Timestamp))

#select only those ratings which are greater then 3
#df_mod<-df_mod[df_mod$Ratings>3,]

#converting data frame to matrix where gender column get coereced as numeric
test<-data.matrix(df_mod)

#fuzzy c-means clustering
#where first argument is dataset we want enter
#second max. no. of clusters
#third max no. of iterations
c_fit<-cmeans(test[,-c(4,8)],30,500)

#hard k-means clustering
kcluster<-kmeans(test[,-c(4,8)],20,500)

#now bind the cluster number with each observations of kcluster
k_test<-cbind(test,clu_num=kcluster$cluster)

#printing the cluster 
#print(k_test)

#now bind the cluster number with each observations of c_fit fuzzy cluster
c_test<-cbind(test,clu_num=c_fit$cluster)

#print(c_test)

#exporting dataset as a file
#write.matrix(,file="dataset.csv",sep=",")

#uncomment these code lines if you want the statistics of two
#cluster models
#k-means cluster demo
#print(kcluster)
#print(kcluster$size)
#print(kcluster$cluster)
