#this peice of code written to calculate genre ranking of each user for
#rank user matrix and genre tpt

index <- 1:nrow(df_um)
trainindex <- sample(index, trunc(length(index)*1/2))
trainset <- df_um[trainindex, ]
print("test set selected procedding genre rank calculation")
#function for movie genre management
#tpt<-genre()

#linking code that run genre code then assgin it into feedback script
#source('~/R workspace/feedback/genre_funct.R')
user<-subset(rank_user,select=c(UserID))

#removing unnecessary data frames
#rm(df_user)

#tpt<-genre_calc()




gn<-1:nrow(tpt)
nu<-1:nrow(user)

for(i in seq(along=nu)){
  
  for(j in seq(along=gn)){
    
    temp1<-trainset[ trainset$Genre==tpt$Genre[j],]
    
    temp2<-temp1[temp1$UserID==user$UserID[i],]
    
    rank<-nrow(temp2)/nrow(temp1)
    #print(rank)
    
    user[i,j+1]<-rank
    
  } 
  names(user)[j+1]<-tpt$genrecode[j]
 # print(user[i,])
  write.table(user[i,],file="C:\\Users\\ankit PBI\\Documents\\R workspace\\final project\\my_final workspace\\userrank1.csv",append=TRUE,sep=",",row.names=FALSE,col.names=TRUE)
}
print("user genre rank calculation completed")
boxplot(user[,2:26],xlab="different movie genre",ylab="unified ranks",main="Genre Box plot")
