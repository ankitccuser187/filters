#code to get most frequent movie id for user

#library(rcpp)
#library(plyr)

#get the user id wich you want to sugesst movie
user<-readline("enter the user ID")

#transfer the matrix into data
tframe<-data.frame(c_test)

# get the frame of user
test_set1<-tframe[tframe$UserID==user,]


#get the ohter users from same cluster
test_set2<-tframe[tframe$clu_num==test_set1[1,8],]


tp<-data.frame(test_set1$MovieID)

names(tp)=c("MovieID")

#other users rated movie simmillar to user
sim_movie_u<-merge(tp,test_set2,by="MovieID")


# remove the data of the user whose rating we want to predict
sim_movie_u<-sim_movie_u[sim_movie_u$UserID!=user,]

#introduce a column in user table for predicted rating
test_set1<-cbind(test_set1,pre_rating=0)

#no. of row in test_set1
n<-1:nrow(test_set1)


#assgining most frequent rating to the same movie by other users having profile
#as simmilar as our user 
for(i in seq(along=n)){
  
  calci<-sim_movie_u[sim_movie_u$MovieID==test_set1$MovieID[i],]
  test_set1$pre_rating[i]<-as.numeric(mean(calci$Ratings))
  
}
#test_set1$pre_rating<-temp

#geting mod error value between predicted and actual rating
test_set1<-cbind(test_set1,error=abs(test_set1$Ratings-test_set1$pre_rating))

#removing those movie which rated by only our user 
test_set1<-test_set1[test_set1$error!=Inf,]

#remove missinng values from final data
test_set1<-na.omit(test_set1)