#this piece of code clean data from both genre data frame as well as user movie data frame also
#it is first code which will get executed and layout the path for other codes oncoming

print("Data set loading is started")

# loading datasets into a data frames
df_rating<-read.csv("C:\\Users\\ankit PBI\\Desktop\\New folder\\ratings.csv")
df_movies<-read.csv("C:\\Users\\ankit PBI\\Desktop\\New folder\\movies.csv")
df_users<-read.csv("C:\\Users\\ankit PBI\\Desktop\\New folder\\users.csv")

#tagging column names
names(df_rating)=c("UserID","MovieID","Rating","timestamp")
names(df_movies)=c("MovieID","Movie_name","Genre")
names(df_users)=c("UserID","Sex","Age","Job","Zip")

#inerjoining of ratings and movies table
df_um<-merge(df_movies,x=df_rating,by="MovieID",all.x=TRUE)

#permutate data frame element according row wise
df_um<-df_um[sample(nrow(df_um)),]

#removing unnecessary files from global enviorment of r
rm(df_rating)
#rm(df_movies)

print("Data set loading part completed starting genre calculation")

genre_class<-function(){
  #get the different movie genre
  tp<-data.frame(unique(df_um$Genre))
  
  #taging the movies Genre
  names(tp)=c("Genre")
  
  #cleannig the dataset
  tp<-data.frame(tp[-grep("19", tp$Genre), ])
  names(tp)=c("Genre")
  
  n<-1:nrow(tp)
  
  #calculate no. of users from each movie Genre 
  for(i in seq(along=n)){
    
    base<-df_um[df_um$Genre==tp$Genre[i],]
    tp$Users[i]<-nrow(base)
    
    }
  
  #calculate movie count in each genre
  for(i in seq(along=n)){
    base<-nrow(df_movies[df_movies$Genre==tp$Genre[i],])
    tp$Movie_count[i]<-base
  }
  
  #defining baseline for movie users
  tp$avg_mov<-tp$Users/tp$Movie_count
  
  
 
  #under construction
  #tpset<-data.frame(tpset[-grep("20",tpset$tp..grep..19...tp.unique.df_um.Genre.....),])
  
  #removing unnecessary declrations
  rm(base)
  t<-1:nrow(tp)
  rm(i)
  col<-paste("g",t,sep="")
  tp<-cbind(tp,genrecode=col)
  rm(col)
  rm(n)
  rm(t)
  #tp<-tp[order(-tp[,2]),]
  print("genre calculation completed starting user calculation")
  return(tp)
  
}

tp<-genre_class()


#seperating genres in tpt having user base above then average
tpt<-tp[tp[,2]>=mean(tp[,2]),]

#removing nan numeric values
tpt<-na.omit(tpt)

#calculating number of movies rated by each user
n<-1:nrow(df_users)
for(i in seq(along=n)){
  df_users[i,6]<-nrow(df_um[df_um$UserID==df_users$UserID[i],]) 
}

print("user calculation completed spliting the user dataset")
rm(n)
#assginig quartile values
qut<-quantile(df_users[,6])

#file that send to rank user predicter
rank_user<-df_users[df_users[,6]>208,]

#file that send to collabrative filteration method
collab_user<-df_users[df_users[,6]<=208,]

#removing uneccasary declaration
rm(i)
rm(qut)