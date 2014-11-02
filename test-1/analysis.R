library(plyr)
acul<-data.frame()
aculist<-list()
gnt<-1:nrow(tpt)
unt<-1:nrow(user)
for(i in seq(along=gnt)){
  for(j in seq(along=unt)){
    
    tk<-data.frame(user[,i]-user[j,i])
    #names(tk)<-c("diff")
    
    tk<-data.frame(tk[tk[,1]>0,])
    
    
    
    erp<-min(tk)
    if(erp=="inf")
      break
    #print(erp)
    erp<-erp+user[j,i]
    usertest<-user[user[,i]==erp,]
    
    usertp<-data.frame(usertest[,c(i)])
    
    names(usertp)<-c("UserID")
    
    df_test<-merge(tpt,df_um,by="Genre")
    
    df_test<-merge(df_test,usertp,by="UserID")
    
    df_test1<-ddply(df_test,.(MovieID),summarise,freq=length(unique(UserID)))
    
    df_test1<-merge(df_test,df_test1,by="MovieID")
    
    df_test1<-df_test1[order(-df_test1$freq),]
    
    temlist<-quantile(df_test1$freq)
    
    df_test1<-df_test1[df_test1$freq>temlist[[4]],]
    
    df_test2<-data.frame(unique(df_test1$MovieID))
    names(df_test2)<-c("MovieID")
    
    smp<-nrow(df_test2)
    
    d_user<-df_um[df_um$UserID==user[j,1],]
    
    d_user<-merge(d_user,df_test2,by="MovieID")
    
    prec<-nrow(d_user)
    
    acc<-prec/smp
    
    aculist<-append(aculist,acc)
      
    print(acc)
    acul[i,j]<-acc
  }
  
}