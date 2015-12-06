#k-means自写版

Mymeans<-function(data,k,iter.max=10){
  rows=nrow(data)
  cols=ncol(data)
  centers=matrix(0,nrow=k,ncol=cols)  #储存中心的矩阵
  rand=sample(1:rows,k,replace=FALSE)
  centers=data[rand,]                 #随机初始化质心
  rownames(centers)=c(1:k)
  sortdata=as.data.frame(matrix(0,ncol=2,nrow=rows))
  colnames(sortdata)=c('distance','category')   #定义储存距离和所属类别的矩阵
  iter=0       #记录迭代次数

  pickdistance=0
  pickcluster=0
  
  while (iter<iter.max){
    changed=FALSE
    for(i in 1:rows){#for计算每个点到质心的距离并选取离他最近的质心
      for (j in 1:k){
        distance=sum((data[i,]-centers[j,])^2)   #欧氏距离
       # distance=sum(abs(data[i,]-centers[j,]))  #曼哈顿距离
        if (pickdistance==0 | distance < pickdistance){
          pickdistance=distance
          pickcluster=j
        }    
      }
      pick=c(pickdistance,pickcluster)
      previouscluster=sortdata[i,2]
      #用change来标记类是否发生了变化
      if (pickcluster!=previouscluster){
        changed=TRUE
      }
      sortdata[i,]=pick
      pickdistance=0
      pickcluster=0
    }        
    #如果没有任何一个发生了变化，change就会是false，循环即可跳出
    if (changed!=TRUE){
      break
    }
    
    for (j in 1:k){         #更新质点
      classdata=data[which(sortdata$category==j),]
      centers[j,]=colMeans(classdata)

    }                      
    iter=iter+1
  
  }
  
  cluster=table(sortdata$category)
  result=list(centers=centers,sortdata=sortdata,cluster=cluster,iter=iter)
  return (result)
  
}

newiris<-iris[,c(1,2,3,4)]
a=Mymeans(newiris,k=2,iter.max = 10)
classiris=a$sortdata
classiris=classiris$category
b=kmeans(newiris,centers=3)




plot(newiris$Sepal.Length,newiris$Sepal.Width,col=rep(c(2,1,3),each=50),
     main='nomeans',pch=17)
plot(newiris$Sepal.Length,newiris$Sepal.Width,col=classiris,
     main='mymeans',pch=17)
plot(newiris$Sepal.Length,newiris$Sepal.Width,col=b$cluster,
     main='kmeans',pch=17)
