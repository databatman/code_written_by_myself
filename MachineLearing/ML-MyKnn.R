#KNN自写版本
#以下数据的保存格式默认为最后一列为分类的类别。如果不是需要自己人为的先改变下数据的结构


#将数据随机分组3组，第一组有row1个数据，第二组有row2个，第三组row3个
#该函数要嘛产生两个随机数组要嘛产生三个
randomsample<-function(data,row1=0,row2=0,row3=0){
  rows=nrow(data)
  RandomNum=sample(1:rows,rows,replace=FALSE)
  if ((row1+row2+row3)<=rows){
    if(row3==0 & row2==0){
      data1row=RandomNum[1:row1]
      data1row=data1row[order(data1row)]
      data1=data[data1row,]
      return (data1)
    }
    else if(row3==0){
      data1row=RandomNum[1:row1]
      data2row=RandomNum[(row1+1):(row1+row2)]
      data1row=data1row[order(data1row)]
      data2row=data2row[order(data2row)]
      
      data1=data[data1row,]
      data2=data[data2row,]
      result=list(data1=data1,data2=data2)
      return (result)
    }
    else if (row1==0){
      print('you must be kidding me...')
    }
    else{
      data1row=RandomNum[1:row1]
      data2row=RandomNum[(row1+1):(row1+row2)]
      data3row=RandomNum[(row1+row2+1):(row1+row2+row3)]
      data1row=data1row[order(data1row)]
      data2row=data2row[order(data2row)]
      data3row=data3row[order(data3row)]
      data1=data[data1row,]
      data2=data[data2row,]
      data3=data[data3row,]
      result=list(data1=data1,data2=data2,data3=data3)
      return (result)
      
    }
    
  }
  else{
    print('rows out of bound')
    return
  }
  
}  
result=randomsample(iris,50,20)


#传入要分类的单个向量，邻居的个数k，已知道所属类的数据集，最后返回其最大可能的类
classify<-function(vector,ClassifyData,k=5){
  require(reshape2)
  
  cols=ncol(ClassifyData)
  data=ClassifyData[,1:cols-1]
  alldistance=numeric(0)
  for (i in 1:nrow(data)){
    distance=sum((vector-data[i,])^2)
    alldistance=c(alldistance,distance)
  }
  #以距离的倒数为权重
  alldistance=data.frame(weight=1/alldistance,sequence=1:nrow(data),class=ClassifyData[,cols])
  alldistance=alldistance[order(alldistance$weight),]
  kNearest=tail(alldistance,k)   #得到权重最高的几个值
  
  m=melt(kNearest,id=c('class'))  #用melt和cast计算各个类别的权重和
  c=cast(m,class~variable,sum)
  temp=c[order(c$weight,decreasing=TRUE),]
  sortvector=cbind(vector,temp[1,1])
  return (sortvector)
  
}
#对未分类的数据集进行分类，注意未分类的数据比已分类的数据少一列，那一列就是他们所属的类别
#最后返回的数据集加上我们对其的预测列
MyKNN<-function(UnClassifyData,ClassifyData,k=5){
  rows_uns=nrow(UnClassifyData)
  cols_s=ncol(ClassifyData)
  result=as.data.frame(matrix(numeric(0),ncol=cols_s))
  colnames(result)=colnames(ClassifyData)
  for (i in 1:rows_uns){   #对数据集的每个数据进行分类
    vect=classify(UnClassifyData[i,],ClassifyData,k=5)
    result=rbind(result,vect)
  }
  colnames(result)=colnames(ClassifyData)
  return (result)
}


#剪辑近邻法,得到剪辑后的数据集
#默认其分类值Y都存在最后一列
cutsample<-function(ReferenceData,TestData,k=1){
  cols=ncol(TestData)
  rows=nrow(TestData)
  TestY=TestData[,cols]
  TestX=TestData[,1:cols-1]
  tempcol=data.frame()
  for (i in 1:rows){
    temp=classify(TestX[i,],ReferenceData,k=k)
    tempcol=rbind(tempcol,temp)
  }
  TestData$tempY=tempcol[,ncol(tempcol)]
  TestData$CutOrNot[TestData$tempY!=TestData[,cols]]='yes'
  TestData$CutOrNot[TestData$tempY==TestData[,cols]]='no'  #我原本直接使用TestY[i],发现最后结果都是一样的了，看了condition里面必须是跟列表有关的东西才可以
  cutresult=TestData[which(TestData$CutOrNot=='no'),1:(ncol(TestData)-2)]
  return (cutresult)
}

#测试
#随机生成参考数据集和测试数据集
a=randomsample(iris,20,50,50)
refer=a$data1
test=a$data2
verify=a$data3

#进行剪辑，获得剪辑后的数据cutresult
cuttestresult=cutsample(refer,test)   
nrow(cutresult)  #看看剩下多少数据

#使用剪辑后的数据作为knn分类的依据
result=MyKNN(verify[,1:ncol(verify)-1],cuttestresult)


#作图分析

plot(result$Sepal.Length,result$Sepal.Width,col=result$Species,
     main='knn category',pch=18)

plot(verify$Sepal.Length,verify$Sepal.Width,col=verify$Species,
     main='real result',pch=18)
#对比
total=merge(verify,result,by=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'))
colnames(total)[c(5,6)]=c('real','knn')
total$YesOrNot[total$real==total$knn]='yes'
total$YesOrNot[total$real!=total$knn]='no'
total







