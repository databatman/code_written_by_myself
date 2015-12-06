#普通感知器学习算法

#定义一个sign来返回+—1
sign<-function(value){
  if (value > 0){
    return (1)
  }
  else{
    return (-1)
  }
}

#获得初始X和Y
data1<-read.table('PLA.txt')
x_data1<-data1[,1:4]
x_data1<-cbind(rep(1,400),x_data1)
y_data1<-data1[,5]


#定义初始权重向量W和update次数n
w<-rep(0,5)
n<-0
j=0  #用来记录是否经历了一次完整的循环

#定义naive循环:1,2,3,4....n,1,2,3...n...
while (1){
  for (i in 1:400){
    if (sign(sum(w*x_data1[i,]))!=y_data1[i]){
      w=w+y_data1[i]*x_data1[i,]
      n=n+1
      print ('this is i and n updates.')
      print (i)
      print (n)
      j=0  #一但更新一次w，就将j初始化为0
    }
    j=j+1
    if (j==401){    
      #j如果为401，说明w在第t次更新之后，经历了一轮重新的测试没发现错误，所以得出结果
      print ('reusult:')
      print (n)
      break
    }
  }
  if (j==401){
    break
  }
  
}
  