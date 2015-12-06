#贪心感知器学习算法

#定义一个sign来返回+—1
sign<-function(value){
  if (value > 0){
    return (1)
  }
  else{
    return (-1)
  }
}

#定义一个函数来返回不符合的x个数
count<-function(w,x,y){
  num=0
  for (i in 1:500){
    if(sign(sum(w*x[i,]))!=y[i])
      num=num+1
  }
  return (num)
}



#获得train和test的初始X和Y
traindata<-read.table('pocket-train.txt')
testdata<-read.table('pocket-test.txt')
x_traindata<-traindata[,1:4]
x_traindata<-cbind(rep(1,500),x_traindata)
y_traindata<-traindata[,5]

x_testdata<-testdata[,1:4]
x_testdata<-cbind(rep(1,500),x_testdata)
y_testdata<-testdata[,5]



#定义初始权重向量W和update次数n
w<-rep(0,5)
now_count<-500  
sum_errs=0
k=0   #计算重复计算h的次数

repeat{
  random_pick=sample(1:500,500)   #随机i
  n<-0   #计算更新次数
  #定义naive循环:1,2,3,4....n,1,2,3...n...
  while (1){
  
    for (i in random_pick){
      if (sign(sum(w*x_traindata[i,]))!=y_traindata[i]){
        w=w+y_traindata[i]*x_traindata[i,]
        n=n+1
        print(n)
        w_count=count(w,x_traindata,y_traindata) #用自定义的count函数来返回w的犯错率
        print ('w_count:')
        print (w_count)
        if (w_count<now_count){   #比较两者的犯错率
          h=w
          now_count=w_count
        }
      }
     #我原本是采用while(n<50),但是发现这样的话在i从1-500的过程中，n会更新超过50次才跳出循环    
    if(n==50) #跳出for循环
      break
    }
    if(n==50) #跳出while循环
      break
  }
  #输入test_data进行错误率判断
  errs<-count(h,x_testdata,y_testdata)/500
  sum_errs<-sum_errs+errs
  k=k+1
  print ('k=')
  print (k)
  if(k==10)
    break
}
avg_errs<-sum_errs/10
print ('avg_errs:')
print (avg_errs)
