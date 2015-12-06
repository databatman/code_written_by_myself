#!usr/bin/env python  
#-*- coding:utf-8 -*-  
  

from random import sample
import time

'''1、堆排序'''
#要写的模块有插入堆后调整排序、删除堆后调整排序、建立最小堆、将堆完整排好序的方法  
#节点i，则其父节点(i-1)/2,左右子节点2i+1/2i+2  

#插入堆后排序  
def InsertHeap(ls,num):  
    ls.append(num)  
    i=len(ls)-1  
    j=(i-1)/2  
  
    while j>=0 :   
        if ls[i]>ls[j]:  
            break  
  
        ls[i],ls[j]=ls[j],ls[i]  
        i=j  
        j=(i-1)/2  
  
#删除堆第一个元素  
def DeleteHeap(ls):  
    ls[0]=ls[len(ls)-1]  
    ls.pop()  
    i=0  
    j=2*i+1  
    while j<=len(ls)-1:  
        if j<len(ls)-1 and ls[2*i+1]>ls[2*i+2]:  
            j+=1  
        if ls[i]<ls[j]:  
            break  
        ls[i],ls[j]=ls[j],ls[i]  
        i=j  
        j=2*i+1  
  
#生成最小堆,利用每次插入一个元素的办法  
def GenerateHeap(ls1):  
    n=len(ls1)  
    ls=[]  
    for i in range(n):  
        InsertHeap(ls,ls1[0])  
        ls1.pop(0)     #加入pop的原因是为了让空间复杂度始终为n,牺牲速度换取空间
    return ls  
  
#堆重排列为有序数组  
def SortHeap(ls):  
    n=len(ls)  
    ls=GenerateHeap(ls)  
    ls_sorted=[]  
    for i in range(n):  
        ls_sorted.append(ls[0])  
        DeleteHeap(ls)  
  
    return ls_sorted  
  


'''2、快速排序'''
def QuickSort(ls,l,r):  
    i=l  
    j=r  
    x=ls[i]  
    while i<j:  
        while i<j:  
            if ls[j]<x:  
                ls[i]=ls[j]  
                i+=1  
                break  
            else:  
                j-=1  
        while i<j:  
            if ls[i]>x:  
                ls[j]=ls[i]  
                j-=1  
                break  
            else:  
                i+=1  
    ls[i]=x  
    if l<r:  
        QuickSort(ls,l,i-1)  
        QuickSort(ls,i+1,r)  



'''3、归并排序'''  
#递归+合并  
  
#合并两个有序组1  
def MergeList(ls1,ls2):  
    temp=[]  
  
    while ls1 and ls2:  
        if ls1[0]<ls2[0]:  
            temp.append(ls1[0])  
            #ls1.pop(0)  
            del ls1[0]  
        else:  
            temp.append(ls2[0])  
            #ls2.pop(0)  
            del ls2[0]  
    while ls1:  
        temp.append(ls1[0])  
        #ls1.pop(0)  
        del ls1[0]  
    while ls2:  
        temp.append(ls2[0])  
        #ls2.pop(0)  
        del ls2[0]  
    return temp  
  
  
#第二种合并两数组方法  
def MergeList2(ls1,ls2):  
    temp=[]  
    n,m=len(ls1),len(ls2)  
    i,j=0,0  
  
    while i<n and j<m:  
        if ls1[i]<=ls2[j]:  
            temp.append(ls1[i])  
            i+=1  
        else:  
            temp.append(ls2[j])  
            j+=1  
    while i<n:  
        temp.append(ls1[i])  
        i+=1  
    while j<m:  
        temp.append(ls2[j])  
        j+=1  
    del ls1,ls2  
    return temp  
  
#拆分成2个组  
def SplitList(ls):  
    if len(ls)==1:  
        print 'unseperable'  
        return ls  
    mid=len(ls)/2  
    ls1=ls[0:mid]  
    ls2=ls[mid:len(ls)]  
    return (ls1,ls2)  
  
  
#归并排序  
def RecursiveAndMerge(ls):  
    if len(ls)>1:  
        ls1=RecursiveAndMerge(SplitList(ls)[0])  
        ls2=RecursiveAndMerge(SplitList(ls)[1])  
        return MergeList(ls1,ls2)  
    else:  
        return ls  
#使用第二种合并数组法的归并排序  
def RecursiveAndMerge2(ls):  
    if len(ls)>1:  
        ls1=RecursiveAndMerge(SplitList(ls)[0])  
        ls2=RecursiveAndMerge(SplitList(ls)[1])  
        return MergeList2(ls1,ls2)  
    else:  
        return ls  
  
  


########################################################################################## 
#分别计算下今天两种归并排序跟堆排序运算速度差距：  
def main():
    ls=sample(range(100000),100000)     
    #归并排序-
    ls1=ls
    start1=time.clock()  
    a=RecursiveAndMerge(ls1)  
    end1=time.clock()  
    print '%ss' % (end1-start1)  
    #归并排序2
    ls2=ls
    start2=time.clock()  
    c=RecursiveAndMerge2(ls2)  
    end2=time.clock()  
    print '%ss' % (end2-start2)  
    #堆排序  
    ls3=ls 
    start3=time.clock()  
    b=SortHeap(ls3)  
    end3=time.clock()  
    print '%ss' % (end3-start3)  
    #快速排序
    ls4=ls
    start4=time.clock()  
    b=SortHeap(ls4)  
    end4=time.clock()  
    print '%ss' % (end4-start4)  

