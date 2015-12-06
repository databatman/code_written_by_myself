--EmpAndDept3
--��Ҫ�Ǽ򵥲�ѯ�ͱ��Ĵ���(�е�Ĭ������check/unique/default����))

--from �Ӿ���ʹ���Ӳ�ѯ
--��ʾ���ڲ���ƽ�����ʵ�Ա������,нˮ�������ڲ��ŵ�ƽ������
--������֪���������ŵ�ƽ������
select avg(sal),deptno from emp group by deptno;
--�ڰ��ϱߵĽ������һ����ʱ��Դ�
select emp.ename,sal,temp.myavg,emp.deptno from
 emp,(select avg(sal) myavg,deptno from emp group by deptno) temp
where emp.deptno=temp.deptno and emp.sal>temp.myavg;


--��ҳ��ѯ
select * from emp;
--top��ߵ�����ʾҪ�����ļ�¼��,��sql server��䣬mysql������
--limit n,i ��һ����ֵ��ʾ�ӵ�n+1��ʼȡ,i��ʾ�ܹ�ȡ��������mysql���

--����ʾ��һ�������ĸ���ְ�Ĺ�Ա
select top 4 * from emp order by hiredate;

select * from emp order by hiredate limit 4;

--����ʾ����������ʮ����ְ�Ĺ�Ա������ʱ����Ⱥ�˳��
select top 5 * from emp where empno not in
   (select top 5 empno from emp order by hiredate)
   order by hiredate;

select * from emp order by hiredate limit 5,5;

select * from emp 
    where empno not in (select empno from emp order by hiredate) 
    order by hiredate;

--��ʾ��ʮһ����ʮ������ְ����Ϣ(ʱ��˳��)
select top 3 * from emp where empno not in
(select top 10 empno from emp order by hiredate)
order by hiredate;

select * from emp order by hiredate limit 10,3;

--����Ч��(ѹ������)ʱ���ݵĲ���,����� ��: 
--identity��sqlserver��������mysql������
create table test(
testId int primary key identity(1,1),
testName varchar(30),
testPass varchar(30))engine=InnoDB;

--mysqlʹ��auto_increment
create table test(
testId int auto_increment primary key,
testName varchar(30),
testPass varchar(30))engine=InnoDB;


insert into test (testName,testPass) values('xupei','xupei');
insert into test (testName,testPass) select testName,testPass from test --������
select count(*) from test
select * from test
select testId from test 
drop table test
--���Ժ�,��ҳ��Ч�ʻ��Ǻܸߵ�

--���ɾ��һ�ű����ظ���¼
create table temp(
catId int,
catName varchar(40)
)

insert into cat values(1,'aa');
insert into cat values(2,'bb');

insert into cat select * from cat;    --��������

insert into temp select * from cat;   --��cat �ļ�¼distinct�󣬷�����ʱ����
delete from cat;                       --cat��ļ�¼���
insert into cat select * from #temp;   --��#temp�е����ݣ����ظ���¼������cat��
drop table temp;                       --ɾ����ʱ��#temp


--�������Ӻ���������
--��ʾ��˾ÿ��Ա���������ϼ�������
--�����ӣ�ƥ���ϵĲ�����ʾ��
select w.ename,b.ename from emp w,emp b where w.mgr=b.empno;

--��ʾ��˾ÿ��Ա���������ϼ�������,Ҫ��û���ϼ�����ҲҪ��ʾ
--��������:�����ߵı��¼ȫ����ʾ,���û��ƥ��ļ�¼,��NULL����
--�������ӣ�����ұߵı��¼ȫ����ʾ,���û��ƥ��ļ�¼,��NULL����
--join��ʵ���ǰ�������������һ����
select w.ename,b.ename from emp w left join emp b on w.mgr=b.empno;
--�����ӵ������ر�ǿ����ʱ��where�ã���Ϊ�ܶ�ʱ�����ǲ�ֻ��Ҫѡ����Ҫ��ֵ��Ҳ��Ҫ����Щ������ĳЩֵ���н��в�����EmpAndDept4������

--�����ı����Լ��
--Լ������ȷ�����ݿ������ض�����ҵ����,��sql server �У�Լ������
--not null,unique,primary key,foreign key��check����
--not null:��������϶�����not null ����ô��������ʱ������Ϊ���ṩ����
--auto_increment,��ʹ�����¼ʧ��Ҳ������1
create table test1(
test1Id int auto_increment primary key,
test1name varchar(30) unique,
test1pass varchar(30) not null,
test1age int
)engine=InnoDB;
--unique:��������ΨһԼ���󣬸���ֵ�ǲ����ظ��ģ����ǿ���Ϊnull,�����ֻ��һ��null
--primary key:����Ψһ��ʾ���е����ݣ�����������Լ���󣬸��в����ظ����Ҳ���Ϊnull
--��Ҫ˵�����ǣ�һ�ű������һ���������������ж��uniqueԼ��
--������и�������
--��������С������
create table test2(
test1Id int ,
test1name varchar(30) ,
test1pass varchar(30) ,
test1age int
primary key(test1Id,test1name)
)engine=InnoDB;
--�м�����ͱ�����
--foreign key�����ڶ�������ʹӱ�֮��Ĺ�ϵ�����Լ��Ҫ�����ڴӱ��ϣ�
--����������������Լ����uniqueԼ�������������Լ����Ҫ�����������
--����������������д��ڻ�Ϊnull

--check:����ǿ�������ݱ���������������ٶ���sal���϶�����checkԼ������Ҫ��
--sal��ֵ��1000~2000֮�䣬�������1000~2000֮�䣬�ͻ���ʾ����
create table test3(
test1Id int ,
test1name varchar(30) ,
test1pass varchar(30) ,
sal int check(sal>=1000 and sal<=2000)
)engine=InnoDB;

--defaultʹ��
create table mes(
mesId int auto_increment primary key,
mescon varchar(2000) not null,
mesdate datetime default getdate()
)engine=InnoDB;
insert into mes(mescon) values('���')
insert into mes(mescon,mesdate) values('���','2015-8-5')
select * from mes




--�̵��ۻ�ϵͳ����ư���
--����һ���̵�����ݿ�,��¼�ͻ����乺�����,���±����������:
--��Ʒgoods(��Ʒ��goodsId����Ʒ��goodsName,����unitprice,��Ʒ���category
--,��Ӧ��pr0vider);
--�ͻ�custumer(�ͻ���customerId,����name,סַaddress,����email,�Ա�sex,
--���֤cardId);
--����purchase(�ͻ���customerId,��Ʒ��goodsId,��������nums);
--��SQL����������й��ܣ�
--1 �����ڶ�����Ҫ��������
--�� ÿ������������
--�� �ͻ�����������Ϊ��ֵ��
--�� ���۱������0����������������1��30֮�䣻
--�� ���ʲ��ܹ��ظ���
--�� �ͻ��Ա�������л���Ů��Ĭ������
--�� ��Ʒ����� ʳ�� ����Ʒ

--goods��
create table goods(
goodId nvarchar(50) primary key,
goodName nvarchar(80) not null,
unitprice numeric(10,2) check (unitprice>0),
category nvarchar(3) check(category in('ʳ��','����Ʒ')),
provider nvarchar(50)
)engine=InnoDB;

--customer��
--��mysql��ͬʱ����default��checkʱ��Ҫ������default ֮���ټ�checkԼ��������ᱨ��
create table customer(
customerId nvarchar(50) primary key,
customername nvarchar(50) not null,
address nvarchar(100),
email nvarchar(100) unique,
sex nchar(1) default '��' check(sex in ('��','Ů'))  ,
cardId nvarchar(18)
)engine=InnoDB;

--purchase��
create table purchase(
customerId nvarchar(50) ,
goodId nvarchar(50) ,
nums int check(nums>0),
foreign key(customerId) references customer(customerId),
foreign key(goodId) references goods(goodId)
)engine=InnoDB;

















