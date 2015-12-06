--win 7 ����
--������޸ĳ�mysqlר��
--������mysql���ݿ�����������ܽ���mysql
--�������ù���Աģʽ����cmd����mysql��bin�ļ�����ʹ��net start mysql����
--�����������mysql -h localhost -u root -p �������ݿ⣬��ʱ��Ҫ���룬�հ�װ�ú�Ĭ�������ǿգ���enter����
--�������ݿ�

--�޸�����,��update����
update mysql.user set password=PASSWORD(1991423) where user='root';

--�������ݿ�cookbook
create database cookbook;
--��ʾ���е����ݿ�
show databases;
--�������ݿ�
use cookbook;
--��ʾ���еı��
show tables;

--����dept�������ĸ�ʽ����ΪInnoDB��,mysqlĬ�ϵĸ�ʽmyisam�����ֱ��Ĳ����ٶȿ죬���ǲ����������
--�޸�Ϊ�ʺ�mysql�Ĵ���

create table dept
(deptno int primary key,  --���ó�����
dname nvarchar(30),
loc nvarchar(30))engine=InnoDB;

--�鿴dept���������
describe dept;


--����emp
create table emp
(empno int primary key,
 ename nvarchar(30),
 job int(32),
 mgr int,
 hiredate datetime,
 sal numeric(10,2),
 comm numeric(10,2),
 deptno int,  --��Ҫ������������������Ϊ���
 foreign key (deptno) references dept(deptno) on delete cascade)engine=innoDB;

--���ô��������޸�
ALTER table emp MODIFY column job nvarchar(30);

--��������ע��
--  �����ֻ��ָ������
--  ���������������������Ҫһ��
insert into dept values(10,'accounting','new york');
insert into dept values(20,'research','dallas');
insert into dept values(30,'sales','chicago');
insert into dept values(40,'operations','boston');

--���µ������д��ʽ�ᰴ��emp��������������˳��������
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7934,'miller','clerk',7782,'1982-1-23',1300.00,10);
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7902,'ford','analyst',7566,'1981-12-3',3000.00,20);
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7900,'james','clerk',7698,'1981-12-3',950.00,30);
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7876,'adams','clerk',7788,'1987-5-23',1100.00,20);
insert into emp values
(7844,'turner','salsman',7698,'1982-9-8',1500.00,0.00,30);
insert into emp(empno,ename,job,hiredate,sal,deptno) values
(7839,'king','president','1981-11-17',5000.00,10);
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7788,'scott','analyst',7566,'1987-4-19',3000.00,20);
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7782,'clark','manager',7839,'1981-6-9',2450.00,10);
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7698,'blake','manager',7839,'1981-5-1',2850.00,30);
insert into emp values
(7654,'martin','salsman',7698,'1981-9-28',1250.00,1400.00,30);
insert into emp(empno,ename,job,mgr,hiredate,sal,deptno) values
(7566,'jones','manager',7839,'1981-4-2',2975.00,20);
insert into emp values
(7521,'ward','salesman',7698,'1981-2-22',1250.00,500.00,30);
insert into emp values
(7499,'allen','salseman',7698,'1981-2-20',1600.00,300.00,30);
insert into emp (empno,ename,job,mgr,hiredate,sal,deptno) values
(7369,'smith','clerk',7902,'1980-12-27',800.00,20);

--�Ӵ�����ı���ʽ���ļ���ֱ��д�����ݿ⣬�磺
LOAD DATA LOCAL infile ��D:/pet.txt�� 
     INTO TABLE emp 
                lines terminated BY ��\r\n��
--ÿ�м�Ҫ�û��з�����

--����csv��ʽ���ļ�
load data infile 'C:\\Users\\UserName\\Desktop\\test.csv' fields terminated by ',' optionally enclosed by '"' escaped by '"' 
lines terminated by '\n'; 

LOAD DATA INFILE "C:\\Users\\public.public-PC\\pydata\\ch06\\ex6.csv"
 REPLACE INTO TABLE test
 FIELDS TERMINATED BY "," optionally ENCLOSED BY ""
 LINES TERMINATED BY "\r\n";

 
 create table test
 ( one float,
   two float,
   three float,
   four float,
   five char
 )engine=InnoDB;
 


--��ѯ������
select * from emp;
select * from dept;
--��ѯָ����
--select �ֶ�1���ֶ�2 from ���� where ����
--�磺��ѯsmith ��нˮ�����������ڲ���
select sal,job,deptno from emp where ename='smith';
--���ȡ���ظ���(distinctֻ��������ȫ��ͬ����)
--select distinct �ֶ� from ���� where ����
--�磺ͳ�ƹ��ж��ٸ����ű�ţ�
select distinct deptno from emp;
--ʹ���������ʽ
--��ʾÿ����Ա���깤��
select ename,sal*13 '�깤��' from emp;
--����ȷ�ı��(���Ͻ���),��δ���NULL����
select ename ,sal*13+isnull(comm*13,0) �깤�� from emp;
--ʹ��where�Ӿ�
--��ʾ���ʸ���3000��Ա��
select * from emp where sal>3000;
--����1982.1.1����ְ��Ա��
select * from emp where hiredate>'1982.1.1';
--��ʾ������2000��2500֮���
select * from emp where sal between 2000 and 2500;
select * from emp where sal>2000 and sal<2500;
--���ʹ��like��������ģ����ѯ����like�������������ʽ�������Ŀɰٶ��£�ʹ�ù��򶼴�ͬС��
--ֻ��ע�⣬�������^��$��Ϊ��ͷ�ͽ�β��ƥ��ʱ��likeҪ��д��regexp����
--��ʾ���ַ�ΪS��Ա�������͹���
select ename,sal from emp where ename like 'S%';
--��ʾ�������ַ�ΪO������Ա�������͹���
select ename,sal from emp where ename like '__O%';

--�����ʾempnoΪ123,345,800...�Ĺ�Ա���
--�������� select * from emp where empno=123 or empno=345 or empno=800
--��������Ч��̫�ͣ��������������һ���� in �ؼ���
select * from emp where empno in(123,345,800);

--is null ��ʹ��
--��ʾû���ϼ���Ա�����
select * from emp where mgr is null;

--ʹ���߼���������
-- ��ѯ���ʸ���500���߸�λΪMANAGER��Ա,ͬʱ��Ҫ����������������ĸΪJ
select * from emp where (sal>500 or job='manager') and ename like 'j%';

--order by �Ӿ�,Ĭ������,����ʱ�� order by desc
--�����ʴӵ͵��ߵ�˳����ʾ��Ա����Ϣ
select * from emp order by sal desc;
--������ְ���Ⱥ�˳������
select * from emp order by hiredate;
--���ź����򣬹��ʽ���,order by ���Ը��ݲ�ͬ���ֶ�����
select * from emp order by deptno,sal desc;
--ʹ���еı��������磺����н���������н�ӵ͵�������
select ename,sal*13+isnull(comm) as nianxin from emp order by nianxin;


--��ĸ��Ӳ�ѯ

--���ݷ��� max,min,avg,sum,count
--��ʾ��ߺ���͹���
select min(sal) from emp;
--�����ʾ�����Ϣ,�漰���Ӳ�ѯ
select * from emp where sal=(select min(sal) from emp);
--��ʾ����Ա����ƽ�����ʺ͹����ܺ�
select avg(sal) ƽ������,sum(sal) �ܹ��� from emp;
--�ҳ�����ƽ�����ʵĹ�Ա�����ֺ����Ĺ���
select ename,sal from emp where sal>(select avg(sal) from emp);
--�ҳ�����ƽ�����ʵĹ�Ա�����ֺ����Ĺ���,����ʾƽ������

--�������Ա��
select count(*) from emp;


--group by ���ڶԲ�ѯ���������ʾ
--having �Ӿ��������Ʒ�����ʾ���
--��ʾÿ�����ŵ�ƽ�����ʺ���߹���
select avg(sal) ƽ������, max(sal) ��߹���,deptno from emp group by deptno;
--��ʾÿ�����ŵ�ÿ�ָ�λ��ƽ�����ʺ���͹���
select avg(sal),min(sal),deptno,job from emp group by deptno,job order by deptno;
--��ʾƽ�����ʵ���2000�Ĳ��źź�����ƽ������
--having ������group by ���ʹ�ã������ԶԷ����ѯ�������ɸѡ
select avg(sal),deptno from emp group by deptno having avg(sal)<2000;
--��ʾƽ�����ʸ���2000�Ĳ��źź�����ƽ������,�����մӵ͵���
select avg(sal),deptno from emp group by deptno having avg(sal)>2000 order by avg(sal);

