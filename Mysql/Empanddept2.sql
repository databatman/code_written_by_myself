--EmpAndDept2
--�򵥵ı���ѯ

--����ѯ
--��Ҫ˼��������
--��ͨ��ʲô���������ű���ϵ����
--��ʾ���۲��ŵ�ַ��Ա������
--������ű�����ͬ���ֵ��ֶ�,����Ҫ������(Ҳ�����Ǳ���)
select ename,sal,loc,emp.deptno from emp ,dept where dept.dname='sales' and emp.deptno=dept.deptno;
select ename,sal,loc,e.deptno from emp as e ,dept as d where d.dname='sales' and e.deptno=d.deptno;
--��ʾ���ź�Ϊ10 �Ĳ�����,Ա�����͹���
select d.dname,e.ename,e.sal from emp e,dept d 
where e.deptno=10 and e.deptno=d.deptno;
--��ʾ��Ա��,��Ա���ʼ����ڲ��ŵ�����,������������
select d.dname,e.ename,e.sal from emp e,dept d 
where e.deptno=d.deptno order by d.dname;


--������:��ͬһ�ű��ϵ����Ӳ�ѯ
--��ʾĳ��Ա�����ϼ��쵼������ ���� ford
--1.֪��ford�ϼ����
select ename from emp 
where empno=(select mgr from emp where ename='ford');
--��ʾ��˾ÿ��Ա�����ֺ������ϼ�����
--����,��emp�������ű�,�ֱ���worker boss
select worker.ename ��Ա,boss.ename �ϰ� from emp worker,emp boss
where worker.mgr=boss.empno;
--�����ӣ��������Ӻ��������ӣ���EmpAndDept3�����н���
--�Ӳ�ѯ��Ƕ�뵽����sql����е�select��䣬Ҳ��Ƕ�ײ�ѯ
--�����Ӳ�ѯ���� ��ʾ��smithͬһ���ŵ�����Ա��
select * from emp where deptno=
(select deptno from emp where ename='smith');
--�����Ӳ�ѯ
-- �� ��ѯ�Ͳ���10�Ĺ�����ͬ�Ĺ�Ա������,��λ,����,���ź�
select distinct job from emp where deptno=10;
select * from emp where job in
(select distinct job from emp where deptno=10);
--����ų�10�����Լ�
select * from emp where (job in
(select distinct job from emp where deptno=10)) and deptno!=10;
--��from�Ӿ���ʹ���Ӳ�ѯ







