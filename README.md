# mysql_to_mnesia
change databases from mysql db to mnesia db
# 转换mysql到erlang的mnesia库下面（表结构和数据）

主函数说明
m2m:mysql_to_mnesia/1
%参数为mysql数据库名
%功能是把该数据库下面的所有表都同步到mnesia

m2m:mysql_to_mnesia/2
%参数为mysql数据库名和表名
%功能是把该数据库下面的某个表同步到mnesia


#erlc -o ebin/ src/*.erl

#erlc -o deps/erlang-mysql-driver/ebin/  deps/erlang-mysql-driver/src/*.erl

#erl -pa ebin/ -pa deps/*/ebin

1> mysql_db:start_link().                             	%连接mysql数据库

{ok,<0.34.0>}

2> m2m:mysql_to_mnesia(test, product).               	%生成同步test数据库的product表的文件

ok

3> c("src/m2m_db_all").                             	%编译生成的文件

{ok,m2m_db_all}

4> m2m_db_all:start().                             	%开始同步数据到mnesia

ok

注意

1、目前只支持linux系统运行

2、每次同步会在src目录生成新的同步代码文件，旧的文件会备份到src目录

3、不会同步数据库，只同步数据库下面的表（如果没有mnesia库，会在当前目前新建一个默认的mnesia库）

