create table article(id int auto_increment primary key, title varchar(1024), bodyText text);
create table users(id int auto_increment primary key, login varchar(256) not null, password varchar(256) not null, roles varchar(1024));
insert into users(login, password, roles) values('root',md5('root'),'admin');
