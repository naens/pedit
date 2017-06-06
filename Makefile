CC=gcc
CFLAGS=-g

pedit-init: pedit-init.c create_db.h pedit_db.o 
	gcc -g pedit-init.c -o pedit-init -lsqlite3

create_db.h: tools
	tools/texthdr create_db.sql create_db.h create_db

tools:
	$(MAKE) -C tools
