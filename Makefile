CC=gcc
CFLAGS=-g

all: clean pedit_init

pedit_init: pedit_init.c create_db.h pedit_db.o 
	gcc -g pedit_init.c pedit_db.o -o pedit_init -lsqlite3

create_db.h: tools
	tools/texthdr create_db.sql create_db.h create_db

clean:
	rm -rf pedit_init pedit_db.o

tools:
	$(MAKE) -C tools

