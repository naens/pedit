CC=gcc
CFLAGS=-g -std=gnu99

all: clean pedit_init pedit_cli

pedit_init: pedit_init.c create_db.h pedit_db.o 
	gcc -g pedit_init.c pedit_db.o -o pedit_init -lsqlite3 -std=gnu99

pedit_cli: pedit_cli.c pedit_db.o
	gcc -g pedit_cli.c pedit_db.o -o pedit_cli -lsqlite3 -std=gnu99

create_db.h: tools
	texthdr create_db.sql create_db.h create_db

clean:
	rm -rf pedit_init pedit_db.o

tools: tools-clean
	$(MAKE) -C tools
	$(MAKE) -C tools install

tools-clean:
	$(MAKE) -C tools clean
