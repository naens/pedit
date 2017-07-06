CC=gcc
CFLAGS=-g -std=gnu99

all: pedit_init pedit_cli

pedit_init: create_db.h pedit_db.o 
	gcc -g pedit_init.c pedit_db.o -o pedit_init -lsqlite3 -std=gnu99

pedit_cli: pedit_cli.c pedit_db.o
#	gcc -g pedit_cli.c tools/utf8conv.c pedit_db.o -o pedit_cli -lsqlite3 -std=gnu99
	gcc -g pedit_cli.c                  pedit_db.o -o pedit_cli -lsqlite3 -std=gnu99

create_db.h:
	texthdr create_db.sql create_db.h create_db

clean:
	rm -rf pedit_init pedit_db.o create_db.h pedit_cli
