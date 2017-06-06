CC=gcc
CFLAGS=-g

db: create_db pedit_db.o 

create_db: tools
	tools/texthdr create_db.sql create_db.h create_db

tools:
	$(MAKE) -C tools
