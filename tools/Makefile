mkfile_path := $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
all:	clean trslt utf8test wdsep texthdr

trslt:
	gcc -g utf8conv.c trslt.c -o trslt -std=gnu99

utf8test:
	gcc -g utf8conv.c convtest.c -o convtest -std=gnu99

wdsep:
	gcc -g utf8conv.c wdsep.c -o wdsep -std=gnu99

texthdr:
	gcc texthdr.c -o texthdr

clean:
	rm -rf trslt utf8test wdsep

install:
	mkdir -pv ~/bin
	cp -v $(mkfile_path)/trslt $(mkfile_path)/wdsep $(mkfile_path)/texthdr ~/bin
