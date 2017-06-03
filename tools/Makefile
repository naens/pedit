all:	clean trslt utf8test wdsep

trslt:
	gcc -g utf8conv.c trslt.c -o trslt -std=gnu99

utf8test:
	gcc -g utf8conv.c convtest.c -o convtest -std=gnu99

wdsep:
	gcc -g utf8conv.c wdsep.c -o wdsep -std=gnu99

clean:
	rm -rf trslt utf8test wdsep
