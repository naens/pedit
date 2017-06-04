#!/bin/sh
# example of transliteration of text and word separation
cat tmp.txt | ./trslt 2uc.txt | ./wdsep seps.txt > nodelist.txt
