DD = ../data
SD = src
RD = run
BR = baserun
PD = res
CD = conf

.PHONY = all compile clean testrun plots updatebase

all: compile 

clean: 
	rm -f $(RD)/*
	rm -f $(BR)/*
	rm -f $(PD)/*
	rm -f $(SD)/*~ 	*~ $(CD)/*~
	rm -f Rplots.ps

compile: $(RD)/ssass

$(RD)/ssass: $(SD)/ssass.tpl
	cd $(SD); admb -r -s ssass; rm ssass.cpp ssass.o ssass.htp; 
	mv $(SD)/ssass $(RD)/

testrun: $(RD)/ssass.std

updatebase: $(RD)/ssass.std
	cp $(RD)/* $(BR)/

$(RD)/ssass.std: $(RD)/ssass $(CD)/model.init $(CD)/model.cfg $(CD)/reduced.cfg $(SD)/datascript.R
	cp $(CD)/model.* $(RD)/
	cp $(CD)/reduced.cfg $(RD)/
	cd $(SD); echo 'source("datascript.R")' | R --vanilla --slave; cd -; 
	cd $(RD); ./ssass -nr 2 -noinit -iprint 1; cd -;


plots: $(PD)/Rplot001.png

$(PD)/Rplot001.png: $(RD)/ssass.std $(BR)/ssass.std $(CD)/plotscript.R $(CD)/common.R
	echo 'source("$(CD)/plotscript.R")' | R --vanilla --slave;
