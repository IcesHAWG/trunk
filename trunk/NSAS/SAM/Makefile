#Directories
SrcDir = src
RunDir = run
BaseDir = baserun
OutDir = outputs
ConfigDir = conf

#Files
ADMB = ssass
DATASCRIPT = $(SrcDir)/datascript.R
PLOTSCRIPT = $(SrcDir)/plotscript.R
COMMONSCRIPT = $(SrcDir)/common.R

.PHONY = all admb clean run plots updatebase

all: admb 

clean: 
	rm -f $(RunDir)/*
	rm -f $(BaseDir)/*
	rm -f $(OutDir)/*
	rm -f $(SrcDir)/*~ 	*~ $(ConfigDir)/*~
	rm -f Rplots.ps

admb: $(RunDir)/$(ADMB)

$(RunDir)/ssass: $(SrcDir)/$(ADMB).tpl
	cd $(RunDir); ln -sf ../$(SrcDir)/$(ADMB).tpl . 
	cd $(RunDir); admb -r -s $(ADMB); rm $(ADMB).cpp $(ADMB).o $(ADMB).htp; 

run: $(RunDir)/$(ADMB).std

updatebase: $(RunDir)/$(ADMB).std
	cp $(RunDir)/* $(BaseDir)/

$(RunDir)/$(ADMB).std: $(RunDir)/$(ADMB) $(ConfigDir)/model.init $(ConfigDir)/model.cfg $(ConfigDir)/reduced.cfg $(DATASCRIPT)
	cp $(ConfigDir)/model.* $(RunDir)/
	cp $(ConfigDir)/reduced.cfg $(RunDir)/
	cd $(RunDir);  R --vanilla --slave < ../$(DATASCRIPT); cd -; 
	cd $(RunDir); ./$(ADMB) -nr 2 -noinit -iprint 1 ; cd -;
	@echo "=====Fit Complete====="


plots: $(OutDir)/Rplots.pdf

$(OutDir)/Rplots.pdf: $(RunDir)/ssass.std $(BaseDir)/ssass.std $(PLOTSCRIPT) $(COMMONSCRIPT)
	R --vanilla --slave < $(PLOTSCRIPT)
