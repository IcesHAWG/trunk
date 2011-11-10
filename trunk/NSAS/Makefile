RES_DIR=./results
ASS_FILE= NSH_SAM.pdf
LOOI_FILE= LOOI_run.pdf
RETRO_FILE= Retrospective.pdf
SCAN_FILES = $(patsubst %.r,%.pdf,$(wildcard Scan_*))

ALL_FILES= $(BINDINGS_FILES) $(ASS_FILE) $(LOOI_FILE) $(RETRO_FILE)
PLOT_FILES_PATH = $(addprefix $(RES_DIR)/,$(ALL_FILES))
R=/usr/local/bin/R --slave --vanilla

all: $(PLOT_FILES_PATH)

assessment: $(RES_DIR)/$(ASS_FILE)

scans: $(addprefix $(RES_DIR)/,$(SCAN_FILES))

looi: $(RES_DIR)/$(LOOI_FILE)

retro: $(RES_DIR)/$(RETRO_FILE)
       	

$(RES_DIR)/%.pdf: %.r
	$R -f $< > $(RES_DIR)/$*.log

clean:
	rm -f *~

clean_all:
	rm -f $(RES_DIR)/*

variables:
	@echo "SCAN_FILES : $(SCAN_FILES)"

