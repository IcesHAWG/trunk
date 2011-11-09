RES_DIR=./results
ASS_FILES= NSH_SAM_assessment.pdf
LOOI_FILES= LOOI_run.pdf
BINDINGS_FILES = HERAS_bindings_scan.pdf IBTS_bindings_scan.pdf Catchability_scan.pdf 
ALL_FILES= $(BINDINGS_FILES) $(ASS_FILES) $(LOOI_FILES)
PLOT_FILES_PATH = $(addprefix $(RES_DIR)/,$(ALL_FILES))
R=/usr/local/bin/R --slave --vanilla

all: $(PLOT_FILES_PATH)

assessment: $(addprefix $(RES_DIR)/,$(ASS_FILES))

scans: $(addprefix $(RES_DIR)/,$(BINDINGS_FILES))

looi: $(addprefix $(RES_DIR)/,$(LOOI_FILES))
       	

$(RES_DIR)/$(ASS_FILES):
	$R -f NSH_SAM.r > $(RES_DIR)/ass.log

$(RES_DIR)/HERAS_bindings_scan.pdf:
	$R -f Scan_HERAS_bindings.r > $(RES_DIR)/HERAS_bindings.log

$(RES_DIR)/IBTS_bindings_scan.pdf:
	$R -f Scan_IBTS_bindings.r > $(RES_DIR)/IBTS_bindings.log 	

$(RES_DIR)/Catchability_scan.pdf:
	$R -f Scan_catchability_binding.r > $(RES_DIR)/Catchability.log

$(RES_DIR)/Obs_var_scan.pdf:
	$R -f Scan_obs_var_binding.r  > $(RES_DIR)/Obs_var.log

$(RES_DIR)/LOOI_run.pdf:
	$R -f Looi_run.r > $(RES_DIR)/LOOI.log	


