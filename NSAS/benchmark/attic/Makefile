SRC_DIR=./benchmark
RES_DIR=$(SRC_DIR)/resultsSAM
STEP2_CFG  = $(SRC_DIR)/02_Setup_All_in_runs.r
STEP2_SRCS = $(wildcard $(SRC_DIR)/02?_*.r)
STEP2_PDFS = $(patsubst $(SRC_DIR)/%.r,$(RES_DIR)/%.pdf,$(STEP2_SRCS))
STEP2_FIRST= $(filter $(RES_DIR)/02a%,$(STEP2_PDFS))
STEP2_OTHERS= $(filter-out $(STEP2_FIRST), $(STEP2_PDFS))

STEP3_CFG  = $(SRC_DIR)/03_Setup_selected_surveys.r
STEP3_SRCS = $(wildcard $(SRC_DIR)/03?_*.r)
STEP3_PDFS = $(patsubst $(SRC_DIR)/%.r,$(RES_DIR)/%.pdf,$(STEP3_SRCS))
STEP3_FIRST= $(filter $(RES_DIR)/03a%,$(STEP3_PDFS))
STEP3_OTHERS= $(filter-out $(STEP3_FIRST), $(STEP3_PDFS))

STEP4_SRCS = $(wildcard $(SRC_DIR)/04?_*.r)
STEP4_PDFS = $(patsubst $(SRC_DIR)/%.r,$(RES_DIR)/%.pdf,$(STEP4_SRCS))

STEP5_SRCS = $(wildcard $(SRC_DIR)/05?_*.r)
STEP5_PDFS = $(patsubst $(SRC_DIR)/%.r,$(RES_DIR)/%.pdf,$(STEP5_SRCS))

R=R --slave --vanilla

all:
	@echo "No target for all. Try 'make step2', 'make step3', or 'make step4'"

step2: $(STEP2_OTHERS)

$(STEP2_OTHERS): $(STEP2_FIRST) $(STEP2_CFG)

$(STEP2_FIRST): $(STEP2_CFG)

step3: $(STEP3_OTHERS)

$(STEP3_OTHERS): $(STEP3_FIRST) 

$(STEP3_FIRST): $(STEP3_CFG) $(STEP2_CFG)

step4: $(STEP4_PDFS)

step5: $(STEP5_PDFS)

key: $(STEP2_FIRST) $(STEP2_CFG) $(STEP3_FIRST)


$(RES_DIR)/%.pdf: $(SRC_DIR)/%.r
	$R -f $< > $(RES_DIR)/$*.log 2>&1

clean:
	find . -name "*~" -print -delete
	-rm nohup.out

clean_all:
	rm -f $(RES_DIR)/*

variables:
	@echo "STEP2_SRCS : $(STEP2_SRCS)"
	@echo "STEP2_PDFS : $(STEP2_PDFS)"
	@echo "STEP2_FIRST : $(STEP2_FIRST)"
	@echo "STEP2_OTHERS : $(STEP2_OTHERS)"

