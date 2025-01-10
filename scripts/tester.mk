NOOP_HOME = $(shell pwd)

# Personal Makefile script for test and analysis
PYTHON = python3
EMU = $(NOOP_HOME)/build/emu
CONFIG = KunminghuV2Config

$(EMU):
	NOOP_HOME=$(shell pwd) \
	NEMU_HOME=$(shell pwd) \
	make emu -j9 EMU_THREADS=16 EMU_TRACE=1 CONFIG=$(CONFIG) | tee compile.log

gen: $(EMU)

gen_dram:
	NOOP_HOME=$(shell pwd) \
    NEMU_HOME=$(shell pwd) \
    DRAMSIM3_HOME=$(NOOP_HOME)/../DRAMsim3 \
    make emu -j200 EMU_THREADS=16 EMU_TRACE=1 CONFIG=$(CONFIG) WITH_DRAMSIM3=1 | tee compile.log

WORKLOAD ?= microbench
WORKLOAD_PATH ?=
DIFF_SO = $(NOOP_HOME)/ready-to-run/riscv64-nemu-interpreter-so

SIM_OUT = $(NOOP_HOME)/rpt/simulation_out.txt
SIM_ERR = $(NOOP_HOME)/rpt/simulation_err.txt

ifeq ($(WORKLOAD), microbench)
	WORKLOAD_PATH = $(NOOP_HOME)/ready-to-run/microbench.bin
else ifeq ($(WORKLOAD), coremark)
	WORKLOAD_PATH = /nfs/home/share/ci-workloads/nexus-am-workloads/apps/coremark/coremark-riscv64-xs.bin
else
	echo "Unknown workload."
	exit
endif

run: $(EMU)
	-@mkdir rpt
	-@rm $(NOOP_HOME)/rpt/*.txt
	$(EMU) -i $(WORKLOAD_PATH) --dump-db --diff $(DIFF_SO) 2>$(SIM_ERR) | tee $(SIM_OUT)

# Performance
PERF_CSV = ./rpt/stats.csv
perf:
	-@mkdir rpt
#	$(PYTHON) ./scripts/perf.py $(SIM_ERR) -o $(PERF_CSV)
	$(PYTHON) ./scripts/mshr_analysis.py

# SimPoint
PERF_PATH = $(NOOP_HOME)/../env-scripts/perf
GCPT_PATH = /nfs/home/share/liyanqin/spec06_rv64gcb_O3_20m_gcc12.2.0-intFpcOff-jeMalloc/checkpoint-0-0-0
JSON_PATH = $(NOOP_HOME)/scripts/simpoint_summary.json
XS_PATH = $(NOOP_HOME)

simpoint:
	-@rm -rf $(NOOP_HOME)/SPEC06_EmuTasks/
	-@rm -rf $(NOOP_HOME)/rpt/dcache_mshr-*.log
	$(PYTHON) $(PERF_PATH)/xs_autorun_multiServer.py $(GCPT_PATH) $(JSON_PATH) --xs $(XS_PATH) --threads 16 --dir SPEC06_EmuTasks --resume