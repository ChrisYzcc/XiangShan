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
    DRAMSIM3_HOME=~/DRAMsim3 \
    make emu -j200 EMU_THREADS=16 EMU_TRACE=fst CONFIG=$(CONFIG) WITH_DRAMSIM3=1 | tee compile.log

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
	$(EMU) -i $(WORKLOAD_PATH) --diff $(DIFF_SO) --dump-wave-full 2>$(SIM_ERR) | tee $(SIM_OUT)

# SimPoint
PERF_PATH = ~/env-scripts/perf
GCPT_PATH = /nfs/home/share/liyanqin/spec06_rv64gcb_O3_20m_gcc12.2.0-intFpcOff-jeMalloc/checkpoint-0-0-0
JSON_PATH_1 = /nfs/home/share/liyanqin/spec06_rv64gcb_O3_20m_gcc12.2.0-intFpcOff-jeMalloc/checkpoint-0-0-0/cluster-0-0.json
JSON_PATH_p = $(NOOP_HOME)/scripts/simpoint_summary.json
JSON_PATH_3 = /nfs/home/share/liyanqin/env-scripts/perf/json/gcc12o3-incFpcOff-jeMalloc-0.3.json
JSON_PATH_8 = /nfs/home/share/liyanqin/env-scripts/perf/json/gcc12o3-incFpcOff-jeMalloc-0.8.json
#SERVER_LIST = open06 open07 open08 open09 open10 open12 open13 open14 open15
SERVER_LIST = node005 node006 node007 node008 node009 node027 node028 node042
XS_PATH = $(NOOP_HOME)

SIMPOPINT_RESDIR = ~/offchip-access/with-prefetch
simpoint:
#	-@rm -rf $(SIMPOPINT_RESDIR)
	$(PYTHON) $(PERF_PATH)/xs_autorun_multiServer.py $(GCPT_PATH) $(JSON_PATH_8) --xs $(XS_PATH) --threads 16 --dir $(SIMPOPINT_RESDIR) --resume -L "$(SERVER_LIST)"
	-@echo "SimPoint analysis done."