# Personal Makefile script for test and analysis
PYTHON = python3
EMU = ./build/emu
CONFIG = KunminghuV2Config

$(EMU):
	NOOP_HOME=$(shell pwd) \
	NEMU_HOME=$(shell pwd) \
	make emu -j9 EMU_THREADS=8 EMU_TRACE=1 CONFIG=$(CONFIG) | tee compile.log

gen: $(EMU)

WORKLOAD？= microbench
WORKLOAD_PATH？=
DIFF_SO = ./ready-to-run/riscv64-nemu-interpreter-so

SIM_OUT = ./rpt/simulation_out.txt
SIM_ERR = ./rpt/simulation_err.txt

ifeq ($(WORKLOAD), microbench)
	WORKLOAD_PATH = ./ready-to-run/microbench.bin
else ifeq ($(WORKLOAD), coremark)
	WORKLOAD_PATH = /nfs/home/share/ci-workloads/nexus-am-workloads/apps/coremark/coremark-riscv64-xs.bin
else
	echo "Unknown workload."
	exit
endif

run: $(EMU)
	-mkdir rpt
	$(EMU) -i $(WORKLOAD_PATH) --diff $(DIFF_SO) 2>$(SIM_ERR) | tee $(SIM_OUT)

$(SIM_OUT): run
$(SIM_ERR): run

# Performance
PERF_CSV = ./rpt/stats.csv
perf: $(SIM_ERR)
	-mkdir rpt
	$(PYTHON) ./scripts/perf.py $(SIM_ERR) -o $(PERF_CSV)