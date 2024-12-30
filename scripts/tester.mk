# Personal Makefile script for test and analysis
PYTHON = python3
EMU = ./build/emu

$(EMU):
	make emu -j9 EMU_THREADS=8 EMU_TRACE=1 CONFIG=MinimalConfig | tee compile.log

WORKLOAD = ./ready-to-run/microbench.bin
DIFF_SO = ./ready-to-run/riscv64-nemu-interpreter-so

SIM_OUT = ./rpt/simulation_out.txt
SIM_ERR = ./rpt/simulation_err.txt

run: $(EMU)
	-mkdir rpt
	$(EMU) -i $(WORKLOAD) --diff $(DIFF_SO) 2>$(SIM_ERR) | tee $(SIM_OUT)

$(SIM_OUT): run
$(SIM_ERR): run


# Performance
PERF_CSV = ./rpt/stats.csv
perf: $(SIM_ERR)
	-mkdir rpt
	$(PYTHON) ./scripts/perf.py $(SIM_ERR) -o $(PERF_CSV)