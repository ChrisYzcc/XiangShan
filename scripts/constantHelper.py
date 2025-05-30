import os
import random
from subprocess import Popen, PIPE
import psutil
import json
import sys
import math
import time
from datetime import datetime

# usage: python3 constantHelper.py JSON_FILE_PATH [BUILD_PATH]
# 
# an example json config file is as follow:
# visit https://bosc.yuque.com/yny0gi/gr7hyo/oy3dagqi9v97p696 for detail
# {
#     "constants": [
#         {
#             "name": "block_cycles_cache_0",
#             "width": 7,
#             "guide": 20,
#             "init": 11
#         },
#         {
#             "name": "block_cycles_cache_1",
#             "width": 7,
#             "init": 18
#         },
#         {
#             "name": "block_cycles_cache_2",
#             "width": 7,
#             "init": 127
#         },
#         {
#             "name": "block_cycles_cache_3",
#             "width": 7,
#             "init": 17
#         }
#     ],
#     "opt_target": [
#         {"successfully_forward_channel_D": {"policy" :"max", "baseline" :0} },
#         {"successfully_forward_mshr": {"policy" :"max", "baseline" :0} },
#         {"dcache.missQueue.entries_0: load_miss_penalty_to_use,": {"policy" :"min", "baseline" :250396} },
#         {"dcache.missQueue.entries_1: load_miss_penalty_to_use,": {"policy" :"min", "baseline" :5634} },
#         {"dcache.missQueue.entries_2: load_miss_penalty_to_use,": {"policy" :"min", "baseline" :4599} },
#         {"dcache.missQueue.entries_3: load_miss_penalty_to_use,": {"policy" :"min", "baseline" :4146} }
#     ],
    
#     "population_num": 50,
#     "iteration_num": 50,
#     "crossover_rate": 50,
#     "mutation_rate": 50,

#     "emu_threads": 16,
#     "concurrent_emu": 4,
#     "max_instr": 1000000,
#     "seed": 3888,
#     "work_load": "~/nexus-am/apps/maprobe/build/maprobe-riscv64-xs.bin"
# }


# parameters according to noop
NOOP_HOME = os.getenv("NOOP_HOME")
XS_PROJECT_ROOT = os.getenv("XS_PROJECT_ROOT")
if NOOP_HOME is None:
    print("Please set NOOP_HOME first.")
    exit(1)
if XS_PROJECT_ROOT is None:
    print("Please set XS_PROJECT_ROOT first.")
    exit(1)
DIFF_PATH = os.path.join(NOOP_HOME, "ready-to-run", "riscv64-nemu-interpreter-so")

# get arguments
if len(sys.argv) > 1:
    JSON_FILE_PATH=sys.argv[1]
else:
    print("Please specify the json file path")
    exit(1)
if len(sys.argv) > 2:
    BUILD_PATH = sys.argv[2]
else:
    BUILD_PATH = os.path.join(NOOP_HOME, "build")

EMU_PATH = os.path.join(BUILD_PATH, "emu")
CONFIG_FILE_PREFIX = ".constant_result_"
PERF_FILE_POSTFIX = "tmp"
MAXVAL = (1 << 63) - 1

class Constant:
    def __init__(self, obj: dict) -> None:
        self.name = obj['name']
        self.width = obj['width']
        self.guide = (1 << self.width - 1) - 1 if 'guide' not in obj.keys() else obj['guide']
        self.init = random.randint(0, self.guide) if 'init' not in obj.keys() else obj['init']
    def maxrange(self) -> int:
        return (1 << self.width) - 1


class Config:
    def __init__(self, constants, opt_target, population_num, iteration_num, crossover_rate, mutation_rate, emu_threads, concurrent_emu, max_instr, seed, work_load, tag) -> None:
        self.constants = constants
        self.opt_target = opt_target
        self.population_num = int(population_num)
        self.iteration_num = int(iteration_num)
        self.crossover_rate = int(crossover_rate)
        self.mutation_rate = int(mutation_rate)
        self.emu_threads = int(emu_threads)
        self.concurrent_emu = int(concurrent_emu)
        self.max_instr = int(max_instr)
        self.seed = int(seed)
        self.work_load = work_load
        self.tag = tag
    def get_ith_constant(self, i) -> Constant:
        return self.constants[i]
    def get_constain_num(self) -> int:
        return len(self.constants)


def loadConfig(json_path, tag) -> Config:
    obj = json.load(open(json_path, "r"))
    constants = [Constant(obj['constants'][i]) for i in range(len(obj['constants']))]
    config = Config(constants, obj['opt_target'], obj['population_num'], obj['iteration_num'], obj['crossover_rate'], obj['mutation_rate'], obj['emu_threads'], obj['concurrent_emu'], obj['max_instr'], obj['seed'], obj['work_load'], tag)
    return config

class RunContext:
    def __init__(self, config: Config) -> None:
        self.config = config
    def checkCoreFree(self) -> bool:
        percent_per_core = psutil.cpu_percent(interval=1 ,percpu=True)
        acc = 0
        for i in range(self.config.concurrent_emu * self.config.emu_threads):
            acc += percent_per_core[i]
        if acc < (0.1 * (100 * self.config.concurrent_emu * self.config.emu_threads)):
            return True
        else:
            print("no free {} core, core usage:".format(self.config.concurrent_emu * self.config.emu_threads))
            print(percent_per_core)
            return False
    def get_free_cores(self) -> tuple[bool, int, int, int]:
        thread = self.config.emu_threads
        # return (Success?, numa node, start_core, end_core)
        num_core = psutil.cpu_count(logical=False) # SMT is not allowed
        core_usage = psutil.cpu_percent(interval=2, percpu=True)
        num_window = num_core // thread
        for i in range(num_window):
            start = i * thread
            end = (i + 1) * thread
            window_usage = core_usage[start:end]
            free = sum(window_usage) < 30 * thread and True not in map(lambda x: x > 80, window_usage)
            if free:
                return (True, int(start >= (num_core // 2)), start, end - 1)
        return (False, 0, 0, 0)
    def getStdIn(self, population: list, id: int) -> str:
        res = 'echo \"'
        res += str(len(population[id]))
        res += '\\n'
        for item in population[id]:
            res += item[0] + ' ' + str(item[1]) + '\\n'
        res += '\"'
        return res

    def genRunCMD(self, population, id, numa = None, coreStart = None, coreEnd = None) -> str:
        stdinStr = self.getStdIn(population, id)
        if None in [numa, coreStart, coreEnd]:
            return "{} | {} -i {} --diff {} -I {} -s {}".format(stdinStr, EMU_PATH, self.config.work_load, DIFF_PATH, self.config.max_instr, self.config.seed)
        return "{} | numactl -m {} -C {}-{} {} -i {} --diff {} -I {} -s {}".format(stdinStr, numa, coreStart, coreEnd, EMU_PATH, self.config.work_load, DIFF_PATH, self.config.max_instr, self.config.seed)
    
    def getOutPath(self, iterid, i):
        dirPath = os.path.join(BUILD_PATH, self.config.tag)
        if not os.path.exists(dirPath):
            os.mkdir(dirPath)
        return os.path.join(dirPath, f"{iterid}-{i}-out.txt")

    def getPerfPath(self, iterid, i):
        # return os.path.join(BUILD_PATH, CONFIG_FILE_PREFIX + str(i) + '.' + PERF_FILE_POSTFIX)
        dirPath = os.path.join(BUILD_PATH, self.config.tag)
        if not os.path.exists(dirPath):
            os.mkdir(dirPath)
        return os.path.join(dirPath, f"{iterid}-{i}-err.txt")

class Solution:
    def __init__(self, config: Config) -> None:
        self.config = config
        self.context = RunContext(config)
    def genFirstPopulation(self) -> list:
        res = []
        used = []
        config = self.config
        for i in range(config.population_num):
            candidate = [[config.get_ith_constant(i).name, random.randint(0, config.get_ith_constant(i).maxrange()) % config.get_ith_constant(i).guide] for i in range(config.get_constain_num())]
            while(candidate in used):
                candidate = [[config.get_ith_constant(i).name, random.randint(0, config.get_ith_constant(i).maxrange()) % config.get_ith_constant(i).guide] for i in range(config.get_constain_num())]
            used.append(candidate)
            res.append(candidate)
        assert(len(res) == config.population_num)
        return res
    def profilling_fitness(self, iterid: int) -> list:
        fitness = []
        lines = []
        for idx in range(self.config.population_num):
            with open(self.context.getPerfPath(iterid, idx), "r") as fp:
                lines = fp.readlines()
                res = 0
                for line in lines:
                    for opt in config.opt_target:
                        if list(opt.keys())[0] in line:
                            # max and min policy
                            if list(opt.values())[0]['policy'] == 'max':
                                res += int(list(filter(lambda x: x != '', line.split(' ')))[-1]) - int(list(opt.values())[0]['baseline'])
                            elif list(opt.values())[0]['policy'] == 'min':
                                res += int(list(opt.values())[0]['baseline']) - int(list(filter(lambda x: x != '', line.split(' ')))[-1])
                fitness.append(res)
        assert(len(fitness) == self.config.population_num)
        return fitness
    def run_one_round(self, iterid: int, population: list) -> None:
        procs = []
        i = 0
        while i < len(population):
            if i % self.config.concurrent_emu == 0:
                for proc in procs:
                    proc.wait()
                procs.clear()
            print(population[i])
            # while True:
            #     (succ, numa, coreStart, coreEnd) = self.context.get_free_cores()
            #     if succ:
            #         with open(self.context.getOutPath(iterid, i), "w") as stdout, open(self.context.getPerfPath(iterid, i), "w") as stderr:
            #             # print(self.context.genRunCMD(population, i, numa, coreStart, coreEnd), flush=True)
            #             procs.append(Popen(args=self.context.genRunCMD(population, i, numa, coreStart, coreEnd), shell=True, encoding='utf-8', stdin=PIPE, stdout=stdout, stderr=stderr))
            #         break
            #     print("no free {} core".format(self.config.concurrent_emu * self.config.emu_threads))
            #     time.sleep(5)
            ## only for tutorial
            with open(self.context.getOutPath(iterid, i), "w") as stdout, open(self.context.getPerfPath(iterid, i), "w") as stderr:
                procs.append(Popen(args=self.context.genRunCMD(population, i), shell=True, encoding='utf-8', stdin=PIPE, stdout=stdout, stderr=stderr))
            i += 1
        for proc in procs:
            proc.wait()
    def mutation(self, item: list) -> list:
        res = []
        for val in item:
            width = 0
            guide = 0
            for constant in self.config.constants:
                if(constant.name == val[0]):
                    width = constant.width
                    guide = constant.guide
            mask = 1 << random.randint(0, width - 1)
            if random.randint(0, 100) > self.config.mutation_rate:
                res.append(val)
            else:
                val[1] = (((val[1] & mask) ^ mask) | val[1]) % guide
                res.append(val)
        assert(len(item) == len(res))
        return res
    def crossover(self, poplulation: list) -> list:
        res = []
        if len(poplulation) < 2:
            return poplulation
        for individual in poplulation:
            indivi = []
            for (index, constant) in enumerate(individual):
                const = constant
                if random.randint(0, 100) < self.config.crossover_rate:
                    crossover_target_id = 0
                    while crossover_target_id == index:
                        crossover_target_id = random.randint(0, len(poplulation) - 1)
                    maskMax = 0
                    guide = 0
                    for config_const in self.config.constants:
                        if config_const.name == constant[0]:
                            maskMax = config_const.width
                            guide = config_const.guide
                    maskMax = int(math.log2(guide)) + 1 if (int(math.log2(guide)) + 1 < maskMax) else maskMax
                    maskLen = random.randint(1, maskMax)
                    mask = (1 << maskLen) - 1
                    shiftLen = random.randint(0, maskMax - maskLen)
                    mask = mask << shiftLen
                    const_now = const[1]
                    target_now = poplulation[crossover_target_id][index][1]
                    const_now = ((const_now & ~(mask)) | (target_now & mask)) % guide
                    const = [constant[0], const_now]
                indivi.append(const)
            res.append(indivi)
        assert(len(poplulation) == len(res))
        return res
    def genNextPop(self, curPop, fitness) -> list:
        nextgen = []
        tmp = sorted(zip(curPop, fitness), key=lambda x : x[1], reverse=True)
        print()
        print("opt constant in this round is ", list(tmp)[0][0], " fitness is ", int(list(tmp)[0][1]))
        cross = []
        for i in range(len(tmp)):
            if i < (len(tmp) // 2):
                # select 
                nextgen.append(tmp[i][0])
            else:
                cross.append(tmp[i][0])
        # crossover
        cross = self.crossover(cross)
        nextgen = nextgen + cross
        # mutation
        for i in range(len(tmp)):
            nextgen[i] = self.mutation(nextgen[i])
        assert(len(curPop) == len(nextgen))
        return nextgen
    
    class HashList:
        def __init__(self, obj: list) -> None:
            # obj: [['test1', 38], ['test2', 15]]
            self.obj = obj
        def __hash__(self) -> str:
            res = ''
            for const in self.obj:
                res += ' '.join(map(lambda x : str(x), const))
            return hash(res)
        def __eq__(self, __o: object) -> bool:
            for (idx, const) in enumerate(self.obj):
                if const != __o.obj[idx]:
                    return False
            return True

    def gene_cal(self) -> None:
        globalMap = dict()
        if(self.config.population_num % 2 != 0):
            print("gene algrithom must ensure that population_num is an even value")
            return
        parentPoplation = self.genFirstPopulation()
        init_indiv = []
        for constant in self.config.constants:
            const = []
            const.append(constant.name)
            const.append(constant.init)
            init_indiv.append(const)
        parentPoplation.pop()
        parentPoplation.append(init_indiv)
        for i in range(self.config.iteration_num):
            if i != 0:
                print()
            print("iteration ", i, " begins")
            print()
            self.run_one_round(i, parentPoplation)
            fitness = self.profilling_fitness(i)
            for (pop, fit) in zip(parentPoplation, fitness):
                globalMap[self.HashList(pop)] = fit
            parentPoplation = self.genNextPop(parentPoplation, fitness)

        globalMap = zip(globalMap.keys(), globalMap.values())
        globalMap = sorted(globalMap, key=lambda x : x[1], reverse=True)
        print("opt constant for gene algrithom is ", list(globalMap)[0][0].obj, " fitness", int(list(globalMap)[0][1]))

tid = datetime.now().strftime("%m%d%H%M")
config = loadConfig(JSON_FILE_PATH, f"constantin_{tid}")
Solution(config).gene_cal()
