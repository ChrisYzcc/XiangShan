# Analysing the MSHR occupancy of the DCache

import re

pattern = r"Cycle (\d+): Unalloc: (\d+), Prefetch: (\d+), Mixed: (\d+), Other: (\d+)"

res = []
# Read the file
with open("./rpt/dcache_mshr.log", "r") as file:
    for line in file:
        match = re.match(pattern, line)
        if match:
            cycle, unalloc, prefetch, mixed, other = map(int, match.groups())
            res.append((cycle, unalloc, prefetch, mixed, other))

# Calculate the occupation rates
index = []
unalloc_rates = []
prefetch_rates = []
mixed_rates = []
other_rates = []
for entry in res:
    cycle, unalloc, prefetch, mixed, other = entry
    total = unalloc + prefetch + mixed + other
    index.append(cycle)
    unalloc_rates.append(unalloc / total)
    prefetch_rates.append(prefetch / total)
    mixed_rates.append(mixed / total)
    other_rates.append(other / total)

# Plot the results
import matplotlib.pyplot as plt
plt.plot(index, unalloc_rates, label="Unallocated", color="blue")
plt.plot(index, prefetch_rates, label="Prefetch", color="green")
plt.plot(index, mixed_rates, label="Mixed", color="red")
plt.plot(index, other_rates, label="Other", color="black")
plt.xlabel("Cycle")
plt.ylabel("Occupancy rate")
plt.legend()
plt.show()
plt.savefig("./rpt/dcache_mshr.png")