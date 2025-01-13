# Analysing the MSHR occupancy of the DCache

import sqlite3

class MSHRData:
    MSHR_UNALLOC    = 0
    MSHR_PREFETCH   = 1
    MSHR_MIXED      = 2
    MSHR_OTHER      = 3

    def __init__(self):
        self.unalloc = 0
        self.prefetch = 0
        self.mixed = 0
        self.other = 0

    def __inc__(self, state):
        if state == MSHRData.MSHR_UNALLOC:
            self.unalloc += 1
        elif state == MSHRData.MSHR_PREFETCH:
            self.prefetch += 1
        elif state == MSHRData.MSHR_MIXED:
            self.mixed += 1
        elif state == MSHRData.MSHR_OTHER:
            self.other += 1

    def __str__(self):
        return "Unalloc: " + str(self.unalloc) + ", Prefetch: " + str(self.prefetch) + ", Mixed: " + str(self.mixed) + ", Other: " + str(self.other)

db_path_list = ["./build/2025-01-10@13:26:37.db"]

# Process each database
for path in db_path_list:
    conn = sqlite3.connect(path)
    c = conn.cursor()

    c.execute("SELEct * FROM MSHRStateTable")
    rows = c.fetchall()
    print(rows)

    # Close the connection
    c.close()
    conn.close()

