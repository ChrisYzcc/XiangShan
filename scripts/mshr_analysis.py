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

db_path_list = ["./build/2025-01-07@13:52:41.db"]

# Process each database
for path in db_path_list:
    conn = sqlite3.connect(path)
    c = conn.cursor()

    mshr_datas = {}
    # Get the data
    for i in range(0, 15):
        raw_data = c.execute("SELECT * FROM " + "mshrStateTable" + str(i) + ";").fetchall()

        for data in raw_data:
            timestamp = data[2]

            if timestamp not in mshr_datas:
                mshr_datas[timestamp] = MSHRData()
            
            mshr_datas[timestamp].__inc__(data[1])

    for key in mshr_datas:
        print("Timestamp: " + str(key) + ", " + mshr_datas[key].__str__())

    # Close the connection
    c.close()
    conn.close()

