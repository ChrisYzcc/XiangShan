# Analysing the offchip load instruction

import sqlite3

db_path_list = ["./build/2025-01-08@18:11:21.db"]

for path in db_path_list:
    conn = sqlite3.connect(path)
    c = conn.cursor()

    c.execute(f"PRAGMA table_info({'LoadInstDelaysTable0'});")
    print(c.fetchall())

    # Get the data
    raw_data = c.execute("SELECT * FROM " + "LoadInstDelaysTable0" + ";").fetchall()

    print(raw_data)

    # Close the connection
    c.close()
    conn.close()