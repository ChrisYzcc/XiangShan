# Analysing the MSHR occupancy of the DCache

import sqlite3
import argparse
import matplotlib.pyplot as plt

# Parse Arguments
parser = argparse.ArgumentParser()
parser.add_argument("--db_path", nargs='+', type=str, help="Path to the database file", required=True)

args = parser.parse_args()

# Process each database
for path in args.db_path:
    conn = sqlite3.connect(path)
    c = conn.cursor()

    c.execute("PRAGMA table_info(MSHRStateTable);")
    columns = [row[1] for row in c.fetchall()]
    print(columns)

    c.execute("SELECT * FROM MSHRStateTable")
    rows = c.fetchall()
    
    x = []
    other = []
    mixed = []
    prefetch = []

    for row in rows:
        x.append(row[0])
        other.append(row[1])
        mixed.append(row[2])
        prefetch.append(row[3])

    plt.plot(x, other, label='Other')
    plt.plot(x, mixed, label='Mixed')
    plt.plot(x, prefetch, label='Prefetch')
    plt.xlabel('Cycle')
    plt.ylabel('Occupancy')
    plt.legend()
    plt.title('MSHR Occupancy')
    plt.savefig(path + '.png')

