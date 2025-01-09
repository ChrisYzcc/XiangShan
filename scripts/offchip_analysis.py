# Analysing the offchip load instruction

import sqlite3

db_path_list = ["./build/2025-01-09@15:53:08.db"]

latencies = []
for path in db_path_list:
    conn = sqlite3.connect(path)
    c = conn.cursor()

    # Get the data
    raw_data = c.execute("SELECT * FROM " + "LoadLatencyTable0" + ";").fetchall()

    for data in raw_data:
        latencies.append(data[1])

    # Close the connection
    c.close()
    conn.close()

from sklearn.cluster import KMeans
import numpy as np
latencies_np = np.array(latencies).reshape(-1, 1)

kmeans = KMeans(n_clusters=2, random_state=42).fit(latencies_np)
labels = kmeans.labels_

unique_labels, counts = np.unique(labels, return_counts=True)
for label, count in zip(unique_labels, counts):
    print(f"Cluster {label}: {count} elements")

print("Cluster Centers:")
print(kmeans.cluster_centers_)