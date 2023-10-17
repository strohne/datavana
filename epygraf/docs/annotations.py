import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

import epygraf as epi

from sklearn.cluster import KMeans
from sklearn.metrics import pairwise_distances

import datetime

# import importlib
# importlib.reload(epi)

# %% Load annotations from the database
# Note: after the first run, you can read the result in the next code cell

annos = epi.db.annotations(
    'epi_all',
    'items',
    {
        'articletype': ['dio-article'],
        'propertytype': ['fonttypes', 'objecttypes', 'texttypes']
    }
)

# Save to csv for later use
annos.to_csv("dio-annotations-selected.csv", index=False)

# %% Read annotations from csv
annos = pd.read_csv("dio-annotations-selected.csv")

# %% Show the top 10 properties per propertytype

# Count properties by propertytype
annos_terms = annos \
    .filter(['properties_id', 'propertytype', 'name']) \
    .value_counts() \
    .reset_index(name='n') \
    .sort_values(by='n', ascending=False) \
    .assign(label = lambda rows: rows['name'] + ' (' + rows['properties_id'].astype(str) + ')')

# Plot the first 10 properties for each propertytype
sns.catplot(
    x='n',
    y='label',
    hue='propertytype',

    data= annos_terms.groupby('propertytype').apply(lambda rows: rows.head(10)),

    kind="bar",
    height=6,
    aspect=2
)

plt.show()

#%% Prepare cluster data

# Select sample with the top terms
annos_topterms = annos_terms.groupby('propertytype').apply(lambda rows: rows.head(200))
annos_sample = annos[annos.properties_id.isin(annos_topterms.properties_id)]

# Dummy code properties
annos_dummies = pd \
    .get_dummies(
        annos_sample[['articles_id','properties_id']],
        columns=['properties_id'],
        sparse=True
    ) \
    .set_index('articles_id')

#%% Find number of clusters

# Caution: takes long for many terms (decrease top terms to 10 or 50, see above)

# Caluclate SSE
sse = {}
for k in range(1, 15):
    print(f"Cluster with k={k}")
    sse[k] = KMeans(
            n_clusters=k,
            n_init=20,
            random_state=42
        ) \
        .fit(annos_dummies) \
        .inertia_

# Plot SSE
plt.figure()
plt.plot(list(sse.keys()), list(sse.values()))
plt.xlabel("Number of clusters")
plt.ylabel("SSE")
plt.show()



#%% Option 1: Cluster data without precomputing the distance matrix
# See next cells for computing the distance matrix first

# TODO: use DBSCAN instead of KMeans

# Create a K-Means model for 6 clusters
kmeans = KMeans(
    n_clusters=6,
    n_init=20,
    random_state=42
)

# Get clusters
kmeans_fit = kmeans.fit_predict(annos_dummies)


#%% Option 2: Calculate distance matrix

# Caution: Takes 2 hours on an 4 core CPU at 4.5 Ghz (Intel Core i7-7700K@4.20Ghz) for 64k cases and 600 features
print(datetime.datetime.now())
annos_distances = pairwise_distances(annos_dummies.to_numpy(),metric="jaccard",n_jobs=-1)
print(datetime.datetime.now())

#%% Option 2: Save distance matrix

# Caution: Results in a 20GB file for 64k cases
# TODO: use sparse format
np.savetxt('annos_distances_top200_jaccard.out', annos_distances, fmt = '%.6f')

#%% Option 2: Save distance matrix

# TODO: Test whether this is more efficient for storing the numpy distance matrix
# See https://github.com/mverleg/array_storage_benchmark/
from numpy import frombuffer
def binary_save(arr, pth):
    with open(pth, 'wb+') as fh:
        fh.write('{0:} {1:} {2:}\n'.format(arr.dtype, arr.shape[0], arr.shape[1]).encode('ascii'))
        fh.write(arr.data)

def binary_load(pth):
    with open(pth, 'rb') as fh:
        header = fh.readline()
        data = fh.read()
    dtype, w, h = header.decode('ascii').strip().split()
    return frombuffer(data, dtype=dtype).reshape((int(w), int(h)))

binary_save(annos_distances, "annos_distances_top200_jaccard.bin")
#%% Option 2: Cluster with precomputed distance matrix

# TODO: test code
kmeans = KMeans(
    n_clusters=6,
    precompute_distances=False,
    random_state=42
)

# Get clusters
kmeans_fit = kmeans.fit_predict(annos_dummies, sample_weight=annos_distances)

#%% Add cluster numbers to the annotations dataset

annos_cluster = pd.DataFrame(
    {
        'articles_id': annos_dummies.index,
        'cluster_no': kmeans_fit.astype(str)
    }
)

annos_cluster = pd.merge(annos_sample, annos_cluster, how='inner', on='articles_id')

#%% Print cluster sizes (articles)

print(
    annos_cluster \
        .filter(['articles_id','cluster_no']) \
        .drop_duplicates() \
        .filter(['cluster_no']) \
        .value_counts() \
        .reset_index() \
        .sort_values(by='cluster_no')
)

#%% Count properties by cluster

# n
annos_cluster_n = annos_cluster \
    .filter(['cluster_no','properties_id', 'propertytype', 'name']) \
    .value_counts() \
    .reset_index(name='n') \
    .assign(label = lambda rows: rows['name'] + ' (' + rows['properties_id'].astype(str) + ')')

# tfidf for clusters as documents and all their properties as terms,
# for finding distinctive properties
document_count = annos_cluster_n['cluster_no'].nunique()
term_document_count = annos_cluster_n.groupby('properties_id')['cluster_no'].nunique()

annos_cluster_n['tf'] = annos_cluster_n \
    .groupby('cluster_no')['n'] \
    .transform(lambda x: x / x.sum())


annos_cluster_n['idf'] = annos_cluster_n['properties_id'] \
    .map(lambda x: document_count / term_document_count[x]) \
    .apply(lambda x: 0 if x == 0 else -1 * (x + 1))

annos_cluster_n['tfidf'] = annos_cluster_n['tf'] * annos_cluster_n['idf']

# TODO: distinct articles and propertytypes?

#%% Plot top distinctive terms per cluster

tfidf_topterms = annos_cluster_n \
    .sort_values(by='tfidf', ascending=True) \
    .groupby('cluster_no').apply(lambda rows: rows.head(10))

annos_cluster_n[annos_cluster_n.properties_id.isin(tfidf_topterms.properties_id)] \
    .filter(['label','tf', 'cluster_no']) \
    .pivot(index='label', columns='cluster_no',values='tf') \
    .plot.barh(stacked=True)

plt.tight_layout()
plt.show()

# TODO: silhouette plot
# TODO: multidimensional scaling
