import pandas as pd
import numpy as np
import epygraf as epi
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.cluster import KMeans, DBSCAN

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
annos_topterms = annos_terms.groupby('propertytype').apply(lambda rows: rows.head(50))
annos_sample = annos[annos.properties_id.isin(annos_topterms.properties_id)]

#%%

# annos_sample.reset_index()
# matrix = annos_sample[['articles_id']].\
#     join(pd.get_dummies(annos_sample['properties_id'], dtype=int)).\
#     groupby('articles_id').\
#     max()

#%%
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


#%% Cluster data
# TODO: use Jaccard metric
# TODO: use DBSCAN instead of KMeans

# Create a K-Means model for 6 clusters
kmeans = KMeans(
    n_clusters=6,
    n_init=20,
    random_state=42
)

# Get clusters
kmeans_fit = kmeans.fit_predict(annos_dummies)
print("Ready")

#%%
from sklearn.metrics import pairwise_distances

jaccard_distances = pairwise_distances(annos_dummies.to_numpy(), metric='jaccard',n_jobs=-1)

#%%
kmeans = KMeans(n_clusters=6, init='k-means++', precompute_distances=False)
kmeans_fit = kmeans.fit_predict(annos_dummies, sample_weight=jaccard_distances)

#%%
from sklearn.cluster import DBSCAN
dbscan = DBSCAN(metric='jaccard')
dbscan_fit = dbscan.fit_predict(annos_dummies)
print("Ready")

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


annos_cluster_n['p'] = annos_cluster_n \
    .groupby('properties_id')['n'] \
    .transform(lambda x: x / x.sum())

# tfidf for clusters as documents and all their properties as terms,
# for finding distinctive properties
# TODO: check if correct
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

# Most distinctive terms
tfidf_topterms = annos_cluster_n \
    .sort_values(by='tfidf', ascending=True) \
    .groupby('cluster_no').apply(lambda rows: rows.head(10))

# Proportion: how many term occurences in a cluster in relation to all term occurences?
annos_cluster_n[annos_cluster_n.properties_id.isin(tfidf_topterms.properties_id)] \
    # .filter(['label','p', 'cluster_no']) \
    # .pivot(index='label', columns='cluster_no',values='p') \
    .filter(['label','tf', 'cluster_no']) \
    .pivot(index='label', columns='cluster_no',values='tf') \
    .plot.barh(stacked=True)


plt.tight_layout()
plt.show()

# TODO: silhouette plot
# TODO: multidimensional scaling
