"""
This code simulates the relationship between bird movement and the commonness
of songs in their repertoires. The simulation involves a population of 200
birds in a 1500m x 1500m square, each capable of singing 4 songs selected from
a pool of 100. The frequency of songs in a bird's repertoire follows an
exponential distribution with a mean of 100. Birds do not initially move
New birds are born and move based on a lognormal distribution parametrised to
represent realistic dispersal behaviour. Each bird collects songs it hears
within a 200m radius as it moves. At the end of their movement, a bird's
repertoire is determined by the four songs it heard most frequently. The
simulation is repeated 1000 times, and the average frequency of songs in each
bird's repertoire and the distance each bird has moved are recorded.
"""

# ──── IMPORTS ────────────────────────────────────────────────────────────────

from collections import Counter

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pyparsing import line
import seaborn as sns
from scipy.stats import expon, lognorm
from tqdm import tqdm

# ──── SETTINGS ───────────────────────────────────────────────────────────────

# Initialize parameters
num_birds = 300
num_songs = 200
song_freq = expon.rvs(scale=100, size=num_songs)
song_freq = song_freq / song_freq.sum()
space_dim = 1500
song_radius = 200
num_repertoire = 4
conform_exp = 3

# set numpy seed
np.random.seed(42)

# Dispersal parameters (approximates empirical dispersal distribution)
dispersal_mu = 6.119
dispersal_sigma = 0.436

# ──── FUNCTION DEFINITIONS ───────────────────────────────────────────────────


def distance(x1, y1, x2, y2):
    return np.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)


# Function to simulate bird movement and song learning
def simulate_bird(birds, start_x, start_y, end_x, end_y, a =2, selection_method="random"):
    # Calculate the distance moved
    distance_moved = distance(start_x, start_y, end_x, end_y)
    n_steps = 10

    # Initialize a list to store bird data for each step
    all_songs = []

    # Iterate over each intermediate position
    for i in range(n_steps):
        # Calculate the intermediate coordinates if the bird moved, otherwise
        # just use the starting coordinates
        if i > 0:
            x = start_x + (end_x - start_x) * i / n_steps
            y = start_y + (end_y - start_y) * i / n_steps
        else:
            x = start_x
            y = start_y

        # Collect songs from birds within radius
        songs_heard = [
            song
            for bird in birds
            if distance(x, y, bird["x"], bird["y"]) <= song_radius
            for song in bird["repertoire"]
        ]

        # Store the bird data for the current step
        all_songs.append(songs_heard)

    # calculate the relative frequency of each song heard by the bird (i.e. the
    # proportion of the total songs heard, collected in all_songs, that each song represents)
    all_songs = [song for sublist in all_songs for song in sublist]
    all_songs_keys, all_songs_probs = np.unique(all_songs, return_counts=True)
    all_songs_normprobs = all_songs_probs / all_songs_probs.sum()

    # Select the repertoire based on the chosen method
    if selection_method == "random":
        total_repertoire = np.random.choice(
            all_songs_keys, num_repertoire, replace=True
        )
    elif selection_method == "linear":
        total_repertoire = np.random.choice(
            all_songs_keys, num_repertoire, p=all_songs_normprobs, replace=True
        )
    elif selection_method == "conformist":
        # select songs in a conformist frequency-dependent manner
        exp_probs = all_songs_probs**a
        exp_probs = exp_probs / exp_probs.sum()
        total_repertoire = np.random.choice(
            all_songs_keys,
            num_repertoire,
            p=exp_probs,
            replace=True,
        )

    elif selection_method == "fully conformist":
        total_repertoire = np.asarray(
            [song for song, _ in Counter(all_songs).most_common(num_repertoire)]
        )

    return {
        "repertoire": total_repertoire,
        "distance_moved": distance_moved,
        "avg_freq": song_freq[total_repertoire].mean(),
        "method": selection_method,
    }


# ──── MAIN ───────────────────────────────────────────────────────────────────



# Simulate bird movement and song learning
nruns = 4000
selection_methods = ["random", "linear", "conformist", "fully conformist"]
all_sims = []

# Initialize bird locations and songs
birds = [
    {
        "x": np.random.uniform(0, space_dim),
        "y": np.random.uniform(0, space_dim),
        "repertoire": np.random.choice(num_songs, num_repertoire, p=song_freq),
    }
    for _ in range(num_birds)
]

for method in selection_methods:
    for i in tqdm(range(nruns)):
        start_x = np.random.uniform(0, space_dim)
        start_y = np.random.uniform(0, space_dim)
        end_x = lognorm.rvs(s=dispersal_sigma, scale=np.exp(dispersal_mu))
        end_y = lognorm.rvs(s=dispersal_sigma, scale=np.exp(dispersal_mu))
        new_bird=  simulate_bird(birds, start_x, start_y, end_x, end_y, a=conform_exp, selection_method=method)
        all_sims.append(new_bird)

# for each bird, calculate the average frequency of the songs in its repertoire
avg_freqs = np.array([song_freq[bird["repertoire"]].mean() for bird in all_sims])
dists = np.array([bird["distance_moved"] for bird in all_sims])
methods = np.array([bird["method"] for bird in all_sims])
# ──── PLOT RESULTS ───────────────────────────────────────────────────────────

# prepare data as dataframe for seaborn
data = {"distance_moved": dists, "avg_freq": avg_freqs, "method": methods}
data = pd.DataFrame(data)
# data = data[data["distance_moved"] <= 1500]


freqs = np.array(
    [song_freq[bird["repertoire"]] for bird in all_sims if bird["method"] == "random"]
)

og_birds = np.array([bird["repertoire"] for bird in birds])

# plot song vs frequency of song in og_birds, arranged by frequency
og_unique, og_counts = np.unique(og_birds, return_counts=True)
# arrange by frequency
og_unique = og_unique[np.argsort(og_counts)]
og_counts = og_counts[np.argsort(og_counts)] / og_counts.sum()
# plot song vs frequency of song in og_birds, arranged by frequency
plt.plot(og_counts)


# do the same for songs
for method in selection_methods:
    songs = np.array(
        [bird["repertoire"] for bird in all_sims if bird["method"] == method]
    )
    freqs = np.array(
        [song_freq[bird["repertoire"]] for bird in all_sims if bird["method"] == method]
    )

    # arrange by frequency
    songs = songs.flatten()

    unique, counts = np.unique(songs, return_counts=True)
    # arrange by frequency
    unique = unique[np.argsort(counts)]
    counts = counts[np.argsort(counts)] / counts.sum()

    # plot song vs frequency of song in og_birds, arranged by frequency
    plt.plot(counts, label=method)

# plot song vs frequency of song in og_birds, arranged by frequency
plt.plot(og_counts, label="Original Birds")

# add legend
plt.legend()
plt.show()

# For each song type in og_unique, plot the frequency of that song in all_sims
# vs in og_birds

# create a color palette for the selection methods
colors = sns.color_palette("deep", len(selection_methods))


import statsmodels.api as sm

def plot_song_frequency_comparison(selection_methods, all_sims, og_unique, og_counts):
    plt.figure(figsize=(5, 5))

    for method in selection_methods:
        songs = np.array([bird["repertoire"] for bird in all_sims if bird["method"] == method]).flatten()
        unique, counts = np.unique(songs, return_counts=True)
        unique = unique[np.argsort(counts)]
        counts = counts[np.argsort(counts)] / counts.sum()
        og_freqs = np.array([og_counts[np.where(og_unique == song)[0]][0] if song in og_unique else np.nan for song in og_unique])
        song_freqs = np.array([counts[np.where(unique == song)[0]][0] if song in unique else np.nan for song in og_unique])
        sns.scatterplot(x=np.log(og_freqs), y=np.log(song_freqs), alpha=0.2, label=method, color=colors[selection_methods.index(method)])
        lowess = sm.nonparametric.lowess(song_freqs, og_freqs, frac=.6)
        plt.plot(np.log(lowess[:, 0]), np.log(lowess[:, 1]), color=colors[selection_methods.index(method)], linewidth=2)
        x_ticks = np.linspace(og_counts.min(), og_counts.max(), 6)
        plt.xticks(ticks=np.log(x_ticks), labels=[f"{val:.2f}" for val in x_ticks])
        y_ticks = np.linspace(counts.min(), counts.max(), 6)
        plt.yticks(ticks=np.log(y_ticks), labels=[f"{val:.2f}" for val in y_ticks])
    
    plt.show()

plot_song_frequency_comparison(selection_methods, all_sims, og_unique, og_counts)



# Create subplots for each selection method
fig, axes = plt.subplots(1, len(selection_methods), figsize=(15, 5))
sns.set_theme(style="whitegrid")

for i, method in enumerate(selection_methods):
    method_data = data[data["method"] == method]
    # get the inverse of avg_freq
    method_data["avg_freq"] = 1 - method_data["avg_freq"]
    sns.regplot(
        x="distance_moved",
        y="avg_freq",
        data=method_data,
        scatter_kws=dict(s=1.5, alpha=0.2, color=colors[i]),
        line_kws=dict(linewidth=2, color=colors[i]),
        ax=axes[i]
    )
    axes[i].set_xlim(0, 1500)
    axes[i].set_xlabel("Distance moved (m)")
    axes[i].set_ylabel("Average frequency of songs in repertoire") # TODO: find a better label

    # set title
    axes[i].set_title(f"Learning Mechanism: {method}\n")
plt.tight_layout()

# Show the plot
plt.show()
