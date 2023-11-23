"""
This code simulates the relationship between bird movement and the commonness
of songs in their repertoires. The simulation involves a population of 200
birds in a 1500m x 1500m square, each capable of singing 4 songs selected from
a pool of 100. The frequency of songs in a bird's repertoire follows an
exponential distribution with a mean of 100. Birds do not initially move
New birds are born and move based on a lognormal distribution parametrised to
represent realistic dispersal behaviour. Each bird collects songs it hears
within a 100m radius as it moves. At the end of their movement, a bird's
repertoire is determined by the four songs it heard most frequently. The
simulation is repeated 1000 times, and the average frequency of songs in each
bird's repertoire and the distance each bird has moved are recorded.
"""

# ──── IMPORTS ────────────────────────────────────────────────────────────────

from collections import Counter

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from scipy.stats import expon, lognorm
from tqdm import tqdm

# ──── SETTINGS ───────────────────────────────────────────────────────────────

# Initialize parameters
num_birds = 200
num_songs = 100
song_freq = expon.rvs(scale=100, size=num_songs)
song_freq = song_freq / song_freq.sum()
space_dim = 1500
song_radius = 200
num_repertoire = 4

# set numpy seed
np.random.seed(42)

# Dispersal parameters (approximates empirical dispersal distribution)
dispersal_mu = 6.119
dispersal_sigma = 0.436


# ──── FUNCTION DEFINITIONS ───────────────────────────────────────────────────


def distance(x1, y1, x2, y2):
    return np.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)


# Function to simulate bird movement and song learning
def simulate_bird(start_x, start_y, end_x, end_y):
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

    # get the four most heard songs
    total_repertoire = [
        song
        for song, _ in Counter(
            [song for songs in all_songs for song in songs]
        ).most_common(num_repertoire)
    ]

    return {"repertoire": total_repertoire, "distance_moved": distance_moved}


# ──── MAIN ───────────────────────────────────────────────────────────────────

# Initialize bird locations and songs
birds = [
    {
        "x": np.random.uniform(0, space_dim),
        "y": np.random.uniform(0, space_dim),
        "repertoire": np.random.choice(num_songs, num_repertoire, p=song_freq),
    }
    for _ in range(num_birds)
]

# Simulate bird movement and song learning
all_sims = []
for i in tqdm(range(1000)):
    start_x = np.random.uniform(0, space_dim)
    start_y = np.random.uniform(0, space_dim)
    end_x = lognorm.rvs(s=dispersal_sigma, scale=np.exp(dispersal_mu))
    end_y = lognorm.rvs(s=dispersal_sigma, scale=np.exp(dispersal_mu))
    new_birds = [simulate_bird(start_x, start_y, end_x, end_y) for _ in range(10)]
    all_sims.append(new_birds)

# Flatten list of lists
all_sims = [bird for sim in all_sims for bird in sim]

# for each bird, calculate the average frequency of the songs in its repertoire
avg_freqs = np.array([song_freq[bird["repertoire"]].mean() for bird in all_sims])
dists = np.array([bird["distance_moved"] for bird in all_sims])

# ──── PLOT RESULTS ───────────────────────────────────────────────────────────


# plot the average frequency of the songs in a bird's repertoire in the
# population against how far a bird has moved

# prepare data as dataframe for seaborn
data = {"distance_moved": dists, "avg_freq": avg_freqs}
data = pd.DataFrame(data)
data = data[data["distance_moved"] <= 1500]

sns.regplot(
    x="distance_moved", y="avg_freq", data=data, scatter_kws=dict(s=1, alpha=0.5)
)
plt.xlabel("Distance moved")
plt.ylabel("Average frequency of songs in repertoire")
plt.show()

# calculate pearson correlation coefficient:
pearson_corr = data["distance_moved"].corr(data["avg_freq"], method="pearson")
print("r =", f"{pearson_corr:.2f}")
