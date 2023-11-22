# Model relationship between movement and repertoire commonness.


# There are 50 birds in space, in a 1000m x 1000m square. Each sings 4 songs,
# each drawn from a pool of 100 songs. Some songs are more common in the bird's repertoires than others, drawn from an exponential distribution with mean 100.
# Each bird sings all songs in its repertoire equally often. These birds do not move.

# New birds are born and move. How much they move is determined by a lognormal with mu = 6.119 and stdev = 0.436

# Each new bird collects each song it hears within a 100 m radius as it moves.

# At the end of the movement, it ends up with a repertoire of the four songs it has heard most often.
# We save the average frequency of the songs in a bird's repertoire in the population, as well as how far a bird has moved.


from collections import Counter

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from scipy.stats import expon, lognorm
from tqdm import tqdm

# Initialize parameters
num_birds = 100
num_songs = 100
song_freq = expon.rvs(scale=100, size=num_songs)
song_freq = song_freq / song_freq.sum()
space_dim = 1000
song_radius = 150
num_repertoire = 4

# Dispersal parameters
dispersal_mu = 6.119
dispersal_sigma = 0.436

# Initialize bird locations and songs
birds = [
    {
        "x": np.random.uniform(0, space_dim),
        "y": np.random.uniform(0, space_dim),
        "repertoire": np.random.choice(num_songs, num_repertoire, p=song_freq),
    }
    for _ in range(num_birds)
]


# Euclidean distance
def distance(x1, y1, x2, y2):
    return np.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)


# Function to simulate bird movement and song learning
def simulate_bird(start_x, start_y, end_x, end_y):
    # Calculate the number of steps based on the distance between the starting
    # and ending coordinates
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
        ).most_common(4)
    ]

    return {"repertoire": total_repertoire, "distance_moved": distance_moved}


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


# plot the average frequency of the songs in a bird's repertoire in the
# population against how far a bird has moved

# prepare data as dataframe for seaborn
data = {"distance_moved": dists, "avg_freq": avg_freqs}
data = pd.DataFrame(data)

sns.regplot(
    x="distance_moved", y="avg_freq", data=data, scatter_kws=dict(s=1, alpha=0.5)
)
plt.xlabel("Distance moved")
plt.ylabel("Average frequency of songs in repertoire")
plt.show()
