# chirpy-learning

This is a simple project to try out streaming data processing in Haskell using `Conduit` and `twitter-conduit`. It accesses data from Twitter's filter API using `twitter-conduit`, runs it through a simple processing pipeline built using `Conduit`, and outputs a running estimate of the mean and variance of tweet lengths to standard output.

The pipeline used here extracts some simple features (length of tweet, proportion of non-vowel characters, although only the first is used) and then calculates running statistics using [Welford's algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm). These stages can easily be replaced with more complex feature extraction and inference steps as desired.

Currently in the process of implementing [Poisson bootstrapping](http://www.unofficialgoogledatascience.com/2015/08/an-introduction-to-poisson-bootstrap_26.html). The `rng-test` directory has some code used for informal checks on the Poisson (p)rng.

To run this, you will need to replace the included `Info-template.hs` with a file `Info.hs` containing the appropriate credentials for the twitter API.
