# This script calls the component scripts to build the ngram tables from their raw sources
# if necessary. This script assumes that it is being called from the package root.

# Check for source files, check md5 sums, and redownload if necessary
source(file.path('data-raw', 'manage_data.R'))

# Make Document Feature Matrices
source(file.path('data-raw', 'make_dfm.R'))

# Make monograms file
source(file.path('data-raw', 'monograms.R'))

# Make bigrams file
source(file.path('data-raw', 'bigrams.R'))

# Make trigrams file
source(file.path('data-raw', 'trigrams.R'))
