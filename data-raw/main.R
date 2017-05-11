# This script calls the component scripts to build the ngram tables from their raw sources
# if necessary. This script assumes that it is being called from the package root.

# Check for source files, check md5 sums, and redownload if necessary
# print('manage data')
# source(file.path('data-raw', 'manage_data.R'))

# # Extract sample text and cast it as a corpus object
# print('extract sample')
# source(file.path('data-raw', 'extract_text.R'))

# # Make Document Feature Matrices
# gc()
# print('make dfm')
# source(file.path('data-raw', 'make_dfm.R'))

# # Make monograms file
# print('make monograms')
# source(file.path('data-raw', 'monograms.R'))

# # Make bigrams file
# print('make bigrams')
# source(file.path('data-raw', 'bigrams.R'))

# Make trigrams file
print('make trigrams')
source(file.path('data-raw', 'trigrams.R'))

# Make quadgrams file
print('make quadgrams')
source(file.path('data-raw', 'quadgrams.R'))
