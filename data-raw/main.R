# This script calls the component scripts to build the ngram tables from their raw sources
# if necessary. This script assumes that it is being called from the package root.

# Check for source files, check md5 sums, and redownload if necessary
# print('manage data')
# source(file.path('data-raw', 'manage_data.R'))

# Extract sample text and cast it as a corpus object
print('extract sample')
source(file.path('data-raw', 'extract_text.R'))

print('make freq tables')
source(file.path('data-raw', 'freq.R'))

print('make grams tables')
source(file.path('data-raw', 'gtbackoff.R'))
