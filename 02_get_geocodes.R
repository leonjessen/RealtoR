# Clear Workspace
# ------------------------------------------------------------------------------
rm(list=ls())

# Load libraries
# ------------------------------------------------------------------------------
library('ggmap')
library('tidyverse')

# Load data
# ------------------------------------------------------------------------------
proj_dir    = '/Users/leon/Projects/GitHub/address_to_map/'
input_file  = paste0(proj_dir,'price_data_2014_3460.txt')
output_file = paste0(proj_dir,'price_data_2014_3460_with_geocodes.txt')
price_data  = read_delim(file = input_file, delim = "\t")

# Get Coordinates
# ------------------------------------------------------------------------------
addresses     = paste0(price_data$street,', ',price_data$city)
coordinates   = geocode(addresses) %>% as_tibble
combined_data = bind_cols(price_data,coordinates)

# Write results to file
# ------------------------------------------------------------------------------
write_delim(x = combined_data, path = output_file, delim = "\t")
