# Clear Workspace
# ------------------------------------------------------------------------------
rm(list=ls())

# Load libraries
# ------------------------------------------------------------------------------
library('ggmap')
library('tidyverse')

# Define functions
# ------------------------------------------------------------------------------
fit_gauss = function(x,res=length(x),alpha=0.05,na.rm=FALSE){
  if( !is.vector(x) | !is.null(dim(x)) | !is.numeric(x) ){
    stop("Please supply a one-dimensional numeric vector as input to fit_gauss")
  }
  if( !na.rm & any(is.na(x)) ){
    stop("Please remove NAs or modify the na.rm option in fit_gauss")
  }
  if( na.rm & any(is.na(x)) ){
    x = x[!is.na(x)]
  }
  x_dens     = density(x,n=res)
  x_at_y_max = x_dens$x[which.max(x_dens$y)]
  ss_mat     = matrix(NA,nrow=res,ncol=2)
  sds        = seq(0,sd(x),length.out=res)
  for( i in 1:res ){
    sd         = sds[i]
    y_fit      = dnorm(x_dens$x,mean=x_at_y_max,sd=sd)
    ss         = sum((x_dens$y - y_fit) ** 2)
    ss_mat[i,] = c(sd,ss)
  }
  sd    = ss_mat[which.min(ss_mat[,2]),1]
  z     = qnorm(1-alpha/2)
  lower = x_at_y_max-z*sd
  upper = x_at_y_max+z*sd
  return(tibble(mu         = x_at_y_max,
                sigma      = sd,
                lower      = lower,
                upper      = upper,
                alpha      = alpha,
                z_twosided = z))
}

# Load data
# ------------------------------------------------------------------------------
proj_dir      = '/Users/leon/Projects/GitHub/address_to_map/'
input_file    = paste0(proj_dir,'price_data_2014_3460_with_geocodes.txt')
combined_data = read_delim(file = input_file, delim = "\t")

# Wrangle
# ------------------------------------------------------------------------------
# Convert quarters to factor
combined_data = combined_data %>% mutate(qos = factor(qos))

# Catch if any addresses were not match
combined_data = combined_data %>% filter(complete.cases(.))

# Filter outliers
x = combined_data$sqmp
fit = fit_gauss(x = x, alpha = 0.01)
combined_data = combined_data %>% filter(fit$lower < sqmp & sqmp < fit$upper)

# Per quarter, z-score adjust each square-meter price with the mean and sd of
# all sales within each quarter
my_list = list()
for( q in unique(combined_data$qos) ){
  tmp   = combined_data %>% filter(qos==q) %>% mutate(z = scale(sqms))
  my_list[[q]] = tmp
}
combined_data = do.call("rbind",my_list)

# Plot
# ------------------------------------------------------------------------------
combined_data %>%
  ggplot(aes(x = sqms, y = price)) +
  geom_point() +
  geom_smooth(method='loess') +
  theme_bw()

combined_data %>%
  ggplot(aes(x = dos, y = sqmp)) +
  geom_point() +
  geom_smooth(method='loess') +
  theme_bw()

x_left   = min(combined_data$lon)
y_bottom = min(combined_data$lat)
x_right  = max(combined_data$lon)
y_top    = max(combined_data$lat)
location = c(left = x_left, bottom = y_bottom, right = x_right, top = y_top)
test_map = get_map(location = location,
                   maptype  = "roadmap",
                   source   = "google")
test_map = get_map(location = c(lon = mean(c(x_left,x_right)),
                                lat = mean(c(y_bottom,y_top))),
                   zoom = 13,
                   maptype  = "roadmap",
                   source   = "google")
test_map = ggmap(test_map)
test_map +
  geom_point(aes(x = lon, y = lat, colour = sqmp), data = combined_data) +
  scale_colour_gradient(low="white", high="darkred")

test_map +
  geom_point(aes(x = lon, y = lat, colour = z), data = combined_data) +
  scale_colour_gradient(low="white", high="darkred")
