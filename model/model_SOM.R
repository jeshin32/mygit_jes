#############################################################################
# Self Organising Map

# Author: Geol Choi, phD.(cinema4dr12@gmail.com)
# Date: April 10, 2017
#############################################################################
base::rm(list = ls())
base::gc()

if (! ("imager" %in% rownames(installed.packages()))) {
  utils::install.packages("imager")
}
base::library(imager)

# creates image output directory for saving visualisation images
mainDir <- "./"
subDir <- "output"
ifelse(!base::dir.exists(base::file.path(mainDir, subDir)), base::dir.create(base::file.path(mainDir, subDir)), FALSE)

##############################################################################
# Find Best Matching Unit
##############################################################################
find_bmu <- function(training, net, m) {
  bmu_idx <- base::c(0, 0)
  
  # set the initial minimum distance to a huge number
  min_dist <- .Machine$integer.max
  
  # calculate the high-dimensional distance between each neuron and the input
  for(x in 1:base::nrow(net)) {
    for(y in 1:base::ncol(net)) {
      w <- net[x, y, ]
      
      # don't bother with actual Euclidean distance, to avoid expensive sqrt operation
      sq_dist <- base::sum((w - training)^2)
      
      if(sq_dist < min_dist) {
        min_dist <- sq_dist
        bmu_idx <- base::c(x, y)
      }
    }
  }
  
  # get vector corresponding to bmu_idx
  bmu <- net[bmu_idx[1], bmu_idx[2], ]
  
  # return the (bmu, bmu_idx) tuple
  return(base::list(bmu, bmu_idx))
}

##############################################################################
# Decay Radius
##############################################################################
decay_radius <- function(initial_radius, i, time_constant) {
  return(initial_radius * base::exp(-i / time_constant))
}

##############################################################################
# Decay Learning Rate
##############################################################################
decay_learning_rate <- function(initial_learning_rate, i, n_iterations) {
  return(initial_learning_rate * base::exp(-i / n_iterations))
}

##############################################################################
# Calculate Influence
##############################################################################
calculate_influence <- function(distance, radius) {
  return(base::exp(-distance / (2 * (radius^2))))
}

##############################################################################
# 8 colours as initial test set
raw_data <- base::matrix(base::c(1, 0, 0), nrow = 1, ncol = 3)
raw_data <- base::rbind(raw_data, base::c(0, 1, 0))
raw_data <- base::rbind(raw_data, base::c(0, 0.5, 0.25))
raw_data <- base::rbind(raw_data, base::c(0, 0, 1))
raw_data <- base::rbind(raw_data, base::c(0, 0, 0.5))
raw_data <- base::rbind(raw_data, base::c(1, 1, 0.2))
raw_data <- base::rbind(raw_data, base::c(1, 0.4, 0.25))
raw_data <- base::rbind(raw_data, base::c(1, 0, 1))
raw_data <- raw_data * 255.0
raw_data <- base::t(raw_data)

network_dimensions <- base::matrix(0L, nrow = 10, ncol = 10)
n_iterations <- 4000
init_learning_rate <- 0.01

# initial neighbourhood radius
init_radius <- base::max(dim(network_dimensions)) / 2.0

# time constant decay parameter
time_constant <- n_iterations / base::log(init_radius)

# dimension of raw_data
m <- base::nrow(raw_data)
n <- base::ncol(raw_data)

normalise_data <- TRUE

# if True, assume all data on common scale
# if False, normalise to [0 1] range along each column
normalise_by_column <- FALSE

new_data = raw_data
# check if data needs to be normalised
if(normalise_data) {
  if(normalise_by_column) {
    # normalise along each column
    base::sapply(X = 1:base::ncol(new_data),
                 FUN = function(x) {
                   col_max <- base::max(new_data[,x])
                   new_data[,x] <<- new_data[,x] / col_max
                 }
    )
  } else {
    # normalise entire dataset
    new_data <- raw_data / base::max(new_data)
  }
}

# setup random weights between 0 and 1
# weight matrix needs to be one m-dimensional vector for each neuron in the SOM
net <- base::array(0, base::c(base::nrow(network_dimensions), base::ncol(network_dimensions), m))
base::sapply(X = 1:3, FUN = function(x) {
  tmp <- stats::runif(base::nrow(network_dimensions) * base::ncol(network_dimensions))
  net[,,x] <<- base::as.array(base::matrix(tmp,
                                           nrow = base::nrow(network_dimensions),
                                           ncol = base::ncol(network_dimensions)))
})

cnt <- 0
for(i in 1:n_iterations) {
  # select a training example at random
  training <- new_data[,base::sample(x = 1:n, size = 1)]
  
  # find its Best Matching Unit
  tmp <- find_bmu(training, net, m)
  bmu <- tmp[[1]]
  bmu_idx <- tmp[[2]]
  
  # decay the SOM parameters
  r = decay_radius(init_radius, i, time_constant)
  l = decay_learning_rate(init_learning_rate, i, n_iterations)
  
  # now we know the BMU, update its weight vector to move closer to input
  # and move its neighbours in 2-D space closer
  # by a factor proportional to their 2-D distance from the BMU
  for(x in 1:base::nrow(net)) {
    for(y in 1:base::ncol(net)) {
      w <- net[x, y, ]
      
      # get the 2-D distance (again, not the actual Euclidean distance)
      w_dist <- base::sum((base::c(x, y) - bmu_idx)^2)
      
      # if the distance is within the current neighbourhood radius
      if(w_dist <= r^2) {
        # calculate the degree of influence (based on the 2-D distance)
        influence <- calculate_influence(w_dist, r)
        
        # now update the neuron's weight using the formula:
        # new w = old w + (learning rate * influence * delta)
        # where delta = input vector (training) - old w
        new_w <- w + (l * influence * (training - w))
        
        # commit the new weight
        net[x, y, ] = new_w
      }
    }
  }
  
  base::cat("\nIterations: ", i)
  # plot every 50th iteration
  if((i %% 50) == 0) {
    cnt <- cnt + 1
    img <- imager::as.cimg(net)
    
    # save image
    imgName <- base::sprintf("%s%s/image_%04d.png", mainDir, subDir, cnt)
    imager::save.image(img, imgName)
  }
}