############### Question 1 #################
#Use a foreach loop to repeat 100 times:generate a random sample from an exponential distribution with mean 1 calculate mean and variance row-bind your results (rbind) (results = mean and variance).


library(doParallel)
library(foreach)

numCores = detectCores() -1
cl <- makeCluster(numCores)
registerDoParallel(numCores)
n <- 100

results <- foreach(i = 1:n, .combine = rbind, .packages = "stats") %dopar% {
  sample_d <- rexp(100, rate = 1)
  mean_s <- mean(sample_d)
  var_s <- var(sample_d)
  c(mean_s, var_s)
}
stopCluster(cl)

result_df <- as.data.frame(results)
colnames(result_df) <- c("Mean", "Variance")
head(result_df)

############# Question 2 ##################
library(foreach)
library(doParallel)
library(MASS)

numCores <- detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

numBootstraps <- 1000
chunkSize <- 100

#Parralel bootstrapping
system.time({
  boot_parallel <- foreach(i = 1:numBootstraps, .combine = c, .packages = "MASS") %dopar% {
    samp_med <- median(sample(galaxies, replace = TRUE))
    samp_med
  }
})

# Serial Bootstrapping
system.time({
  boot_serial <- sapply(1:numBootstraps, function(i) median(sample(galaxies, replace = TRUE)))
})

# Chunked Parallel Bootstrapping
system.time({
  boot_chunked <- foreach(i = 1:(numBootstraps/chunkSize), .combine = c, .packages = "MASS") %dopar% {
    replicate(chunkSize, median(sample(galaxies, replace = TRUE))) 
  }
})

stopCluster(cl)

head(boot_parallel)
head(boot_serial)
head(boot_chunked)

chunkSize <- 1000

system.time({
  boot_parallel <- foreach(i = 1:numBootstraps, .combine = c, .packages = "MASS") %dopar% {
    samp_med <- median(sample(galaxies, replace = TRUE))
    samp_med
  }
})

# Serial Bootstrapping
system.time({
  boot_serial <- sapply(1:numBootstraps, function(i) median(sample(galaxies, replace = TRUE)))
})

# Chunked Parallel Bootstrapping
system.time({
  boot_chunked <- foreach(i = 1:(numBootstraps/chunkSize), .combine = c, .packages = "MASS") %dopar% {
    replicate(chunkSize, median(sample(galaxies, replace = TRUE)))
  }
})

stopCluster(cl)

head(boot_parallel)
head(boot_serial)
head(boot_chunked)

numBootstraps <- 10000
chunkSize <- 100

#Parralel bootstrapping
system.time({
  boot_parallel <- foreach(i = 1:numBootstraps, .combine = c, .packages = "MASS") %dopar% {
    samp_med <- median(sample(galaxies, replace = TRUE))
    samp_med
  }
})

# Serial Bootstrapping
system.time({
  boot_serial <- sapply(1:numBootstraps, function(i) median(sample(galaxies, replace = TRUE)))
})

# Chunked Parallel Bootstrapping
system.time({
  boot_chunked <- foreach(i = 1:(numBootstraps/chunkSize), .combine = c, .packages = "MASS") %dopar% {
    replicate(chunkSize, median(sample(galaxies, replace = TRUE))) 
  }
})

stopCluster(cl)

head(boot_parallel)
head(boot_serial)
head(boot_chunked)

chunkSize <- 1000

system.time({
  boot_parallel <- foreach(i = 1:numBootstraps, .combine = c, .packages = "MASS") %dopar% {
    samp_med <- median(sample(galaxies, replace = TRUE))
    samp_med
  }
})

# Serial Bootstrapping
system.time({
  boot_serial <- sapply(1:numBootstraps, function(i) median(sample(galaxies, replace = TRUE)))
})

# Chunked Parallel Bootstrapping
system.time({
  boot_chunked <- foreach(i = 1:(numBootstraps/chunkSize), .combine = c, .packages = "MASS") %dopar% {
    replicate(chunkSize, median(sample(galaxies, replace = TRUE)))
  }
})

stopCluster(cl)

head(boot_parallel)
head(boot_serial)
head(boot_chunked)


############## Question 3 ##################

library(boot)
library(doParallel)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

boot_mean <- function(data, indices) {
  return(mean(data[indices]))
}

set.seed(123)
n_sim <- 1000
n_samp <- 50
alpha <- 0.05

# Run simulations in parallel
results <- foreach(i = 1:n_sim, .combine = c, .packages = "boot") %dopar% {
  samp_data <- rexp(n_samp, rate = 1)  # Mean is 1
  boot_res <- boot(samp_data, statistic = boot_mean, R = 1000)
  ci <- boot.ci(boot_res, type = "perc")$percent[4:5]
  (ci[1] <= 1) & (ci[2] >= 1)
}

stopCluster(cl)

cov_prob <- mean(results)
print(paste("Estimated Coverage:", round(cov_prob, 3)))

############# Question 4 ###################

library(foreach)
library(iterators)

set.seed(1234)

it <- irnorm(3, count=5)
nextElem(it)
nextElem(it)
nextElem(it)
try(nextElem(it)


max_vals <- foreach(vec = iterat, .combine = c) %do% {
  max(vec)
}

print(max_vals)


############### Question 5 #############
library(parallel)
library(foreach)
library(iterators)
library(microbenchmark)

set.seed(1234)

n_iters <- 1000
vec_size <- 5


generate_max <- function() {
  max(rnorm(vec_size))
}

replicate_test <- function() {
  replicate(n_iters, generate_max())
}

foreach_test <- function() {
  foreach(i = 1:n_iters, .combine = c) %do% {
    generate_max()
  }
}

cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("generate_max", "vec_size")) 
clusterSetRNGStream(cl, 1234)

parLapply_test <- function() {
  unlist(parLapply(cl, 1:n_iters, function(i) generate_max()))
}

benchmark_results <- microbenchmark(
  replicate = replicate_test(),
  foreach = foreach_test(),
  parLapply = parLapply_test(),
  times = 10
)

stopCluster(cl)

print(benchmark_results)





library(foreach)
library(iterators)

set.seed(1234)

# Create an iterator that generates 3 vectors of 5 normally distributed random numbers each
it <- irnorm(3, 5)

# Use foreach to iterate over the vectors and find the largest value in each
largest_values <- foreach(i = it, .combine = c) %do% {
  max(i)
}

# Print the largest values
print(largest_values)