library(RMThreshold)

## Not run: 
N = 500
some.mat = matrix(rep(1, N*N), nrow = N)	
some.mat[1:3, 1:10]
# res <- rm.matrix.validation(some.mat)		# not really a proper matrix for this approach.

## End(Not run)

## It can help to add Gaussian noise to an improper matrix
## Not run: 
noisy.matrix <- add.Gaussian.noise(some.mat, mean = 0, stddev = 1, symm = TRUE)
noisy.matrix[1:3, 1:10]
res <- rm.matrix.validation(noisy.matrix)	# better!
res <- rm.get.threshold(noisy.matrix)		# about 4.3	

## End(Not run)