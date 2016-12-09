Alist <- list()
Xlist <- list()

for (task in 1:10) {
	#task <- as.numeric(Sys.getenv('PBS_ARRAYID'))
	rowidx <- round(seq(0,nrow(fixedbbsmat_nonzerorows),length.out=11))
	rowidxmin <- rowidx[task]+1
	rowidxmax <- rowidx[task+1]

	Alist[[task]] <- fixedbbsmat_nonzerorows[rowidxmin:rowidxmax, ]

	zerocols <- apply(Alist[[task]], 2, sum) == 0

	Xlist[[task]] <- birdtraitclean[!zerocols, ]
	print(task)
}

Acolsums <- sapply(Alist, colsums)

flag <- rep(0, nrow(Acolsums))

for (i in 1:nrow(Acolsums)) {
	if (Acolsums[i, 5] > 0 & Acolsums[i,6] > 0 & sum(Acolsums[i, -(5:6)] == 0)) flag[i] <- 1
}

dimnames(Alist[[1]])[[2]][flag == 1]