# Test mpd for each single row to find where the error(s) are

task <- 1

xx <- round(seq(0, nrow(fixedbbsmat), length.out=11))
xxmat <- cbind((xx+1)[-11], xx[-1])
rowstouse <- (xxmat[task,1]:xxmat[task,2])

x <- fixedbbsmat_nonzero[rowstouse,]

mpd_err <- rep(0, nrow(x))
mpd_value <- rep(-9999, nrow(x))

for (i in 1:nrow(x)) {
	x_i <- matrix(x[i,], ncol=ncol(x))
	dimnames(x_i)[[2]] <- dimnames(x)[[2]]
	try_i <- try(mpd(x_i, ericsondist, abundance.weighted=TRUE), TRUE)
	if (class(try_i) == 'try-error') mpd_err[i] <- 1 else mpd_value[i] <- try_i
}


mpdx <- mpd(x, ericsondist, abundance.weighted=T) # Fails.