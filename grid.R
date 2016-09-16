GRID <- function(DAT,RES,lag=0.25)
{
	x.range <- (range(DAT[,1])) + c(0,1)
	y.range <- (range(DAT[,2])) + c(0,1)
	xygrid <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=lag),y=seq(from=y.range[1], to=y.range[2], by=lag))

	#chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))
	#Reajustar el grid
	#xlim <- length(which(xygrid[,2]==xygrid[1,2]))
	#ylim <- length(which(xygrid[,1]==xygrid[1,1]))
	#numMatrix <- (xlim*ylim*RES$var)%/%300
	#x <- chunk(seq(from=x.range[1], to=x.range[2], by=lag),round(sqrt(numMatrix)))
	#y <- chunk(seq(from=y.range[1], to=y.range[2], by=lag),round(sqrt(numMatrix)))

	#combgrid <- t(cbind(rbind(1:round(sqrt(numMatrix)),1:round(sqrt(numMatrix))),combn(1:round(sqrt(numMatrix)),2),combn(1:round(sqrt(numMatrix)),2)[c(2,1),]))
	#combgrid <- combgrid[order(combgrid[,1],combgrid[,2]),]
	#subgrid <- function(index)
	#{
	#	newgrid <- expand.grid(x[[combgrid[index,1]]],y[[combgrid[index,2]]])
	#}

	#newgrid <- lapply(1:length(combgrid[,1]),subgrid)
}