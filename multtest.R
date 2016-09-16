multtest <- function(data,ccont,rcont,ccoord,rcoord,dim)
{
	norm_vec <- function(x) sqrt(sum(x^2))

	esc <- function(x,y) x%*%y

	axiscont <- function(index)
	{axis <- ccont[,index]}

	planecont <- function(index)
	{plane <- apply(ccont[,c(comb[,index])],1,sum)}

	planercont <- function(index)
	{plane <- apply(rcont[,c(comb[,index])],1,sum)}

	fitcluster <- function(index)
	{
		d <- dist(ccoord[,comb[,index]], method = "euclidean")
		fit[[index]] <- hclust(d, method="ward.D")
	}

	getangle <- function(index)
	{for (i in 1:long)
		{angle[i] <- esc(v[index,],v[i,])/(norm_vec(v[i,])*norm_vec(v[index,]))}
		angle
	}

	axis <- lapply(1:dim,axiscont)
	names(axis) <- paste("axis",as.character(1:dim))
	bestaxis <- apply(ccont,1,which.max)
	#long <- length(data[1,])

	comb <- combn(1:dim,2)

	plane <- lapply(1:length(comb[1,]),planecont)
	for(i in 1:length(comb[1,])){names(plane)[i] <- paste("plane",paste(comb[1,i],comb[2,i],collapse="-"))}
	plane <- matrix(unlist(plane), ncol = dim)
	rownames(plane) <- rownames(ccont)
	colnames(plane) <- paste0("plane ",as.character(1:length(comb[1,])))
	bestplane <- apply(plane,1,which.max)

	#ANGLEM <- list()

	#for (j in 1:length(comb[1,]))
	#{
	#	v <- ccoord[,c(comb[,j])]
	#	ANGLE <- lapply(1:long,getangle)
	#	ANGLEm <- matrix(unlist(ANGLE), ncol = long, byrow = TRUE)
	#	#ANGLEm[upper.tri(ANGLEm,diag = T)] <- 0
	#	rownames(ANGLEm) <- rownames(ccoord)
	#	colnames(ANGLEm) <- rownames(ccoord)
	#	ANGLEM[[j]] <- ANGLEm
	#}
	
	#names(ANGLEM) <- paste("angle",colnames(plane))
	fit <- list()
	FIT <- lapply(1:length(comb[1,]),fitcluster)
	names(FIT) <- paste("plane",as.character(1:length(comb[1,])))
	VAR <- list()

	for (i in 1:length(comb[1,]))
	{
		print(paste0("Plano nº ",i))
		par(mfrow=c(2,1))
		par(mar=c(1.7,1.7,1.7,1.7))
		plot(ccoord[,comb[,i]],pch='.',xlab=NA,ylab=NA, main=NA)
		arrows(0,0,ccoord[,comb[1,i]],ccoord[,comb[2,i]])
		text(ccoord[,comb[,i]],rownames(ccoord))
		plot(FIT[[i]],xlab=NA,ylab=NA, main=NA)
		o <- rep("|",length(ccont[,1]))
		print(cbind.data.frame(ccont,o,plane))

		num <- readline(paste0("¿Cuántas variables quieres asignar para el plano ",i,"? "))
		var <- rep(0,num)
		for (j in 1:num)
		{
			var[j] <- readline(paste0("variable ",j," "))
			if (var[j]%in%rownames(ccont)==FALSE){stop("La variable no existe")}
		}
		VAR[[i]] <- var
	}
	names(VAR) <- colnames(plane)

	planer <- lapply(1:length(comb[1,]),planercont)
	planer <- matrix(unlist(planer), ncol = dim)
	colnames(planer) <- colnames(plane)
	colnames(rcont) <- names(axis)
	rows <- cbind.data.frame(rcoord,rcont,planer)

	par(mfrow=c(3,1))
	par(mar=c(1.7,1.7,1.7,1.7))

	for (i in 1:length(planer[1,]))
	{hist(planer[,i],main=colnames(planer)[i])}

	GOAL <- list()

	for (i in 1:length(planer[1,]))
	{
		max <- readline(paste0("Límite inferior calidad para el plano ",i," "))
		col <- as.matrix(data[which(planer[,i]>=max),which(colnames(data)==VAR[[i]][1])])
		if (length(VAR[[i]])>1)
		{for (j in 2:length(VAR[[i]]))
		{col <- cbind(col,as.matrix(data[which(planer[,i]>=max),which(colnames(data)==VAR[[i]][j])]))}}
		colnames(col) <- VAR[[i]]
		cor <- rcoord[which(planer[,i]>=max),comb[,i]]
		GOAL[[i]] <- list(var=VAR[[i]],coord=cbind.data.frame(cor,col))
	}
	names(GOAL) <- colnames(plane)
	GOAL


	#screenout <- function(index)
	#{
	#	for (i in 1:length(ccont[,1]))
	#	{del[[i]] <- which(abs(ANGLEM[[index]][,i]-1)<0.09)[1]}
	#	names(del) <- rownames(ccont)
	#	del
	#}
	#del <- list()
	#DEL <- lapply(1:length(ANGLEM),screenout)
	#names(DEL) <- paste("plane",as.character(1:dim))
	#DEL <- lapply(DEL, function(x) x[!is.na(x)])



}