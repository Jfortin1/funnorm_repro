  printOverlapFromData <- function(data, 
                               xcutoff, ycutoff = 0, ycutoff2=1,
                               main, 
                               colors, 
                               lty,
                               names,lwd=rep(1,length(colors))){
  	


		
		plot(c(0,xcutoff),c(ycutoff,ycutoff2), 
		           col="white", 
		              xlab = "List size (k)",
		                  ylab="Concordance( % overlap)",
		                     main = main, bty="l"
		)
		

		for (i in 1:length(data[[1]])){
		    lines(data$sequences[[i]],data$overlaps[[i]],col=colors[i],lty=lty[i], lwd=2)
		}
		

		
		 legend("bottomright",names,lwd=2,col=colors, lty=lty, cex=1, bty="n")

	}
	