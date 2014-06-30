  printROCFromROCData <- function(data, 
                               xcutoff = 1, ycutoff=0,
                               main, 
                               colors, 
                               lty,
                               names=names(data), lwd=rep(1,length(colors)), legend=TRUE){
  	

		plot(c(0,xcutoff),c(ycutoff,1), 
		           col="white", 
		              xlab = "1-Specificity",
		                  ylab="Sensitivity",
		                     main = main, bty="l"
		)
		

		for (i in 1:length(data[[1]])){
		    lines(1-data$spec[[i]],data$sens[[i]],col=colors[i],lty=lty[i], lwd=lwd[i])
		}
		
		
		# Plot diagonal line for complete ROC curve
		if (xcutoff == 1){
			abline(a=0,b=1,lty=3)
		}
		
		# Plot vertical lines for specificity cutoff
		if (xcutoff==0.1){
			abline(v=0.01, lty=3,lwd=2)
			abline(v=0.05, lty=1,lwd=2)
		}
		
		if (legend){
			legend("bottomright",names,lwd=1.4,col=colors, lty=lty, bty="n")
		}
		

	}