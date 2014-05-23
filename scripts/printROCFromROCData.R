  printROCFromROCData <- function(data, 
                               xcutoff = 1,
                               main, 
                               colors, 
                               lty,
                               names, ylim=c(0,1), lwd=rep(2, length(names))){
  	
	    

		plot(c(0,xcutoff),c(0,1), 
		           col="white", 
		              xlab = "1-Specificity",
		                  ylab="Sensitivity",
		                     main = main, ylim=ylim
		)
		

		for (i in 1:length(data[[1]])){
		    lines(1-data$spec[[i]],data$sens[[i]],col=colors[i],lty=lty[i], lwd=lwd[i])
		}
		
		if (xcutoff == 1){
			abline(a=0,b=1,lty=3)
		}
		legend("bottomright",names,lwd=3,col=colors, lty=lty)
	}
	
	