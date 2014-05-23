  
  
  
  printOverlapFromData <- function(data, 
                               xcutoff, ycutoff = 0, ycutoff2=1,
                               main, 
                               colors, 
                               lty,
                               names){
  	


		
		plot(c(0,xcutoff),c(ycutoff,ycutoff2), 
		           col="white", 
		              xlab = "List size (k)",
		                  ylab="Concordance( % overlap)",
		                     main = main, bty="l"
		)
		

		for (i in 1:length(data[[1]])){
		    lines(data$sequences[[i]],data$overlaps[[i]],col=colors[i],lty=lty[i], lwd=2)
		}
		
		
		# # Plot diagonal line for complete ROC curve
		# if (xcutoff == 1){
			# abline(a=0,b=1,lty=3)
		# }
		
		# # Plot vertical lines for specificity cutoff
		# if (xcutoff==0.1){
			# abline(v=0.01, lty=3)
			# abline(v=0.05, lty=1)
		# }
		
		 legend("bottomright",names,lwd=2,col=colors, lty=lty, cex=1, bty="n")

	}
	