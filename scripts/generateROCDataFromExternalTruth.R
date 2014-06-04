
	### To generate data for ROC curves:
	generateROCDataFromExternalTruth <- function(truth, validation){
		

		discovery  = validation
		specList <- vector("list",length(discovery))
		sensList <- vector("list",length(discovery))
		names(specList) <- names(validation)
		names(sensList) <- names(validation)
		for (i in 1:length(discovery)){
			
			validation.names <- rownames(validation[[i]])
			current.truth <- intersect(validation.names, truth[[i]])
			
			n2 = length(current.truth)
			sequence <- seq(0,nrow(discovery[[i]]),by = nrow(discovery[[i]])/500)
			
			totalNumber <- nrow(validation[[i]]) 
	    	spec <- rep(0, length(sequence))
	    	sens <- rep(0, length(sequence)) 
	   		k=1
	    
	    	
			for (j in sequence){
				
				
				positives <- rownames(validation[[i]][1:j,])
				nTruePositives <- length(intersect(current.truth,positives))
				sens[k] <- nTruePositives/n2 # Sensitivity
	
				nTrueNegatives <- (totalNumber-j) - (n2 - nTruePositives)
				spec[k] <- nTrueNegatives/(totalNumber-n2) # Specificity
				k=k+1
			}
			specList[[i]] <- spec
			sensList[[i]] <- sens
			print(i)
		}
		return(list(spec = specList, sens = sensList))	
	}