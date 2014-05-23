
	### To generate data for ROC curves:
	generateROCData <- function( discovery, validation, truthCutoff){
		n2 = truthCutoff

		
		
		specList <- vector("list",length(discovery))
		sensList <- vector("list",length(discovery))
		names(specList) <- names(discovery)
		names(sensList) <- names(validation)
		for (i in 1:length(discovery)){

			# Both discovery and validation must contain the same loci	
			# names1 <- rownames(discovery[[i]])
			# names2 <- rownames(validation[[i]])
			# common <- intersect(names1,names2)
			# discovery[[i]] <- discovery[[i]][match(common,names1),]
			# validation[[i]] <- validation[[i]][match(common,names2),]

			# # To reorder the loci:
			# o1 <- order(discovery[[i]]$f, decreasing=TRUE)
			# o2 <- order(validation[[i]]$f, decreasing=TRUE)
			# discovery[[i]] <- discovery[[i]][o1,]
			# validation[[i]] <- validation[[i]][o2,]

			truth <- rownames(discovery[[i]][1:n2,]) # Names of the true loci
			sequence <- seq(0,nrow(discovery[[i]]),by = nrow(discovery[[i]])/500)
			
			totalNumber <- nrow(validation[[i]]) 
	    	spec <- rep(0, length(sequence))
	    	sens <- rep(0, length(sequence)) 
	   		k=1
	    
	    	
			for (j in sequence){
				
				positives <- rownames(validation[[i]][1:j,])
				nTruePositives <- length(intersect(truth,positives))
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
	
	
	