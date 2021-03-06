 ### Modified on Jun 25 2014
 ### Jean-Philippe Fortin
 
 
 
 generateROCDataGender <- function(dir, dataset){

 		dir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
 		
 		load(file.path(dir,"cross.probes.info.rda"))
 		load(file.path(dir,"sex_analysis/x.probes.status.Rda"))
 		load(file.path(dir,"sex_analysis/chrX.Rda"))
 		load(file.path(dir,"sex_analysis/chrY.Rda"))



 		crossProbes <- cross.probes.info$TargetID # 29233 probes

 		# inactivated.x.probes : 1678
 		# unknown.x.probes: 9414 
 		# escaped.x.probes: 140
 		# chrY: 416
 		# chrX: 11232
 		sexProbes <- union(chrY, inactivated.x.probes) # 2094
 		sexProbes <- setdiff(sexProbes, crossProbes) # 1877 left probes
 		toRemove <- union(crossProbes, unknown.x.probes) # 37953
 		# 140 that escape. 
	    
		specList <- vector("list",length(dataset))
		sensList <- vector("list",length(dataset))
		
	
		for (i in 1:length(dataset)){

			good <- !(rownames(dataset[[i]]) %in% toRemove)
			dataset[[i]] <- dataset[[i]][good, ]
			# Need to reorder:
			o <- order(dataset[[i]]$pval)
			dataset[[i]] <- dataset[[i]][o,]

			truth <- sexProbes[sexProbes %in% rownames(dataset[[i]])]
			n2 <- length(truth)
			sequence <- seq(0,nrow(dataset[[i]]),by = nrow(dataset[[i]])/500)
			
			totalNumber <- nrow(dataset[[i]]) 
	    	spec <- rep(0, length(sequence))
	    	sens <- rep(0, length(sequence)) 
	   		k=1
	    
	    	
			for (j in sequence){
				
				
				positives <- rownames(dataset[[i]][1:j,])
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