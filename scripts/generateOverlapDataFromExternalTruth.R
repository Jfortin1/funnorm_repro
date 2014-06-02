
	### To generate data for ROC curves:
	generateOverlapDataFromExternalTruth <- function(truth, dataset, step){
		###Assume that dataset1 and dataset 2 are ranked
		
		overlaps <- vector("list", length(dataset))
		jaccards <- vector("list", length(dataset))
		sequences <- vector("list", length(dataset))
		
		for (i in 1:length(dataset)){
			
			print(i)
			
			candidate.loci <- rownames(dataset[[i]])
			n <- nrow(dataset[[i]])
			myseq <- seq(0, n, step)
			overlap <- rep(0, length(myseq))
			jaccard <- rep(0, length(myseq))
			
			for (j in 1:length(myseq)){
				
				k <- myseq[j]
					if (i ==1){print(k)}
				positives <- candidate.loci[1:k]
				
				inter <- sum(positives %in% truth)
				uni   <- length(union(positives, truth))
				overlap[j] <- inter/k
				jaccard[j] <- inter/uni
			}
			overlaps[[i]] <- overlap
			jaccards[[i]] <- jaccard
			sequences[[i]] <- myseq
		}
		names(overlaps) <- names(dataset)
		names(jaccards) <- names(dataset)
		
	
		return(list(overlaps = overlaps, jaccards=jaccards, sequences = sequences))	
	}