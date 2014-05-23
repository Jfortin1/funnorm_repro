
	### To generate data for ROC curves:
	generateOverlapData <- function(dataset1, dataset2, step){
		###Assume that dataset1 and dataset 2 are ranked
		
		overlaps <- vector("list", length(dataset1))
		jaccards <- vector("list", length(dataset1))
		sequences <- vector("list", length(dataset1))
		for (i in 1:length(dataset1)){
			print(i)
			names1 <- rownames(dataset1[[i]])
			names2 <- rownames(dataset2[[i]])
			n <- nrow(dataset1[[i]])
			myseq <- seq(0, n, step)
			overlap <- rep(0, length(myseq))
			jaccard <- rep(0, length(myseq))
			for (j in 1:length(myseq)){
				
				k <- myseq[j]
				#if (i ==1){print(k)}
				list1 <- names1[1:k]
				list2 <- names2[1:k]
				inter <- length(intersect(list1,list2))
				uni   <- length(union(list1, list2))
				overlap[j] <- inter/k
				jaccard[j] <- inter/uni
			}
			overlaps[[i]] <- overlap
			jaccards[[i]] <- jaccard
			sequences[[i]] <- myseq
		}
		names(overlaps) <- names(dataset1)
		names(jaccards) <- names(dataset1)
		
	
		return(list(overlaps = overlaps, jaccards=jaccards, sequences = sequences))	
	}