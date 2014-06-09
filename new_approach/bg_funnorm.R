# Method 1:
# This method combines background correction and dye bias normalization as implemented in methylumi,
# then applies funnorm

# Pipeline:
#library(methylumi)
#library(minfi)

#setwd("/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/raw_datasets")
#load("rgset_ontario_ebv.Rda")
#load("methylumi_ontario_ebv.Rda")

#rgSet <- rgset_ontario_ebv
#methylumiObject <- methylumi_ontario_ebv


#norm_bg_funnorm <- preprocessFunnorm.bg.before(rgSet=rgSet, methylumiObject = methylumiObject, nPCs = 2)







preprocessFunnorm.bg.before <- function(rgSet, methylumiObject, nPCs=2, sex = NULL, verbose = TRUE) {
    minfi:::.isRG(rgSet)
    rgSet <- updateObject(rgSet) ## FIXM: might not KDH: technically, this should not be needed, but might be nice
    if(verbose) cat("[preprocessFunnorm] Mapping to genome\n")
    gmSet <- mapToGenome(rgSet)

	# Let's apply the background correction in methylumi
	methylumiObject <- normalizeMethyLumiSet(methylumi.bgcorr(methylumiObject))

	meth <- methylated(methylumiObject)
	unmeth <- unmethylated(methylumiObject)

	meth <- meth[match(rownames(gmSet), rownames(meth)),]
	unmeth <- unmeth[match(rownames(gmSet), rownames(unmeth)),]

	assay(gmSet, "Meth") <- meth
    assay(gmSet, "Unmeth") <- unmeth



    subverbose <- max(as.integer(verbose) - 1L, 0)
    
    if(verbose) cat("[preprocessFunnorm] Quantile extraction\n")
    extractedData <- minfi:::.extractFromRGSet450k(rgSet)
    
    if (is.null(sex)) {
        gmSet <- addSex(gmSet, getSex(gmSet, cutoff = -3))
        sex <- rep(1L, length(gmSet$predictedSex))
        sex[gmSet$predictedSex == "F"] <- 2L
    }
    rm(rgSet)
    if(verbose) cat("[preprocessFunnorm] Normalization\n")
    CN <- getCN(gmSet)
    gmSet <- minfi:::.normalizeFunnorm450k(object = gmSet, extractedData = extractedData,
                                   sex = sex, nPCs = nPCs, verbose = subverbose)
    grSet <- ratioConvert(gmSet, type = "Illumina")
    assay(grSet, "CN") <- CN
    grSet@preprocessMethod <- c(preprocessMethod(gmSet),
                                mu.norm = sprintf("Funnorm, nPCs=%s", nPCs))
    return(grSet)
 }	


preprocessFunnorm.bg.after <- function(rgSet, methylumiObject, nPCs=2, sex = NULL, verbose = TRUE) {
    minfi:::.isRG(rgSet)
    rgSet <- updateObject(rgSet) ## FIXM: might not KDH: technically, this should not be needed, but might be nice
    if(verbose) cat("[preprocessFunnorm] Mapping to genome\n")
    gmSet <- mapToGenome(rgSet)

    subverbose <- max(as.integer(verbose) - 1L, 0)
    

    # Funnorm part:
    if(verbose) cat("[preprocessFunnorm] Quantile extraction\n")
    extractedData <- minfi:::.extractFromRGSet450k(rgSet)
    
    if (is.null(sex)) {
        gmSet <- addSex(gmSet, getSex(gmSet, cutoff = -3))
        sex <- rep(1L, length(gmSet$predictedSex))
        sex[gmSet$predictedSex == "F"] <- 2L
    }
    rm(rgSet)
    if(verbose) cat("[preprocessFunnorm] Normalization\n")
    CN <- getCN(gmSet)
    gmSet <- minfi:::.normalizeFunnorm450k(object = gmSet, extractedData = extractedData,
                                   sex = sex, nPCs = nPCs, verbose = subverbose)


    # Noob part:
    meth <- getMeth(gmSet)
    unmeth <- getUnmeth(gmSet)

    meth.methylumi <- methylated(methylumiObject)
    unmeth.methylumi <- unmethylated(methylumiObject)

    meth.methylumi[match(rownames(meth), rownames(meth.methylumi)),] <- meth
    unmeth.methylumi[match(rownames(unmeth), rownames(unmeth.methylumi)),] <- unmeth

    methylated(methylumiObject) <- meth.methylumi
    unmethylated(methylumiObject) <- unmeth.methylumi

    methylumiObject <- normalizeMethyLumiSet(methylumi.bgcorr(methylumiObject))

   
    return(betas(methylumiObject))
 }	

