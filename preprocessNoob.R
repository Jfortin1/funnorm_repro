# Let's open methylumi:

library(methylumi)
library(minfi)

setwd("/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/raw_datasets")
load("methylumi_aml.Rda")
load("rgset_aml.Rda")
library(methylumi)

x <- methylumi_aml[,1:3]
x <- rgset_aml[,1:3]
x <- preprocessNoob(x, offset = 15, method="noob", dyebias=TRUE)
x <- normalizeMethyLumiSet(methylumi.bgcorr(x))

meth <- x$meth
meth <- meth[sort(rownames(meth)),]

meth <- methylated(x)
meth <- meth[sort(rownames(meth)),]


x <- preprocessNoob(x)

preprocessNoob <-function(x, method='noob', offset=15, dye.corr=TRUE) { 

    correct = TRUE
    getOOB <- function(object) {
        minfi:::.isRG(object)
        IRed   <- getProbeInfo(object, type = "I-Red")
        IGrn <- getProbeInfo(object, type = "I-Green")
        oob.green <- rbind(getGreen(object)[IRed$AddressA,], getGreen(object)[IRed$AddressB,])
        oob.red   <- rbind(getRed(object)[IGrn$AddressA,], getRed(object)[IGrn$AddressB,])
        return(list(Grn = oob.green, Red = oob.red))
    }

    if(tolower(method) == 'noob') method = 'normexp'
    #if(tolower(method) == 'goob') method = 'gamma'

    # Extraction of the out-of-band controls
    controls <- getOOB(x)
    names(controls) <- c("Cy3","Cy5")
    
    mset <- preprocessRaw(x)
    meth <- getMeth(mset)
    unmeth <- getUnmeth(mset)


    if (any(meth<=0)){
      meth[which(meth<=0)] <- 1
    }
    if (any(unmeth<=0)){
      unmeth[which(unmeth<=0)] <- 1
    }

    probe.type <- getProbeType(mset, withColor=TRUE)
    cy3.probes <- which(probe.type=="IGrn")
    cy5.probes <- which(probe.type=="IRed")
    d2.probes <- which(probe.type=="II")

    dat = list( Cy3=list(  M =  as.matrix(meth[cy3.probes,]), 
                           U =  as.matrix(unmeth[cy3.probes,]),
                           D2 = as.matrix(meth[d2.probes,])
                        ), 
                Cy5=list(  M =  as.matrix(meth[cy5.probes,]), 
                           U =  as.matrix(unmeth[cy5.probes,]),
                           D2 = as.matrix(unmeth[d2.probes,])
                        )
              ) 

   

    rows=lapply(dat,function(ch) sapply(names(ch),function(nch)nrow(ch[[nch]])))
    last = lapply(rows, cumsum)
    first = lapply(names(last), function(nch) last[[nch]] - rows[[nch]] + 1)
    names(first) = names(last)

    estimates = lapply(names(dat), function(nch) { 
        xf = rbind(dat[[nch]][['M']], dat[[nch]][['U']],dat[[nch]][['D2']])
        xs = methylumi:::get.xs(xf, controls[[nch]], method=method,
                    offset=offset, correct=correct, parallel=FALSE)
        names(xs[['params']]) = paste(names(xs[['params']]), nch, sep='.')
        names(xs[['meta']]) = paste(names(xs[['meta']]), nch, sep='.')
        xs
    }) 

    names(estimates) = names(dat)

    cy3.M = first[['Cy3']][['M']]:last[['Cy3']][['M']]
    meth[cy3.probes, ] <- estimates[['Cy3']][['xs']][cy3.M,]

    cy3.U = first[['Cy3']][['U']]:last[['Cy3']][['U']]
    unmeth[cy3.probes,] <- estimates[['Cy3']][['xs']][cy3.U,]

    cy5.M = first[['Cy5']][['M']]:last[['Cy5']][['M']]
    meth[cy5.probes,] <- estimates[['Cy5']][['xs']][cy5.M,]

    cy5.U = first[['Cy5']][['U']]:last[['Cy5']][['U']]
    unmeth[cy5.probes,] <- estimates[['Cy5']][['xs']][cy5.U,]

    d2.M = first[['Cy3']][['D2']]:last[['Cy3']][['D2']]
    d2.U = first[['Cy5']][['D2']]:last[['Cy5']][['D2']]

    meth[d2.probes,] <- estimates[['Cy3']][['xs']][d2.M,]
    unmeth[d2.probes,] <- estimates[['Cy5']][['xs']][d2.U,]
    
    for(ch in names(estimates)) { 
      chnames = names(estimates[[ch]][['params']])
      for(nm in chnames) pData(x)[,nm] = estimates[[ch]][['params']][[nm]]
      varMetadata(x)[chnames,] = paste(ch, estimates[[ch]][['meta']]) 
    } 

    # Performing dye bias normalization
    if (dye.corr){

        reference=NULL
        # Correction of the Illumina control probes with the background correction:
        ctrls <- getProbeInfo(x, type = "Control")
        ctrls <- ctrls[ctrls$Address %in% featureNames(x),]

        redControls <- getRed(x)[ctrls$Address,]
        greenControls <- getGreen(x)[ctrls$Address,]
        
        rownames(redControls) <- rownames(greenControls) <- ctrls$Type
        internal.controls <- list(Cy3 = greenControls, Cy5 = redControls)


        xcs = lapply(names(internal.controls), function(nch) {
          xcf = as.matrix(internal.controls[[nch]])
          methylumi:::get.xcs(xcf, method, params=estimates[[nch]][['params']], correct=correct)
        })
        names(xcs) = names(dat)
        internal.controls[['Cy3']] <- xcs[["Cy3"]]
        internal.controls[['Cy5']] <- xcs[["Cy5"]]

        CG.controls <- rownames(internal.controls[[1]]) %in% c("NORM_C", "NORM_G")
        AT.controls <- rownames(internal.controls[[1]]) %in% c("NORM_A", "NORM_T")


        # Dye bias normalizastion with the corrected Illumina control probes:
        Green.avg <- colMeans(internal.controls[["Cy3"]][CG.controls,])
        Red.avg   <- colMeans(internal.controls[["Cy5"]][AT.controls,])
        R.G.ratio = Red.avg/Green.avg

        if(is.null(reference)) reference = which.min(abs(R.G.ratio-1) )
        message(paste('Using sample number', reference, 'as reference level...'))

        ref <- (Green.avg + Red.avg)[reference]/2
        if(is.na(ref)) stop("'reference' refers to an array that is not present")
        Grn.factor <- ref/Green.avg
        Red.factor <- ref/Red.avg

        Grn <- list(  M =  as.matrix(meth[cy3.probes,]), 
                           U =  as.matrix(unmeth[cy3.probes,]),
                           D2 = as.matrix(meth[d2.probes,])
            )

        Red <- list(  M =  as.matrix(meth[cy5.probes,]), 
                           U =  as.matrix(unmeth[cy5.probes,]),
                           D2 = as.matrix(unmeth[d2.probes,])
                )

        Grn <- lapply(Grn, function(y) sweep(y, 2, FUN="*", Grn.factor))
        Red <- lapply(Red, function(y) sweep(y, 2, FUN="*", Red.factor))

        meth[cy3.probes,] <- Grn$M
        unmeth[cy3.probes,] <- Grn$U
        meth[d2.probes,] <- Grn$D2


        meth[cy5.probes,] <- Red$M
        unmeth[cy5.probes,] <- Red$U
        unmeth[d2.probes,] <- Red$D2
    }

    assayDataElement(mset, "Meth") <- meth
    assayDataElement(mset, "Unmeth") <- unmeth


    return(mset)
    
}





