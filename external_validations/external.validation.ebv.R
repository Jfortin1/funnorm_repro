# External validation with the bbseq data
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
externalValDir <- paste0(funnormDir,"/external_validations")
dmpDir <- paste0(funnormDir, "/dmps")
svaDir <- paste0(funnormDir,"/sva_results")
svaFunnormDir <- paste0(funnormDir,"/sva_funnorm_results")
ruvDir <- paste0(funnormDir,"/ruv_results")
ruvFunnormDir <- paste0(funnormDir,"/ruv_funnorm_results")


# Let's load the dmps for EBV:
setwd(dmpDir)
load("dmps_dis_ontario_ebv.Rda")
dis <- dmps
load("dmps_val_ontario_ebv.Rda")
val <- dmps
setwd




# To load the sequencing data:
library(bsseq)
setwd(externalValDir)
load("blocks_ebv_null.rda")
load("dmrs_ebv_null.rda")

# Only considering blocks and dmrs with fwer=0
dmrs <- dmrs.ld[dmrs.ld$fwer==0,] 
blocks <- blocks.ld[blocks.ld$fwer==0,]
dmrs      <- data.frame2GRanges(dmrs)
blocks    <- data.frame2GRanges(blocks) 

### Overlap with the 450k
overlapDmrs <- names(subsetByOverlaps(locations, dmrs)) #1,824 loci
overlapBlock <- names(subsetByOverlaps(locations, blocks)) #228,121 loci
overlap <- union(overlapDmrs, overlapBlock) # 228,696 loci
