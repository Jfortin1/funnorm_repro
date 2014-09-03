

library(minfiData)
library(minfi)


# To create the probes containing SNPs:
snps <- getSnpInfo(RGsetEx)[,c(4,6)] # Default is dbSNP 137
snps[,1][snps[,1]<0.01] = NA
snps[,2][snps[,2]<0.01] = NA
snps <- (!is.na(snps))
snps <- snps[,1] | snps[,2] # 17,302 snps with m.a.f >= 0.01
snps <- names(snps)[snps] 

dir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/bad_probes"
setwd(dir)
snp.probes <- snps
save(snp.probes, file="snp.probes.rda")

# Need to load the cross-hybridizing probes:

# To download the list of cross-reactive probes:
# curl -O http://www.sickkids.ca/MS-Office-Files/Research/Weksberg%20Lab/48639-non-specific-probes-Illumina450k.xlsx

library(xlsx)
cross.probes.info <- read.xlsx2("48639-non-specific-probes-Illumina450k.xlsx", sheetIndex=1,colIndex=1:5)
names(cross.probes.info)[2:5] <- 47:50
save(cross.probes.info, file="cross.probes.info.rda")


load("cross.probes.info.rda")
cross.probes <- as.character(rownames(cross.probes.info)) #29,233 such probes
save(cross.probes, file="cross.probes.rda")
bad.probes <- union(snp.probes, cross.probes) #46,535 probes in total
save(bad.probes, file="bad.probes.rda")
