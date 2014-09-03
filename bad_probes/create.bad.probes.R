

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

