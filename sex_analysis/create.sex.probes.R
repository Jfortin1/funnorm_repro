# To get the genes on the X-chromosome that escape X-inactivation
# From X-inactivation profile reveals extensive variability in X-linked gene expression in females, Laura Carrel1 & Huntington F. Willard2

setwd("/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sex_analysis")
library(xlsx)
data <- read.xlsx2("nature03479-s9.xls", 
			sheetInd=1, colIndex=c(1,3,7,8), startRow=5, endRow=639, header=FALSE)
data <- data[-c(22,23,270),] # Missing information for position



pos <- as.character(data[,1])
pos <- strsplit(pos, " - ")

start <- matrix(unlist(pos),ncol=2, byrow=TRUE)[,1]
end   <- matrix(unlist(pos),ncol=2, byrow=TRUE)[,2]
start <- as.numeric(start)
end   <- as.numeric(end)
chr   <- "chrX"
library(bsseq)


# The authors made a mistake on line 462 of the excel sheet...
# which corresponds to line 455 of data:
end[455] <- as.numeric(paste0(end[455],"0"))
granges <- data.frame2GRanges(data.frame(start=start, end=end, chr=chr))


# To calculate the percentage of silencing:
scores <- as.character(data[,3])
scores <- gsub(" ","", scores)
scores[scores==""] <- as.character(data[,4])[scores==""]
scores <- gsub(" ","", scores)
scores <- substr(scores,1,3)
scores <- as.numeric(substr(scores,1,1))/as.numeric(substr(scores,3,3))
scores <- 1-round(scores,1)


x.inactivation.data <- data.frame(start=start,end=end,gene=data[,2],perc.silenced=scores)
save(x.inactivation.data, file="x.inactivation.data.Rda")


# What about the probes from the 450k array:
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir, "/raw_datasets")

setwd(rawDir)
load("rgset_aml.Rda")
library(minfi)
ann <- getAnnotation(rgset_aml)
chrX <- ann[ann$chr=="chrX",c(1,2)]

granges.450k <-  data.frame2GRanges(data.frame(start=chrX$pos, end=chrX$pos, chr="chrX"))

overlap <- findOverlaps(granges.450k, granges, select="first")
# For 8255 genes, we don't have the information for X inactivation escape
scores.450k <- x.inactivation.data$perc.silenced[overlap]

inactivated.x.probes <- as.character(na.omit(rownames(chrX)[scores.450k==1])) # 1678 probes
escaped.x.probes     <- as.character(na.omit(rownames(chrX)[scores.450k==0])) # 140 probes
unknown.x.probes     <- as.character(rownames(chrX)[is.na(scores.450k) | (scores.450k > 0 & scores.450k <1)]) # 9414

setwd(paste0(funnormDir, "/sex_analysis"))
save(inactivated.x.probes, escaped.x.probes, unknown.x.probes, file="x.probes.status.Rda")




