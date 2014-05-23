
setwd('/amber1/archive/sgseq/workspace/hansen_lab1/funnorm/aml')
load("link27kto450k.Rda")
clinical <- read.csv("clinical_patient_laml-1.txt",sep="\t")
subtypes <- clinical$leukemia_french_american_british_morphology_code
names(subtypes) <- clinical$patient_id
link$patient_id <- substr(link$TCGA.ID,9,12)
indices <- match(link$patient_id, names(subtypes))
link$subtype <- subtypes[indices]
link$gender  <- clinical$gender[indices]
link <- link[,-1]
indices <- which(link$subtype=="Not Classified")
link <- link[-indices,]

design_aml <- link[,c(1,4)]
names(design_aml) <- c("sampleName","group")

setwd('/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/designs')
save(design_aml, file="design_aml.Rda")


for (i in 1:5){
	c <- norm.matrices[[i]]
	c <- c[,-match(badNames, colnames(c))]
	norm.matrices[[i]] <- c
	print(i)
}