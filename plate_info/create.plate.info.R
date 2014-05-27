metaDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/metadata"
plateDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/plate_info"

setwd(metaDir)

# Create plate info for KIRC 450K
kirc <- read.csv("KIRC.mappings.450k.csv", sep=",")
sampleName <- kirc$X
plate <- as.character(kirc$plate)

kirc_plate_450k <- data.frame(sampleName = sampleName, plate=plate)
rownames(kirc_plate_450k) <- sampleName
setwd(plateDir)
save(kirc_plate_450k, file="kirc_plate_450k.Rda")

# Create plate info for KIRC 27k
setwd(metaDir)
kirc <- read.csv("KIRC.mappings.27k.csv", sep=",")
sampleName <- kirc$barcode
plate <- as.character(kirc$plate)

kirc_plate_27k <- data.frame(sampleName = sampleName, plate=plate)
rownames(kirc_plate_27k) <- sampleName
setwd(plateDir)
save(kirc_plate_27k, file="kirc_plate_27k.Rda")

# Create plate info for aml 450k
setwd(metaDir)
aml <- read.csv("LAML.mappings.450k.csv", sep=",")
sampleName <- aml$X
sampleName <- as.character(sampleName)
# No plate information ...
plate <- as.character(substr(sampleName, 1,10))


aml_plate_450k <- data.frame(sampleName = sampleName, plate=plate)
rownames(aml_plate_450k) <- sampleName
setwd(plateDir)
save(aml_plate_450k, file="aml_plate_450k.Rda")


# Create plate info for aml 27k 
setwd(metaDir)
aml <- read.csv("LAML.mappings.27k.csv", sep=",")

sampleName <- aml$barcode
sampleName <- as.character(sampleName)
plate <- as.character(aml$plate)

aml_plate_27k <- data.frame(sampleName = sampleName, plate=plate)
rownames(aml_plate_27k) <- sampleName
setwd(plateDir)
save(aml_plate_27k, file="aml_plate_27k.Rda")

