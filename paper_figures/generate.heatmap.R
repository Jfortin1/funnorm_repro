







create.main.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

input.dir = "/Users/Jean-Philippe/funnorm_repro"
output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro"




library(RColorBrewer)
library(shinyMethyl)
library(minfi)

load(file.path(inputDir,"designs","design_ontario_ebv.Rda"))
design <- design_ontario_ebv
retainedSamples <- as.character(design$sampleName)

load(file.path(inputDir,"shinyMethylSets","shiny_dis_ontario_ebv.Rda"))
shinyMethylSet1 <- shinyMethylSet
load(file.path(inputDir,"shinyMethylSets","shiny_val_ontario_ebv.Rda"))
shinyMethylSet2 <- shinyMethylSet
shinyMethylSet <- shinyMethyl:::.shinyCombine(shinyMethylSet1, shinyMethylSet2)


retainedSamples <- as.character(design$sampleName)
model.matrix <- buildControlMatrix(shinyMethylSet)
model.matrix <- model.matrix[retainedSamples,]


names <- retainedSamples
names <- substr(names,1,8)
names <- as.numeric(as.factor(names))
oo <- names


oo[names==2] <- 1 
oo[names==7] <- 2
oo[names==1] <- 3
oo[names==3] <- 4
oo[names==8] <- 5
oo[names==6] <- 6
oo[names==4] <- 7
oo[names==5] <- 8

names[names==1] <- "deeppink3"
names[names==2] <- "orange"
names[names==3] <- "deeppink4"
names[names==4] <- "deepskyblue2"
names[names==6] <- "deepskyblue3"
names[names==7] <- "red"
names[names==8] <- "blue"

o <- order(oo)
model.matrix <- model.matrix[o,]
names <- names[o]


	heatmap(model.matrix,
	        Rowv=NA, 
	        labRow = c(""),
	        labCol = c(""),
	        ylab="",
	        RowSideColors = as.character(names)
	)

pcs <- princomp(model.matrix)$scores
pp <- princomp(model.matrix)

mQuantiles <- getMeth(shinyMethylSet)[[3]]
mQuantiles <- mQuantiles[, retainedSamples][,o]



buildControlMatrix <- function(shinyMethylSet) {
			 
		     greenControls <- shinyMethylSet@greenControls
		     redControls <- shinyMethylSet@redControls
			 controlNames <- names(greenControls)          
			             	
			 	
			# Dye bias:         
			index     <- match("NEGATIVE",controlNames)
			greenControls.current   <- greenControls[[index]]
			redControls.current     <- redControls[[index]]
			dyebiasMatrix <- log2(greenControls.current / redControls.current)
			dyebias <- apply(dyebiasMatrix, 2, median)
		
			# Bisulfite conversion extraction for probe type II:
			index <-    match("BISULFITE CONVERSION II",controlNames)
			redControls.current     <- redControls[[ index ]]
			bisulfite2 <- colMeans(redControls.current, na.rm=T)
			
			# Bisulfite conversion extraction for probe type I:
			index <-    match("BISULFITE CONVERSION I",controlNames)
			redControls.current     <- redControls[[ index ]][7:9,]
			greenControls.current   <- redControls[[ index ]][1:3,]
			bisulfite1 <- colMeans(redControls.current + greenControls.current, na.rm=T)
		               	
			# Staining
			index <-    match("STAINING",controlNames)
			sg   <- greenControls[[ index ]][3, ] 
			sr   <- redControls[[ index ]][1, ] 
		
			# Extension
			index <-    match("EXTENSION",controlNames)
			redControls.current     <- redControls[[index]]
			greenControls.current   <- greenControls[[index]]
			extr <- redControls.current[1:2,]
			extg <- greenControls.current[3:4,]
		
			# Hybridization should be monitored only in the green channel
			index  <-    match("HYBRIDIZATION",controlNames)
			h1     <-    greenControls[[index]][1, ]
			h2     <-    greenControls[[index]][2, ]
			h3     <-    greenControls[[index]][3, ]
		
			# Target removal should be low compared to hybridization probes
			index  <-    match("TARGET REMOVAL",controlNames)
			tar    <-    greenControls[[index]]
			
			# Non-polymorphic probes
			index <-    match("NON-POLYMORPHIC",controlNames)
			npr   <-    redControls[[index]][1:2, ]
			npg   <-    greenControls[[index]][3:4, ]
			
			# Specificity II
			index <-    match("SPECIFICITY II",controlNames)
			greenControls.current   <- greenControls[[index]]
			redControls.current     <- redControls[[index]]
			spec2g      <- colMeans(greenControls.current, na.rm=T)
			spec2r      <- colMeans(redControls.current, na.rm=T)
			spec2ratio  <- spec2g / spec2r
			spec2g      <- greenControls.current
			spec2r      <- redControls.current
			
			# Specificity I
			index <- match("SPECIFICITY I", controlNames)
			greenControls.current     <- greenControls[[index]][1:3,]
			redControls.current       <- redControls[[index]][7:9,]
			spec1g  <- greenControls.current
			spec1r  <- redControls.current
			greenControls.current     <- greenControls[[index]][1:3,]
			redControls.current       <- redControls[[index]][1:3,]
			ratio1 <- colMeans(redControls.current, na.rm=T) /
			                   colMeans(greenControls.current, na.rm=T)
			greenControls.current     <- greenControls[[index]][7:9,]
			redControls.current       <- redControls[[index]][7:9,]
			ratio2 <- colMeans(greenControls.current, na.rm=T) / 
			                  colMeans(redControls.current, na.rm=T)
		    spec1ratio <- (ratio1 + ratio2) / 2
			
			# Normalization probes:
			index <-    match(c("NORM_A"), controlNames)
			normA <-	colMeans(redControls[[index]], na.rm=T)
			index <-    match(c("NORM_T"), controlNames)
			normT <-    colMeans(redControls[[index]], na.rm=T)
			index <-    match(c("NORM_C"), controlNames)
			normC <-    colMeans(greenControls[[index]], na.rm=T)
			index <-    match(c("NORM_G"), controlNames)
			normG <- 	colMeans(greenControls[[index]], na.rm=T)
		    
		    dyebias2 <- (normC + normG)/(normA+normT)
			
		  	model.matrix <- cbind(
		   		    bisulfite1, bisulfite2, t(extg), t(extr), h1, h2,h3, sg, sr, t(npg),
		   			t(npr), t(tar), t(spec1g), t(spec1r), t(spec2g), t(spec2r),ratio1,
		   			spec1ratio, spec2ratio, ratio2, normA, normC, normT, normG, dyebias2)
		         
	  
	
		   
		      # Imputation
		      for (colindex in 1:ncol(model.matrix)){
		      	column <- model.matrix[,colindex]
		     	column[is.na(column)]    <- mean(column, na.rm=T)
		     	model.matrix[ , colindex] <- column
		      }          
		              
		      # Scaling   
		      model.matrix <- scale(model.matrix)   
		      
		      # Fixing outliers 
		      model.matrix[model.matrix>3] <- 3
		      model.matrix[model.matrix<(-3)] <- -3   
				
			  # Rescaling
		      model.matrix <- scale(model.matrix) 
		             
		      return(model.matrix)
		}
