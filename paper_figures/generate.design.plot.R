

create.design.plot.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {


	library("RColorBrewer")
	library(shinyMethyl)
	library(minfi)

	load(file.path(input.dir,"designs/design_ontario_ebv.Rda"))
	design <- design_ontario_ebv

	discoverySamples <- as.character(design$sampleName[design$set=="Discovery"])
	validationSamples <-as.character(design$sampleName[design$set=="Validation"])

	load(file.path(input.dir,"shinyMethylSets","shiny_dis_ontario_ebv.Rda"))
	shinyMethylSet1 <- shinyMethylSet
	load(file.path(input.dir,"shinyMethylSets","shiny_val_ontario_ebv.Rda"))
	shinyMethylSet2 <- shinyMethylSet
	shinyMethylSet <- shinyMethyl:::.shinyCombine(shinyMethylSet1, shinyMethylSet2)

	extractedData <- shinyMethylSet

	bis <- extractedData@greenControls[[5]]
	indices <- match(design$sampleName, colnames(bis))

	bis <- apply(bis,2,mean)
	bis = bis[indices]

	m <- log2(extractedData@methQuantiles[[3]][250,])[indices]
	u <- log2(extractedData@unmethQuantiles[[3]][250,])[indices]

	colors <- design$group
	colors <- as.numeric(as.factor(colors))
	colors[colors==2] <- "deeppink3"

	cex=1
	pch=20
	amount=0.05
	data <- matrix(bis,50,4)
	color1 <- "black"
	color2 <- "deeppink3"
	colors <- c(color1,color2,color1,color2)
	
	if (save){
		setwd(output.dir)
		pdf("Lympho_Design.pdf", width=5, height=5,pointsize=8)
		par(bty="l")
		boxplot(data, 
			   notch=TRUE, 
			   col=colors,
			   ylim=c(0,2000),
			   pars = list(boxwex = 0.5, staplewex = 0.5, outwex = 0.5),
			   outline=FALSE,
			   labs=c(1,1,3,4),
			   names=rep(c("EBV-transformed","Lymphocyte"),2),
			   ylab="Median background intensity"
		)

	    abline(v=2.5, lty=3)
		text(x=1.5, y=1900,"Discovery dataset")
		text(x=3.5, y=1900,"Validation dataset")
		dev.off()

	}

	if (print){
		setwd(output.dir)
		par(bty="l")
		boxplot(data, 
			   notch=TRUE, 
			   col=colors,
			   ylim=c(0,2000),
			   pars = list(boxwex = 0.5, staplewex = 0.5, outwex = 0.5),
			   outline=FALSE,
			   labs=c(1,1,3,4),
			   names=rep(c("EBV-transformed","Lymphocyte"),2),
			   ylab="Median background intensity"
		)

	    abline(v=2.5, lty=3)
		text(x=1.5, y=1900,"Discovery dataset")
		text(x=3.5, y=1900,"Validation dataset")

	}


}





create.design.plot.kirc <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	library("RColorBrewer")
	library(shinyMethyl)
	library(minfi)

	load(file.path(input.dir,"designs/design_kirc.Rda"))
	design <- design_kirc


	load(file.path(input.dir,"shinyMethylSets","shiny_dis_kirc.Rda"))
	shinyMethylSet1 <- shinyMethylSet
	load(file.path(input.dir,"shinyMethylSets","shiny_val_kirc.Rda"))
	shinyMethylSet2 <- shinyMethylSet
	shinyMethylSet <- shinyMethyl:::.shinyCombine(shinyMethylSet1, shinyMethylSet2)

	extractedData <- shinyMethylSet

	load(file.path(input.dir,"plate_info/kirc_plate_450k.Rda"))
	plate <- kirc_plate_450k
	pp <- as.numeric(as.factor(as.numeric(as.factor(plate$plate[match(design$sampleName,plate$sampleName)]))))
	design$plate <- pp

	neg <- apply(extractedData@greenControls$NEGATIVE,2,mean)
	design$neg <- neg[match(design$sampleName,names(neg))]
	design$chip <- substr(design$sampleName,1,10)

	o <- order(design$set, design$plate,design$group)
	design <- design[o,]

	color1 <- 1
	color2 <- "grey"
	color3 <- "red"
	color4 <- "deeppink4"
	colors <- rep(color1,nrow(design))
	colors[design$set=="Discovery" & design$group=="Tumor"] <- color2
	colors[design$set=="Validation"] <- color3
	colors[design$set=="Validation" & design$group=="Tumor"] <- color4



	if (print){

		plot(1:nrow(design), design$neg, xlab="Plates",
	      col=colors,
	          pch=20, 
	              ylim=c(0,700), bty="n", xaxt="n", ylab="Median background intensity")
	

		abline(v=sum(design$set=="Discovery"), lty=1)
		
		
		ticks <- which(diff(design$plate)!=0)+0.5
		for (i in 1:length(ticks)){
			segments(x0=ticks[i], y0= 0, y1=500,lty=3)
		}
		centers <- diff(c(0,ticks))/2 + c(0,ticks[-c(length(ticks))])
		centers <- c(centers, centers[length(centers)]+75)
		for (i in 1:length(centers)){
				text(x=centers[i], y=0, paste0("P",i), cex=0.7)
		}
		
		legend(x=10, y=700, c("Normal","Tumor"), col=c(color1,color2), pch=20, bty="n", cex=1.2)
		legend(x=230, y=700, c("Normal","Tumor"), col=c(color3,color4), pch=20, bty="n", cex=1.2)

	}

	if (save){
		setwd(output.dir)

		pdf("KIRC_Design.pdf", width=5, height=5,pointsize=8)


			plot(1:nrow(design), design$neg, xlab="Plates",
	      col=colors,
	          pch=20, 
	              ylim=c(0,700), bty="n", xaxt="n", ylab="Median background intensity")
	

		abline(v=sum(design$set=="Discovery"), lty=1)
		
		
		ticks <- which(diff(design$plate)!=0)+0.5
		for (i in 1:length(ticks)){
			segments(x0=ticks[i], y0= 0, y1=500,lty=3)
		}
		centers <- diff(c(0,ticks))/2 + c(0,ticks[-c(length(ticks))])
		centers <- c(centers, centers[length(centers)]+75)
		for (i in 1:length(centers)){
				text(x=centers[i], y=0, paste0("P",i), cex=0.7)
		}
		
		legend(x=10, y=700, c("Normal","Tumor"), col=c(color1,color2), pch=20, bty="n", cex=1.2)
		legend(x=230, y=700, c("Normal","Tumor"), col=c(color3,color4), pch=20, bty="n", cex=1.2)
		dev.off()


	}
	


}






