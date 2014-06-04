# To get internal negative probes from an RGChannelSet
getNegative <- function(rgSet){
	
		if(!(is(rgSet, "RGChannelSet"))) {stop("object must be of class 'RGChannelSet'")} 
	
		ctrlAddress <- getControlAddress(rgSet,"NEGATIVE")
	   	Red=getRed(rgSet)[ctrlAddress,]
		Grn=getGreen(rgSet)[ctrlAddress,]
	    return(list(Grn = Grn, Red = Red))
}

















