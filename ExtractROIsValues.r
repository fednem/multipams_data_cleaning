library("stringr")
library("R.matlab")


ExtractROIsValues = function(directory, CouplesToExtract, ConditionsToExtract = "rest", export = FALSE, sep = ",", filename = NULL) {
	
	
	internal.count = 1


	ConditionsToExtract.number = length(ConditionsToExtract)
	Subjects.number = length(list.files(directory, pattern = "*Subject.*.Condition001.mat"))
	Conditions = read.table(paste0(directory,"_list_conditions.txt"), sep = ",")
	CouplesToExtractRead = read.table(paste0(directory,CouplesToExtract), sep = sep, header = F)
	Couples = paste(CouplesToExtractRead[,1],CouplesToExtractRead[,2], sep = "_To_")
	ROIs.number = length(Couples)
	FinalMatrix = as.data.frame(matrix(nr = Subjects.number * ConditionsToExtract.number, nc = ROIs.number + 2))
	
	

		
			
				for (ss in 1:Subjects.number) {
				
					for (cond in 1:ConditionsToExtract.number) {
	
						ConditionNumber = which(str_detect(Conditions$V1, paste0(" ",ConditionsToExtract[cond],"$")))
				
						Structure = readMat(paste0(directory,"resultsROI_Subject",
							str_pad(as.character(ss),3,pad="0"),
							"_Condition",
							str_pad(as.character(cond),3,pad="0"),
							".mat"))
						FirstNames = make.names(unlist(Structure$names))
						SecondNames = make.names(unlist(Structure$names2))
						
						
							for (ROI in 1:ROIs.number) {
	
								FirstROI = make.names(CouplesToExtractRead[ROI,1])
								SecondROI = make.names(CouplesToExtractRead[ROI,2])
								FirstROI.row = which(str_detect(FirstNames,as.character(paste0(FirstROI,"$"))))
								SecondROI.column = which(str_detect(SecondNames,as.character(paste0(SecondROI,"$"))))
								ValuesMatrix = Structure$Z
								ThisValue = ValuesMatrix[FirstROI.row, SecondROI.column]
								FinalMatrix [internal.count, ROI] = ThisValue
			
				}
				
				FinalMatrix[internal.count, ROIs.number + 1] = ConditionsToExtract[cond]
				FinalMatrix[internal.count, ROIs.number + 2] = ss
				internal.count = internal.count + 1
					
		}

	}

	colnames(FinalMatrix) = Couples
	colnames(FinalMatrix)[ROIs.number + 1] = "Condition"
	colnames(FinalMatrix)[ROIs.number + 2] = "Subject"
	
	if (export) {
	
		if (length(filename) == 0) {
	
			write.csv(file = paste(paste("ConnectivityExtraction",paste(unlist(strsplit(date(), " "))[c(3,2,5,4)],collapse = "_"),sep="_"),".csv"), FinalMatrix, row.names=F) 
			
		} else {
		
				write.csv(file = paste0(filename,".csv"), FinalMatrix, row.names=F) }
	
	}
	
	
	
	return(FinalMatrix)
	
}

ListROIs = function(directory, n = 1) {

	Structure = readMat(paste0(directory,"resultsROI_Condition001.mat"))
	if (n == 1) {
		Names = unlist(Structure$names) } 
	else {
		Names = unlist(Structure$names2)}
	for (ROI in 1:length(Names)) {
		print(Names[ROI])
		invisible(readline())
		}
}

    
		invisible(readline())
	