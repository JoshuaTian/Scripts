n <- 50

ID <- c(paste('GCGR-',"E",1:n,sep=""),paste('GCGR-',"L",1:n,sep=""))
TranscriptionalSubtype <- sample(c('classical','neural','proneural','mesenchymal'), n*2,replace=TRUE)
MethylationSubtype <- sample(c('RTK1','OLIG','PDGFRα','IDH1','RAS','P13K','P53','CDKN2A'), n*2,replace=TRUE)
WildType <- sample(c("TRUE","FALSE"),n*2,replace=TRUE)
Mutant <- ''
GFP_Luc <- sample(c("TRUE","FALSE"),n*2,replace=TRUE)
SOX2_Cherry <- sample(c("TRUE","FALSE"),n*2,replace=TRUE)
Other <- ''
Xenograft <- sample(c("TRUE","FALSE"),n*2,replace=TRUE)
Request_cell_line <- sample(c("TRUE","FALSE"),n*2,replace=TRUE)

GNS_cell_lines_unmodified <- data.frame('type'="GNS cell lines unmodified",
                                        'Cell line ID'=ID,
                                        'Transcriptional subtype'=TranscriptionalSubtype,
                                        'Methylation subclass'=MethylationSubtype,
                                        'Wild type'=WildType,
                                        'Mutant'=Mutant,
                                        'GFP-Luc'=GFP_Luc,
                                        'SOX2-Cherry'=SOX2_Cherry,
                                        'Other'=Other,
                                        'Xenograft'=Xenograft,
                                        'Request cell line'=Request_cell_line)

write.csv(GNS_cell_lines_unmodified,file="./GNS_cell_lines_unmodified.csv",quote=F,row.names=F)

## ========================================================================

ID <- c(paste('GCGR-',"E",1:n,sep=""),paste('GCGR-',"L",1:n,sep=""))
ID <- sort(c(paste(ID,'-AAVS1-GFP-LUC',sep=""),paste(ID,'-SOX2-Cherry',sep="")))

TranscriptionalSubtype <- sample(c('classical','neural','proneural','mesenchymal'), n*4,replace=TRUE)

MethylationSubtype <- sample(c('RTKI','RTKII','PDGFRα','IDH1','RAS','P13K','P53','CDKN2A'), n*4,replace=TRUE)

WildType <- sample(c("TRUE","FALSE"),n*4,replace=TRUE)

Mutant <- ''

EGFR <- sample(c("TRUE","FALSE"),n*4,replace=TRUE)

PDGFRα <- sample(c("TRUE","FALSE"),n*4,replace=TRUE)

Other <- ''

Request_cell_line <- sample(c("TRUE","FALSE"),n*2,replace=TRUE)

GNS_cell_lines_derivatives <- data.frame("type"="GNS cell lines derivatives",
                                         'Cell line ID'=ID,
                                        'Transcriptional subtype'=TranscriptionalSubtype,
                                        'Methylation subclass'=MethylationSubtype,
                                        'Wild type'=WildType,
                                        'Mutant'=Mutant,
                                        'EGFR'=EGFR,
                                        'PDGFRα'=PDGFRα,
                                        'Other'=Other,
                                        'Request cell line'=Request_cell_line)

write.csv(GNS_cell_lines_derivatives,file="./GNS_cell_lines_derivatives.csv",quote=F,row.names=F)

## ========================================================================

ID <- paste('GCGR-',"NS",1:n,sep="")
region <- c('FB','MB','HB','CO','ST','DI','MI','CE','PO','ME','SC')
regionname <- c('forebrain','midbrain','hindbrain','cortex','striatum','diencephalon','midbrain','cerebellum','pons','medulla','spinal cord')
ID <- unname(unlist(sapply(ID,function(x) paste(x,sample(region,sample(1:11,1)),sep="-"))))
names(regionname) <- region
Region <- unname(regionname[matrix(unlist(strsplit(ID,split="-")),nrow=3)[3,]])
Q <- sample(paste(6*c(1:5),0:7,sep="+"),n,replace=T)
names(Q) <- paste("NS",1:n,sep="")
Age <- unname(Q[matrix(unlist(strsplit(ID,split="-")),nrow=3)[2,]]) 
Cell_line_derivatives <- sample(c("TRUE","FALSE"),length(ID),replace=TRUE)
Request_cell_line <- sample(c("TRUE","FALSE"),length(ID),replace=TRUE)

NS_cell_lines_unmodified <- data.frame( 'type'="NS cell lines unmodified",
                                       'Cell line ID'=ID,
                                        'Region'=Region,
                                        'Age'=Age,
                                        'Cell line derivatives'=Cell_line_derivatives,
                                        'Request cell line'=Request_cell_line)

write.csv(NS_cell_lines_unmodified,file="./NS_cell_lines_unmodified.csv",quote=F,row.names=F)

## ========================================================================

Gene <- c('G56R','TP53null','PTENnull','NF1null','EGFvlll','NP','NPE')
newID <- unname(unlist(sapply(ID,function(x) paste(x,sample(Gene,sample(1:7,1)),sep="-"))))
newRegion <- unname(regionname[matrix(unlist(strsplit(newID,split="-")),nrow=4)[3,]])
newAge <- unname(Q[matrix(unlist(strsplit(newID,split="-")),nrow=4)[2,]])
ICC<- sample(c("TRUE","FALSE"),length(newID),replace=TRUE)
newXenograft <- sample(c("TRUE","FALSE"),length(newID),replace=TRUE)
Request_cell_line <- sample(c("TRUE","FALSE"),length(newID),replace=TRUE)

NS_cell_lines_derivatives <- data.frame("type"="NS cell lines derivatives",
                                        "Cell line ID"=newID,
                                        "Region"=newRegion,
                                        "Age"=newAge,
                                        "ICC"=ICC,
                                        "Xenograft"=newXenograft,
                                        "Request cell line"=Request_cell_line)

write.csv(NS_cell_lines_derivatives,file="./NS_cell_lines_derivatives.csv",quote=F,row.names=F)

library("plyr")

Q <- rbind.fill(GNS_cell_lines_unmodified,GNS_cell_lines_derivatives,NS_cell_lines_unmodified,NS_cell_lines_derivatives)

library("mongolite")
CellLines <- mongo("celllines",url = "mongodb://localhost:27017/GCGRdb")
CellLines$drop()
CellLines$insert(Q)

