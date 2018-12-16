id <- as.character(1000 - sample(1000))

categary_name <- c('[Primary Tumour Tissue]',
              '[GBM Cell Line]', 
              '[Engineered GBM Cell Line]', 
              '[Xenograph Derive Cell Line]', 
              '[Peripheral Blood Mononuclear Cell]',
              '[Patient Derived Neural Stem Cell]',
              '[Human Feotal Primary Tissue]',
              '[Human Neural Stem Cell Line]',
              '[Isogenic Gene Cell Line]')

categary <- sample(categary_name,1000,replace=T)

labs <- sample(c("UCL","Edinburgh","Spain"),1000,replace=T)

epic <- sample(c(rep(TRUE,800),rep(FALSE,200)))
rna_seq <- sample(c(rep(TRUE,800),rep(FALSE,200)))
wgs <- sample(c(rep(TRUE,800),rep(FALSE,200)))
atac_seq <- sample(c(rep(TRUE,800),rep(FALSE,200)))

father <- rep('',1000)
index <- rep(0,1000)
patients <- sample(paste("G",sample(1000,50),sep=""),1000,replace=T)

level1 <- which(categary %in% c('[Primary Tumour Tissue]', '[Human Feotal Primary Tissue]','[Peripheral Blood Mononuclear Cell]'))
father[level1] <- sample(patients,length(level1),replace=TRUE)
tr <- table(father[level1])
tr_name <- names(tr)
for(i in 1:length(tr)) {
  index[father ==tr_name[i]] <- 1:tr[i]
}


getfather <- function(s,f,father,index)
{
  level <- which(categary %in% s)
  tmp_id <- as.character(id[categary == f])
  father[level] <- sample(tmp_id,length(level),replace=T)
  tr <- table(father[level])
  tr_name <- names(tr)
  for(i in 1:length(tr)) {
      index[father ==tr_name[i]] <- 1:tr[i]
  }
  return(list(father,index))
}

result <- getfather('[GBM Cell Line]','[Primary Tumour Tissue]',father,index)
father <- result[[1]]
index <- result[[2]]

father <- result[[1]]
index <- result[[2]]
result <- getfather('[Engineered GBM Cell Line]','[GBM Cell Line]',father,index)
father <- result[[1]]
index <- result[[2]]

result <- getfather('[Xenograph Derive Cell Line]','[Engineered GBM Cell Line]',father,index)
father <- result[[1]]
index <- result[[2]]

result <- getfather('[Patient Derived Neural Stem Cell]','[Peripheral Blood Mononuclear Cell]',father,index)
father <- result[[1]]
index <- result[[2]]

result <- getfather('[Human Neural Stem Cell Line]','[Human Feotal Primary Tissue]',father,index)
father <- result[[1]]
index <- result[[2]]
result <- getfather('[Isogenic Gene Cell Line]','[Human Neural Stem Cell Line]',father,index)
father <- result[[1]]
index <- result[[2]]


data <- data.frame(id=id,categary=categary,trace=father,index=index,labs=labs,epic=epic,rna_seq=rna_seq,wgs=wgs,atac_seq=atac_seq)
rownames(data) <- id

traceName <- function(id) {

  id = as.character(id)

  v_id <- vector()
  v_ca <- vector()
  v_index <- vector()

  while(TRUE) {

    v_id <- c(id,v_id)
    v_ca <- c(as.character(data[id,'categary']),v_ca)
    v_index <- c(as.character(data[id,'index']),v_index)
    if(substr(id,1,1)=="G") break
    id <- as.character(data[id,'trace'])
  }
  code_index <- paste(c(v_id[1],v_index[-1]),collapse="-")
  code_id <- paste(c(v_id),collapse="-")
  code_ca <- paste(c(v_id[1],v_ca[-1]),collapse="-")

  return(list(code_index=code_index,code_id=code_id,code_ca=code_ca))
}

trace <- apply(data,1,function(x) traceName(x['id']))
trace_name <- as.data.frame(t((matrix(as.character(unlist(trace)),nrow=3))))
colnames(trace_name) <- c("code_index","code_id","code_ca")
#
#tr <- table(trace_name)
#tr <- tr[tr > 1]
#tr_name <- names(tr)
#
#trace2 <- trace_name
#for(i in 1:length(tr)) {
#  trace2[trace2 ==tr_name[i]] <- paste(tr_name[i],1:tr[i],sep="-")
#}
#
data <- data.frame(trace_name,data)

write.csv(data,file="./mockdata.csv",quote=F,row.names=F)
