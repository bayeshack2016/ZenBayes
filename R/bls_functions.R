library(data.table)

get.onet.data.table<-function(path){
  df<-data.table(read.csv(path, sep = "\t"));
  setkey(df,'O.NET.SOC.Code', 'Title')  
  return(df)
}


get.titles<-function(bls.sec.code, onet.data){
return(paste(onet.data[O.NET.SOC.Code %like% bls.sec.code, Title], collapse = "_"))
}

