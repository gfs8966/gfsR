#' standardize.catch
#' @description merges FN121 to FN123 and generates a catch table
#' @param NorW specifies 'CATCNT' or 'CATWT' 
#' @param fn121 a valid FN121 table
#' @param fn123 a valid FN123 table
#' @export


standardize.catch<-function(NorW, fn121, fn123, netnights=FALSE){
  if (!(NorW %in% c("CATCNT", "CATWT"))) {stop ("NorW must be either CATCNT or CATWT")}
  if (anyNA(fn123[,NorW])) {cat (sprintf('NA values are present in %s, consider calculating missing values',NorW))}
  
  raw.catch<-merge(fn121, fn123, by=c('PRJ_CD', 'SAM'), all.x=T)
  
  # gl1
  if (levels(fn121$PROG)=="GL1") {
    raw.catch$CATSTAN<- ifelse(raw.catch$GR=='GL6B' & raw.catch$EFF=='038', round(raw.catch[,NorW]*15.2/4.6,0), raw.catch[,NorW])
    raw.catch$DATE<-raw.catch$EFFDT0
    fn121$DATE<-fn121$EFFDT0
  }
  if (netnights==TRUE & levels(raw.catch$PROG)=='GL1') {raw.catch$CATSTAN<-raw.catch$CATSTAN/as.numeric(raw.catch$xEFFDUR)}
  
  #tw1
  if (levels(fn121$PROG)=='TW1') {
    raw.catch$CATSTAN<-raw.catch[,NorW]/raw.catch$EFFDUR * 0.2
  }
  
  raw.catch<-raw.catch[,c('PROG', 'PRJ_CD', 'SITE', 'DATE', 'SAM','GR', 'SPC','EFF','CATSTAN')]
  
  # Make a catch table
  my.spc <- data.frame(SPC=unique(raw.catch$SPC))
  all.sam.spc <- merge(fn121, my.spc)
  
  my.col<-c('PROG', 'PRJ_CD', 'SITE', 'DATE', 'SAM', 'GR', 'SPC')
  
  catch.table<-merge(all.sam.spc, raw.catch, by=my.col, all.x=T)
  catch.table$CATSTAN[is.na(catch.table$CATSTAN)]<-0
  
  
  # final output
  catch.table
}
  

