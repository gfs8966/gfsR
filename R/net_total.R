#'net.total
#'@description computes whole net catches
#'@export

net.total<-function(NorW,fn121,fn123, ...) {
  raw.data <- standardize.catch(NorW, fn121, fn123, ...)
  sam.data <- aggregate(CATSTAN~PROG+YEAR+PRJ_CD+DATE+AREA+SITE+SPC+SAM, raw.data, sum)
  # final output
  #cat('this is not done yet\n')
  sam.data
}