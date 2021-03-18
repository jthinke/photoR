#' import_validation
#'
#' Function to check input data headers for compatibility with the app. Returns information if required headers are missing. This runs in the background and you shouldn't need to access this function. To run this file independently, you will need to import the .csv and format the date field to POSIXct appropriately.
#'
#' @param dat1 A data frame of adult attendance at each nest of interest. The data from should have the following headers (case sensitive):SPLIT_YEAR, ROOKERY,COLONY, CAMERA,	SPP, NEST,	DATE, and	MAXN. Note that DATE can be specified in anyway (default is for m/d/y), or as separate columns named DAY, MON, YR.
#' @param dat2 A data frame of nest content observations. The data frame should have the following headers (case sensitive):SPLIT_YEAR,  ROOKERY,  SPP,  COLONY,  CAMERA,  NEST, DATE,  LAY,  MAXE,  HATCH,  MAXC, and  CRECHE. Note that DATE can be specified in anyway (default is for m/d/y), or as separate columns named DAY, MON, YR.
#'
#' @return A list of errors in data headings
#'
#' @usage import_validation(dat1, dat2)
import_validation<-function(dat1, dat2){
  ticker<-1
  # create output holder
  out<-list()
  # attendance data must have the following headers
  #if(type=="att"){
    Val_ok<-c("YEAR", "CAMERA","NEST", "SPP", "DATE", "MAXN")
    ttt<-Val_ok%in%names(dat1)
    if(ttt[5]==FALSE){
      # if a valid 'DATE' Header is absent, check that YR, MON, and DAY are present
      Val_ok2<-c("YEAR", "CAMERA","NEST", "SPP", "DATE", "YR","MON", "DAY", "MAXN")
      tttt<-Val_ok2%in%names(dat1)
      if(any(tttt[6:8]==FALSE)){
        out[[ticker]]<-paste("Headers specifying valid dates, either 'DATE', or all three  of 'YR', 'MON', and 'DAY' are not specified correctly in the attendance data", sep="")
        ticker<-ticker+1
        # now that Date is dealt with, remove from the list
        ttt[5]<-TRUE
      } else {
        if(all(tttt[6:8]==TRUE)){
          # if YR MON and DAY are correctly specifed, remove flag on Date
          ttt[5]<-TRUE
        }
      }
    }
    bad_val<-which(ttt==FALSE)
    if(length(bad_val)>0){
      nerror<-length(bad_val)
      for(i in 1:nerror){
        out[[ticker]]<-paste("Missing header '", Val_ok[bad_val[i]], "' in the attendance data", sep="")
        ticker<-ticker+1
      }
    }
  #} else {
    # nest content data must have the following headers
    Val_ok<-c("YEAR", "CAMERA","NEST", "SPP", "DATE", "COPULATION","LAY", "MAXE","HATCH","MAXC", "CRECHE")
    ttt<-Val_ok%in%names(dat2)
    if(ttt[5]==FALSE){
      # if a valid 'DATE' Header is absent, check that YR, MON, and DAY are present
      Val_ok2<-c("YEAR", "CAMERA","NEST", "SPP", "DATE", "YR","MON", "DAY", "COPULATION","LAY", "MAXE","HATCH","MAXC", "CRECHE")
      tttt<-Val_ok2%in%names(dat2)
      if(any(tttt[6:8]==FALSE)){
        out[[ticker]]<-paste("Headers specifying valid dates, either 'DATE', or all three  of 'YR', 'MON', and 'DAY' are not specified correctly in the nest content data", sep="")
        ticker<-ticker+1
        # now that Date is dealt with, remove from the list
        ttt[5]<-TRUE
      } else {
        if(all(tttt[6:8]==TRUE)){
          # if YR MON and DAY are correctly specifed, remove flag on Date
          ttt[5]<-TRUE
        }
      }
    }
    bad_val<-which(ttt==FALSE)
    if(length(bad_val)>0){
      nerror<-length(bad_val)
      for(i in 1:nerror){
        out[[ticker]]<-paste("Missing header '", Val_ok[bad_val[i]], "' in the nest content data", sep="")
        ticker<-ticker+1
      }
    }
  #}
  return(out)
}
