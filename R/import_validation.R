
# check column headers
import_validation<-function(dat1, dat2){
#import_validation<-function(dat=dat1, type="att"){
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