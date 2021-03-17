#' eerror_checking
#'
#' @param dat1 A data frame of adult attendance at each nest of interest. The data from should have the followng headers (case senstive):SPLIT_YEAR,	ROOKERY,COLONY,	CAMERA,	SPP, NEST,	DATE (m/d/y), and	MAXN 
#' @param dat2 A data frame of nest content observations. The data frame should have the following headers (case senstive):SPLIT_YEAR,  ROOKERY,  SPP,  COLONY,  CAMERA,  NEST,  
#DATE,  LAY,  MAXE,  HATCH,  MAXC, and  CRECHE 
#
#'
#' @return A list with information about the nature and location (row) of suspected errors in each data file
#' @export
#'
#' @examples
error_checking<-function(dat1=dat, dat2=rdat){
# this function created to run basic checks on data formatting to help analysts
#Dt<-"%m/%d/%Y"  
 
#dat1<-read.csv(file="test_att.csv", stringsAsFactors=FALSE, header=TRUE)
#dat1$DATE<-lubridate::ymd(paste(dat1$YR, dat1$MON, dat1$DAY), tz="GMT")
#dat1$DATE<-as.POSIXct(strptime(dat1$DATE, format=Dt), tz="GMT")
#dat2<-read.csv(file="test_repro.csv", stringsAsFactors=FALSE, header=TRUE)
#dat2$DATE<-lubridate::ymd(paste(dat2$YR, dat2$MON, dat2$DAY), tz="GMT")
#dat2$DATE<-as.POSIXct(strptime(dat2$DATE, format=Dt), tz="GMT")
  # for testing

ticker<-1
# add index for identifying row in main data
dat1$INDEX<-1:length(dat1$YEAR)
# create output holder
out<-list()
# check column headers
# attendance data must have the following

#Val_ok<-c("YEAR", "CAMERA","NEST", "SPP", "DATE", "YR","MON", "DAY", "MAXN")
#ttt<-Val_ok%in%names(dat1)
#if(ttt[5]==TRUE){
#  # if a valid 'DATE' Header is present, assume that YR, MON, and DAY are implied and absence is not an error
#  dat1Names<-c(names(dat1), "YR", "MON", "DAY")
#  tt<-Val_ok%in%dat1Names
#} else {
#  tt<-ttt
#}
#bad_val<-which(tt==FALSE)
#if(length(bad_val)>0){
#  nerror<-length(bad_val)
#  for(i in 1:nerror){
#    out[[ticker]]<-paste("Missing header '", Val_ok[bad_val[i]], "' in the attendance data", sep="")
#    ticker<-ticker+1
#  }
#}

#Val_ok<-c("YEAR", "CAMERA","NEST", "SPP", "DATE", "YR","MON", "DAY", "COPULATION","LAY", "MAXE","HATCH","MAXC", "CRECHE")
#ttt<-Val_ok%in%names(dat2)
#if(ttt[5]==TRUE){
  # if a valid 'DATE' Header is present, assume that YR, MON, and DAY are implied and absence in not an error
#  dat1Names<-c(names(dat2), "YR", "MON", "DAY")
#  tt<-Val_ok%in%dat1Names
#} else {
#  tt<-ttt
#}
#bad_val<-which(tt==FALSE)
#if(length(bad_val)>0){
#  nerror<-length(bad_val)
#  for(i in 1:nerror){
#    out[[ticker]]<-paste("Missing header '", Val_ok[bad_val[i]], "' in the nest content data", sep="")
#    ticker<-ticker+1
#  }
#}
# common error checking
# are all dates valid?
invalidDate<-which(is.na(dat1$DATE))
if(length(invalidDate)>0){
  nerror<-length(invalidDate)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Attendance data has invalid date for camera ", dat1$CAMERA[invalidDate[i]], " nest ", dat1$NEST[invalidDate[i]], "  in row ", invalidDate[i], ".", sep="")
    ticker<-ticker+1
  }
}
# now do the same for the repro data
invalidDate<-which(is.na(dat2$DATE))
if(length(invalidDate)>0){
  nerror<-length(invalidDate)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid date for camera ", dat2$CAMERA[invalidDate[i]], " nest ", dat2$NEST[invalidDate[i]], "  in row ", invalidDate[i], ".", sep="")
    ticker<-ticker+1
  }
}

# now look for repeated dates within each nest in the attendance data
cams<-unique(dat1$CAMERA)
ncams<-length(cams)
for(i in 1:ncams){
  dat<-dat1[dat1$CAMERA==cams[i],]
  nests<-unique(dat$NEST)
  nnests<-length(nests)
  for(k in 1:nnests){
    ndat<-dat[dat$NEST==nests[k],]
    dupes<-diff(ndat$DATE)
    dupDates<-which(dupes==0)
    if(length(dupDates)>0){
      nerror<-length(dupDates)
      for(j in 1:length(dupDates)){
        out[[ticker]]<-paste("Attendance data has duplicated dates for camera ", ndat$CAMERA[dupDates[j]], " nest ", ndat$NEST[dupDates[j]], " in rows ", ndat$INDEX[dupDates[j]], " and ", ndat$INDEX[dupDates[j]]+1,  sep="")
        ticker<-ticker+1
      }
    }
  }
}
# now do the same for the repro data set
# now look for repeated dates within each nest in the attendance data
cams<-unique(dat2$CAMERA)
ncams<-length(cams)
# add index for identifying row in main data
dat2$INDEX<-1:length(dat2$YEAR)
for(i in 1:ncams){
  dat<-dat2[dat2$CAMERA==cams[i],]
  nests<-unique(dat$NEST)
  nnests<-length(nests)
  for(k in 1:nnests){
    ndat<-dat[dat$NEST==nests[k],]
    dupes<-diff(ndat$DATE)
    dupDates<-which(dupes==0)
    if(length(dupDates)>0){
      nerror<-length(dupDates)
      for(j in 1:length(dupDates)){
        out[[ticker]]<-paste("Nest content data has duplicated dates for camera ", ndat$CAMERA[dupDates[j]], " nest ", ndat$NEST[dupDates[j]], " in rows ", ndat$INDEX[dupDates[j]], " and ", ndat$INDEX[dupDates[j]]+1,  sep="")
        ticker<-ticker+1
      }
    }
  }
}
# now check basic data entry errors in the attendance data
# allowed attendance data MAXN must be 0, 1,2,4,5 or NA
maxn_ok<-c(0, 1,2,4,5,NA)
tt<-dat1$MAXN%in%maxn_ok
bad_maxn<-which(tt==FALSE)
if(length(bad_maxn)>0){
  nerror<-length(bad_maxn)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Attendance data has invalid entry for MAXN in nest ", dat1$CAMERA[bad_maxn[i]], " nest ", dat1$NEST[bad_maxn[i]], " in row ", bad_maxn[i], sep="")
    ticker<-ticker+1
  }
}
## now check basic data entry errors in the nest content data
# allowed values for copulation are 1 and NA
Val_ok<-c(1,NA)
tt<-dat2$COPULATION%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for COPULATION in camera ", dat2$CAMERA[bad_val[i]], " nest ", dat2$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for lay are 1 and NA
Val_ok<-c(1,NA)
tt<-dat2$LAY%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for LAY in camera ", dat2$CAMERA[bad_val[i]], " nest ", dat2$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}

#
# allowed values for hatch ay are 1 and NA
Val_ok<-c(1,NA)
tt<-dat2$HATCH%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for HATCH in camera ", dat2$CAMERA[bad_val[i]], " nest ", dat2$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for MAXE are 0, 1, 2, and NA
Val_ok<-c(0,1,2,NA)
tt<-dat2$MAXE%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for MAXE in camera ", dat2$CAMERA[bad_val[i]], " nest ", dat2$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for MAXC are 0, 1, 2, and NA
Val_ok<-c(0,1,2,NA)
tt<-dat2$MAXC%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for MAXC in camera ", dat2$CAMERA[bad_val[i]], " nest ", dat2$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for CRECHE are 0, 1, 2, and NA
Val_ok<-c(0,1,2,NA)
tt<-dat2$CRECHE%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for CRECHE in camera ", dat2$CAMERA[bad_val[i]], " nest ", dat2$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
return(out)
}
