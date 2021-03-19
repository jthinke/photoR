#' error_checking
#'
#' This function checks the input data frames for common entry errors and returns information to support the analyst in correcting data entry if errors are encountered. This runs in the background and the user should not need to access this function. To run this file independently, you will need to import your .csv files and ensure the 'DATE" field is formatted as POSIXct.
#'
#' @param dat A data frame of adult attendance at each nest of interest (attendance data). The data from should have the following headers (case sensitive):SPLIT_YEAR, ROOKERY,COLONY, CAMERA,	SPP, NEST,	DATE, and	MAXN. Note that DATE can be specified in anyway (default is for m/d/y), or as separate columns named DAY, MON, YR.
#' @param rdat A data frame of nest content observations (reproductive data). The data frame should have the following headers (case sensitive):SPLIT_YEAR,  ROOKERY,  SPP,  COLONY,  CAMERA,  NEST, DATE,  LAY,  MAXE,  HATCH,  MAXC, and  CRECHE. Note that DATE can be specified in anyway (default is for m/d/y), or as separate columns named DAY, MON, YR.
#
#'
#' @return A list with information about the nature and location (row) of suspected errors in each data file. If no errors are encounterd, list has length 0.
#'
#' @examples
#' error_checking(test_att, test_repro)
error_checking<-function(dat, rdat){
# this function created to run basic checks on data formatting to help analysts
# for testing
ticker<-1
# add index for identifying row in main data
dat$INDEX<-1:length(dat$YEAR)
# create output holder
out<-list()
# are all dates valid?
invalidDate<-which(is.na(dat$DATE))
if(length(invalidDate)>0){
  nerror<-length(invalidDate)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Attendance data has invalid date for camera ", dat$CAMERA[invalidDate[i]], " nest ", dat$NEST[invalidDate[i]], "  in row ", invalidDate[i], ".", sep="")
    ticker<-ticker+1
  }
}
# now do the same for the repro data
invalidDate<-which(is.na(rdat$DATE))
if(length(invalidDate)>0){
  nerror<-length(invalidDate)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid date for camera ", rdat$CAMERA[invalidDate[i]], " nest ", rdat$NEST[invalidDate[i]], "  in row ", invalidDate[i], ".", sep="")
    ticker<-ticker+1
  }
}
# now look for repeated dates within each nest in the attendance data
cams<-unique(dat$CAMERA)
ncams<-length(cams)
for(i in 1:ncams){
  dat<-dat[dat$CAMERA==cams[i],]
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
# now look for repeated dates within each nest in the attendance data
cams<-unique(rdat$CAMERA)
ncams<-length(cams)
# add index for identifying row in main data
rdat$INDEX<-1:length(rdat$YEAR)
for(i in 1:ncams){
  dat<-rdat[rdat$CAMERA==cams[i],]
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
tt<-dat$MAXN%in%maxn_ok
bad_maxn<-which(tt==FALSE)
if(length(bad_maxn)>0){
  nerror<-length(bad_maxn)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Attendance data has invalid entry for MAXN in nest ", dat$CAMERA[bad_maxn[i]], " nest ", dat$NEST[bad_maxn[i]], " in row ", bad_maxn[i], sep="")
    ticker<-ticker+1
  }
}
## now check basic data entry errors in the nest content data
# allowed values for copulation are 1 and NA
Val_ok<-c(1,NA)
tt<-rdat$COPULATION%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for COPULATION in camera ", rdat$CAMERA[bad_val[i]], " nest ", rdat$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for lay are 1 and NA
Val_ok<-c(1,NA)
tt<-rdat$LAY%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for LAY in camera ", rdat$CAMERA[bad_val[i]], " nest ", rdat$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
#
# allowed values for hatch ay are 1 and NA
Val_ok<-c(1,NA)
tt<-rdat$HATCH%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for HATCH in camera ", rdat$CAMERA[bad_val[i]], " nest ", rdat$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for MAXE are 0, 1, 2, and NA
Val_ok<-c(0,1,2,NA)
tt<-rdat$MAXE%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for MAXE in camera ", rdat$CAMERA[bad_val[i]], " nest ", rdat$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for MAXC are 0, 1, 2, and NA
Val_ok<-c(0,1,2,NA)
tt<-rdat$MAXC%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for MAXC in camera ", rdat$CAMERA[bad_val[i]], " nest ", rdat$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
# allowed values for CRECHE are 0, 1, 2, and NA
Val_ok<-c(0,1,2,NA)
tt<-rdat$CRECHE%in%Val_ok
bad_val<-which(tt==FALSE)
if(length(bad_val)>0){
  nerror<-length(bad_val)
  for(i in 1:nerror){
    out[[ticker]]<-paste("Nest content data has invalid entry for CRECHE in camera ", rdat$CAMERA[bad_val[i]], " nest ", rdat$NEST[bad_val[i]], " in row ", bad_val[i], sep="")
    ticker<-ticker+1
  }
}
return(out)
}
