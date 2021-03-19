#' phenology_app
#'
#' Function to process adult attendance of nest data and nest content data to estimate phenology and reproductive success. The app requests you input the data as a .csv file. To run this file independently, you will need to import the .csv and format the date field to POSIXct appropriately.
#'
#' @param dat1 A data frame of adult attendance at each nest of interest. The data should have the following headers (case sensitive):SPLIT_YEAR, ROOKERY, COLONY, CAMERA,	SPP, NEST,	DATE, and	MAXN. Note that DATE can be specified in any way (default is for m/d/y), or as separate columns named DAY, MON, YR.
#' @param dat2 A data frame of nest content observations. The data frame should have the following headers (case sensitive):SPLIT_YEAR,  ROOKERY,  SPP,  COLONY,  CAMERA,  NEST, DATE,  LAY,  MAXE,  HATCH,  MAXC, and  CRECHE. Note that DATE can be specified in anyway (default is for m/d/y), or as separate columns named DAY, MON, YR.
#' @param tabletype A character vector indicating output of interest, either "Raw", "CEMP Protocol A6b", or "CEMP Protocol A9"
#' @param degf numeric value for degrees of freedom used in the smooth.spline function
#'
#' @return A data frame with results
#'
#' @examples
#' phenology_app(test_att, test_repro, tabletype="Raw", degf=10)
phenology_app<-function(dat1, dat2, tabletype="Raw", degf=10){
  # attendance is a dataframe of attendance data organized with headers to identify SPLIT_YEAR,	ROOKERY,	COLONY,	CAMERA,	SPP,	NEST,	DATE (m/d/y), and	MAXN
  # repro is a dataframe of nest content observations organized with headers to identify SPLIT_YEAR,  ROOKERY,  SPP,  COLONY,  CAMERA,  NEST,
  #DATE,  LAY,  MAXE,  HATCH,  MAXC, and  CRECHE
  #
  # first step is to error check data entry using custom function "error_checking"
  #out<-error_checking(dat1=dat, dat2=rdat)
  #out<-as.data.frame(do.call("rbind", out))
  #
  # if errors in the data were found, terminate app and display errors
  #if(length(out)==0){
    # set up some summary functions for later
    mean.func<-function(x){mean(x, na.rm=TRUE)}
    min.func<-function(x){min(x, na.rm=TRUE)}
    max.func<-function(x){max(x, na.rm=TRUE)}
    sd.func<-function(x){sd(x, na.rm=TRUE)}
    median.func<-function(x){median(x, na.rm=TRUE)}
    # specify a file name to write out data to the working direction
    #filename<-"app.csv"
    #
    # index set up
    #
    n.years<-length(unique(dat$YEAR))
    years<-unique(dat$YEAR)
    n.spp<-length(na.omit(unique(dat$SPP)))
    #print(n.spp)
    #print(unique(dat$SPP))
    #
    # exclude dates of creche or failure for now
    #
    dat<-dat[dat$MAXN!=4,]
    dat<-dat[dat$MAXN!=5,]
    #
    # omit NAs from Attendance data, if present
    #
    dat<-na.omit(dat)
    #
    # generate additional indices for nest-level analysis
    #
    n.cameras<-length(unique(dat$CAMERA))
    cameras<-unique(dat$CAMERA)
    #
    # now run estimation for each nest in each camera
    #
    cid.list<-list()
    repro.list<-list()
    info<-list()
    #
    # specify correction factors - these are Egg lay interval and incubation interval data derived from the chronolgoy data set available with the paper
    # in the matrix, row 1 is for Adelie, row 2 is for gentoo, and row 3 is for chintraps. Col 1 is the corrected E1-E2 interval, Col2 is the E1 to C1 incubation interval
    #
    xfactors<-matrix(c(3.172885, 36.44776, 2.419168,37.4352, 2.400504, 36.68471), ncol=2, byrow=TRUE)
    #
    # loop over each camera
    #
    for(i in 1:n.cameras){
      #
      # create space for data for nest-level data within camera
      #
      cam.dat<-dat[dat$CAMERA==cameras[i],]
      cam.rdat<-rdat[rdat$CAMERA==cameras[i],]
      n.nests<-length(unique(cam.dat$NEST))
      nests<-unique(cam.dat$NEST)
      switch.out<-numeric(n.nests)
      lay.dates<-numeric()
      hatch.dates<-numeric()
      eobs.dates<-numeric()
      eobs1.dates<-numeric()
      xfac.out<-data.frame(E1E2=NA, E1C1=NA)
      repro.out<-matrix(0, nrow=n.nests, ncol=7)
      #
      # loop over each nest within camera
      #
      for(j in 1:n.nests){
        nest.dat<-cam.dat[cam.dat$NEST==nests[j],]
        nest.rdat<-cam.rdat[cam.rdat$NEST==nests[j],]
        #
        # pull out the species code for proper estimation of the correction factors
        #
        Spec<-unique(nest.dat$SPP)
        xfac<-xfactors[Spec,]
        xfac.out<-rbind(xfac.out,xfac)
        #
        # first compute CID and HATCH Dates
        #
        if(dim(nest.dat)[1]>4 & max(nest.dat$MAXN, na.rm=TRUE)==2){
          # can only use a spline if you have more than 4 data points & you have some observations of 2 birds on the nest
          tt.spline<-smooth.spline(nest.dat$DATE, nest.dat$MAXN, df=degf)
          # derive the first derivatime of the splne
          pred.spline1<-predict(tt.spline, x=tt.spline$x, deriv=1)
          # specify conditions on spline data for picking the switch date
          #
          # conditions set here are that the switch data should occur when nest goes from 2 to 1, so the only days with that transition are included
          xx<-pred.spline1$x
          yy<-pred.spline1$y
          z<-c(nest.dat$MAXN[-1],1) # lags yy by one and finishes the observation with an arbitrary obs of 1 adult on the nest
          zz<-nest.dat$MAXN-z
          ttt<-data.frame(x=xx, y=yy, z=zz)
          #
          # bit to examine only initial 6 weeks of data
          if(dim(ttt)[1]>42){
            # for longer time series that are cut to 42 days
            ttt<-ttt[1:42,] # CID should occur within first few weeks of attendance data collection
            if(max(ttt$y==1)){
              # no observations of 2 adults on the nest precludes estimation based on attendance.
              switch.out[j]<-NA
              switch<-NA
            } else {
              # where there are obs of 2 adults in the window
              ttt<-ttt[ttt$z==1,] # force switch to occur when obs goes from 2 to 1
              if(dim(ttt)[1]>0){
                switch<-ttt$x[ttt$y==min(ttt$y)] # select minimum slope
                #
                # format switch value as date
                switch<-as.POSIXct(as.Date(switch/(60*60*24), origin="1970-1-1"))
                # ADD IN CHECK TO SEE IF EARLIEST EGG OBS OCCURS BEFORE THE ESTIMATED switch. IF SO, USE THAT AS THE ESTIMATED CID
                # First, check if a LAY date is confirmed
                lay.dat<-na.omit(nest.rdat$DATE[nest.rdat$LAY==1])
                if(length(lay.dat)>0){
                  lay.date<-min(na.omit(lay.dat))
                  lay.dates[j]<-lay.date
                } else {
                  lay.dates[j]<-NA
                }
                # Also check to see if hatch date is confirmed
                hatch.dat<-na.omit(nest.rdat$DATE[nest.rdat$HATCH==1])
                if(length(hatch.dat)>0){
                  hatch.date<-min(na.omit(hatch.dat))
                  hatch.dates[j]<-hatch.date
                } else {
                  hatch.dates[j]<-NA
                }
                eobs.dat<-na.omit(nest.rdat$DATE[nest.rdat$MAXE>0])
                if(length(eobs.dat>0)){
                  eobs.dates[j]<-min(eobs.dat)
                  eobs1.dates[j]<-min(eobs.dat)
                } else {
                  # if no egg observations were made, add 10 days to the estimated CID(switch date)
                  # note that the above addition of 10 days is  as a placeholder only to simplify later calculations
                  lay.dates[j]<-NA
                  hatch.dates[j]<-NA
                  eobs.dates[j]<-switch+10*60*60*24
                  eobs1.dates[j]<-NA
                }
                              #
                switch.out[j]<-switch
              } else {
                switch.out[j]<-NA
                lay.dates[j]<-NA
                hatch.dates[j]<-NA
                eobs.dates[j]<-NA
                eobs1.dates[j]<-NA
              }
            }
          } else {
            if(max(ttt$y==1)){
              # no observations of 2 adults on the nest precludes estimation based on attendance.
              switch.out[j]<-NA
              lay.dates[j]<-NA
              hatch.dates[j]<-NA
              eobs.dates[j]<-NA
              eobs1.dates[j]<-NA
              switch<-NA
            } else {
              # for any record shorter than 42 days
              ttt<-ttt[ttt$z==1,] # force switch to occur when obs goes from 2 to 1
              switch<-ttt$x[ttt$y==min(ttt$y)] # select minimum slope
              #
              # format switch value as date
              switch<-as.POSIXct(as.Date(switch/(60*60*24), origin="1970-1-1"))
              #
              switch.out[j]<-switch
              #
              # ADD IN CHECK TO SEE IF EARLIEST EGG OBS OCCURS BEFORE THE ESTIMATED CID. IF SO, USE THAT AS THE ESTIMATED CID
              # First, check if a LAY date is confirmed
              lay.dat<-na.omit(nest.rdat$DATE[nest.rdat$LAY==1])
              if(length(lay.dat)>0){
                lay.date<-min(na.omit(lay.dat))
                lay.dates[j]<-lay.date
              } else {
                lay.dates[j]<-NA
              }
              # Also check to see if hatch date is confirmed
              hatch.dat<-na.omit(nest.rdat$DATE[nest.rdat$HATCH==1])
              if(length(hatch.dat)>0){
                hatch.date<-min(na.omit(hatch.dat))
                hatch.dates[j]<-hatch.date
              } else {
                hatch.dates[j]<-NA
              }
              eobs.dat<-na.omit(nest.rdat$DATE[nest.rdat$MAXE>0])
              if(length(eobs.dat>0)){
                eobs.dates[j]<-min(eobs.dat)
                eobs1.dates[j]<-min(eobs.dat)
              } else {
                # if no egg observations were made, add 10 days to the estimated CID(switch date)
                eobs.dates[j]<-switch+10*60*60*24
                eobs1.dates[j]<-NA
              }

           }
        }
      } else {
        # for any data set with too few points for estimating a spline
        switch.out[j]<-NA
        lay.dates[j]<-NA
        hatch.dates[j]<-NA
        eobs.dates[j]<-NA
        eobs1.dates[j]<-NA
        print(paste("Not enough data to estimate CID with spline for camera ", cameras[i], " nest ", nests[j], sep=""))
      }
      #
      # now compute nest level success/failure
      eggs<-na.omit(nest.rdat$MAXE)
      if(length(eggs)>0){
        max.egg<-max(eggs)
      } else {
         max.egg<-0
      }
      #
      chicks<-na.omit(nest.rdat$MAXC)
      if(length(chicks)>0){
        max.chick<-max(chicks)
      } else {
         max.chick<-0
      }
      #
      creche<-na.omit(nest.rdat$CRECHE)
      if(length(creche)>0){
        max.creche<-max(creche)
      } else {
        max.creche<-0
      }
      # now deal with dates for specific observations of ACTUAL LAY
      #print(summary(nest.rdat))
      obs.lay<-na.omit(data.frame(date=nest.rdat$DATE, lay=nest.rdat$LAY))
      if(dim(obs.lay)[1]>0){
        observed.lay<-min(obs.lay$date, na.rm=TRUE)
      } else {
        observed.lay<-NA
      }
      #
      obs.hatch<-na.omit(data.frame(date=nest.rdat$DATE, hatch=nest.rdat$HATCH))
      if(dim(obs.hatch)[1]>0){
        observed.hatch<-min(obs.hatch$date, na.rm=TRUE)
      } else {
        observed.hatch<-NA
      }
      #
      obs.creche<-na.omit(data.frame(date=nest.rdat$DATE, creche=nest.rdat$CRECHE))
      if(dim(obs.creche)[1]>0){
        observed.creche.date<-min(obs.creche$date, na.rm=TRUE)
      } else {
        observed.creche.date<-NA
      }
      spp<-nest.rdat$SPP[1]
      #
      repro.out[j,]<-c(spp, max.egg, max.chick, max.creche, observed.lay, observed.hatch, observed.creche.date)
      }
      #
      # now apply corrections to the estimated clutch completion date to get estimated CID and HATCH dates based on repro data
      xfac.out<-na.omit(xfac.out)
      CID<-switch.out-(xfac.out[,1]*(60*60*24))
      JCID<-as.POSIXct(as.Date(CID/(60*60*24), origin="1970-1-1"))
      JCID<-lubridate::yday(JCID)
      # ensure the JCID is continuous over the new year
      # first get the last day of the year (assuming only data from one field season are used)
      Ndays<-max(JCID)
      JCID<-ifelse(JCID<100, JCID+Ndays, JCID)
      ORIG.CID<-CID
      # now replace estimated CID with observed CID if observation of egg was before estimated CID
      j.eobs.dates<-as.POSIXct(as.Date(eobs.dates/(60*60*24), origin="1970-1-1"))
      j.eobs.dates<-lubridate::yday(j.eobs.dates)
      # ensure the j.eobs.dates is continuous over the new year
      j.eobs.dates<-ifelse(j.eobs.dates<100, j.eobs.dates+Ndays, j.eobs.dates)
      CIDcheck<-j.eobs.dates<JCID # if an  egg obs date occurs before estimated CID, identify those records as TRUE
      # note that, on average, the first observation of an egg in the nest occurs, on aveage, about 2 days after lay, based on our validation data
      # this allows use of first egg observation as a proxy for clutch completion
      CID<-ifelse(CIDcheck, eobs.dates-2*(60*60*24), CID)
      type<-ifelse(CIDcheck, "Cor", "Est")
      orig.switch<-ifelse(CIDcheck, switch.out, NA)
      # now, if a lay date was confirmed in the photo repro, use the observed lay date rather than the estimated CID
      lay.index<-!is.na(lay.dates>1)
      CID<-ifelse(lay.index, lay.dates, CID)
      type<-ifelse(lay.index, "Cor", type)
      # also, correct switch date if CID was corrected
      orig.switch<-ifelse(lay.index, switch.out, orig.switch)
      switch.index<-type=="Est"
      switch.new<-ifelse(switch.index, switch.out, CID+xfac.out[,1]*(60*60*24))
      #
      HATCH<-CID+(xfac.out[,2]*(60*60*24)) # based on mean time from E1 to C1
      JHATCH<-as.POSIXct(as.Date(HATCH/(60*60*24), origin="1970-1-1"))
      JHATCH<-lubridate::yday(JHATCH)
      JHATCH<-ifelse(JHATCH<300, 366+JHATCH,JHATCH) # make sure january hatches are correctly coded
      # Now correct hatch if observed hatch occured prior to estimated hatch
      j.hatch.dates<-as.POSIXct(as.Date(hatch.dates/(60*60*24), origin="1970-1-1"))
      j.hatch.dates<-lubridate::yday(j.hatch.dates)
      # ensure j.hatch.dates are continious over the new year
      j.hatch.dates<-ifelse(j.hatch.dates<100, j.hatch.dates+Ndays, j.hatch.dates)
      hatch.index<-j.hatch.dates<JHATCH
      hatch.index<-ifelse(is.na(hatch.index), FALSE, hatch.index) # make sure january hatches are correctly coded
      HATCH<-ifelse(hatch.index, hatch.dates, HATCH)
      type<-ifelse(hatch.index, "Cor", type)
      ORIG.CID<-ifelse(type=="Est", NA, ORIG.CID)
      out<-data.frame(Type=type, Orig_Switch=as.Date(orig.switch/(60*60*24), origin="1970-1-1"), Orig_CID=as.Date(ORIG.CID/(60*60*24), origin="1970-1-1"), Switch=as.Date(switch.new/(60*60*24), origin="1970-1-1"), CID=as.Date(CID/(60*60*24), origin="1970-1-1"),HATCH=as.Date(HATCH/(60*60*24), origin="1970-1-1"), EOBS=as.Date(eobs1.dates/(60*60*24),origin="1970-1-1"))
      repro.df<-data.frame(repro.out)
      cid.list[[i]]<-out
      repro.list[[i]]<-repro.out
      which.year<-unique(cam.dat$YEAR)
      year<-rep(which.year, n.nests)
      cam<-rep(cameras[i], n.nests)
      hinfo<-data.frame(YEAR=year, CAMERA=cam, NEST=nests)
      info[[i]]<-hinfo
    }
    names(info)<-cameras
    names(cid.list)<-cameras
    names(repro.list)<-cameras
    #
    # now append creche date and reproductive success data
    cid<-do.call("rbind", cid.list)
    repro<-do.call("rbind", repro.list)
    headers<-do.call("rbind", info)
    #
    out<-cbind(cid, repro)
    names(out)<-c("TYPE", "ORIG_SWITCH", "ORIG_CID", "SWITCH","CID", "HATCH", "1stEOBS", "SPP", "EGGS","CHICKS","CRECHE","OBS.LAY.DATE", "OBS.HATCH.DATE", "CRECHE.DATE")
    out<-cbind(headers, out)
    # convert dates to Date objects
    out$OBS.LAY.DATE<-as.Date(out$OBS.LAY.DATE/(60*60*24), origin="1970-1-1")
    out$OBS.HATCH.DATE<-as.Date(out$OBS.HATCH.DATE/(60*60*24), origin="1970-1-1")
    out$CRECHE.DATE<-as.Date(out$CRECHE.DATE/(60*60*24), origin="1970-1-1")
    out$CRECHE.AGE<-out$CRECHE.DATE-out$HATCH
    #
    # now remove estimates of hatch and creche dates for nest where no chicks were hatched and/or creched
    out$HATCH<-ifelse(out$CHICKS>0, out$HATCH, NA)
    out$CRECHE.DATE<-ifelse(out$CRECHE>0, out$CRECHE.DATE, NA)
    out$HATCH<-as.Date(out$HATCH, origin="1970-1-1")
    out$CRECHE.DATE<-as.Date(out$CRECHE.DATE, origin="1970-1-1")
    #write.csv(out, "out.csv")
    #print(filename)
    # specific code for app output - whether "Raw", "A6b", or "A9"
    if(tabletype=="Raw"){
      app_out<-data.frame(Year=out$YEAR, Species=out$SPP, Camera=out$CAMERA, Nest=out$NEST, CID=as.character(out$CID), Hatch=as.character(out$HATCH), Creche=as.character(out$CRECHE.DATE), N_Eggs=out$EGGS, N_Hatch=out$CHICKS, N_Creche=out$CRECHE)
      # update numbers of eggs that must have been in the nest given maximum n of chick and creche obs
      prod.dat<-data.frame(Egg=app_out$N_Eggs, Chick=app_out$N_Hatch, Creche=app_out$N_Creche)
      prod.dat<-apply(prod.dat, 1, max)
      app_out$N_Eggs<-prod.dat # updated N_Eggs with the number that must have been there given observations of chicks and creche
      # update numbers of eggs that must have been in the nest given maximum n of chick and creche obs
      prod.dat<-data.frame(Chick=app_out$N_Hatch
                           , Creche=app_out$N_Creche)
      prod.dat<-apply(prod.dat, 1, max)
      app_out$N_Hatch<-prod.dat
      #write.csv(app_out, file="app_out_raw.csv")
    }
    if(tabletype=="CEMP Protocol A6b"){
        # create A6b data from the output file created by the phenology_app
        Spps<-na.omit(unique(out$SPP))
        Spps<-Spps[order(Spps)]
        Spps<-as.character(Spps)
        # cell 11 in for min of observation date, or first date in attendacne data
        r11<-tapply(dat$DATE, dat$SPP, min.func)
        r11<-as.character(as.Date(r11/(60*60*24), origin="1970-1-1"))
        # cell 12 is for the max of the observation date, or final CRECHE date
        r12<-tapply(out$CRECHE.DATE, out$SPP, max.func)
        r12<-as.character(as.Date(r12, origin="1970-1-1"))
        # cell 13 is the first CID
        r13<-tapply(out$CID, out$SPP, min.func)
        r13<-as.character(as.Date(r13, origin="1970-1-1"))
        # cell 14 is the date of first female departure, or the first switch date
        r14<-tapply(out$SWITCH, out$SPP, min.func)
        r14<-as.character(as.Date(r14, origin="1970-1-1"))
        # cell 15 is the date of first hatch
        r15<-tapply(out$HATCH, out$SPP, min.func)
        r15<-as.character(as.Date(r15, origin="1970-1-1"))
        # cell 16 is the first creche date
        r16<-tapply(out$CRECHE.DATE, out$SPP, min.func)
        r16<-as.character(as.Date(r16, origin="1970-1-1"))
        # cell 17 is the count of nests
        r17<-tapply(out$NEST, out$SPP, length)
        # cell 18 is the count of nests with only one confirmed egg
        # cell 19 is the count of nests with two confirmed eggs - do 17 first
        prod.dat<-data.frame(Egg=out$EGGS, Chick=out$CHICKS, Creche=out$CRECHE)
        prod.dat<-apply(prod.dat, 1, max)
        out$neggs<-prod.dat
        outa<-out[out$neggs>=1,] # to get rid of nests that maybe have 0 eggs - though this shouldn't occur unless dummy nests are included
        #assume that the number of records where max=2 is the number of nests with 2 eggs.
        # assume that the differce from the number of nests is then the number of nests with 1 egg clutches
        # If the data set only as nests with the same number of eggs, (either 1 or 2 in all nests)
        # then you need to tell the code to count the missing nests - tapply won't, by itself, know to count 0 for the nests that aren't present
        # setting the levels of the nests forces that level to be counted.
        if(length(unique(outa$neggs))==1){
          outa$neggsf<-ordered(outa$neggs, levels=c(1,2))
          r1819<-tapply(outa$NEST, list(outa$neggsf, outa$SPP), length)
        } else {
          r1819<-tapply(outa$NEST, list(outa$neggs, outa$SPP), length)
        }
         r1819<-ifelse(is.na(r1819),0, r1819)
        # cell 20 is the sum of eggs that hatched from the 1 egg nest
        # cell 21 is the sum of eggs that hatched from the 2 egg nests
        # need to fill in missing chick obs. in case they weren't observed, but known to have creched
        prod.dat<-data.frame(Chick=out$CHICKS, Creche=out$CRECHE)
        prod.dat<-apply(prod.dat, 1, max)
        out$nchicks<-prod.dat
        outa<-out[out$nchicks>=1,]
        if(length(unique(outa$nchicks))==1){
          outa$nchicksf<-ordered(outa$nchicks, levels=c(1,2))
          r2021<-tapply(outa$nchicks, list(outa$nchicksf, outa$SPP), sum)
        } else {
          r2021<-tapply(outa$nchicks, list(outa$nchicks, outa$SPP), sum)
        }
        r2021<-ifelse(is.na(r2021), 0, r2021)
        # cell 22 is the count of nests raising 1 chick to creche
        # cell 23 is the count of nests raising 2 chicks to creche
        outa<-out[out$CRECHE>=1,]
        if(length(unique(outa$CRECHE))==1){
          outa$ncrechef<-ordered(outa$CRECHE, levels=c(1,2))
          r2223<-tapply(outa$NEST, list(outa$ncrechef,outa$SPP), length)
        } else {
          r2223<-tapply(outa$NEST, list(outa$CRECHE,outa$SPP), length)
        }
        r2223<-ifelse(is.na(r2223), 0, r2223)
        # cell 24 is the mean number of nests reared to creche per nest.
        # cell 25 is the sd of the the mean calculated above.
        # here, we assume the camera is the unit from which to calculate a mean
        if(n.cameras==1){
          r24<-tapply(out$CRECHE, list(out$CAMERA, out$SPP), mean.func)
          r25<-tapply(out$CRECHE, list(out$CAMERA, out$SPP), sd.func)
        } else {
          est.mbar<-tapply(out$CRECHE, list(out$CAMERA, out$SPP), mean.func)
          r24<-apply(est.mbar,2, mean.func)
          r25<-apply(est.mbar,2, sd.func)
        }
        #est.mbar<-tapply(out$CRECHE, list(out$CAMERA, out$SPP), mean.func)
        #r22<-apply(est.mbar,2, mean.func)
        #r23<-apply(est.mbar,2, sd.func)
        # now combine for final output
        outA6b<-rbind(r11, r12, r13, r14, r15, r16, r17, r1819, r2021, r2223, r24, r25)
        outA6b<-data.frame(Cell=11:25, outA6b)
        names(outA6b)<-c("Cell", Spps)
        app_out<-outA6b
        #write.csv(app_out, file="app_out_A6b.csv")
    }
    if(tabletype=="CEMP Protocol A9"){
      # exclude nest for which no estimate of CID was possible
      out<-out[!is.na(out$CID),]
      Spps<-na.omit(unique(out$SPP))
      Spps<-Spps[order(Spps)]
      Spps<-as.character(Spps)
      # create A9 data from the input and output files
      # this requires a 5-day interval in which to bin estimated CID, HATCH, and CRECHE
      d1<-min(dat$DATE)
      d1<-as.Date(d1)
      dend<-max(out$CRECHE.DATE, na.rm=TRUE)
      dend<-as.Date(dend)
      dseq<-seq(lubridate::ymd(d1), lubridate::ymd(dend), by="day")
      n.out<-ceiling(length(dseq)/5)
      n.out2<-ceiling(length(dseq))
      dseq<-as.character(dseq)
      dseq<-substr(dseq,1,10)# ensure that the date contains only yyyy-mm-dd
      ints<-rep(1:n.out, each=5)
      ints<-ints[1:length(dseq)]
      ints<-data.frame(DATE=dseq, GROUP=ints)
      ints$DATE<-as.character(ints$DATE)
      keep<-seq(from=1, by=5, to=n.out2)
      dseqch<-ints$DATE[keep]

      # now estimate timeline of CID

      tttt<-data.frame(SPP=out$SPP, DATE=as.character(out$CID), N_Eggs=out$EGGS)
      tttt$DATE<-as.character(tttt$DATE)
      tttt<-na.omit(tttt)

      intsm<-merge(tttt, ints, by="DATE", all=TRUE)
      intsm$egg<-ifelse(is.na(intsm$N_Eggs),0,1)
      c1<-tapply(intsm$egg, list(intsm$GROUP, intsm$SPP), sum)
      nnests<-colSums(c1, na.rm=TRUE)

        # now for female departure

      tttt<-data.frame(SPP=out$SPP, DATE=as.character(out$SWITCH), SWITCH=out$SWITCH)
      tttt$DATE<-as.character(tttt$DATE)
      tttt<-na.omit(tttt)

      intsm<-merge(tttt, ints, by="DATE", all=TRUE)
      intsm$switch<-ifelse(is.na(intsm$SWITCH),0,1)
      c2<-tapply(intsm$switch, list(intsm$GROUP, intsm$SPP), sum)
      nswitch<-colSums(c2, na.rm=TRUE)

      # now for hatching

      tttt<-data.frame(SPP=out$SPP, DATE=as.character(out$HATCH), N_Hatch=out$CHICKS)
      tttt$DATE<-as.character(tttt$DATE)
      tttt<-na.omit(tttt)

      intsm<-merge(tttt, ints, by="DATE", all=TRUE)
      intsm$hatch<-ifelse(is.na(intsm$N_Hatch),0,1)
      c3<-tapply(intsm$hatch, list(intsm$GROUP, intsm$SPP), sum)
      nhatch<-colSums(c3,na.rm=TRUE)

      # now for creche

      tttt<-data.frame(SPP=out$SPP, DATE=as.character(out$CRECHE.DATE), N_Creche=out$CRECHE)
      tttt$DATE<-as.character(tttt$DATE)
      tttt<-na.omit(tttt)

      intsm<-merge(tttt, ints, by="DATE", all=TRUE)
      intsm$creche<-ifelse(is.na(intsm$N_Creche),0,1)
      c4<-tapply(intsm$creche, list(intsm$GROUP, intsm$SPP), sum)
      ncreche<-colSums(c4, na.rm=TRUE)
      efails<-nnests-nhatch
      hfails<-nhatch-ncreche
      # for now, this protocol doesn't recognize arrival or fledging, but it could in the future
      # add the arrival and fledge columns to the output as all NA
      arrive<-matrix(NA, ncol=n.spp, nrow=dim(c1)[1])
      fledge<-arrive
      outA9<-data.frame(dseqch, arrive, c1, c2, c3, c4, fledge)
      NAMES<-c("A",rep(c("B","C","D","E","F","G"), each=n.spp))
      r1<-c("Date", rep(Spps, 6))
      outA9<-rbind(r1, outA9)
      names(outA9)<-NAMES

      # create the summary data for category B data
      row1<-c("Total monitored", rep(NA,n.spp), nnests, nswitch, nhatch, ncreche,rep(NA, n.spp))
      row2<-c("Total failed", rep(NA, n.spp), efails, rep(NA, n.spp), hfails, rep(0,n.spp),rep(NA, n.spp))
      row1<-data.frame(rbind(row1, row2))
      names(row1)<-names(outA9)
      outA9<-rbind(outA9, row1)

      # now create the category C summary data
      medlay<-tapply(out$CID, out$SPP, median.func)
      medlay<-as.character(as.Date(medlay, origin="1970-1-1"))
      medswitch<-tapply(out$SWITCH, out$SPP, median.func)
      medswitch<-as.character(as.Date(medswitch, origin="1970-1-1"))
      medhatch<-tapply(out$HATCH, out$SPP, median.func)
      medhatch<-as.character(as.Date(medhatch, origin="1970-1-1"))
      medcreche<-tapply(out$CRECHE.DATE, out$SPP, median.func)
      medcreche<-as.character(as.Date(medcreche, origin="1970-1-1"))

      #mode
      # the following code for computing the mode was taken from RBloggers
      Mode = function(x){
        ta = table(x)
        tam = max(ta)
        if (all(ta == tam))
          mod = NA
        else
          if(is.numeric(x))
            mod = as.numeric(names(ta)[ta == tam])
        else
          mod = names(ta)[ta == tam]
        return(mod)
      }
      # the function only works for character or numeric classes
      modlay<-tapply(as.character(out$CID), out$SPP, Mode)
      if(is.list(modlay)){
        #implies the Mode found multiple values with the same frequency, i.e., there is no true mode.
        # this will set the mode value to NA for such instances (or perhaps, select the first instance)
        mlay<-numeric(n.spp)
        for(i in 1:n.spp){
          mlay[i]<-modlay[[i]][1]
        }
        modlay<-mlay
      }
      modlay<-as.character(as.Date(modlay, origin="1970-1-1"))
      # for female depature
      modswitch<-tapply(as.character(out$SWITCH), out$SPP, Mode)
      if(is.list(modswitch)){
        #implies the Mode found multiple values with the same frequency, i.e., there is no true mode.
        # this will set the mode value to NA for such instances (or perhaps, select the first instance)
        mswitch<-numeric(n.spp)
        for(i in 1:n.spp){
          mswitch[i]<-modswitch[[i]][1]
        }
        modswitch<-mswitch
      }
      modswitch<-as.character(as.Date(modswitch, origin="1970-1-1"))
      # now hatch
      modhatch<-tapply(as.character(out$HATCH), out$SPP, Mode)
      if(is.list(modhatch)){
        #implies the Mode found multiple values with the same frequency, i.e., there is no true mode.
        # this will set the mode value to NA for such instances (or perhaps, select the first instance)
        mlay<-numeric(n.spp)
        for(i in 1:n.spp){
          mlay[i]<-modhatch[[i]][1]
        }
        modhatch<-mlay
      }
      modhatch<-as.character(as.Date(modhatch, origin="1970-1-1"))
      modcreche<-tapply(as.character(out$CRECHE.DATE), out$SPP, Mode)
      if(is.list(modcreche)){
        #implies the Mode found multiple values with the same frequency, i.e., there is no true mode.
        # this will set the mode value to NA for such instances (or perhaps, select the first instance)
        mlay<-numeric(n.spp)
        for(i in 1:n.spp){
          mlay[i]<-modcreche[[i]][1]
        }
        modcreche<-mlay
      }
      #print(modcreche)
      modcreche<-as.character(as.Date(modcreche, origin="1970-1-1"))
      #
      minlay<-tapply(out$CID, out$SPP, min.func)
      minlay<-as.character(as.Date(minlay, origin="1970-1-1"))
      minswitch<-tapply(out$SWITCH, out$SPP, min.func)
      minswitch<-as.character(as.Date(minswitch, origin="1970-1-1"))
      minhatch<-tapply(out$HATCH, out$SPP, min.func)
      minhatch<-as.character(as.Date(minhatch, origin="1970-1-1"))
      mincreche<-tapply(out$CRECHE.DATE, out$SPP, min.func)
      mincreche<-as.character(as.Date(mincreche, origin="1970-1-1"))
      #
      maxlay<-tapply(out$CID, out$SPP, max.func)
      maxlay<-as.character(as.Date(maxlay, origin="1970-1-1"))
      maxswitch<-tapply(out$SWITCH, out$SPP, max.func)
      maxswitch<-as.character(as.Date(maxswitch, origin="1970-1-1"))
      maxhatch<-tapply(out$HATCH, out$SPP, max.func)
      maxhatch<-as.character(as.Date(maxhatch, origin="1970-1-1"))
      maxcreche<-tapply(out$CRECHE.DATE, out$SPP, max.func)
      maxcreche<-as.character(as.Date(maxcreche, origin="1970-1-1"))
      # now the funky indices
      #95% nests with eggs - assumed to be 95th percentile of CID
      qfunc<-function(x){quantile(x, 0.95)}
      tth<-out[!is.na(out$CID),]
      tth$CIDa<-as.character(tth$CID)
      tth$CIDa<-as.POSIXct(strptime(tth$CIDa, format="%Y-%m-%d"))
      qCID<-tapply(tth$CIDa, tth$SPP, qfunc)
      qCID<-as.character(as.Date(qCID/(24*60*60), origin="1970-1-1"))
      #95% of depatures
      tts<-out[!is.na(out$SWITCH),]
      tts$Sa<-as.character(tts$SWITCH)
      tts$Sa<-as.POSIXct(strptime(tts$Sa, format="%Y-%m-%d"))
      qS<-tapply(tts$Sa, tts$SPP, qfunc)
      qS<-as.character(as.Date(qS/(24*60*60), origin="1970-1-1"))

      # 1/3 eggs hatching
      qfunc<-function(x){quantile(x, 0.333)}
      tth<-out[!is.na(out$HATCH),]
      tth$HATCHa<-as.character(tth$HATCH)
      tth$HATCHa<-as.POSIXct(strptime(tth$HATCHa, format="%Y-%m-%d"))
      qHatch<-tapply(na.omit(tth$HATCHa), tth$SPP, qfunc)
      qHatch<-as.character(as.Date(qHatch/(24*60*60), origin="1970-1-1"))
      qfunc<-function(x){quantile(x, 0.666)}
      #
      tth<-out[!is.na(out$CRECHE.DATE),]
      tth$CRECHEa<-as.character(tth$CRECHE.DATE)
      tth$CRECHEa<-as.POSIXct(strptime(tth$CRECHEa, format="%Y-%m-%d"))
      qCreche<-tapply(na.omit(tth$CRECHEa), tth$SPP, qfunc)
      qCreche<-as.character(as.Date(qCreche/(24*60*60), origin="1970-1-1"))

      row1<-c("Median Dates",rep(NA, n.spp), medlay, medswitch, medhatch, medcreche,rep(NA, n.spp))
      row2<-c("Mode Dates", rep(NA, n.spp), modlay, modswitch, modhatch,modcreche, rep(NA, n.spp))
      row3<-c("First Dates", rep(NA, n.spp), minlay, minswitch, minhatch, mincreche,rep(NA, n.spp))
      row4<-c("Last Dates", rep(NA, n.spp), maxlay, maxswitch, maxhatch, maxcreche, rep(NA, n.spp))
      row5<-c("Nest at 95%", rep(NA, n.spp), qCID, rep(NA,4*n.spp))
      row6<-c("F Departures at 95%", rep(NA, n.spp), qS, rep(NA,4*n.spp))
      row7<-c("Hatch at 33%", rep(NA, n.spp), qHatch, rep(NA, 4*n.spp))
      row8<-c("Creche at 66%", rep(NA, n.spp), qCreche,rep(NA,4*n.spp))
      row8<-data.frame(rbind(row1, row2, row3, row4, row5, row6, row7, row8))
      names(row8)<-names(outA9)
      outA9<-rbind(outA9, row8)
      # now reformat the output with reasonable headers and species grouped together
      NAMES<-c("Date-Header", rep("N_arriving", n.spp), rep("N_lay", n.spp), rep("N_F_departure", n.spp), rep("N_hatch", n.spp), rep("N_cease", n.spp), rep("N_fledge", n.spp))
      names(outA9)<-NAMES
      xx<-t(as.matrix(outA9))
      xx<-t(xx[order(xx[,1]),])
      ColF<-dim(xx)[2]
      outA9<-cbind(xx[,ColF], xx[,-ColF])
      outA9[1,1]<-"Species code ->"
      app_out<-outA9
      #write.csv(app_out, file="app_out_A9.csv")
    }
  #  return(app_out)
  #} else {
  #  app_out<-do.call("rbind", out)
  #  return(app_out)
  #}
  return(app_out)
  # end of file
}
