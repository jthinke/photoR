## code to prepare `create_data.R` data set goes here

test_att<-read.csv(file="inst/extdata/test_att.csv", stringsAsFactors=FALSE, header=TRUE)
test_att$DATE<-as.POSIXct(strptime(test_att$DATE, format="%m/%d/%Y"), tz="GMT")

test_repro<-read.csv(file="inst/extdata/test_repro.csv", stringsAsFactors=FALSE, header=TRUE)
test_repro$DATE<-as.POSIXct(strptime(test_repro$DATE, format="%m/%d/%Y"), tz="GMT")
usethis::use_data(test_repro, overwrite = TRUE)
usethis::use_data(test_att, overwrite=TRUE)
