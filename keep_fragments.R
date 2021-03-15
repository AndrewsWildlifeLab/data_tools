
rocky_frag<-c("1E33","7819","3378")

beeps_rocky1<-filter(beeps, str_detect(TagId, rocky_frag)==TRUE)
