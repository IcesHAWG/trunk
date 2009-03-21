writeICAsum <- function(stk,fname) {
    #Setup header - copied directly from the heart of ICA
    hdr <-  rbind(" ³ Year ³  Recruits  ³  Total  ³ Spawning³ Landings ³ Yield ³ Mean F ³ SoP ³",
                  " ³      ³   Age      ³ Biomass ³ Biomass ³          ³ /SSB  ³  Ages  ³     ³",
                  " ³      ³  thousands ³  tonnes ³ tonnes  ³ tonnes   ³ ratio ³        ³ (%) ³",
                  "\n")

    #Add extra information into header
    rec.age  <- dims(stk)$min
    f.ages   <- range(stk)[6:7]
    substr(hdr[2],19,19) <- as.character(rec.age)
    substr(hdr[3],63,64) <- sprintf("%2i",f.ages[1])
    substr(hdr[3],65,65) <- "-"
    substr(hdr[3],66,67) <- sprintf("%2i",f.ages[2])

    #Setup data content
    dat <- cbind(Year=dims(stk)$minyear:dims(stk)$maxyear,
                 Recruits=rec(stk),
                 TSB=stk@stock,
                 SSB=ssb(stk),
                 Landings=stk@landings,
                 Yield.SSB=stk@landings/ssb(stk),
                 Fbar=fbar(stk),
                 SoP=100*sop(stk))

    #And format it, following the following Fortran format string
    #160   format(3X,I4,3X,I9,'0',3X,I7,3X,I7,3X,I7,3X,F6.4,3X,F6.4,3X,I3)
    dat.fmt <- sprintf("   %4i   %9.0f0   %7.0f   %7.0f   %7.0f   %6.4f   %6.4f   %3.0f",
                dat[,1],dat[,2],dat[,3],dat[,4],dat[,5],dat[,6],dat[,7],dat[,8])

    #Write it out to a file
    output <- c(hdr,dat.fmt)
    write(output,fname)
}



