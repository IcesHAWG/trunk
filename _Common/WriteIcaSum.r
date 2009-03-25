## writeFLStock	{{{
writeFLStock <- function(FLStock, output.file=FLStock@name, type="VPA") {
	if (!inherits(FLStock, "FLStock"))
		stop("FLStock must be an 'FLStock' object!")
	switch(type,
		"VPA" = writeVPA(FLStock, output.file),
		"ICAsum" = writeICAsum(FLStock, output.file),
		"YPR"  = writeYPR(FLStock, output.file),
		stop("type must be either 'VPA','ICAsum' or 'YPR'!"))
}	# }}}


writeICAsum <- function(stk,fname) {
    #Setup header - copied directly from the heart of ICA
    hdr <-  rbind("³ Year ³  Recruits  ³  Total  ³ Spawning³ Landings ³ Yield ³ Mean F ³ SoP ³",
                  "³      ³   Age      ³ Biomass ³ Biomass ³          ³ /SSB  ³  Ages  ³     ³",
                  "³      ³  thousands ³  tonnes ³ tonnes  ³ tonnes   ³ ratio ³        ³ (%) ³",
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
    dat.fmt <- sprintf("  %4i   %9.0f    %7.0f   %7.0f   %7.0f   %6.4f   %6.4f   %3.0f",
                dat[,1],dat[,2],dat[,3],dat[,4],dat[,5],dat[,6],dat[,7],dat[,8])

    #Write it out to a file
    output <- c(hdr,dat.fmt)
    write(output,fname)
}

# writeVPA - Mark Payne, DTU-Aqua		{{{
writeVPA <- function(FLStock, output.file=FLStock@name,slots="missing") {
    #Check for dimensions that Lowestoft VPA can't handle
    if(dims(FLStock)$iter >1) stop("FLStock object contains more than
one iteration. Lowestoft VPA can't handle this unfortunately.")
    if(dims(FLStock)$area >1) stop("FLStock object contains more than
one area. Lowestoft VPA can't handle this unfortunately.")
    if(dims(FLStock)$season >1) stop("FLStock object contains more than
one season. Lowestoft VPA can't handle this unfortunately.")
    if(dims(FLStock)$unit >1) stop("FLStock object contains more than
one unit. Lowestoft VPA can't handle this unfortunately.")

    #List of configuration information
    config.df  <-  rbind(
      c(1, "landings", "LATON", "total landings"),
      c(25, "landings.n", "LANUM", "landings-at-age"),
      c(26, "landings.wt", "WELAND", "landings weight-at-age"),
      c(4, "stock.wt", "WEST", "stock weight-at-age"),
      c(5, "m", "NATMOR", "natural mortality"),
      c(6, "mat", "MATPROP", "maturity-at-age ogive"),
      c(7, "harvest.spwn", "FPROP", "proportion of F before spawning"),
      c(8, "m.spwn", "MPROP", "proportion of M before spawning"),
      c(12,"harvest", "F", "fishing mortality"),
      c(13,"stock.n", "N", "stock numbers at age"),
      c(21, "discards", "DISTON", "total discards"),
      c(22, "discards.n", "DISNUM", "discards-at-age"),
      c(23, "discards.wt", "WEDIS", "discards weight-at-age"),
      c(24, "catch", "CATON", "total catch"),
      c(2, "catch.n", "CANUM", "catch-at-age"),
      c(3, "catch.wt", "WECA", "catch weight-at-age"))
    colnames(config.df) <-  c("idx","stock.slot","file.ext","desc")

    config.df   <- as.data.frame(config.df)

    #Only write the harvest slot if units are f.
    if(!any(units(FLStock@harvest) %in% c("f","F")))
    {
      config.df <- config.df[-which(config.df$stock.slot=="harvest"),]
      warning("Harvest slot is not a fishing mortality - it will not
        be written as part of the output.")
    }

    #calculates the numbers of ages
    nage  <- FLStock@range[2]-FLStock@range[1]+1
    #calculates the number of years
    nyear <- dims(FLStock)$year

    #Function to write files. Based on original code by David Bromley, CEFAS
    writeVPAFile <- function(file.ext,stock.slot,idx,desc)
    {
      # handles the annoying "strings as factors" option
      stock.slot <- as.character(stock.slot)
      # open the output file connections
      temp <- try(file(paste(output.file,"-",file.ext,".txt",sep=""),
        "w"),silent=TRUE)
      if(is(temp,"try-error"))
      {
        stop(paste("Cannot open output file. The supplied name,
        \"",output.file,"\", may not be a valid filename. Try setting the
        output.file argument.",sep=""))
        }
        # adds the VPA format info to the begining of each file
        cat(paste(FLStock@name, desc, "-",stock.slot,
          "(units :",units(slot(FLStock,stock.slot)),")\n"), file=temp)
        cat(1, idx,"\n", file=temp, sep="\t")
        cat(FLStock@range["minyear"], FLStock@range["maxyear"], "\n",
          file=temp, sep="\t")
        cat(FLStock@range["min"], FLStock@range["max"], "\n", file=temp,
          sep="\t")
        cat(1,"\n",  file=temp)
        # append the data to the file
 			  write(matrix(slot(FLStock,stock.slot), nrow=nage, ncol=nyear),
          ncolumns=nage, file=temp)
        close(temp)
    }

    #Now write the data out
    if(missing(slots))
    {
      #Write all the data out, and an index file
      #Write the slots sequentially
      do.call(mapply,list(config.df$file.ext, config.df$stock.slot,
        config.df$idx, config.df$desc, FUN=writeVPAFile))
      # produces the index file
      temp <- file(paste(output.file, "-INDEX.txt", sep=""), "w")
      cat(FLStock@name, "\n", file=temp)
      cat(1,paste(output.file,"-",config.df$file.ext,".txt",sep=""),
        file=temp, sep="\n")
      close(temp)
    } else
    {
      output.config <-  config.df[config.df$stock.slot %in% slots,]
      do.call(mapply,c(as.list(output.config),FUN=writeVPAFile))
    }
    return(invisible(NULL))
}	# }}}

writeYPR <- function(stk,fname) {
    hdr <- c("Pseudo MFYPR output, generated from FLR",
             "Run: -----",
             stk@name,
             paste("Time and date:",date()),
             paste("Fbar age range:",paste(range(stk)[c("minfbar","maxfbar")],sep="",collapse="-")),
             "","")
    #The data table takes the information from the last year of the stock object
    TY <- as.character(dims(stk)$maxyear)
    tbl <- cbind(Age=dimnames(stk@stock.n)$age,
                 M=stk@m[,TY],
                 Mat=stk@mat[,TY],
                 PF=stk@harvest.spwn[,TY],
                 PM=stk@m.spwn[,TY],
                 SWt=stk@stock.wt[,TY],
                 Sel=stk@harvest[,TY],
                 CWt=stk@catch.wt[,TY])
    tbl.hdr <- colnames(tbl)
    ftr <- c("","Weights in kilograms")

    #Write table
    write.table(hdr,fname,row.names=FALSE,col.names=FALSE,quote=TRUE)
    write.table(t(tbl.hdr),fname,row.names=FALSE,col.names=FALSE,quote=TRUE,append=TRUE,sep=",")
    write.table(tbl,fname,row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE,sep=",")
    write.table(ftr,fname,row.names=FALSE,col.names=FALSE,quote=TRUE,append=TRUE)

    #Finished
    return(invisible(NULL))

}

