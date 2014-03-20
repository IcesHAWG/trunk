stf.out   <-  function(FLStock,Recruitment,format="TABLE %i.") {
  #Check  validity of objects
  if(!validObject(FLStock) | !is.FLStock(FLStock)) {stop("\"FLStock\" argument is not a valid FLStock object")}
  
  #Initialise key values
  counter <- 1 
  opt     <- NULL
  
  #First go through the input data
  output.structure <- rbind(c("catch.wt","WEIGHTS AT AGE IN THE CATCH"),
                        c("stock.wt","WEIGHTS AT AGE IN THE STOCK"),
                        c("stock.n","STOCK IN NUMBER"),
                        c("harvest","FISHING MORTALITY AT AGE IN THE STOCK"),
                        c("m","NATURAL MORTALITY"),
                        c("mat","PROPORTION MATURE"),
                        c("harvest.spwn","FRACTION OF HARVEST BEFORE SPAWNING"),
                        c("m.spwn","FRACTION OF NATURAL MORTALITY BEFORE SPAWNING"))
  for(i in 1:nrow(output.structure)) {
    #First the title line, then the data
    opt <- c(opt, paste(sprintf(format,counter),output.structure[i,2]),"")
    opt <- c(opt, format.quant(slot(FLStock,output.structure[i,1])),"","")
    counter <- counter +1
  }  

  opt <- c(opt,"")
  
  #add the recruitment values
  output.structure <- rbind(c(paste("Recruitment in ",range(FLStock)["maxyear"]-1,sep="")),
                            c(paste("Recruitment in ",range(FLStock)["maxyear"],sep="")))
  for(i in 1:nrow(output.structure)){
    opt <- c(opt, paste(sprintf(format,counter),output.structure[i,1]),"")
    opt <- c(opt, round(c(RECS[[i+1]])),"","")
    counter <- counter +1
  }
  
  
  #FLR and R Package Information
  descrips <- list(packageDescription("FLSAM"),packageDescription("FLAssess"),packageDescription("FLCore"))
  descrips <- lapply(descrips, function(b) {
    rbind(paste("Package  :",b$Package),
          paste("Version  :",b$Version),
          paste("Packaged :",b$Packaged),
          paste("Built    :",b$Built),
          "")
  })
  descrip.str <- do.call(rbind,c(R.Version()$version.string,"",descrips))
  opt <- c(opt, paste(sprintf(format,counter),"FLR, R SOFTWARE VERSIONS"),"",
            capture.output(write.table(descrip.str,row.names=FALSE,quote=FALSE,col.names=FALSE)),"")
  counter <- counter + 1

  
  #And finish  
  return(opt)

}

ica.out   <-  function(...) {
  FLICA.out(...)
}
format.quant <- function(x) {
  #Drop extraneous dimensions
  mat.obj <-  drop(x@.Data)
  #Check that we haven't "overdropped", and are left with at least quant vs year
  if(is.null(dim(mat.obj))) {
    mat.obj <-  array(as.vector(mat.obj),dim=dim(x@.Data)[1:2],dimnames=dimnames(x@.Data)[1:2])
  }

  #Write output
  opt <-  capture.output({
    cat(paste("Units  : ",units(x),"\n"))
    print.default(mat.obj,quote=FALSE,right=TRUE)
    })  #End  capture output

  return(opt)

}   #End function, end setMethod