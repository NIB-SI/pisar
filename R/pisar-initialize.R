## ----echo=FALSE-----------------------------------------------------
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################




## ----Package description1-------------------------------------------
#' pisar: pISA-tree support functions
#'
#' The package provides several functions for support
#' and use of pISA-tree.
#'
#' @section Usage:
#' pISA-tree is a standardized directory tree
#' for storing project information under ISA paradigm.
#' This set of functions
#' enables use of metadata for reproducible documents. Export of investigations to ISA-tab format is also provided.
#'
#' @docType package
#' @name pisar
NULL


## -------------------------------------------------------------------
#' @importFrom utils choose.files read.table write.table
NULL
#> NULL


## ----fileName-------------------------------------------------------
#' Extract file name
#'
#' Extract file name from a file path.
#'
#' @param x Complete file path or  file name.
#' @param ... Any other arguments.
#' @return File name (character string).
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @seealso \code{\link{fileType}}
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fileName(".\\validation.Rnw")
#' fileName("./bla/validation.Rnw")
#' fileName("./validation.")
#' fileName("./validation")
fileName <- function(x,...){
gsub("\\..*$","",basename(x))
gsub("(.*)\\.(.*)","\\1",basename(x))
}


## ----fileType-------------------------------------------------------
#' Extract file type
#'
#' Extract file type from a file path.
#'
#' @param x Complete file path or  file name.
#' @param ... Any other arguments.
#' @return File type (character string).
#' @export
#' @note Parameter ... is ignored at this time.
#' @keywords file
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fileName(".\\validation.Rnw")
#' fileName("./bla/validation.Rnw")
#' fileName("./validation.")
#' fileName("./validation")
fileType <- function(x,...){
type <- gsub("(.*)\\.(.*)","\\2",basename(x))
if(type==x)  type  <- ""
return(type)
}
fileType("bla")
fileType("./bla.Rnw")


## ----fsummary-------------------------------------------------------
#' As factor summary of a data frame
#'
#' @param x Data frame.
#' @param ... Any other arguments.
#' @return Summary object.
#' @export
#' @note Argument ... not used
#' @keywords summary
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' fsummary(data.frame(x=rnorm(20),txt=sample(letters,20,rep=TRUE)))
fsummary <- function(x,...){
    dims <- dimnames(x)
    x <- apply(x,2,factor)
    dimnames(x) <- dims
    summary(x)
}
fsummary(data.frame(x=rnorm(20),txt=sample(letters,20,rep=TRUE)))


## ----getRoot--------------------------------------------------------
#' Get root directory for pISA layer
#'
#' @param x Character characteristic for pISA layer (one of "p", "I", "S", or "A").
#' @param path Path within the pISA-tree.
#' @param ... Any other arguments.
#' @return Relative path to the layer directory (from working directory).
#' @export
#' @note The path should be compliant with the pISA-tree structure.
#'     Path defaults to the working directory, which is
#'     usually in or below an assay. Argument ... is not used.
#' @keywords pISA
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' getRoot("p", path="d:/_p_prj/_I_inv/_S_st/_A_asy/other/doc")
#' getRoot("A", path="d:/_p_prj/_I_inv/_S_st/_A_asy")
getRoot <- function(x="p",path=getwd(),...){
dirs <- strsplit(path,"/")[[1]]
nl <- length(dirs)-which(regexpr(paste0("_",x,"_"),dirs)>0)
if(length(nl)<=0) stop(paste("Path is not within a pISA-tree:\n"
        , path))
if( nl > 0 ) rpath <- paste(rep("..",nl),collapse="/") else rpath="."
rpath
}




## ----readMeta-------------------------------------------------------
#' Read metadata file from the given directory
#'
#' @param x File path to the pISA layer.
#' @param ... Any other arguments.
#' @return Data frame with Key/Value pairs with class 'pISAmeta'.
#' @export
#' @note Metadata table gets the class 'Dlist' to inherit a convenient print.
#' @keywords pISA
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' .aroot <- getRoot("A")
#' .ameta <- readMeta(.aroot)
#' .ameta
#' setwd(oldwd)
#' }
readMeta <- function(x=".",  ...){
d <- dir(x)
d
lfn <- d[regexpr("^_[pisa].*_metadata.txt", d, ignore.case = TRUE)>0]
if(length(lfn)==0) warning("No metadata file found")
if(length(lfn)>1) warning("More than one metadata file found:", d)
if(length(lfn)==1){
  p <- read.table(file.path(x, lfn)
  ,sep="\t", stringsAsFactors=FALSE, col.names=c("Key","Value"), header=FALSE, comment.char="#", fill=TRUE,blank.lines.skip=TRUE)
class(p)<- c("pISAmeta", "Dlist", class(p))
} else {p = ""}
return(p)
}
##
#.pISAloc <- system.file("extdata","_p_Demo",package="pisar")
#readMeta(.pISAloc)


## ----print.pISAmeta-------------------------------------------------
#' Print metadata object as Dlist
#'
#' @param x Metadata object, data.frame with two columns.
#' @param width Estimated text width.
#' @param ... Any other arguments.
#' @export
#' @note Metadata table is printed in convenient Dlist form.
#' @keywords package
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' .pISAloc <- system.file("extdata","_p_Demo",package="pisar")
#' readMeta(.pISAloc)
#' }
print.pISAmeta <- function(x, width = max(nchar(x[,1]))*3.5,  ...){
    #if(inherits(x,"pISAmeta")
    #print(width)
    #print.Dlist(x, width=width, ...)
    width <- max(nchar(x[,1]))+1
    key <- c(paste("",names(x)[1])
           , paste0(rep("-",nchar(names(x)[1])),collapse="")
           , x[,1])
    key <- substr(paste(key,paste(rep(" ",15),collapse=" ")),1,width)
    val <- c(paste("",names(x)[2])
           , paste0(rep("-",nchar(names(x)[2])),collapse="")
           , x[,2])
    keyval <- paste0(key,val,"\n")
    cat(keyval)
    }



## ----getMeta--------------------------------------------------------
#' Get metadata value
#'
#' @param x Two column character data frame with Key / Value pairs.
#' @param key String, key name. Trailing colon can be omitted.
#' @param nl Logical, expand backslash character for new lines.
#' @return Key value (character string or number).
#' @export
#' @note Argument key is matched exactly to the key names.
#' @keywords pisa
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' .iroot <- getRoot("I")
#' .imeta <- readMeta(.iroot)
#' getMeta(.imeta, "Description")
#' # list
#' listmeta <- list(Title = "My title"
#'    , Description = "A longer description")
#' getMeta( listmeta, "Title")
#' getMeta( listmeta, "Description")
#' setwd(oldwd)
#' }
#' @rdname getMeta
#' @export getMeta
getMeta <- function(x,key,nl=TRUE) {
if (is.data.frame(x)) {
key <- paste0(gsub(":","",key),":")
ret <- unclass(x[match(key, x[,1]), 2])
if(is.character(ret)&&nl) ret <- sub("\\\\n","\n",ret)
return(ret)
}
if (is.list(x)) {
   nm <- paste0(gsub(":","",names(x)),":")
   xd <- data.frame(Key=nm,Value=unlist(x), stringsAsFactors=FALSE)
   getMeta(xd, key, nl)
   }
}


## ----pasteMeta------------------------------------------------------
#' Paste metadata values
#'
#' Paste metadata values into a string.
#' @param x Two column character data frame with Key / Value pairs or
#'    a list of Vales named by Keys.
#' @param kvsep Key/Value separator (default is a tab character).
#' @param nlsep line separator (default is new line).
#' @return Character string with concatenated key values.
#' @export
#' @keywords pisa
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' .iroot <- getRoot("I")
#' .imeta <- readMeta(.iroot)
#' x <- pasteMeta(.imeta)
#' x
#' cat(x)
#' # list
#' listmeta <- list(Title = "My title"
#'    , Description = "A longer description")
#' x <- pasteMeta( listmeta )
#' x
#' cat(x)
#' setwd(oldwd)
#' }
pasteMeta <- function (x, kvsep="\t", nlsep="\n") {
   if(!is.data.frame(x)&is.list(x)) x <- data.frame(Key=names(x),Value=as.vector(unlist(x)))
   paste0(paste(apply(x, 1,
            function(x) paste(x, collapse=kvsep) )
            , collapse=nlsep)
            ,nlsep)
}








## ----getLayer-------------------------------------------------------
#' Get pISA layer name
#'
#' @param x Layer character (one of "p", "I", "S", or "A").
#' @param path Directory path, defaults to working directory.
#' @return Character string with layer name.
#' @export
#' @keywords pisa
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' .pname <- getLayer("p")
#' .pname
#' getLayer("I")
#' getLayer("S")
#' getLayer("A")
#' if(interactive()) setwd(oldwd)
#' getwd()
#' }
getLayer <- function(x, path=getwd()){
  loc <- strsplit(path,"/")[[1]]
  lyr <- paste0("_",x,"_")
  lname <- loc[regexpr(lyr,loc)==1]
  if(length(lname)==0) {
    lname <- ""
    warning("No layer '", x, "' in path")
    }
  return(lname)
  }


## -------------------------------------------------------------------
#' Create output directory
#'
#' Create output directory, name it with appended arguments.
#'
#' @param out.dir Character string, base output directory.
#' @param args  Character vector, arguments used for sub-analysis.
#'     or from a batch call
#' @param which Numeric vector, which arguments to use.
#' @return Directory name.
#' @note Directory is created
#' @export
#' @keywords pisa
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' out.path()
#' out.path(args="")
#' dir.exists("../out/xx-ena-dva")
#' out.path(args=c("xx.txt","ena","dva"))
#' dir.exists("../out/xx-ena-dva")
#' unlink("../out/xx-ena-dva",recursive=TRUE)
#' dir.exists("../out/xx-ena-dva")
out.path <- function(out.dir="../out", args="", which=1:length(args)) {
  args[1] <- sub("\\..*","",basename(args[1]))
  out <- if(!all(c(args[which])=="")) file.path(out.dir,paste(c(args[which]),collapse="-")) else out.dir
  dir.create(out,showWarnings = FALSE)
  out
}


## ----pisa-----------------------------------------------------------
#' Extract pISA-tree details
#'
#' Extract pISA-tree details: name, root and metadata
#'     for all layers above the current directory (below or in the Assay).
#'
#' @param path Path to part of pISA-tree, defaults to working directory.
#' @param addArgs Character vector, additional arguments.
#' @param global If \code{TRUE} (default) auxiliary objects will be
#'     created in the global environment (see note).
#' @return A list with layer information components, possibly changing
#' objects in the global environment (Se Note).
#' @note If argument global is \code{TRUE} (default), auxiliary objects with
#'     pISA related information will be created in the global environment.
#'     Sometimes it is more convenient to use such object instead
#'     of the elements of the (invisibly) returned pisa list.
#'     The object are hidden (names start with dot); use
#'     \code{ls(pattern="^\\.",all.names=TRUE)}
#'     to get a full list of hiddent objects. The created objects are
#' \describe{
#'     \item{.[pisa]name}{layer name}
#'     \item{.[pisa]root}{layer path (relative to the working directory)}
#'     \item{.[pisa]meta}{data frame with corresponding layer metadata}
#'     \item{.oroot}{output directory path}
#'     \item{.inroot}{input (data) directory path}
#'     \item{.reproot}{report directory path}
#'     \item{.reproot}{report directory path}
#'     \item{.outputFile}{output file path and name}
#'     \item{.args}{a vector of additional arguments (possibly from a batch call)}
#'     \item{.pfn}{phenodata file path and name (relative to Investigation)}
#'     \item{.ffn}{featuredata fiel path and name}
#'     \item{.outfn}{output file basename (no type)}
#'     \item{.rnwfn}{knitr source file name (*.Rnw or *.Rmd)}
#' }
#' @export
#' @keywords pisa
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' astring <- "_p_Demo/_I_Test/_S_Show/_A_Work-R/other"
#' oldwd <- setwd(system.file("extdata",astring,package="pisar"))
#' oldwd
#' pisa <- pisa(global=FALSE)
#' str(pisa)
#' names(pisa)
#' dir(pisa$p$root)
#' # Set pisa options
#' options(pisa=pisa(global=FALSE))
#' # Access details with $
#' options()$pisa$p$root
#' setwd(oldwd)
#' }
pisa <- function(path=getwd(), addArgs=NULL, global=TRUE){
# options list
if(global) pos = .GlobalEnv else pos = -1
#
assign(".proot", getRoot("p", path), pos=pos)
assign(".iroot", getRoot("I", path), pos=pos)
assign(".sroot", getRoot("S", path), pos=pos)
assign(".aroot", getRoot("A", path), pos=pos)
#
assign(".pmeta", readMeta(.proot), pos=pos)
assign(".imeta", readMeta(.iroot), pos=pos)
assign(".smeta", readMeta(.sroot), pos=pos)
assign(".ameta", readMeta(.aroot), pos=pos)
#
assign(".pname", getLayer("p", path), pos=pos)
assign(".iname", getLayer("I", path), pos=pos)
assign(".sname", getLayer("S", path), pos=pos)
assign(".aname", getLayer("A", path), pos=pos)
#
assign(".pfn", getMeta(.imeta,"Phenodata:"), pos=pos)
assign(".ffn", getMeta(.ameta,"Featuredata:"), pos=pos)
#
# Output directory - arguments are used to prepare the name
assign(".inroot", out.path(file.path(.aroot,"input")), pos=pos)
assign(".reproot", out.path(file.path(.aroot,"reports")), pos=pos)
# Create output directory and aoutput file name
args <- commandArgs(trailingOnly = TRUE)
if(length(args)==0) {
    # interactive
    args <- c("Interactive.Rnw",addArgs)
    }
if(any(args=="knit")) args <- c(args[1],addArgs) # call from WinEdt
if(length(args)<2) {
    # interactive or run.bat without arguments
    args <- c(args,addArgs)
    }
##
assign(".outfn", fileName(args[1]), pos=pos)
assign(".rnwfn", args[1], pos=pos)
if(length(args)>1) {
    # NON NULL arguments
    (outputFile <- paste0(.outfn,"_", paste(args[-1],sep="_",collapse="-"),".pdf"))} else {
    (outputFile <- paste0(.outfn,".pdf"))
    }
    outputFile <- file.path(.reproot,outputFile)
    # clean file name, no special characters
    assign("outputFile",gsub("[(),]","",outputFile), pos=pos)
    dump("outputFile","_outputFile.R")
#
.oroot <- out.path(file.path(.aroot,"output"),args=args)
assign(".oroot", gsub("[:(),]","",.oroot), pos=pos)
#
pisa <- list()
pisa$p$name <- .pname
pisa$I$name <- .iname
pisa$S$name <- .sname
pisa$A$name <- .aname
pisa$p$root <- .proot
pisa$I$root <- .iroot
pisa$S$root <- .sroot
pisa$A$root <- .aroot
pisa$p$meta <- .pmeta
pisa$I$meta <- .imeta
pisa$S$meta <- .smeta
pisa$A$meta <- .ameta
pisa$oroot <- .oroot
pisa$inroot <- .inroot
pisa$reproot <- .reproot
pisa$outputFile <- outputFile
pisa$args <- args
pisa$pfn <- .pfn
pisa$ffn <- .ffn
pisa$outfn <- .outfn
pisa$rnwfn <- .rnwfn
#
invisible(pisa)
}


## ----createISAtab---------------------------------------------------
#' Export to ISA-tab
#'
#' Export pISA-tree investigation into ISA-tab files.
#'
#' @param iroot path to investigation. If missing, a interactively navigate
#'        to the desired investigation and select investigation metadata
#'        file.
#' @param files a vector of assay metadata file names
#'        (with study and assay path) to export. If missing,
#'        all assays in selected investigation will be processed.
#' @param path path to the exported files, default is /ISA-tab/ folder in the
#'       investigation path.
#' @param zip controls creation of zip file, created if TRUE.
#' @param isamap path and name of the file with mapping of
#'       pISA-tree and ISA-tab keys.
#'       If missing, the mapping file that comes with
#'       the package pisar is used.
#' @return character vector, names of created files.
#' @export
#' @keywords pISA-tree, ISA-tab
#' @author Andrej Blejec \email{andrej.blejec@nib.si}
#' @examples
#' \dontrun{
#' # interactively select investigation metadata file for export.
#' isafiles <- createISAtab()
#' # export one assay from the demonstrative investigation
#' iroot <- file.path(find.package("pisar"),"extdata/_p_Test-pISA-tab/_I_i")
#' files <- "_S_s/_A_a-GCMS/_ASSAY_METADATA.TXT"
#' path <- "./ISA-tab"
#' createISAtab(iroot, files, path)
#' }
createISAtab <- function(iroot, files,  path, zip=TRUE, isamap){
if(missing(iroot)) {
     Filtr <- t(matrix(c("Investigation metadata file","_INVESTIGATION_METADATA.TXT")))
     ifile <- choose.files(
        caption = "Select investigation metadata file",
        filters = Filtr,
        multi = FALSE)
     iroot <- dirname(ifile)
     }
if(missing(files)) {
     files <- dir(iroot, pattern="^_A.*_METADATA.TXT", recursive=TRUE, ignore.case = TRUE)
     }
if(missing(isamap)) {
    mfn <- "ISA_pISA_mapping.txt"
    mfn <- file.path(find.package("pisar"),"extdata",mfn)
    isamap <- read.table( mfn, sep="\t", header=TRUE)
    isamap$status[is.na(isamap$status)] <- 4
    }
if(missing(path)) path <- file.path(iroot,"ISA-tab")
files <- sort(files)
isa <- NULL
test <- function(...) {cat("---\n",deparse(substitute(...)),"\n",unlist(...),"\n---\n")}
if(!dir.exists(path)) dir.create(path)
###########################
# Investigation lines
layer <- "I"
# imeta <- read.table(file.path(iroot,"_INVESTIGATION_METADATA.TXT"), sep="\t", header=FALSE)
    imeta = readMeta(iroot)
iname <-  basename(iroot)
ind <- which(isamap$layer==layer)
for(i in ind){
  line <- isamap[i,]
  if (line$status==0) isa <- c(isa, line$isa_key)
  if (line$status==1) {
  value <- imeta[ imeta[,1]==paste0(line$pisa_key,":"),2]
  if( length(value)==0) value <- line$default
  isa <- c(isa, paste(line$isa_key, value, sep="\t"))
  }
  if (line$status==4) isa <- c(isa, line$isa_key)
  if (line$status==5) isa <- c(isa, paste(line$isa_key,line$default, sep="\t"))
  }
###########################
# Investigation Contacts lines
layer <- "IC"
ind <- which(isamap$layer==layer)
persons <- isamap[ind,]$isa_key
i <- which( persons=="Investigation Person Address" )
# Get person address
info <- imeta[grep("Person Address",imeta[,1]),2]
persons[i] <- paste( persons[i],
    paste(info,  collapse="\t" ), sep="\t")
i <- which( persons=="Investigation Person Last Name" )
persons[i] <- paste( persons[i],
    paste(rep("See ORCID",length(info)),  collapse="\t" ), sep="\t")
i <- which( persons=="Investigation Person First Name" )
persons[i] <- paste( persons[i],
    paste(rep("See ORCID", length(info)),  collapse="\t" ), sep="\t")
i <- which( persons=="Investigation Person Mid Initials" )
persons[i] <- paste( persons[i],
    paste(rep(";",length(info)),  collapse="\t" ), sep="\t")
isa <- c(isa, persons)
## STUDIES
# Step through metadata files list
lvls <- t(as.data.frame(sapply(files,strsplit,"/")))
colnames(lvls) <- c("Study","Assay","file")
###########################
# Study lines
isas <- NULL
oldsname <- ""
lin <- 1
snames <- unique(lvls[,"Study"])
for( sname in snames ){
.sroot <- file.path(iroot,sname)
layer <- "S"
# smeta <- read.table(file.path(.sroot,"_STUDY_METADATA.TXT"), sep="\t", header=FALSE)
    smeta = readMeta(.sroot)
#
ind <- which(isamap$layer==layer)
for(i in ind){
  line <- isamap[i,]
  if (line$status==0) isa <- c(isa, line$isa_key )
  if (line$status==1) {
  value <- smeta[ smeta[,1]==paste0(line$pisa_key,":"),2]
  if( length(value)==0) value <- line$default
  isa <- c(isa, paste(line$isa_key, value, sep="\t"))
  }
  if (line$status==2) {
       phname <- dir(iroot,pattern="phenodata")[1]
       sfname <- paste0("s_",sname,"-",phname)
       pdata <- read.table(file.path(iroot,phname),sep="\t",header=TRUE)
       names(pdata)[1] <- "Sample Name"
       pdata$source <- "pISA-tree"
       names(pdata)[ncol(pdata)] <- "Source Name"
       names(pdata)
       write.table(pdata, file=file.path(path, sfname), sep="\t", row.names=FALSE)

       isa <- c(isa, paste(line$isa_key, sfname, sep="\t"))
       }
  if (line$status==3) isa <- c(isa, paste0(line$isa_key,"\t",line$file ))
  if (line$status==4) isa<-c(isa, line$isa_key)
  if (line$status==5) isa<-c(isa, paste(line$isa_key, line$default, sep="\t"))

  }

###########################
# Study Assay lines
layer <- "SA"
ind <- which(isamap$layer==layer)
filter <- which(lvls[,"Study"]==sname)
anames <- unique(lvls[filter,"Assay"])
assays <- isamap[ind,]$isa_key
       amap <- isamap[ind,]
       amap <- amap[amap[,"status"]==1,]
       amap$index <- which(assays %in% amap$isa_key)
       ifn <- which( assays=="Study Assay File Name" )
       imt <- which( assays=="Study Assay Measurement Type" )
       itt <- which( assays=="Study Assay Technology Type" )

for (aname in anames) {
       phname <- dir(iroot,pattern="phenodata")[1]
       afname <- paste0("a_",sname,"-",aname,"-",phname)
       pdata <- read.table(file.path(iroot,phname),sep="\t",header=TRUE)
              # ameta <- read.table(file.path(.sroot,aname,"_ASSAY_METADATA.TXT"), sep="\t", header=FALSE)
    ameta = as.data.frame(readMeta(file.path(.sroot, aname)))
# Type one fields
          for(j in 1:length(amap$isa_key)){
          line <- amap[j,]
          ii <- line$index
          info <- ameta[ ameta[,1]==paste0(line$pisa_key,":"),2]
          if(length(info)==0) info <- line$default
          assays[ii] <- paste( assays[ii], info[1], sep="\t")
           }
# Prepare assay file
              ameta[,1] <- gsub(":","",ameta[,1])
              ameta[,1] <- gsub("\\(.*\\)","",ameta[,1])
              ameta[,1] <- gsub(" *$","",ameta[,1])
       # check Analytes.txt
       if(file.exists(file.path(.sroot,aname,"Analytes.txt"))){
           analytes <- read.table(file.path(.sroot,aname,"Analytes.txt"), sep="\t", header=TRUE)
           colnames(analytes) <-gsub("\\_"," ",colnames(analytes))
           pd <- pdata[match( analytes[,1],pdata[,1]),]
           if(!all(pd[,1]==analytes[,1])) warning("Layer SA: SampleID mismatch in phenodata and analytes")
           ameta <- ameta[!ameta[,1]%in%colnames(analytes),]
           } else {
              analytes <- NULL
              pd <- pdata
              }

       # add metadata rows to pdata


       if(!is.null(analytes)) pd <- cbind(pd,t(ameta[,2]),analytes) else
           pd <- cbind(pd,t(ameta[,2]))
       colnames(pd)[ncol(pdata)+(1:nrow(ameta))] <- ameta[,1]
       pdata <- pd
       colnames(pdata) <- gsub("  *"," ",colnames(pdata))
       colnames(pdata) <- gsub(" $","",colnames(pdata))
       colnames(pdata) <- gsub("\\[","(",colnames(pdata))
       colnames(pdata) <- gsub("\\]",")",colnames(pdata))
       colnames(pdata) <- gsub(" Name","Name",colnames(pdata))
       names(pdata)[1] <- "Sample Name"
       pdata$source <- "pISA-tree"
       names(pdata)[ncol(pdata)] <- "Source Name"
       # odstrani dvojne presledke

       write.table(pdata, file=file.path(path, afname), sep="\t", row.names=FALSE)

       assays[ifn] <- paste( assays[ifn], afname, sep="\t")
       #

       }
isa <- c(isa, assays)
###########################
# Study Protocol lines
layer <- "SP"
ind <- which(isamap$layer==layer)
for(i in ind){
  line <- isamap[i,]
  if (line$status==0) isa <- c(isa, line$isa_key )
  if (line$status==1) {
  isa <- c(isa, paste(line$isa_key,smeta[ smeta[,1]==paste0(line$pisa_key,":"),2], sep="\t"))
  }
  if (line$status==2) {
       phname <- dir(iroot,pattern="phenodata")[1]
       sfname <- paste0("s_",sname,"_",phname)
       pdata <- read.table(file.path(iroot,phname),sep="\t",header=TRUE)
       names(pdata)[1] <- "Sample Name"
       pdata$source <- "pISA-tree"
       names(pdata)[ncol(pdata)] <- "Source Name"
       names(pdata)
       write.table(pdata, file=sfname, sep="\t", row.names=FALSE)
              write.table(pdata, file="", sep="\t", row.names=FALSE)
       isa <- c(isa, paste(line$isa_key, sfname, sep="\t"))
       }
  if (line$status==3) isa <- c(isa, paste0(line$isa_key,"\t",line$file ))
  if (line$status==4) isa<-c(isa, line$isa_key)
  if (line$status==5) isa<-c(isa, paste(line$isa_key, line$default, sep="\t"))
  }
###########################
# Study Contacts lines
layer <- "SC"
ind <- which(isamap$layer==layer)
persons <- isamap[ind,]$isa_key
i <- which( persons=="Study Person Address" )
# Get person address
#info <- smeta[ unlist(sapply(c("creator", "investigator", "[pP]erson"), grep, x=smeta[,1])), 2 ]
#persons[i] <- paste( persons[i], paste(info, collapse="\t"), sep="\t" )
#
# Get person address
# smeta <- read.table(file.path(.sroot,"_STUDY_METADATA.TXT"), sep="\t", header=FALSE)
    smeta = readMeta(.sroot)
info <- smeta[grep("Person Address",smeta[,1]),2]
persons[i] <- paste( persons[i],
    paste(info,  collapse="\t" ), sep="\t")
i <- which( persons=="Study Person Last Name" )
persons[i] <- paste( persons[i],
    paste(rep("Use ORCID",length(info)),  collapse="\t" ), sep="\t")
i <- which( persons=="Study Person First Name" )
persons[i] <- paste( persons[i],
    paste(rep("Use ORCID", length(info)),  collapse="\t" ), sep="\t")
i <- which( persons=="Study Person Mid Initials" )
persons[i] <- paste( persons[i],
    paste(rep(";",length(info)),  collapse="\t" ), sep="\t")
#

isa <- c(isa, persons)
#isa <- c(isa, persons)
#if(sname != oldsname ) {
#  if( sname != "") {
#        apply(isas,1,paste, collapse="\t")
#        isa <- c(isa,isas)
#        isas <- ""
#        oldsname <- sname
#        }
#  }
  }  # Morda mora biti tu? end of study processing?
  ifile <- paste0("i_",iname,".txt")
  cat(paste(isa, collapse="\n"), file=file.path(path,ifile))

#

  cat("\n-------------------------\n")
  cat("List of created files in:\n")
  cat(normalizePath(path,winslash="/"),"\n")
  cat("-------------------------\n")
  files <- dir(path,pattern="^[isa]_")
  print(files)
  oldwd <- setwd(path)
  on.exit(setwd(oldwd))
  if (zip) {
  zipname <- paste0(iname,"-ISAtab.zip")
  cat("\nCreating zip file:",zipname,"\n")
  #file.remove(file.path(path,zipname))
  zip(file.path(zipname), dir(".",pattern="^[isa]_"))
  }
  # sys::exec_wait("explorer.exe",normalizePath(getwd()))
return(files)
}

