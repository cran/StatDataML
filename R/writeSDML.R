writeSDML <- function(x, file="", textdata=NULL,
                      dtd=NULL, sep=" \\n",
                      na.string="NA", null.string="NULL",
                      title=deparse(substitute(x)), source="R",
                      version=" ", date=NULL, comment=" ",
                      class="")
{
    if(is.null(date)) { date <- date() }
    if(is.null(dtd)){
        dtd <- system.file("dtd/StatDataML.dtd", pkg="StatDataML")[1]
    }
    cat("\<\?xml version=\"1.0\"\?>\n", file=file, sep="")
    cat("\<!DOCTYPE StatDataML SYSTEM \"", dtd,
        "\">\n", file=file, append=T, sep="")
    cat("\<StatDataML>\n", file=file, append=T, sep="")
    writeDescriptionSDML(title=title, source=source,
                         version=version, date=date,
                         comment=comment,file=file,
                         class=class)
    writeDatasetSDML(x, file=file, textdata=textdata, sep=sep,
                     na.string=na.string,
                     null.string=null.string)	
    cat("\</StatDataML>\n", file=file, append=T, sep="")
}


