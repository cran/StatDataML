writeDescriptionSDML <- function(title="RDataset", source="R",
                                 version=" ", date=NULL, comment="",
                                 class="NULL", file="")
{
    ## writes the description tag to file
    
    catSDML("<description>\n", file=file)
    catSDML("<title>", title, "</title>\n", file=file)
    catSDML("<source>", source, "</source>\n", file=file)
    cat("<date>", date, "</date>\n", file=file, append=T, sep=" ")
    catSDML("<version>", version, "</version>\n", file=file)
    catSDML("<comment>", comment, "</comment>\n", file=file)
    catSDML("<creator>R-", R.version$major, ".",  R.version$minor,
            ":StatDataML_0.1-7</creator>\n", file=file)
    catSDML("<class>", class, "</class>\n", file=file)
    catSDML("</description>\n", file=file)
}
