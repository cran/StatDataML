writeDescriptionSDML <- function(title = "RDataset",
                                 source = "R",
                                 version = " ",
                                 date = NULL,
                                 comment = "",
                                 class = "",
                                 file = "")
{
    ## writes the description tag to file
    
    catSDML("<description>\n", file = file)
    catSDML("<title>", title, "</title>\n", file = file)
    catSDML("<source>", source, "</source>\n", file = file)
    cat("<date>", date, "</date>\n", file = file, append = TRUE, sep = " ")
    catSDML("<version>", version, "</version>\n", file = file)
    catSDML("<comment>", comment, "</comment>\n", file = file)
    sdmlib <- .path.package("StatDataML")
    sdmlib <- substr(sdmlib, 1, nchar(sdmlib)-10)
    catSDML("<creator>R-", R.version$major, ".",  R.version$minor,
            ":StatDataML_",
            package.description("StatDataML", lib=sdmlib)$Version,
            "</creator>\n", file = file)
    catSDML("<class>", class, "</class>\n", file = file)
    catSDML("</description>\n", file = file)
}
