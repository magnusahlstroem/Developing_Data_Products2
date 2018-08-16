medline = function(file_name){
    lines <- readLines(file_name)
    medline_records <- list()
    key <- 0
    record <- 0
    for(line in lines){
        header <- sub(" {1,20}", "", substring(line, 1, 4))
        value <- sub("^.{6}", "", line)
        if(header == "" & value == ""){
            next
        }
        else if(header == "PMID"){
            record = record + 1
            medline_records[[record]] <- list()
            medline_records[[record]][header] <- value
        }
        else if(header == "" & value != ""){
            medline_records[[record]][key] <- paste(medline_records[[record]][key], value)
        }
        else{
            key <- header
            if(is.null(medline_records[[record]][key][[1]])){
                medline_records[[record]][key] <- value
            }
            else { 
                medline_records[[record]][key] <- paste(medline_records[[record]][key], value, sep=";")
            }
        }
    }
    return(medline_records)
}