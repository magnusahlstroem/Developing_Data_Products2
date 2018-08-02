library(RISmed)
#res <- EUtilsSummary("", 
#                     type="esearch", 
#                     db="pubmed", 
#                     datetype='pdat', 
#                     mindate="2018/07/01", 
#                     maxdate="2018/07/31", 
#                     retmax=120000)

res <- EUtilsSummary("", 
                     type="esearch", 
                     db="pubmed", 
                     datetype='pdat', 
                     mindate="2018/07/01", 
                     maxdate="2018/07/31", 
                     retmax=1000)

time_1000 <- system.time(
    articles <- EUtilsGet(res)
)

affs <- Affiliation(articles)
PMIDs <- PMID(articles)

Universities <- lapply(affs, function(x) {
    #fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else NA
    x <- x[1]
    index_uni <- regexpr("\\,[[:alpha:][:space:]]*[Uu][Nn][Ii][[:alpha:][:space:]]*\\,", x)
    uni <- sub("^\\,\\ ", "", substr(x, index_uni, index_uni[1] + attributes(index_uni)[[1]]))
    uni_cleansed <- trimws(sub("\\,\\ $", "", sub("^\\,\\ *", "", uni)))
    uni_cleansed #list(University = uni_cleansed, PMID = x$PMID)
})
