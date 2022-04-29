

cpg_positions_path <- './CpGs_position_v2.csv'
track ucsc names...
options ...
...elt()
library(rtracklayer)


###########################################################################
#Generate RDS positions
###########################################################################
positions <- read.csv(params$cpg_positions, row.names = 1, header = TRUE)
colnames(positions) <- c("chr", "start", "strand")
saveRDS(positions, file = "./ExampleOfReport/test_positions-probesEPIC-hg38.rds")


###########################################################################
#Load UCSC session to retrieve data (requires internet connexion)
###########################################################################
session <- browserSession()
genome(session) <- "hg38"


###########################################################################
#Generate RDS genes
###########################################################################
query <- ucscTableQuery(session, track="NCBI RefSeq",
                        table="refGene")
annot_gene_list <- getTable(query)
saveRDS(annot_gene_list, file = "./ExampleOfReport/test_annotGene_NCBI-RefSeq_refGene.rds")



###########################################################################
#Generate RDS CGI
###########################################################################
query <- ucscTableQuery(session, track="CpG Islands",
                        table="cpgIslandExt")
annot_CGI_list <- getTable(query)
saveRDS(annot_CGI_list, file = "./ExampleOfReport/annotCGI-CpG-Islands-cpgIslandExt.rds")


###########################################################################
#Generate RDS Repeats
###########################################################################
#Too many requests if directly using the following commands:
# query <- ucscTableQuery(session, track="RepeatMasker",
#                         table="rmsk")
# annot_repeats_list <- getTable(query)
# saveRDS(annot_repeats_list, file = "./ExampleOfReport/annotRepeats-RepeatMasker-rmsk.rds")
#so a fractionned loop is necessary:


query <- ucscTableQuery(session, track="RepeatMasker",
                        table="rmsk")
chrSizes <- query@range

#get data chr by chr
annot_repeats_list <- lapply(1:length(chrSizes), function(x){
  print(as.character(chrSizes[x]@seqnames))
  query <- ucscTableQuery(session, track="RepeatMasker",
                        range=chrSizes[x],
                        table="rmsk")
  annot_repeats <- getTable(query)
  return(annot_repeats)
})
annot_repeats_list <- do.call("rbind", annot_repeats_list)

#filter to reduce file size?
annot_repeats_list <- unique(annot_repeats_list)
saveRDS(annot_repeats_list, file = "./ExampleOfReport/annotRepeats-RepeatMasker-rmsk.rds")


# annot_repeats_list <- lapply(1:nrow(chrSizes), function(x){
#   print(x)
#   query <- ucscTableQuery(session, track="RepeatMasker",
#                         range=as(chrSizes, "GRanges")[x],
#                         table="rmsk")
#   annot_repeats <- getTable(query)
#   return(annot_repeats)
# })
# annot_repeats_list <- do.call("rbind", annot_repeats_list)


# annot_repeats_list <- read.table(file = "chrMainOnly_repeats_hg38.tsv", header=TRUE, sep="\t")
# annot_repeats_list <-
# annot_repeats_list[,c("genoName",
#   "genoStart",
#   "genoEnd",
#   "strand",
#   "repClass")]
# saveRDS(annot_repeats_list, file = "annotRepeats.rds")

# annot_repeats_list <- lapply(1:nrow(regions), function(x){
#   query <- ucscTableQuery(session, track="RepeatMasker",
#                         range=as(regions, "GRanges")[x],
#                         table="rmsk")
#   annot_repeats <- getTable(query)
#   return(annot_repeats)
# })
# annot_repeats_list <- do.call("rbind", annot_repeats_list)
# if(nrow(annot_repeats_list) == 0){
#   annot_repeats_list <- data.frame(
#     "bin"=NA, "swScore"=NA,
#     "milliDiv"=NA, "milliDel"=NA, "milliIns"=NA,
#     "genoName"=NA, "genoStart"=NA, "genoEnd"=NA,
#     "genoLeft"=NA, "strand"=NA,
#     "repName"=NA, "repClass"=NA, "repFamily"=NA,
#     "repStart"=NA, "repEnd"=NA, "repLeft"=NA, "id"=NA
#   )
# }
# annot_repeats_list <- unique(annot_repeats_list)
annot_repeats_list <- readRDS("./annotRepeats.rds")


