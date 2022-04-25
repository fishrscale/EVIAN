

positions <- read.csv(params$cpg_positions, row.names = 1, header = TRUE)
colnames(positions) <- c("chr", "start", "strand")
saveRDS(positions, file = "./annot_rds_files/positions-probesEPIC-hg38.rds")



