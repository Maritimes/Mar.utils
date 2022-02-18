get_data_tables(schema = "MARFISSCI", data.dir = "c:/DFO-MPO/wrangledData/", usepkg = 'roracle',
                                 fn.oracle.username = marfis.username, fn.oracle.password = marfis.password, fn.oracle.dsn = oracle.dsn,
                                 tables = c("SUM_DOC_DEFN_COLS", "COLUMN_DEFNS"),
                                 quietly = FALSE, fuzzyMatch=FALSE)
SUM_DOC_DEFN_COLS <- unique(SUM_DOC_DEFN_COLS[,c("SUM_DOC_DEFN_COL_ID", "COLUMN_DEFN_ID")])
COLUMN_DEFNS <- unique(COLUMN_DEFNS[,c("COLUMN_DEFN_ID","DESC_ENG")])
COL_LOOKUP <- unique(merge(SUM_DOC_DEFN_COLS, COLUMN_DEFNS, all=T))
COL_LOOKUP$DESC_ENG <- gsub("#", "Num", COL_LOOKUP$DESC_ENG)
COL_LOOKUP$DESC_ENG <- gsub("\\.", "", COL_LOOKUP$DESC_ENG)
COL_LOOKUP$DESC_ENG <- gsub(":", "", COL_LOOKUP$DESC_ENG)
COL_LOOKUP$DESC_ENG <- gsub("\\\n", " ", COL_LOOKUP$DESC_ENG)
COL_LOOKUP$DESC_ENG <- gsub("-", " ", COL_LOOKUP$DESC_ENG)
COL_LOOKUP$DESC_ENG <- gsub("\\(|\\)", " ", COL_LOOKUP$DESC_ENG)
COL_LOOKUP$DESC_ENG <- trimws(gsub("\\s+", " ", COL_LOOKUP$DESC_ENG))
COL_LOOKUP$DESC_ENG <- gsub(" ", "_", COL_LOOKUP$DESC_ENG)
COL_LOOKUP$DESC_ENG <- toupper(COL_LOOKUP$DESC_ENG)
DETS_COL_LOOKUP <- COL_LOOKUP
setwd("C:/git/Maritimes/Mar.utils/inst")
usethis::use_data(DETS_COL_LOOKUP, DETS_COL_LOOKUP, overwrite = T)
