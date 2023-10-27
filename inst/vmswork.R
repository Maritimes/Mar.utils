R.utils::sourceDirectory("c:/git/Maritimes/Mar.utils/R/", modifiedOnly=F)
test = VMS_get_recs(dateStart = "2020-01-01", dateEnd = "2020-03-31", 
                    vrnList = c('19832', '101019','100745'), usepkg = "roracle")
testClean = VMS_clean_recs(test)