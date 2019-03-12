#get the data
library(ROracle)
library(R.utils)
library(dplyr)
library(Mar.utils)
#channel = make_oracle_cxn(usepkg = 'roracle')
usepkg = 'roracle'
tblName = "VMSTEMP4"

VMSPopulateTable('roracle', 
                 tblName = tblName, 
                 hrBuffer = 4, 
                 shp=shp, 
                 shp.field='FULL_DESCR')



shp = '/home/mike/sf_Documents/GIS/OCMD/OCMD2018/OCMD2018_MMM.shp'
shp.field="FULL_DESCR"
hrBuffer = 2
dateStart = "2018-03-01"
dateEnd = "2018-03-15"

VMSData = VMSGetRecs(shp=shp, shp.field=shp.field, dateStart = dateStart, dateEnd = dateEnd)
test=makeSegments(VMSData,"SEGMID", points="orphans",filename = "testMar2018", plot =T)