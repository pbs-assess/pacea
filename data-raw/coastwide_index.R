# Obtaining the oceanographic basin indices and compile them into PACea.
# Converted from original code supplied by Chris Rooper.

# Note that these time series of indices are NOT stored across the
#  BC grid. This saves a lot of space. Instead, PACea recognises that a covariate
#  is `coastwide' if the `Poly_ID` value is set to `-1`.

```{r}
#ENSO MEI
nlines<-as.numeric(format(Sys.time(), "%Y"))-1978
download.file("https://psl.noaa.gov/enso/mei/data/meiv2.data", destfile="ENSO_MEI.txt",mode="wb", quiet = FALSE)
ENSO_MEI<-read.table("ENSO_MEI.txt",skip=1,as.is=TRUE, nrows = nlines)
colnames(ENSO_MEI)<-c("YEAR","1","2","3","4","5","6","7","8","9","10","11","12")
ENSO_MEI[ENSO_MEI==-999]<-NA
ENSO_MEI<-reshape::melt(ENSO_MEI,id="YEAR")
colnames(ENSO_MEI)<-c("Year","Month","ENSO_MEI")
ENSO_MEI$Month <- as.numeric(ENSO_MEI$Month)

#ENSO ONI
download.file("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt", destfile="ENSO_ONI.txt",mode="wb", quiet = FALSE)
ENSO_ONI<-read.table("ENSO_ONI.txt",skip=0,as.is=TRUE,header=TRUE)
colnames(ENSO_ONI)<-c("Month","Year","ENSO_ONI_Total","ENSO_ONI_Anom")
ENSO_ONI$Month <- as.numeric(factor(ENSO_ONI$Month, levels=unique(ENSO_ONI$Month), ordered=T))

#PDO
#download.file("https://www.ncdc.noaa.gov/teleconnections/pdo/data.csv", destfile="PDO.csv",mode="wb", quiet = FALSE)
download.file("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat", destfile="PDO.dat",mode="wb", quiet = FALSE)
PDO<-read.table("PDO.dat",skip=1,as.is=TRUE,header=TRUE, sep='', fill=T)
colnames(PDO)<-c("Year","1","2","3","4","5","6","7","8","9","10","11","12")
PDO<-reshape::melt(PDO,id="Year")
colnames(PDO)<-c('Year','Month','PDO_Index')
PDO$Month <- as.numeric(PDO$Month)
PDO$PDO_Index <- as.numeric(PDO$PDO_Index)

#AO
download.file("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table", destfile="AO.dat",mode="wb", quiet = FALSE)
AO<-read.table("AO.dat",skip=1,as.is=TRUE,header=F, fill=T)
colnames(AO)<-c("Year","1","2","3","4","5","6","7","8","9","10","11","12")
AO<-reshape::melt(AO,id="Year")
colnames(AO)<-c('Year','Month','AO_Index')
AO$Month <- as.numeric(AO$Month)

#SOI
download.file("https://www.cpc.ncep.noaa.gov/data/indices/soi", destfile="SOI.dat",mode="wb", quiet = FALSE)
SOI<-read.table("SOI.dat",skip=3,as.is=TRUE,header=TRUE, fill=T)
SOI$YEAR <- as.numeric(SOI$YEAR)
SOI <- SOI[!is.na(SOI$YEAR),]
SOI<-reshape::melt(SOI,id="YEAR")
colnames(SOI)<-c('Year','Month','SOI_Anomaly_Index')
SOI$SOI_Anomaly_Index[grepl(SOI$SOI_Anomaly_Index, pattern='9-999')] <- NA
SOI <- SOI %>%
  group_by(Year, Month) %>%
  mutate(SOI_Standardized_Index = as.numeric(SOI_Anomaly_Index[2]),
         SOI_Anomaly_Index = as.numeric(SOI_Anomaly_Index[1])) %>%
  filter(row_number()==1)
SOI$Month <- as.numeric(SOI$Month)

#NGPO
download.file("http://www.o3d.org/npgo/npgo.php", destfile="NPGO.txt",mode="wb", quiet = FALSE)
NPGO<-read.table("NPGO.txt",skip=30,as.is=TRUE,header=FALSE,fill=TRUE)
colnames(NPGO)<-c("Year","Month","NPGO_index")
NPGO$Year<-as.numeric(NPGO$Year)
NPGO<-subset(NPGO,is.na(NPGO$NPGO_index)==FALSE)

#ALPI - CURRENT DATA NOT AVAILABLE - LAST UPDATE 2015
download.file("http://www.pac.dfo-mpo.gc.ca/od-ds/science/alpi-idda/ALPI_1900_2015_EN.csv", destfile="ALPI.csv",mode="wb", quiet = FALSE)
ALPI<-read.csv("ALPI.csv",skip=0,as.is=TRUE,header=TRUE)
colnames(ALPI)<-c("Year","ALPI")
plot(ALPI)

#NPI
download.file("https://climatedataguide.ucar.edu/sites/default/files/npindex_monthly.txt",destfile="NPI.txt",mode="wb", quiet = FALSE)
NPI<-read.table("NPI.txt",skip=1,as.is=TRUE,header=FALSE,fill=TRUE)
NPI[NPI==(-999)]<-NA
NPI<-data.frame(Year=floor(NPI$V1/100), Month=seq(1,12,1),NPI=NPI$V2)

Coastwide_Index_DF<-merge(merge(merge(merge(merge(merge(ENSO_MEI,ENSO_ONI,by=c("Year",'Month'),all=TRUE),AO,by=c("Year",'Month'),all=TRUE),NPGO,by=c("Year",'Month'),all=TRUE),PDO,by=c("Year",'Month'),all=TRUE),SOI,by=c("Year",'Month'),all=TRUE),NPI,by=c("Year",'Month'),all=TRUE)

# Poly_ID == -1 tells PACea that these values are coastwide
Coastwide_Index_DF$Poly_ID <- -1

Coastwide_Index_DF$Month <- as.numeric(Coastwide_Index_DF$Month)

Coastwide_Index_DF <-
  Coastwide_Index_DF[!is.na(Coastwide_Index_DF$Year) &
                       !is.na(Coastwide_Index_DF$Month),]

#write.csv(basin_indicators,"basin_indicators.csv",row.names=FALSE)
use_data(Coastwide_Index_DF, overwrite = T)
```
