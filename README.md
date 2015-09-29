R から Azure 情報を収集する
 




theURL <- "https://azure.microsoft.com/en-us/documentation/articles/virtual-machines-size-specs/"

targetXPath <- "//table"

doc <- getURL(theURL)
doc <- htmlParse(doc)

body <- xpathSApply(doc, targetXPath, xmlValue)


---
require(XML)
require(rvest)
require(stringr)

textdata <- html("https://azure.microsoft.com/en-us/documentation/articles/virtual-machines-size-specs/")
nodes <- html_nodes(textdata, xpath = "//tbody")


basic <- readHTMLTable(nodes[[3]], whichi = 1, header = FALSE)

basic_size <- data.frame(Reduce(rbind, str_split(string = basic[,1], pattern = "\\\\")))
names(basic_size) <- c("SizePortal","SizeAPI")

basic_disk <- data.frame(Reduce(rbind, str_split(string = basic[,4], pattern = "=")))
basic_disk[,2] <- str_replace(string = basic_disk[,2] , pattern = "GBTemporary", replacement = "")
basic_disk[,3] <- str_replace(string = basic_disk[,3], pattern = " GB", replacement = "")
basic_disk <- basic_disk[,2:3]
names(basic_disk) <- c("MaxOSDiskSize", "MaxTempDiskSize")

basic[,3] <- as.character(basic[,3])
basic[,3] <- str_replace(string = basic[,3], pattern = " MB", replacement = "")
basic[,3] <- str_replace(string = basic[,3], pattern = " GB", replacement = "")
basic[,3][[1]] <- as.numeric(basic[,3][[1]])/1000

basic <- cbind(basic_size, basic)
basic <- cbind(basic, basic_disk)
basic <- basic[,c(1,2,4,5,7,9,10,8)]
names(basic)[[3]] <- "CPUCore"
names(basic)[[4]] <- "Memory"
names(basic)[[5]] <- "MaxDataDiskSize"
names(basic)[[8]] <- "MaxIOPS"



standard <- readHTMLTable(nodes[[4]], whichi = 1, header = FALSE)
standard_size <- data.frame(Reduce(rbind, str_split(string = standard[,1], pattern = "\\\\")))
names(standard_size) <- c("SizePortal","SizeAPI")
standard_size$SizePortal <- as.character(standard_size$SizePortal)
standard_size$SizeAPI <- as.character(standard_size$SizeAPI)
standard_size[standard_size$"SizeAPI" == "same","SizeAPI"] <- standard_size[standard_size$"SizeAPI" == "same","SizePortal"]

standard_disk <- data.frame(Reduce(rbind, str_split(string = standard[,4], pattern = "=")))
standard_disk[,2] <- str_replace(string = standard_disk[,2] , pattern = "GBTemporary", replacement = "")

standard_disk[,3] <- str_replace(string = standard_disk[,3], pattern = "GB", replacement = "")
standard_disk[,3] <- str_replace(string = standard_disk[,3] , pattern = "Note.*", replacement = "")
standard_disk[,3] <- str_trim(standard_disk[,3])

standard_disk$LocalDisk <- "HDD"
standard_disk$LocalDisk[str_detect(string = standard_disk[,2], pattern = "SSD")] <- "SSD"
standard_disk[,2] <- str_replace(string =standard_disk[,2], pattern = " \\(SSD\\)", replacement = "")
standard_disk[,2] <- str_trim(standard_disk[,2])
standard_disk <- standard_disk[,c(2,3,4)]
names(standard_disk)[1:2] <- c("MaxOSDiskSize", "MaxTempDiskSize")

standard[,3] <- as.character(standard[,3])
standard[,3] <- str_replace(string = standard[,3], pattern = " MB", replacement = "")
standard[,3] <- str_replace(string = standard[,3], pattern = " GB", replacement = "")
standard[,3][[1]] <- as.numeric(standard[,3][[1]])/1000

standard <- cbind(standard_size, standard)
standard <- cbind(standard,standard_disk)

standard <- standard[,c(1,2,4,5,7,9,10,11,8)]
names(standard)[[3]] <- "CPUCore"
names(standard)[[4]] <- "Memory"
names(standard)[[5]] <- "MaxDataDiskSize"
names(standard)[[9]] <- "MaxIOPS"


standard_ds <- readHTMLTable(nodes[[5]], whichi = 1, header = FALSE)
standard_ds_size <-data.frame(Reduce(rbind, str_split(string = standard_ds[,1], pattern = "\\\\")))
names(standard_ds_size) <- c("SizePortal","SizeAPI")
standard_ds_size$SizePortal <- as.character(standard_ds_size$SizePortal)
standard_ds_size$SizeAPI <- as.character(standard_ds_size$SizeAPI)
standard_ds_size[,"SizeAPI"] <- standard_ds_size[,"SizePortal"]

standard_ds_disk <- data.frame(Reduce(rbind, str_split(string = standard_ds[,4], pattern = "=")))
standard_ds_disk[,2] <- str_replace(string = standard_ds_disk[,2] , pattern = " GBLocal SSD disk", replacement = "")
standard_ds_disk[,2] <- str_trim(standard_ds_disk[,2])

standard_ds_disk[,3] <- str_replace(string = standard_ds_disk[,3], pattern = "GB", replacement = "")
standard_ds_disk[,3] <- str_trim(standard_ds_disk[,3])

standard_ds_disk$LocalDisk <- "SSD"
standard_ds_disk <- standard_ds_disk[,c(2,3,4)]
names(standard_ds_disk)[1:2] <- c("MaxOSDiskSize", "MaxTempDiskSize")

standard_ds[,3] <- as.numeric(as.character(standard_ds[,3]))
standard_ds[,5] <- as.numeric(as.character(standard_ds[,5]))
standard_ds[,6] <- as.numeric(as.character(standard_ds[,6]))

standard_ds$MaxIOPS <- str_extract(string = standard_ds[,7], pattern = "\\d{1,2},\\d{3}")
standard_ds$MaxIOPS <- str_replace(string = standard_ds$MaxIOPS, pattern = ",", replace = "")
standard_ds$MaxIOPS <- as.numeric(standard_ds$MaxIOPS)

standard_ds$bandwidth <- str_replace(string = standard_ds[,7], pattern = "\\d{1,2},\\d{3}", replace = "")
standard_ds$bandwidth <- str_replace(string = standard_ds$bandwidth, pattern = " MB per second", replacement = "")
standard_ds$bandwidth <- as.numeric(standard_ds$bandwidth)

standard_ds <- cbind(standard_ds_size, standard_ds)
standard_ds <- cbind(standard_ds,standard_ds_disk)

standard_ds <- standard_ds[,c(1,2,4,5,7,8,12,13,14,10,11)]
names(standard_ds)[[3]] <- "CPUCore"
names(standard_ds)[[4]] <- "Memory"
names(standard_ds)[[5]] <- "MaxDataDiskSize"
names(standard_ds)[[6]] <- "CacheSize"


> readHTMLTable(nodes[[3]], whichi = 1, header = FALSE)
            V1 V2      V3                             V4 V5     V6
1 A0\\Basic_A0  1  768 MB  OS = 1023 GBTemporary = 20 GB  1  1x300
2 A1\\Basic_A1  1 1.75 GB  OS = 1023 GBTemporary = 40 GB  2  2x300
3 A2\\Basic_A2  2  3.5 GB  OS = 1023 GBTemporary = 60 GB  4  4x300
4 A3\\Basic_A3  4    7 GB OS = 1023 GBTemporary = 120 GB  8  8x300
5 A4\\Basic_A4  8   14 GB OS = 1023 GBTemporary = 240 GB 16 16x300

参照 URL
====

*[【R】初心者向けRでWebスクレイピングする方法](http://uytaz.com/2015/02/25/post-905/)