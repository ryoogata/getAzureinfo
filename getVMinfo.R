require(XML)
require(rvest)
require(stringr)

# Web 上から情報を取得
textdata <- html("https://azure.microsoft.com/en-us/documentation/articles/virtual-machines-size-specs/")
nodes <- html_nodes(textdata, xpath = "//tbody")

### Basic tier

# list: nodes の 3 番目に Basic tier の情報がある
basic <- readHTMLTable(nodes[[3]], whichi = 1, header = FALSE)

# VM のサイズ記載を Portal 用と API 用で分割する
basic_size <- data.frame(Reduce(rbind, str_split(string = basic[,1], pattern = "\\\\")))
names(basic_size) <- c("SizePortal","SizeAPI")

# VM の Disk Size の関する情報を整形
basic_disk <- data.frame(Reduce(rbind, str_split(string = basic[,4], pattern = "=")))
basic_disk[,2] <- str_replace(string = basic_disk[,2] , pattern = "GBTemporary", replacement = "")
basic_disk[,3] <- str_replace(string = basic_disk[,3], pattern = " GB", replacement = "")
basic_disk <- basic_disk[,2:3]
names(basic_disk) <- c("MaxOSDiskSize", "MaxTempDiskSize")

# VM の Memory に関する情報を整形
basic[,3] <- as.character(basic[,3])
basic[,3] <- str_replace(string = basic[,3], pattern = " MB", replacement = "")
basic[,3] <- str_replace(string = basic[,3], pattern = " GB", replacement = "")
basic[,3][[1]] <- as.numeric(basic[,3][[1]])/1000

# VM のサイズと Disk Size を結合
basic <- cbind(basic_size, basic)
basic <- cbind(basic, basic_disk)
basic <- basic[,c(1,2,4,5,7,9,10,8)]

# 列名を指定
names(basic)[[3]] <- "CPUCore"
names(basic)[[4]] <- "Memory"
names(basic)[[5]] <- "MaxDataDiskSize"
names(basic)[[8]] <- "MaxIOPS"

# ToDo: 各 VM Size 毎の data.frame を結合後に各列の型を検討する
# 各列の型を変更
# basic$SizePortal <- factor(basic$SizePortal, levels = c("A0", "A1", "A2", "A3", "A4"))

# MaxIOPS 列の文字列を整形
basic$MaxIOPS <- str_replace(string = basic$MaxIOPS, pattern = "x", replacement = " x ")

# 不要な中間 Data を削除
rm(basic_disk, basic_size)

# 列情報の追加
basic$tier <- "Basic"
basic$VMCategory <- "A"
basic$VMSize <- basic$SizePortal
basic$LocalDisk <- "HDD"
basic$CacheSize <- "-"
basic$bandwidth <- "-"

### Standard tier A series and D series

# list: nodes の 4 番目に Standard tier A と D の情報がある
standard <- readHTMLTable(nodes[[4]], whichi = 1, header = FALSE)

# VM のサイズ記載を Portal 用と API 用で分割する
standard_size <- data.frame(Reduce(rbind, str_split(string = standard[,1], pattern = "\\\\")))
names(standard_size) <- c("SizePortal","SizeAPI")

# VM のサイズ記載が Portal と API が同一の物は同じ記載になるように調整
standard_size$SizePortal <- as.character(standard_size$SizePortal)
standard_size$SizeAPI <- as.character(standard_size$SizeAPI)
standard_size[standard_size$"SizeAPI" == "same","SizeAPI"] <- standard_size[standard_size$"SizeAPI" == "same","SizePortal"]

# VM の Disk Size の関する情報を整形
standard_disk <- data.frame(Reduce(rbind, str_split(string = standard[,4], pattern = "=")))
standard_disk[,2] <- str_replace(string = standard_disk[,2] , pattern = "GBTemporary", replacement = "")
standard_disk[,3] <- str_replace(string = standard_disk[,3], pattern = "GB", replacement = "")
standard_disk[,3] <- str_replace(string = standard_disk[,3] , pattern = "Note.*", replacement = "")
standard_disk[,3] <- str_trim(standard_disk[,3])

# VM の Disk の種別 ( HDD or SSD ) を記載する列を追加して整形
standard_disk$LocalDisk <- "HDD"
standard_disk$LocalDisk[str_detect(string = standard_disk[,2], pattern = "SSD")] <- "SSD"
standard_disk[,2] <- str_replace(string =standard_disk[,2], pattern = " \\(SSD\\)", replacement = "")
standard_disk[,2] <- str_trim(standard_disk[,2])
standard_disk <- standard_disk[,c(2,3,4)]
names(standard_disk)[1:2] <- c("MaxOSDiskSize", "MaxTempDiskSize")

# VM の Memory に関する情報を整形
standard[,3] <- as.character(standard[,3])
standard[,3] <- str_replace(string = standard[,3], pattern = " MB", replacement = "")
standard[,3] <- str_replace(string = standard[,3], pattern = " GB", replacement = "")
standard[,3][[1]] <- as.numeric(standard[,3][[1]])/1000

# VM のサイズと Disk Size を結合
standard <- cbind(standard_size, standard)
standard <- cbind(standard,standard_disk)
standard <- standard[,c(1,2,4,5,7,9,10,11,8)]

# 列名を指定
names(standard)[[3]] <- "CPUCore"
names(standard)[[4]] <- "Memory"
names(standard)[[5]] <- "MaxDataDiskSize"
names(standard)[[9]] <- "MaxIOPS"

# MaxIOPS 列の文字列を整形
standard$MaxIOPS <- str_replace(string = standard$MaxIOPS, pattern = "x", replacement = " x ")
standard$MaxIOPS <- str_replace(string = standard$MaxIOPS, pattern = "X", replacement = " x ")

# 不要な中間 Data を削除
rm(standard_disk, standard_size)

# 列情報の追加
standard$tier <- "Standard"
standard$VMCategory <- str_replace(string = standard$SizePortal, pattern = "\\d{1,2}$", replacement = "")
standard$VMCategory <- str_replace(string = standard$VMCategory, pattern = "Standard_", replacement = "")
standard$VMSize <- str_replace(string = standard$SizePortal, pattern = "Standard_", replacement = "")
standard$CacheSize <- "-"
standard$bandwidth <- "-"

### Standard tier – DS series

# list: nodes の 5 番目に Standard tier DS の情報がある
standard_ds <- readHTMLTable(nodes[[5]], whichi = 1, header = FALSE)

# VM のサイズ記載を Portal 用と API 用で分割する
standard_ds_size <-data.frame(Reduce(rbind, str_split(string = standard_ds[,1], pattern = "\\\\")))
names(standard_ds_size) <- c("SizePortal","SizeAPI")

# VM のサイズ記載が Portal と API が同一の物は同じ記載になるように調整
standard_ds_size$SizePortal <- as.character(standard_ds_size$SizePortal)
standard_ds_size$SizeAPI <- as.character(standard_ds_size$SizeAPI)
standard_ds_size[,"SizeAPI"] <- standard_ds_size[,"SizePortal"]

# VM の Disk Size の関する情報を整形
standard_ds_disk <- data.frame(Reduce(rbind, str_split(string = standard_ds[,4], pattern = "=")))
standard_ds_disk[,2] <- str_replace(string = standard_ds_disk[,2] , pattern = " GBLocal SSD disk", replacement = "")
standard_ds_disk[,2] <- str_trim(standard_ds_disk[,2])
standard_ds_disk[,3] <- str_replace(string = standard_ds_disk[,3], pattern = "GB", replacement = "")
standard_ds_disk[,3] <- str_trim(standard_ds_disk[,3])

# VM の Disk の種別 ( 全て SSD ) を記載する列を追加して整形
standard_ds_disk$LocalDisk <- "SSD"
standard_ds_disk <- standard_ds_disk[,c(2,3,4)]
names(standard_ds_disk)[1:2] <- c("MaxOSDiskSize", "MaxTempDiskSize")

# ToDo: 各 VM Size 毎の data.frame を結合後に各列の型を検討する
# standard_ds[,3] <- as.numeric(as.character(standard_ds[,3]))
# standard_ds[,5] <- as.numeric(as.character(standard_ds[,5]))
# standard_ds[,6] <- as.numeric(as.character(standard_ds[,6]))

# MaxIOPS の列の整形
standard_ds$MaxIOPS <- str_extract(string = standard_ds[,7], pattern = "\\d{1,2},\\d{3}")
standard_ds$MaxIOPS <- str_replace(string = standard_ds$MaxIOPS, pattern = ",", replace = "")
standard_ds$MaxIOPS <- as.numeric(standard_ds$MaxIOPS)

# bandwidth 列の整形
standard_ds$bandwidth <- str_replace(string = standard_ds[,7], pattern = "\\d{1,2},\\d{3}", replace = "")
standard_ds$bandwidth <- str_replace(string = standard_ds$bandwidth, pattern = " MB per second", replacement = "")
standard_ds$bandwidth <- as.numeric(standard_ds$bandwidth)

# VM のサイズと Disk Size を結合
standard_ds <- cbind(standard_ds_size, standard_ds)
standard_ds <- cbind(standard_ds,standard_ds_disk)
standard_ds <- standard_ds[,c(1,2,4,5,7,8,12,13,14,10,11)]

# 列名を指定
names(standard_ds)[[3]] <- "CPUCore"
names(standard_ds)[[4]] <- "Memory"
names(standard_ds)[[5]] <- "MaxDataDiskSize"
names(standard_ds)[[6]] <- "CacheSize"

# 不要な中間 Data を削除
rm(standard_ds_disk, standard_ds_size)

# 列情報の追加
standard_ds$tier <- "Standard"
standard_ds$VMCategory <- "DS"
standard_ds$VMSize <- str_replace(string = standard_ds$SizePortal, pattern = "Standard_", replacement = "")

# 型の変換
standard_ds$MaxIOPS <- as.character(standard_ds$MaxIOPS)

### Standard tier – G series

# list: nodes の 6 番目に Basic tier の情報がある
standard_g <- readHTMLTable(nodes[[6]], whichi = 1, header = FALSE)

# VM のサイズ記載を Portal 用と API 用で分割する
standard_g_size <- data.frame(Reduce(rbind, str_split(string = standard_g[,1], pattern = "\\\\")))
names(standard_g_size) <- c("SizePortal","SizeAPI")

# VM のサイズ記載が Portal と API が同一の物は同じ記載になるように調整
standard_g_size$SizePortal <- as.character(standard_g_size$SizePortal)
standard_g_size$SizeAPI <- as.character(standard_g_size$SizeAPI)
standard_g_size[,"SizeAPI"] <- standard_g_size[,"SizePortal"]

# VM の Disk Size の関する情報を整形
standard_g_disk <- data.frame(Reduce(rbind, str_split(string = standard_g[,4], pattern = "=")))
standard_g_disk[,2] <- str_replace(string = standard_g_disk[,2] , pattern = " GBLocal SSD disk", replacement = "")
standard_g_disk[,3] <- str_replace(string = standard_g_disk[,3], pattern = " GB", replacement = "")
standard_g_disk <- standard_g_disk[,2:3]
names(standard_g_disk) <- c("MaxOSDiskSize", "MaxTempDiskSize")
standard_g_disk$MaxTempDiskSize <- str_replace(string= standard_g_disk$MaxTempDiskSize, pattern = ",", replacement = "")

# VM の Disk の種別 ( 全て SSD ) を記載する列を追加して整形
standard_g_disk$LocalDisk <- "SSD"

# VM の Memory に関する情報を整形
standard_g[,3] <- as.character(standard_g[,3])
standard_g[,3] <- str_replace(string = standard_g[,3], pattern = " GB", replacement = "")

# VM のサイズと Disk Size を結合
standard_g <- cbind(standard_g_size, standard_g)
standard_g <- cbind(standard_g, standard_g_disk)
standard_g <- standard_g[,c(1,2,4,5,7,9,10,8)]
standard_g$CacheSize <- "-"
standard_g$LocalDisk <- "SSD"
standard_g$bandwidth <- "-"

# 列名を指定
names(standard_g)[[3]] <- "CPUCore"
names(standard_g)[[4]] <- "Memory"
names(standard_g)[[5]] <- "MaxDataDiskSize"
names(standard_g)[[8]] <- "MaxIOPS"

# 不要な中間 Data を削除
rm(standard_g_disk, standard_g_size)

# Tier 情報の追加
standard_g$tier <- "Standard"
standard_g$VMCategory <- "G"
standard_g$VMSize <- str_replace(string = standard_g$SizePortal, pattern = "Standard_", replacement = "")

### Standard tier – GS series

# list: nodes の 7 番目に Standard tier GS の情報がある
standard_gs <- readHTMLTable(nodes[[7]], whichi = 1, header = FALSE)

# VM のサイズ記載を Portal 用と API 用で分割する
# VM のサイズ記載が Portal と API が同一の物は同じ記載になるように調整
standard_gs_size <- cbind(standard_gs[1], standard_gs[1])
names(standard_gs_size) <- c("SizePortal","SizeAPI")

# VM の Disk Size の関する情報を整形
standard_gs_disk <- data.frame(Reduce(rbind, str_split(string = standard_gs[,4], pattern = "=")))
standard_gs_disk[,2] <- str_replace(string = standard_gs_disk[,2] , pattern = " GBLocal SSD disk", replacement = "")
standard_gs_disk[,2] <- str_trim(standard_gs_disk[,2])
standard_gs_disk[,3] <- str_replace(string = standard_gs_disk[,3], pattern = "GB", replacement = "")
standard_gs_disk[,3] <- str_trim(standard_gs_disk[,3])

# VM の Disk の種別 ( 全て SSD ) を記載する列を追加して整形
standard_gs_disk$LocalDisk <- "SSD"
standard_gs_disk <- standard_gs_disk[,c(2,3,4)]
names(standard_gs_disk)[1:2] <- c("MaxOSDiskSize", "MaxTempDiskSize")

# ToDo: 各 VM Size 毎の data.frame を結合後に各列の型を検討する
# standard_gs[,3] <- as.numeric(as.character(standard_gs[,3]))
# standard_gs[,5] <- as.numeric(as.character(standard_gs[,5]))
# standard_gs[,6] <- as.numeric(as.character(standard_gs[,6]))

# MaxIOPS の列の整形
standard_gs$MaxIOPS <- str_extract(string = standard_gs[,7], pattern = "\\d{1,2},\\d{3}")
standard_gs$MaxIOPS <- str_replace(string = standard_gs$MaxIOPS, pattern = ",", replace = "")
standard_gs$MaxIOPS <- as.numeric(standard_gs$MaxIOPS)

# bandwidth 列の整形
standard_gs$bandwidth <- str_replace(string = standard_gs[,7], pattern = "\\d{1,2},\\d{3}", replace = "")
standard_gs$bandwidth <- str_replace(string = standard_gs$bandwidth, pattern = " MB per second", replacement = "")
standard_gs$bandwidth <- str_replace(string = standard_gs$bandwidth, pattern = ",", replacement = "")
standard_gs$bandwidth <- as.numeric(standard_gs$bandwidth)

# VM のサイズと Disk Size を結合
standard_gs <- cbind(standard_gs_size, standard_gs)
standard_gs <- cbind(standard_gs,standard_gs_disk)
standard_gs <- standard_gs[,c(1,2,4,5,7,8,12,13,14,10,11)]

# 列名を指定
names(standard_gs)[[3]] <- "CPUCore"
names(standard_gs)[[4]] <- "Memory"
names(standard_gs)[[5]] <- "MaxDataDiskSize"
names(standard_gs)[[6]] <- "CacheSize"

# 不要な中間 Data を削除
rm(standard_gs_disk, standard_gs_size)

# Tier 情報の追加
standard_gs$tier <- "Standard"
standard_gs$VMCategory <- "GS"
standard_gs$VMSize <- str_replace(string = standard_gs$SizePortal, pattern = "Standard_", replacement = "")

# 型の変換
standard_gs$MaxIOPS <- as.character(standard_gs$MaxIOPS)

# bind data.frame ---------------------------------------------------------

# 行の結合
vmlist <- rbind(basic, standard, standard_ds, standard_g, standard_gs)

# 列の並び替え
vmlist <- vmlist[,c("tier", "VMCategory", "VMSize", "CPUCore",  "Memory", "MaxDataDiskSize", "MaxOSDiskSize",  "MaxTempDiskSize", "LocalDisk", "MaxIOPS", "CacheSize", "bandwidth", "SizePortal", "SizeAPI")]

# ファイルの書き出し
write.table(vmlist, file="vmlist.csv", sep=",", row.names = FALSE)

  

