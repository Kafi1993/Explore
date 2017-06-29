#Big5 Datensatz frisieren
big5 <- read.csv2("../tryPackaging/inst/data.csv", stringsAsFactors = F, sep = "\t")
head(big5)
table(big5[,2])
big5 <- big5[big5$age < 101,]
table(big5_f[,2])

save(... = big5, file = "data/big5.RData")
rm(big5_f)

load("data/big5.RData")
head(big5)
