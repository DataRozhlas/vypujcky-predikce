install.packages('devtools')
devtools::install_github("rstats-db/bigrquery")
devtools::install_github("r-lib/progress")

require(bigrquery)
require(plyr)
require(dplyr)
require(magrittr)
require(lubridate)

### \casova-rada\vypujcky.txt   
###       STAT$VYP_PTR_SVAZKY   STAT$VYP_PTR_LEG    STAT$VYP_TIME   STAT$VYP_OPID   STAT$VYP_PTR_KNODDEL
### \casova-rada\legitky.txt
###       LEG_KEY               POHLAVI             VEK
### \2016\knihovny.txt
###       KNODDEL_KEY           KNODDEL_NAZEV       KNODDEL_ULICE   KNODDEL_PSC
### \2016\opidy.txt
###       OPIDY_OPID            OPIDY_POPIS
### \2016\svazky.txt
###       SVAZKY_KEY            SVAZKY_PTR_TITUL
### \2016\tituly.txt
###       TITUL_KEY             TITUL_SIGN_FULL     TITUL_ZAHLAVI   TITUL_NAZEV   TITUL_ROK_VYDANI   
###       TITUL_JAZYK           TITUL_DRUH_DOKUMENTU

project <- 'knihovny-175106'

query <- "SELECT * FROM vypucjky.svazky WHERE TRUE"
svazky <- query_exec(query, project = project, use_legacy_sql = F, max_pages = Inf)
colnames(svazky)[2] <- 'TITUL_ID'

query <- "SELECT * FROM vypucjky.tituly WHERE TRUE"
tituly <- query_exec(query, project = project, use_legacy_sql = F, max_pages = Inf)

query <- paste("SELECT SVAZEK_ID, LEGITKA_ID, DATUMCAS FROM vypucjky.vypujcky WHERE OP_ID IN ('4', '94', '97', '225') AND LEGITKA_ID_INT < 100000")
vypujcky <- query_exec(query, project = project, use_legacy_sql = F, max_pages = Inf)

rm(project); rm(query)

vypujcky$DATUMCAS <- gsub(' .*', '', vypujcky$DATUMCAS)
colnames(vypujcky)[3] <- 'DATE'
vypujcky$DATE <- gsub('JAN', '01', vypujcky$DATE)
vypujcky$DATE <- gsub('FEB', '02', vypujcky$DATE)
vypujcky$DATE <- gsub('MAR', '03', vypujcky$DATE)
vypujcky$DATE <- gsub('APR', '04', vypujcky$DATE)
vypujcky$DATE <- gsub('MAY', '05', vypujcky$DATE)
vypujcky$DATE <- gsub('JUN', '06', vypujcky$DATE)
vypujcky$DATE <- gsub('JUL', '07', vypujcky$DATE)
vypujcky$DATE <- gsub('AUG', '08', vypujcky$DATE)
vypujcky$DATE <- gsub('SEP', '09', vypujcky$DATE)
vypujcky$DATE <- gsub('OCT', '10', vypujcky$DATE)
vypujcky$DATE <- gsub('NOV', '11', vypujcky$DATE)
vypujcky$DATE <- gsub('DEC', '12', vypujcky$DATE)
vypujcky$DATE <- as.Date(vypujcky$DATE, format = '%d-%m-%Y')

vypujcky <- merge(vypujcky, svazky)

vypujcky_backup <- vypujcky



###

vypujcky <- vypujcky_backup[year(vypujcky_backup$DATE) == 2016,]

df <- data.frame(AKT = numeric(), PRE = numeric(), AKT_DATE = as.Date(character()), PRE_DATE = as.Date(character()), stringsAsFactors = F)

legitky <- unique(vypujcky$LEGITKA)

# pro každou legitku 
for (i in 1:length(legitky)) {
  
# najdi všechny výpůjčky a seřaď je od nejnovější k nejstarší
  vypctenar <- vypujcky[vypujcky$LEGITKA_ID == legitky[i],]
  vypctenar <- vypctenar[order(vypctenar$DATE, decreasing = T), 3:4]

# čtenáři s jedinou výpůjčkou nás nezajímají
  if(nrow(vypctenar) < 2) next

# a pro každou výpůjčku vypiš všechny starší
  for (j in 1:(nrow(vypctenar)-1)) {
    df <- rbind(df, data.frame(vypctenar[j,]$TITUL, vypctenar[(j+1):nrow(vypctenar),]$TITUL, vypctenar[j,]$DATE, vypctenar[(j+1):nrow(vypctenar),]$DATE))
  }
  
  print(paste(round(i/length(legitky)*100, 2), '%'))

}

colnames(df) <- c('AKT', 'PRE', 'AKT_DATE', 'PRE_DATE')

rm(i); rm(j); rm(legitky); rm(vypctenar)

# najdi nejčastější dvojice a setřiď je od největší
vypfreq <- df %>% group_by(AKT, PRE) %>% summarize(Count = n())
vypfreq <- vypfreq[order(vypfreq$Count, decreasing = T),]

# porovnej s celkovým počtem výpůjček daného titulu
vypkontrol <- vypujcky %>% group_by(TITUL_ID) %>% summarize(Count = n())
vypfreq <- merge(vypfreq, vypkontrol, by.x = 'AKT', by.y = 'TITUL_ID')
colnames(vypfreq) <- c('AKT', 'PRE', 'COUNT', 'CONTROL')

# přidej autory a názvy
vypfreq <- merge(vypfreq, tituly, by.x = 'AKT', by.y = 'TITUL_ID')
vypfreq <- vypfreq[, c(1, 6:7, 2:4)]

vypfreq <- merge(vypfreq, tituly, by.x = 'PRE', by.y = 'TITUL_ID')
vypfreq <- vypfreq[, c(2, 8:9, 1, 3:6)]
colnames(vypfreq) <- c('AKT', 'AKT_AUTOR', 'AKT_NAZEV', 'PRE', 'PRE_AUTOR', 'PRE_NAZEV', 'COUNT', 'CONTROL')










