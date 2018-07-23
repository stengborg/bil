library(rvest)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

i <- 2485 # Antal sidor
snapshot_dt <- as.Date("2018-07-22")

for(i in 2:i) {
  print(paste0("Iteration: ", i))
url<- paste0('https://www.blocket.se/hela_sverige?q=&cg=1020&w=3&st=s&ps=&pe=&mys=&mye=&ms=&me=&cxpf=&cxpt=&fu=&gb=&ca=11&l=0&md=th&cp=&o=',i)
webpage <- read_html(url)
titel_html <- html_nodes(webpage, '.item_link')
pris_html <- html_nodes(webpage,'#ps_0 , #item_list .font-large')
typ_html <- html_nodes(webpage, '.motor-li-thumb-extra-info')

# Converting the title data to text
titel <- html_text(titel_html)
pris <- html_text(pris_html)
pris <- pris[-1]
typ <- html_text(typ_html)

alles <- cbind(titel, pris, typ)

df_alles <- as.data.frame(alles)

if(i == 2) {
    tot <- df_alles
} else {
    tot <- rbind(tot, df_alles)}
}

tot$snapshot_dt <- snapshot_dt

c_tot <- tot %>% 
mutate(titel = as.character(titel)) %>% 
separate(titel, c("marke", "modell"), extra = 'drop', remove = FALSE) %>% 
mutate(ar = sub(".+-", "", titel)) %>% 
mutate(kost = parse_number(pris, locale = locale(grouping_mark = " "))) %>% 
separate(typ, c("drivmedel", "lada", "stracka"), extra = 'drop', remove = FALSE) %>% 

mutate(c_ar = nchar(gsub(" ", "", ar, fixed = TRUE))) %>% 
filter(c_ar == 2) %>% 
group_by(ar) %>% 
mutate(n = n()) %>% 
filter(n >= 100) %>% 
ungroup() %>% 
filter(kost >= 10000) %>% 
mutate(c_marke = tolower(marke)) %>% 
mutate(c_modell = tolower(modell)) %>% 
mutate(c_marke = ifelse(c_marke %in% c("vw", "wv", "passat"), "volkswagen", 
    ifelse(c_marke %in% c("v70","v50","v60","xc70","xc90"),"volvo",c_marke))) %>% 
    filter(ar %in% c("00","01","02","03","04","05","06","07","08","09","10",
                     "11","12","13","14","15","16","17","18","19")) %>% 
#filter(drivmedel %in% c("Diesel", "Bensin", "Miljöbränsle", "El")) %>% 
#filter(lada %in% c("Manuell","Automat","Hybrid")) %>% 
#filter(!stracka %in% c("Automat","Mer","Manuell")) %>% 
mutate(ar = as.factor(ar), c_marke = as.factor(c_marke))
    

# ar
# drivmedel
# lada
# stracka
# pris
modell
 
tmp <-  c_tot %>% group_by(c_modell) %>% summarise(n = n()) %>% arrange(desc(n))
    
# TODO
# 1. Per märke
# 2. Per modell
# 3. Per modell och år

# 1. Per märke
agg <- c_tot %>% group_by(c_marke) %>% 
    filter(c_marke %in% c("volvo", "bmw", "volkswagen", "mercedes", "audi", "ford", "peugeot")) %>% 
    filter(kost <= 1500000)


ggplot(agg, aes(x=c_marke, y=kost, fill=c_marke)) + 
    geom_boxplot() +
    guides(fill=FALSE)
ggsave("pris_per_marke.png", width = 20, height = 20, units = "cm")



# 2. Per modell
# Börjar med volvo
agg_modell <- c_tot %>% group_by(c_modell) %>% 
    filter(c_marke == "volvo") %>% 
    filter(ar %in% c("00","01","02","03","04","05","06","07","08","09","10",
                     "11","12","13","14","15","16","17","18","19"))

p <- ggplot(agg_modell, aes(x=c_modell, y=kost, fill=c_modell)) + 
    geom_boxplot() +
    guides(fill=FALSE)
ggplotly(p)
ggsave("pris_per_modell_volvo.png", width = 20, height = 20, units = "cm")


