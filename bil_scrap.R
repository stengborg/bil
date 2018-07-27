library(rvest)
library(tidyr)
library(readr)
library(tidyverse)
library(plotly)
# library(RKing)
options(scipen=999)

i <- 2485 # Antal sidor
snapshot_dt <- as.Date("2018-07-24")

for(i in 2:i) {
if(i == 2) {
  start = Sys.time()
}
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

if(i == max(i)) {
  slut = Sys.time()
  tid = slut-start
  print(tid)
  }
}


tot$snapshot_dt <- snapshot_dt

c_tot <- tot %>% 
mutate(titel = as.character(titel)) %>% 
separate(titel, c("marke", "modell"), extra = 'drop', remove = FALSE) %>% 
mutate(ar = sub(".+-", "", titel)) %>% 
mutate(kost = parse_number(pris, locale = locale(grouping_mark = " "))) %>% 
separate(col =  typ,into = c("drivmedel", "lada", "stracka"),sep = "[|]", remove = FALSE) %>% 
separate(stracka, into = c("lag", "hog"), sep = "[-]", remove = FALSE) %>% 
mutate(c_mil = gsub(x = lag,pattern = "[mil]",replacement = "")) %>% 
mutate(trim_mil = trimws(c_mil, which = "both")) %>% 

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
mutate(c_stracka = parse_integer(gsub(pattern = " ",replacement = "",x = trim_mil))) %>% 
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
# Borjar med volvo
agg_modell <- c_tot %>% group_by(c_modell) %>% 
    filter(c_marke == "volvo") %>% 
    filter(ar %in% c("00","01","02","03","04","05","06","07","08","09","10",
  "11","12","13","14","15","16","17","18","19"),
  c_modell %in% c("v70","v60","xc60","v40","xc70","s60","v50","v90","xc90","s80"))

df_agg_modell <-  agg_modell %>% group_by(c_modell) %>% 
  summarise(n = n()) %>% arrange(desc(n))

ggplot(agg_modell, aes(x=c_modell, y=kost, fill=c_modell)) + 
  geom_boxplot() +
  guides(fill=FALSE) +
  labs(x = "", y = "Pris", title = "Pris f?r volvo modeller") +
  theme(plot.title = element_text(hjust = 0.5), 
    axis.line = element_blank())

ggsave("pris_per_modell_volvo.png", width = 20, height = 20, units = "cm")



# 3. Per modell och ar
agg_modell_ar <- c_tot %>% 
    filter(c_marke == "volvo") %>% 
    filter(ar %in% c("00","01","02","03","04","05","06","07","08","09","10",
  "11","12","13","14","15","16","17","18","19"),
  c_modell == "v60") %>% 
  group_by(ar) %>% 
  summarise(avg_pris = mean(kost)) %>% 
  ungroup %>% 
  mutate(ar = as.factor(ar)) %>% 
  mutate(position = factor(ar, levels = rev(levels(ar))))


ggplot(agg_modell_ar, aes(x=position, y=avg_pris, group = 1)) + 
  geom_line() +
  geom_point() +
  labs(x = "", y = "Genomsnittspris", title = "Pris for volvo v60") +
  theme(plot.title = element_text(hjust = 0.5), 
    axis.line = element_blank(),
    plot.background = element_blank())

ggsave("pris_per_ar_xc60_volvo.png", width = 20, height = 20, units = "cm")
ggsave("pris_per_ar_v60_volvo.png", width = 20, height = 20, units = "cm")


# boxplot per ar
c_tot %>% 
filter(c_marke == "volvo") %>% 
    filter(ar %in% c("00","01","02","03","04","05","06","07","08","09","10",
                     "11","12","13","14","15","16","17","18","19"),
           c_modell == "v90") %>% 
    mutate(ar = as.factor(ar)) %>% 
    mutate(position = factor(ar, levels = rev(levels(ar)))) %>% 
ggplot(aes(x=position, y=kost/1000, fill = position)) + 
    geom_boxplot() +
    guides(fill=FALSE) +
    labs(x = "", y = "Pris [K SEK]", title = "Pris for volvo V90") +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.line = element_blank(),
          plot.background = element_blank())

ggsave("boxplot_pris_per_ar_xc60_volvo.png", width = 20, height = 20, units = "cm")
ggsave("boxplot_pris_per_ar_v90_volvo.png", width = 20, height = 20, units = "cm")


# 3.1 Volvo allamodeller per ar
c_tot %>% group_by(c_modell) %>% 
    filter(c_marke == "volvo") %>% 
    filter(ar %in% c("10","11","12","13","14","15","16","17","18","19"), 
  c_modell %in% c("v60","xc60","xc70","v90")) %>% 
 mutate(position = factor(ar, levels = rev(levels(ar)))) %>% 
ggplot(aes(x=position, y=kost/1000, fill = c_modell)) + 
    geom_boxplot() +
    labs(x = "", y = "Pris [K SEK]", title = "Pris for volvo per modell och ar") +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.line = element_blank(),
          plot.background = element_blank())

ggsave("boxplot_pris_per_ar_volvo.png", dpi=300,width=320, height=180, units = "mm")





# 4. Per modell och ar och mil
agg_drivmedel <- c_tot %>% 
    group_by(drivmedel) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n))

agg_modell <- c_tot %>% 
    group_by(c_modell) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n))

unique(c_tot$drivmedel)

agg_modell_ar <- c_tot %>% 
    filter(c_marke == "volvo") %>% 
    filter(ar %in% c("00","01","02","03","04","05","06","07","08","09","10",
                     "11","12","13","14","15","16","17","18","19"),
           c_modell == "v90") %>% 
    mutate(ar = as.factor(ar)) %>% 
    mutate(position = factor(ar, levels = rev(levels(ar)))) %>% 
    mutate(seg_stracka = ifelse(c_stracka == 0, "0",
                        ifelse(c_stracka <= 1500, "0-1500",
                        ifelse(c_stracka <= 5000, "1500-5000",
                        ifelse(c_stracka <= 10000, "5000-10000", "10000+"))))) %>% 
    filter(!drivmedel %in% c("Diesel","Bensin","Milj�br�nsle/Hybrid","El")) %>% 
    mutate(seg_stracka = as.factor(seg_stracka), drivmedel = as.factor(drivmedel)) %>% 
  mutate(seg_stracka = factor(seg_stracka, levels = c("0","0-1500","1500-5000","5000-10000","10000+"))) %>%
    group_by(position, seg_stracka, drivmedel) %>% 
   summarise(avg_pris = mean(kost))


ggplot(agg_modell_ar,aes(x = position, 
                         y = avg_pris, 
                         color = seg_stracka, 
                         group = seg_stracka)) + 
    geom_line() +
    geom_point() +
    #geom_boxplot() +
    labs(x = "", y = "Genomsnittspris", title = "Pris for volvo V90") +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.line = element_blank(),
          plot.background = element_blank()) +
    facet_wrap(~drivmedel)

ggsave("pris_per_ar_mil_xc60_volvo_linegraph_drivmedel.png", width = 20, height = 20, units = "cm")
ggsave("pris_per_ar_mil_xc90_volvo_linegraph_drivmedel.png", width = 20, height = 20, units = "cm")
ggsave("pris_per_ar_mil_v90_volvo_linegraph_drivmedel.png", width = 20, height = 20, units = "cm")


# v90
# 4. Per modell och ar och mil

agg_modell_ar <- c_tot %>% 
    filter(c_marke == "audi") %>% 
    filter(ar %in% c("00","01","02","03","04","05","06","07","08","09","10",
                     "11","12","13","14","15","16","17","18","19"),
           c_modell == "a6") %>% 
    mutate(ar = as.factor(ar)) %>% 
    mutate(position = factor(ar, levels = rev(levels(ar)))) %>% 
    mutate(seg_stracka = ifelse(c_stracka == 0, "0",
                        ifelse(c_stracka <= 1500, "0-1500",
                        ifelse(c_stracka <= 5000, "1500-5000",
                        ifelse(c_stracka <= 10000, "5000-10000", "10000+"))))) %>% 
    mutate(seg_stracka = as.factor(seg_stracka)) %>% 
  mutate(seg_stracka = factor(seg_stracka, levels = c("0","0-1500","1500-5000","5000-10000","10000+")))


ggplot(agg_modell_ar %>% filter(kost > 100000),
       aes(x = position, 
           y = kost/1000, 
           fill = seg_stracka)) + 
  geom_boxplot() +
  labs(x = "", y = "Genomsnittspris", title = "Pris for Audi A6") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.line = element_blank(),
        plot.background = element_blank())

ggsave("arsmodell_a6_stracka.png",dpi=300,width=320, height=180, units = "mm")
