
#sessionInfo()

if(Sys.info()[4] == "D01RI1700308") {
  wd <- "D:/xavi_rp/JRC_eBMS/"
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- "D:/rotllxa/JRC_eBMS/"
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03", "jeodpp-terminal-dev-12", "jeodpp-terminal-jd002-03")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/JRC_eBMS/")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/JRC_eBMS/")
  wd <- "/eos/jeodpp/home/users/rotllxa/JRC_eBMS/"
}else{
  wd <- "/Users/xavi_rp/Documents/FFGRCC_laptop/JRC_eBMS_data/"
}

setwd(wd)
list.files()



library(tidyverse)
library(data.table)
library(viridis)
library(cowplot)



## 2022 data #### 

data_2022_orig <- fread("download-64bbf82ce44f35.92392491.csv", header = TRUE)
data_2022_orig <- fread("download-64bbf82ce44f35.92392491_modif.csv", header = TRUE)

# 2022
data_2022_orig <- data_2022_orig %>% 
  filter(grepl("2022", Date))

View(data_2022_orig)
unique(data_2022_orig$Date)


data_2022 <- fread("2022_Mydata_summaryDataGrid_20230613170159.csv", header = FALSE)
data_2022
nrow(data_2022)
ncol(data_2022)
names(data_2022)

data_2022_aggr <- data_2022[c(2, nrow(data_2022)), ]
data_2022_aggr <- data_2022_aggr[, 2:(ncol(data_2022_aggr) - 1)]
data_2022_aggr


data_2022_aggr <- data_2022_aggr %>% 
  slice(1) %>%
  pivot_longer(cols = everything()) %>%
  add_column(Abundance = unlist(data_2022_aggr[2])) %>%
  mutate_at(c("Abundance"), as.numeric) %>%
  mutate(Date = as.POSIXct(value, format = "%d/%m/%Y"))  

data_2022_aggr
str(data_2022_aggr)

## Plotting abundances #### 

## Total species

p1 <- data_2022_aggr %>%
  filter(Date > "2022-03-12" & Date < "2022-10-30") %>% 
  ggplot(aes(x = Date, y = Abundance, color = "Abundance")) +
  geom_point()  +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, aes(color = "darkorchid3")) +
  scale_color_manual(name = NULL, 
                     labels = c("Abundance", "Loess Regression"), 
                     values = c("black", "darkorchid3")) +
  #theme_bw() +
  scale_x_datetime(breaks = scales::date_breaks("month"), labels = scales::date_format("%B %Y")) +
  theme_half_open(font_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("Abundance (number of butterflies)") 
  #theme_half_open(12)

png("abundance_all.png", units = "cm", res = 300, height = 15, width = 15)
print(p1)
dev.off()



## more abundant

sps_moreAbund <- data_2022 %>%
  select(1, ncol(data_2022)) %>%
  mutate_at("V55", as.numeric) %>%
  arrange(desc(V55)) %>%
  #slice(1:6) %>%
  slice(1:5) %>%
  select(1) %>%
  unlist() %>%
  as.vector() 

sps_moreAbund <- sps_moreAbund[!grepl("Tot", sps_moreAbund)]
sps_moreAbund


data_2022_aggr_Abund <- data_2022 %>%
  filter(V1 %in% c("Date", sps_moreAbund))  %>%
  select(- "V55") %>% 
  mutate(V1 = replace(V1, V1 == "Date", values = "Species")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(cols = c(everything(), -Species)) %>% 
  mutate_at(c("value"), as.numeric) %>%
  mutate(Date = as.POSIXct(name, format = "%d/%m/%Y")) %>%
  #replace_na(list(value = 0))  %>%
  filter(Date > "2022-03-12" & Date < "2022-10-30") %>%
  mutate(Species = fct_relevel(Species, sps_moreAbund))

p2 <- data_2022_aggr_Abund %>%
  ggplot(aes(x = Date, y = value)) +
  geom_point()  +
  #geom_line() +
  geom_smooth(method = "loess", se = FALSE, colour = "darkorchid3") +
  #theme_bw() +
  scale_x_datetime(breaks = scales::date_breaks("month"), labels = scales::date_format("%B %Y")) +
  theme_half_open(font_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("Abundance (number of butterflies)") + 
  facet_wrap(~ Species, ncol = 2, scales = "free")


list.files(full.names = TRUE)

logo_file_maniola <- "./Maniola-jurtina.png"
logo_file_cardamines <- "./anthocharis-cardamines.png"
logo_file_rapae <- "./Pieris_rapae.png"
logo_file_napi<- "./Pieris_napi.png"

p3 <- ggdraw() +
  draw_image(logo_file_maniola, scale = .1, halign = 0.45, valign = 0.92) +
  draw_image(logo_file_cardamines, scale = .1, halign = 0.98, valign = 0.92) +
  draw_image(logo_file_rapae, scale = .1, halign = 0.45, valign = 0.40) +
  draw_image(logo_file_napi, scale = .1, halign = 0.98, valign = 0.40) +
  draw_plot(p2)


png("abundance_4more.png", units = "cm", res = 300, height = 15, width = 15)
print(p3)
dev.off()

#


# abundance + richness


data_2022_orig
names(data_2022_orig)
View(data_2022_orig)
(unique(data_2022_orig$Date))



abund <- data_2022_orig %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"))  %>%
  group_by(Date, `Section Name`) %>%
  summarise(Abundance = sum(`Abundance count`, na.rm = TRUE))

abund_trans <- data_2022_orig %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"))  %>%
  group_by(Date) %>%
  summarise(Abundance_transect = sum(`Abundance count`, na.rm = TRUE))

abund <- merge(abund, abund_trans, all.x = TRUE)
View(abund)
  

p4 <- data_2022_orig %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"))  %>%
  count(Date, `Section Name`, sort = FALSE, name = "Richness") %>%
  full_join(abund, by = c("Date", "Section Name")) %>% #View()
  mutate(Abundance_transect_01 = Abundance_transect / 10) %>%
  #mutate_at(c("Abundance"), as.numeric) %>% View()
  ggplot(aes(x = Date)) +
  geom_bar(aes(y = Richness, fill = factor(`Section Name`)), stat = "identity") +
  scale_x_datetime(breaks = scales::date_breaks("month"), labels = scales::date_format("%B %Y")) +
  theme_half_open(font_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  #ylab("Richness (number of species)") +
  scale_fill_viridis(option = "viridis", #"plasma", 
                     discrete = TRUE, direction = -1, name = "Richness: Transect Section") +
  geom_point(aes(y = Abundance_transect_01))  +
  geom_line(aes(y = Abundance_transect_01, linetype = "Abundance")) +
  scale_linetype_manual(name = NULL, values = 1) +
  #geom_smooth(aes(y = Abundance_transect_01), method = "loess", se = FALSE, 
  #            colour = "darkorchid3") +
  scale_y_continuous(name = "Species Richness (Number of Species)", 
                     sec.axis = sec_axis(~., name = "Abundance (Number of Butterflies) / 10"))
  

png("richness_abundance.png", units = "cm", res = 300, height = 15, width = 18)
print(p4)
dev.off()




####

#





















