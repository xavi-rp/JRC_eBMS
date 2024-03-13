
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



## 2022 - 2023 data #### 

data_2022_23_orig <- fread("download-65e462713b9490.11320904.csv", header = TRUE)
#data_2022_23_orig <- fread("download-64bbf82ce44f35.92392491_modif.csv", header = TRUE)

# 2022_23
#data_2022_23_orig <- data_2022_23_orig %>% 
#  filter(grepl("2022", Date))

View(data_2022_23_orig)
(unique(data_2022_23_orig$Date))


#data_2022 <- fread("2022_Mydata_summaryDataGrid_20230613170159.csv", header = FALSE)
#data_2022
#nrow(data_2022)
#ncol(data_2022)
#names(data_2022)

data_2022_23_orig
names(data_2022_23_orig)

data_2022_23 <- data_2022_23_orig[, .SD, .SDcols = c("Section Name", "Date", "Preferred Species Name", "Abundance count")]
data_2022_23
data_2022_23[Date == "13/07/2023", ]


data_2022_23_agg <- data_2022_23 %>% 
  #slice(1) %>%
  #pivot_longer(cols = everything()) %>%
  #add_column(Abundance = unlist(data_2022_23[2])) %>%
  mutate_at(c("Abundance count"), as.numeric) %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"))  %>%
  rename(Abundance = `Abundance count`, 
         Species = `Preferred Species Name`, 
         Section = `Section Name`) %>%
  group_by(Date) %>%
  summarise(Total_abundance = sum(Abundance, na.rm = TRUE)) %>% 
  mutate(year = if_else(Date < "2023-01-01", 2022, 2023)) 


data_2022_23_agg
View(data_2022_23_agg)
str(data_2022_23_agg)
data_2022_23_agg$Date

## Plotting abundances #### 

## Total species

p1 <- data_2022_23_agg %>%
  #filter(Date > "2022-01-01" & Date < "2022-12-31") %>% 
  ggplot(aes(x = Date, y = Total_abundance, color = factor(year)))+
  geom_point()  +
  geom_line() +
  #geom_smooth(method = "loess", se = FALSE, aes(color = "darkorchid3")) +
  #scale_color_manual(name = NULL, 
  #                   labels = c("Abundance", "Loess Regression"), 
  #                   values = c("black", "darkorchid3")) +
  scale_x_datetime(breaks = scales::date_breaks("month"), 
                   labels = scales::date_format("%B %Y", tz = "CET")) +
  theme_half_open(font_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("Abundance (number of butterflies)") +
  labs(color = "Year")
    #theme_half_open(12)
p1

png("figures_2022_2023/abundance_all.png", units = "cm", res = 300, height = 15, width = 15)
print(p1)
dev.off()



## more abundant

sps_moreAbund <- data_2022_23 %>%
  mutate_at(c("Abundance count"), as.numeric) %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"))  %>%
  rename(Abundance = `Abundance count`, 
         Species = `Preferred Species Name`, 
         Section = `Section Name`) %>% 
  mutate(Year = if_else(Date < "2023-01-01", 2022, 2023))  %>%
  group_by(Species, Year) %>%
  summarise(Abundance_species = sum(Abundance, na.rm = TRUE)) %>%
  #arrange(Year, desc(Abundance_species)) %>%
  grouped_df(vars = "Year") %>% 
  slice_max(n = 5, order_by = Abundance_species) #%>% #View()
  arrange(Species, Year)

sps_moreAbund

sps_moreAbund_2022 <- as.vector(unlist(sps_moreAbund[sps_moreAbund$Year == 2022, "Species"]))
sps_moreAbund_2022
sps_moreAbund_2023 <- as.vector(unlist(sps_moreAbund[sps_moreAbund$Year == 2023, "Species"]))
sps_moreAbund_2023
sps_moreAbund

sps_moreAbund_1 <- unique(c(sps_moreAbund_2022[1:4], sps_moreAbund_2023[1:4]))




data_2022_aggr_Abund <- data_2022_23 %>%
  mutate_at(c("Abundance count"), as.numeric) %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"))  %>%
  rename(Abundance = `Abundance count`, 
         Species = `Preferred Species Name`, 
         Section = `Section Name`) %>% 
  filter(Species %in% sps_moreAbund_1) %>%
  select(- "Section") %>%
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y"))  %>%
  group_by(Date, Species) %>%
  summarise(Abundance = sum(Abundance)) %>%
  pivot_wider(names_from = Date, values_from = Abundance) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = c(everything(), -Species), names_to = "Date")  %>%
  #mutate(V1 = replace(V1, V1 == "Date", values = "Species")) %>%
  #janitor::row_to_names(row_number = 1) %>%
  #pivot_longer(cols = Date) %>% 
  mutate_at(c("value"), as.numeric) %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>%
  mutate(Year = lubridate::year(Date), 
         Month = lubridate::month(Date), 
         Day = lubridate::day(Date)) %>%
  #mutate(Year = if_else(Date < "2023-01-01", 2022, 2023)) %>%
  mutate(datetime = lubridate::make_datetime(2022, Month, Day)) 

  ##replace_na(list(value = 0))  %>%
  #filter(Date > "2022-03-12" & Date < "2022-10-30") %>%
  #mutate(Species = fct_relevel(Species, sps_moreAbund))
  
data_2022_aggr_Abund

sps_moreAbund_1
data_2022_aggr_Abund[data_2022_aggr_Abund$Species == "Anthocharis cardamines"]

p2 <- data_2022_aggr_Abund %>%
  ggplot(aes(x = datetime, y = value, color = factor(Year)))+
  geom_point()  +
  geom_line() +
  #geom_smooth(aes(group = Year), method = "loess", se = FALSE) +
  scale_x_datetime(breaks = lubridate::make_datetime(2022, 1:12), labels = month.abb) +
  theme_half_open(font_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab("Abundance (number of butterflies)")  +
  labs(color = "Year") + 
  facet_wrap(~ Species, ncol = 2, scales = "free") 

p2

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


png("figures_2022_2023/abundance_5more.png", units = "cm", res = 300, height = 15, width = 15)
#png("figures_2022_2023/abundance_5more_loess.png", units = "cm", res = 300, height = 15, width = 15)
p2
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





















