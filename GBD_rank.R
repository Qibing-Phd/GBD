setwd("C:/Manuscript/LPH/Results")

library(easyGBDR)
library(data.table)
library(dplyr)
library(ggplot2)
GBD_edition(2021)

data4 <- GBDread_csv(file = "J:/Manuscript/LPH/Cardiovascular diseases_rank_15_39.csv")

unique_variable <- list(measure=unique(data4$measure),
                        location=unique(data4$location),
                        sex=unique(data4$sex),
                        age=unique(data4$age),
                        cause=unique(data4$cause),
                        rei=unique(data4$rei),
                        metric=unique(data4$metric),
                        year=unique(data4$year))
unique_variable


Rank_Deaths_Global_Both <- data4 |>
  filter(metric=="Rate") |> # 选择Number
  filter(age=="15-39 years") |> 
  filter(!cause=="Cardiovascular diseases") |> 
  filter(sex=="Both") |> 
  filter(year%in%c("1990","2000","2010","2021")) |> 
  filter(measure=="Deaths") |> 
  filter(location=="Global") # 选择地区
colnames(Rank_Deaths_Global_Both)

Rank_Deaths_Global_Both <- Rank_Deaths_Global_Both %>%
  group_by(year) %>%
  mutate(rank = dense_rank(desc(val))) %>%
  arrange(desc(rank))

print(Rank_Deaths_Global_Both)



sort_1990_Both <- subset(Rank_Deaths_Global_Both, year == "1990")
sort_1990_Both <- sort_1990_Both[order(sort_1990_Both$rank), ]
cause_Both_order <- sort_1990_Both$cause
cause_Both_order <- rev(cause_Both_order)

year_order <- c("1990", "2000","2010","2021")

Rank_Deaths_Global_Both$cause <- factor(Rank_Deaths_Global_Both$cause, levels = cause_Both_order)
Rank_Deaths_Global_Both <- Rank_Deaths_Global_Both[order(Rank_Deaths_Global_Both$rank), ]


Rank_Deaths_Global_Both$year <- factor(Rank_Deaths_Global_Both$year, levels = year_order)
Rank_Deaths_Global_Both <- Rank_Deaths_Global_Both[order(Rank_Deaths_Global_Both$rank), ]

p1 <- ggplot(Rank_Deaths_Global_Both, aes(x = year, y = cause, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), vjust = 1, color = "black") +
  scale_fill_gradient2(low = "#d72e25", mid = "#fee08b", high = "#1a9850", midpoint = mean(Rank_Deaths_Global_Both$rank)) +
  labs(title = "Changes in cardiovascular disease rankings from 1990 to 2021 in Both", x = "", y = "Cardiovascular diseases", fill = "Rank") +
  theme_minimal() +
  theme(axis.title.x = element_text(angle = 0, vjust = 0.5),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Cardiovascular diseases_Rank_change_Deaths_Both_1990_2021.pdf",p1,width=8,height=6,dpi=600)



#AAPC按地区分类：

library(easyGBDR)
library(data.table)
GBD_edition(2021)
library(ggplot2)
library(dplyr)
library(dplyr)
library(readr)

file_path <- "C:/Manuscript/LPH/ASR"

# 获取目录中的所有 CSV 文件路径
csv_files <- list.files(file_path, pattern = "\\.csv$", full.names = TRUE)

# 读取并合并所有 CSV 文件
merged_data <- purrr::map_dfr(csv_files, read_csv)

print(merged_data)

write.csv(merged_data, "Cardiovascular diseases_ASR_15_39.csv", row.names = FALSE)


data4 <- merged_data %>%
  filter(age == "Age-standardized") %>%
  filter(!cause == "Cardiovascular diseases") %>%
  filter(year%in%c("1990","2021")) %>%
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"))


data4 <- data4[,c(1,2,3,4,5,7,8)] 


Rank_Prevalence <- data4 %>%
  filter(measure == "Prevalence") %>%
  filter(year == "1990") %>%
  filter(!cause == "Cardiovascular diseases") %>%
  filter(sex == "Both")


Rank_Prevalence <- Rank_Prevalence %>%
  group_by(location) %>%
  mutate(rank = dense_rank(desc(val)))
print(Rank_Prevalence)

Sort_Global <- subset(Rank_Prevalence, location == "Global")
Sort_Global <- Sort_Global[order(Sort_Global$rank), ]
rei_Male_order <- Sort_Global$cause
rei_Male_order <- rev(rei_Male_order)

location_order <- c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")

Rank_Prevalence$cause <- factor(Rank_Prevalence$cause, levels = rei_Male_order)
Rank_Prevalence <- Rank_Prevalence[order(Rank_Prevalence$rank), ]


Rank_Prevalence$location <- factor(Rank_Prevalence$location, levels = location_order)
Rank_Prevalence <- Rank_Prevalence[order(Rank_Prevalence$rank), ]

p1 <- ggplot(Rank_Prevalence, aes(x = location, y = cause, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), vjust = 1, color = "black") +
  scale_fill_gradient2(low = "#d72e25", mid = "#fee08b", high = "#1a9850", midpoint = mean(Rank_Prevalence$rank)) +
  labs(title = "ASR in cardiovascular diseases rankings from 1990 to 2021", x = "", y = "", fill = "Rank") +
  theme_minimal() +
  theme(axis.title.x = element_text(angle = 0, vjust = 0.5),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Cardiovascular diseases_Rank_AAPC_Prevalence_region_1990_2021.pdf",p1,width=12,height=6,dpi=600)



data4 <- merged_data %>%
  filter(age == "Age-standardized") %>%
  filter(!location%in% c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")) %>%
  filter(!cause == "Cardiovascular diseases")



Rank_DALYs <- data4 %>%
  filter(measure == "DALYs (Disability-Adjusted Life Years)") %>%
  filter(!cause == "Cardiovascular diseases") %>%
  filter(sex == "Both")


Rank_DALYs <- Rank_DALYs %>%
  group_by(location) %>%
  mutate(rank = dense_rank(desc(EAPC)))
print(Rank_DALYs)

Sort_Global <- subset(Rank_DALYs, location == "Armenia")
Sort_Global <- Sort_Global[order(Sort_Global$rank), ]
rei_Male_order <- Sort_Global$cause
rei_Male_order <- rev(rei_Male_order)

location_order <- c("Armenia","Azerbaijan","Ghana","Kiribati","Latvia","Morocco","Thailand","Türkiye","Vanuatu","Albania","Botswana","Burkina Faso","Croatia","Czechia","India","Mozambique","Northern Mariana Islands","Portugal","Saint Kitts and Nevis","Singapore","Somalia","South Africa","Belgium","Ethiopia","Lesotho","Madagascar","Rwanda","Saint Lucia","Uganda","Australia","Niger","Bulgaria","Kazakhstan","Djibouti","Solomon Islands","Central African Republic","Guam","Uruguay","Argentina","China","Uzbekistan","Andorra","Austria","Belize","Cyprus","Dominican Republic","France","Gabon","Greece","Grenada","Indonesia","Italy","Jamaica","Japan","Malawi","Mauritania","Montenegro","Nicaragua","Oman","Puerto Rico","Senegal","Sudan","Syria","Taiwan (province of China)","Ukraine","Bosnia and Herzegovina","El Salvador","Philippines","Antigua and Barbuda","Bahrain","Belarus","Benin","Bhutan","Cuba","Ecuador","Egypt","Guatemala","Honduras","Hungary","Jordan","Qatar","Samoa","San Marino","Sao Tome and Principe","Switzerland","Tonga","USA","Comoros","Coted'Ivoire","Eritrea","Guinea-Bissau","Iceland","Monaco","Nigeria","Papua New Guinea","Viet Nam","Brunei","Peru","Afghanistan","Algeria","Bangladesh","Equatorial Guinea","Ireland","Israel","Kenya","Laos","Liberia","Luxembourg","Myanmar","Pakistan","Panama","Romania","Seychelles","Sweden","Tajikistan","Trinidad and Tobago","Tunisia","UK","Yemen","Barbados","Bolivia","Iran","New Zealand","Palau","Colombia","Russia","Tanzania","American Samoa","Costa Rica","Finland","Guinea","Kyrgyzstan","Mauritius","Mongolia","Netherlands","North Macedonia","Norway","Palestine","Paraguay","Saudi Arabia","South Korea","Togo","Tokelau","Turkmenistan","Venezuela","Cameroon","Iraq","Lebanon","Mali","Malta","Moldova","Nauru","Poland","Slovakia","Suriname","The Bahamas","The Gambia","Virgin Islands","Angola","Chad","Cook Islands","Dominica","Estonia","Georgia","Cabo Verde","Congo (Brazzaville)","DR Congo","Eswatini","Fiji","Kuwait","Malaysia","Maldives","Namibia","Saint Vincent and the Grenadines","South Sudan","Sri Lanka","Tuvalu","United Arab Emirates","Zambia","Brazil","Federated States of Micronesia","Libya","Nepal","Spain","Zimbabwe","Bermuda","Burundi","Cambodia","Canada","Chile","Denmark","Germany","Greenland","Guyana","Haiti","Lithuania","Marshall Islands","Mexico","Niue","North Korea","Serbia","Sierra Leone","Slovenia","Timor-Leste")

Rank_DALYs$cause <- factor(Rank_DALYs$cause, levels = rei_Male_order)
Rank_DALYs <- Rank_DALYs[order(Rank_DALYs$rank), ]


Rank_DALYs$location <- factor(Rank_DALYs$location, levels = location_order)
Rank_DALYs <- Rank_DALYs[order(Rank_DALYs$rank), ]

p1 <- ggplot(Rank_DALYs, aes(x = location, y = cause, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), vjust = 1, color = "black") +
  scale_fill_gradient2(low = "#d72e25", mid = "#fee08b", high = "#1a9850", midpoint = mean(Rank_DALYs$rank)) +
  labs(title = "EAPC in cardiovascular diseases rankings from 1990 to 2021", x = "", y = "", fill = "Rank") +
  theme_minimal() +
  theme(axis.title.x = element_text(angle = 0, vjust = 0.5),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Cardiovascular diseases_Rank_EAPC_DALYs_country_1990_2021.pdf",p1,width=30,height=6,dpi=600)


data4 <- merged_data %>%
  filter(age_group%in% c("15-19 years","20-24 years","25-29 years","30-34 years","35-39 years")) %>%
  filter(location%in% c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")) %>%
  filter(metric == "Rate") %>%
  filter(age == "All ages") %>%
  filter(!cause == "Cardiovascular diseases")



Rank_Deaths <- data4 %>%
  filter(measure == "Deaths") %>%
  filter(!cause == "Cardiovascular diseases") %>%
  filter(location == "Global") %>%
  filter(year == "2021") %>%
  filter(sex == "Both")


# 假设数据框名为 Rank_Deaths，删除 cause 列中的 NA 值
Rank_Deaths <- Rank_Deaths[!is.na(Rank_Deaths$cause), ]



Rank_Deaths <- Rank_Deaths %>%
  group_by(age_group) %>%
  mutate(rank = dense_rank(desc(val)))
print(Rank_Deaths)

Sort_Global <- subset(Rank_Deaths, age_group == "15-19 years")
Sort_Global <- Sort_Global[order(Sort_Global$rank), ]
rei_Male_order <- Sort_Global$cause
rei_Male_order <- rev(rei_Male_order)

location_order <- c("15-19 years","20-24 years","25-29 years","30-34 years","35-39 years")

Rank_Deaths$cause <- factor(Rank_Deaths$cause, levels = rei_Male_order)
Rank_Deaths <- Rank_Deaths[order(Rank_Deaths$rank), ]


Rank_Deaths$age_group <- factor(Rank_Deaths$age_group, levels = location_order)
Rank_Deaths <- Rank_Deaths[order(Rank_Deaths$rank), ]


# 假设数据框名为 Rank_Deaths，删除 cause 列中的 NA 值
Rank_Deaths <- Rank_Deaths[!is.na(Rank_Deaths$cause), ]



Rank_Deaths <- Rank_Deaths %>%
  group_by(age_group) %>%
  mutate(rank = dense_rank(desc(val)))
print(Rank_Deaths)

Sort_Global <- subset(Rank_Deaths, age_group == "15-19 years")
Sort_Global <- Sort_Global[order(Sort_Global$rank), ]
rei_Male_order <- Sort_Global$cause
rei_Male_order <- rev(rei_Male_order)

location_order <- c("15-19 years","20-24 years","25-29 years","30-34 years","35-39 years")

Rank_Deaths$cause <- factor(Rank_Deaths$cause, levels = rei_Male_order)
Rank_Deaths <- Rank_Deaths[order(Rank_Deaths$rank), ]


Rank_Deaths$age_group <- factor(Rank_Deaths$age_group, levels = location_order)
Rank_Deaths <- Rank_Deaths[order(Rank_Deaths$rank), ]

p1 <- ggplot(Rank_Deaths, aes(x = age_group, y = cause, fill = rank)) +
  geom_tile(color = "white") +
  geom_text(aes(label = rank), vjust = 1, color = "black") +
  scale_fill_gradient2(low = "#d72e25", mid = "#fee08b", high = "#1a9850", midpoint = mean(Rank_Deaths$rank)) +
  labs(title = "EAPC in cardiovascular diseases rankings from 1990 to 2021", x = "", y = "", fill = "Rank") +
  theme_minimal() +
  theme(axis.title.x = element_text(angle = 0, vjust = 0.5),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Cardiovascular diseases_Rank_ASR_Deaths_age_2021.pdf",p1,width=12,height=6,dpi=600)