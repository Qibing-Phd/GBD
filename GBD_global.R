setwd("C:/Manuscript/LPH/Results")

library(easyGBDR)
library(data.table)
GBD_edition(2021)
library(ggplot2)
library(dplyr)



data1 <- read.csv(file = "J:/Manuscript/LPH/Cardiovascular diseases_age_15_39.csv", header = TRUE)

D1_NUM_1990 <- data1 |>
  filter(metric=="Number") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="All ages") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="1990")  |> 
  filter(measure=="Incidence") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_NUM_1990)


D1_NUM_1990 <- D1_NUM_1990[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_NUM_1990$val <- round(D1_NUM_1990$val / 1000,2)  ###取整
D1_NUM_1990$lower <- round(D1_NUM_1990$lower / 1000,2) ###取整
D1_NUM_1990$upper <- round(D1_NUM_1990$upper / 1000,2)  ###取整
D1_NUM_1990$Num_1990 <- paste(D1_NUM_1990$lower,D1_NUM_1990$upper,sep = '-') ## 用-连接95%UI上下数值
D1_NUM_1990$Num_1990 <- paste(D1_NUM_1990$Num_1990,')',sep = '')  ##95%UI前后加括号             
D1_NUM_1990$Num_1990 <- paste('(',D1_NUM_1990$Num_1990,sep = '')  ##95%UI前后加括号
D1_NUM_1990$Num_1990 <- paste(D1_NUM_1990$val,D1_NUM_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

D1_NUM_2021 <- data1 |>
  filter(metric=="Number") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="All ages") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="2021")  |> 
  filter(measure=="Incidence") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_NUM_2021)

D1_NUM_2021 <- D1_NUM_2021[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_NUM_2021$val <- round(D1_NUM_2021$val / 1000,2)   ###取整
D1_NUM_2021$lower <- round(D1_NUM_2021$lower / 1000,2) ###取整
D1_NUM_2021$upper <- round(D1_NUM_2021$upper / 1000,2)  ###取整
D1_NUM_2021$Num_2021 <- paste(D1_NUM_2021$lower,D1_NUM_2021$upper,sep = '-') ## 用-连接95%UI上下数值
D1_NUM_2021$Num_2021 <- paste(D1_NUM_2021$Num_2021,')',sep = '')  ##95%UI前后加括号             
D1_NUM_2021$Num_2021 <- paste('(',D1_NUM_2021$Num_2021,sep = '')  ##95%UI前后加括号
D1_NUM_2021$Num_2021 <- paste(D1_NUM_2021$val,D1_NUM_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

D1_ASR_1990 <- data1 |>
  filter(metric=="Rate") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="Age-standardized") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="1990")  |> 
  filter(measure=="Incidence") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_ASR_1990)

D1_ASR_1990 <- D1_ASR_1990[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_ASR_1990$val <- round(D1_ASR_1990$val,2)  ###取整
D1_ASR_1990$lower <- round(D1_ASR_1990$lower,2)###取整
D1_ASR_1990$upper <- round(D1_ASR_1990$upper,2) ###取整
D1_ASR_1990$ASR_1990 <- paste(D1_ASR_1990$lower,D1_ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
D1_ASR_1990$ASR_1990 <- paste(D1_ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号             
D1_ASR_1990$ASR_1990 <- paste('(',D1_ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
D1_ASR_1990$ASR_1990 <- paste(D1_ASR_1990$val,D1_ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接


D1_ASR_2021 <- data1 |>
  filter(metric=="Rate") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="Age-standardized") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="2021")  |> 
  filter(measure=="Incidence") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_ASR_2021)

D1_ASR_2021 <- D1_ASR_2021[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_ASR_2021$val <- round(D1_ASR_2021$val,2)  ###取整
D1_ASR_2021$lower <- round(D1_ASR_2021$lower,2)###取整
D1_ASR_2021$upper <- round(D1_ASR_2021$upper,2) ###取整
D1_ASR_2021$ASR_2021 <- paste(D1_ASR_2021$lower,D1_ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
D1_ASR_2021$ASR_2021 <- paste(D1_ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号             
D1_ASR_2021$ASR_2021 <- paste('(',D1_ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
D1_ASR_2021$ASR_2021 <- paste(D1_ASR_2021$val,D1_ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接

D1_EAPC_1990_2021 <- GBDeapc(
  data = data1 %>%
    filter(metric == "Rate") %>%
    filter(cause == "Cardiovascular diseases") %>%
    filter(age == "Age-standardized") %>%
    filter(age_group=="15-39 years") %>% 
    filter(sex == "Both") %>%
    filter(between(year, 1990, 2021)) %>%
    filter(measure == "Incidence") %>%
    filter(location %in% c(
      "Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI",
      "High SDI", "High-income Asia Pacific", "Central Asia", "East Asia",
      "South Asia", "Southeast Asia", "Australasia", "Caribbean", "Central Europe",
      "Eastern Europe", "Western Europe", "Andean Latin America", "Central Latin America",
      "Southern Latin America", "Tropical Latin America", "North Africa and Middle East",
      "High-income North America", "Oceania", "Central Sub-Saharan Africa",
      "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa",
      "Western Sub-Saharan Africa", "World Bank High Income", "World Bank Low Income",
      "World Bank Lower Middle Income", "World Bank Upper Middle Income",
      "Advanced Health System", "Basic Health System", "Limited Health System",
      "Minimal Health System"
    )),
  rei = F,
  EAPC_95CI = TRUE,
  digits = 2,
  sep = "-")
head(D1_EAPC_1990_2021)






### 数据提取
D1_NUM_1990 <- D1_NUM_1990[,c(1,5)]
D1_ASR_1990 <- D1_ASR_1990[,c(1,5)]
D1_NUM_2021 <- D1_NUM_2021[,c(1,5)]
D1_ASR_2021 <- D1_ASR_2021[,c(1,5)]
D1_EAPC_1990_2021 <- D1_EAPC_1990_2021[,c(1,9)]
### 数据去重
D1_NUM_1990 <- distinct(D1_NUM_1990, location, .keep_all = TRUE)
print(D1_NUM_1990)
D1_ASR_1990 <- distinct(D1_ASR_1990, location, .keep_all = TRUE)
print(D1_ASR_1990)
D1_NUM_2021 <- distinct(D1_NUM_2021, location, .keep_all = TRUE)
print(D1_NUM_2021)
D1_ASR_2021 <- distinct(D1_ASR_2021, location, .keep_all = TRUE)
print(D1_ASR_2021)
D1_EAPC_1990_2021 <- distinct(D1_EAPC_1990_2021, location, .keep_all = TRUE)
print(D1_EAPC_1990_2021)
### 数据整合
Incidence_region <- merge(D1_NUM_1990,D1_ASR_1990,by='location')
Incidence_region <- merge(Incidence_region,D1_NUM_2021,by='location')
Incidence_region <- merge(Incidence_region,D1_ASR_2021,by='location')
Incidence_region <- merge(Incidence_region,D1_EAPC_1990_2021,by='location')
write.csv(Incidence_region, "Cardiovascular diseases_Incidence_ASR_15_39_region.csv", row.names = FALSE)






D1_NUM_1990 <- data1 |>
  filter(metric=="Number") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="All ages") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="1990")  |> 
  filter(measure=="Deaths") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_NUM_1990)


D1_NUM_1990 <- D1_NUM_1990[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_NUM_1990$val <- round(D1_NUM_1990$val / 1000,2)  ###取整
D1_NUM_1990$lower <- round(D1_NUM_1990$lower / 1000,2) ###取整
D1_NUM_1990$upper <- round(D1_NUM_1990$upper / 1000,2)  ###取整
D1_NUM_1990$Num_1990 <- paste(D1_NUM_1990$lower,D1_NUM_1990$upper,sep = '-') ## 用-连接95%UI上下数值
D1_NUM_1990$Num_1990 <- paste(D1_NUM_1990$Num_1990,')',sep = '')  ##95%UI前后加括号             
D1_NUM_1990$Num_1990 <- paste('(',D1_NUM_1990$Num_1990,sep = '')  ##95%UI前后加括号
D1_NUM_1990$Num_1990 <- paste(D1_NUM_1990$val,D1_NUM_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

D1_NUM_2021 <- data1 |>
  filter(metric=="Number") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="All ages") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="2021")  |> 
  filter(measure=="Deaths") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_NUM_2021)

D1_NUM_2021 <- D1_NUM_2021[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_NUM_2021$val <- round(D1_NUM_2021$val / 1000,2)   ###取整
D1_NUM_2021$lower <- round(D1_NUM_2021$lower / 1000,2) ###取整
D1_NUM_2021$upper <- round(D1_NUM_2021$upper / 1000,2)  ###取整
D1_NUM_2021$Num_2021 <- paste(D1_NUM_2021$lower,D1_NUM_2021$upper,sep = '-') ## 用-连接95%UI上下数值
D1_NUM_2021$Num_2021 <- paste(D1_NUM_2021$Num_2021,')',sep = '')  ##95%UI前后加括号             
D1_NUM_2021$Num_2021 <- paste('(',D1_NUM_2021$Num_2021,sep = '')  ##95%UI前后加括号
D1_NUM_2021$Num_2021 <- paste(D1_NUM_2021$val,D1_NUM_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

D1_ASR_1990 <- data1 |>
  filter(metric=="Rate") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="Age-standardized") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="1990")  |> 
  filter(measure=="Deaths") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_ASR_1990)

D1_ASR_1990 <- D1_ASR_1990[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_ASR_1990$val <- round(D1_ASR_1990$val,2)  ###取整
D1_ASR_1990$lower <- round(D1_ASR_1990$lower,2)###取整
D1_ASR_1990$upper <- round(D1_ASR_1990$upper,2) ###取整
D1_ASR_1990$ASR_1990 <- paste(D1_ASR_1990$lower,D1_ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
D1_ASR_1990$ASR_1990 <- paste(D1_ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号             
D1_ASR_1990$ASR_1990 <- paste('(',D1_ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
D1_ASR_1990$ASR_1990 <- paste(D1_ASR_1990$val,D1_ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接


D1_ASR_2021 <- data1 |>
  filter(metric=="Rate") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="Age-standardized") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="2021")  |> 
  filter(measure=="Deaths") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(D1_ASR_2021)

D1_ASR_2021 <- D1_ASR_2021[,c(2,8,9,10)]  ### 只取需要的变量：地区以及对应的数值
D1_ASR_2021$val <- round(D1_ASR_2021$val,2)  ###取整
D1_ASR_2021$lower <- round(D1_ASR_2021$lower,2)###取整
D1_ASR_2021$upper <- round(D1_ASR_2021$upper,2) ###取整
D1_ASR_2021$ASR_2021 <- paste(D1_ASR_2021$lower,D1_ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
D1_ASR_2021$ASR_2021 <- paste(D1_ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号             
D1_ASR_2021$ASR_2021 <- paste('(',D1_ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
D1_ASR_2021$ASR_2021 <- paste(D1_ASR_2021$val,D1_ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接

D1_EAPC_1990_2021 <- GBDeapc(
  data = data1 %>%
    filter(metric == "Rate") %>%
    filter(cause == "Cardiovascular diseases") %>%
    filter(age == "Age-standardized") %>%
    filter(age_group=="15-39 years") %>% 
    filter(sex == "Both") %>%
    filter(between(year, 1990, 2021)) %>%
    filter(measure == "Deaths") %>%
    filter(location %in% c(
      "Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI",
      "High SDI", "High-income Asia Pacific", "Central Asia", "East Asia",
      "South Asia", "Southeast Asia", "Australasia", "Caribbean", "Central Europe",
      "Eastern Europe", "Western Europe", "Andean Latin America", "Central Latin America",
      "Southern Latin America", "Tropical Latin America", "North Africa and Middle East",
      "High-income North America", "Oceania", "Central Sub-Saharan Africa",
      "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa",
      "Western Sub-Saharan Africa", "World Bank High Income", "World Bank Low Income",
      "World Bank Lower Middle Income", "World Bank Upper Middle Income",
      "Advanced Health System", "Basic Health System", "Limited Health System",
      "Minimal Health System"
    )),
  rei = F,
  EAPC_95CI = TRUE,
  digits = 2,
  sep = "-")
head(D1_EAPC_1990_2021)






### 数据提取
D1_NUM_1990 <- D1_NUM_1990[,c(1,5)]
D1_ASR_1990 <- D1_ASR_1990[,c(1,5)]
D1_NUM_2021 <- D1_NUM_2021[,c(1,5)]
D1_ASR_2021 <- D1_ASR_2021[,c(1,5)]
D1_EAPC_1990_2021 <- D1_EAPC_1990_2021[,c(1,9)]
### 数据去重
D1_NUM_1990 <- distinct(D1_NUM_1990, location, .keep_all = TRUE)
print(D1_NUM_1990)
D1_ASR_1990 <- distinct(D1_ASR_1990, location, .keep_all = TRUE)
print(D1_ASR_1990)
D1_NUM_2021 <- distinct(D1_NUM_2021, location, .keep_all = TRUE)
print(D1_NUM_2021)
D1_ASR_2021 <- distinct(D1_ASR_2021, location, .keep_all = TRUE)
print(D1_ASR_2021)
D1_EAPC_1990_2021 <- distinct(D1_EAPC_1990_2021, location, .keep_all = TRUE)
print(D1_EAPC_1990_2021)
### 数据整合
Deaths_region <- merge(D1_NUM_1990,D1_ASR_1990,by='location')
Deaths_region <- merge(Deaths_region,D1_NUM_2021,by='location')
Deaths_region <- merge(Deaths_region,D1_ASR_2021,by='location')
Deaths_region <- merge(Deaths_region,D1_EAPC_1990_2021,by='location')
write.csv(Deaths_region, "Cardiovascular diseases_Deaths_ASR_15_39_region.csv", row.names = FALSE)