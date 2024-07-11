setwd("C:/Manuscript/LPH/Results")

library(easyGBDR)
library(data.table)
GBD_edition(2021)
library(ggplot2)
library(dplyr)



#年龄标准化


data1 <- GBDread(folder=T,foldername = "C:/Manuscript/data/Cardiovascular_diseases_all")

unique_variable <- list(measure=unique(data1$measure),
                        location=unique(data1$location),
                        sex=unique(data1$sex),
                        age=unique(data1$age),
                        cause=unique(data1$cause),
                        rei=unique(data1$rei),
                        metric=unique(data1$metric),
                        year=unique(data1$year))
unique_variable

temp1 <- data1 |>
  filter(metric == "Number") |> # 选择Number
  filter(cause == "Cardiovascular diseases") |> 
  filter(age == "15-19 years") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(temp1)

ASR_15_19 <- GBDage_recal(temp1,startage=15,endage=15,CI=T)
ASR_15_19$age_group <- "15-19 years"
write.csv(ASR_15_19, "Cardiovascular diseases_15_19.csv", row.names = FALSE)


temp2 <- data1 |>
  filter(metric == "Number") |> # 选择Number
  filter(cause == "Cardiovascular diseases") |> 
  filter(age == "20-24 years") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(temp2)

ASR_20_24 <- GBDage_recal(temp2,startage=20,endage=20,CI=T)
ASR_20_24$age_group <- "20-24 years"
write.csv(ASR_20_24, "Cardiovascular diseases_20_24.csv", row.names = FALSE)

temp3 <- data1 |>
  filter(metric == "Number") |> # 选择Number
  filter(cause == "Cardiovascular diseases") |> 
  filter(age == "25-29 years") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(temp3)

ASR_25_29 <- GBDage_recal(temp3,startage=25,endage=25,CI=T)
ASR_25_29$age_group <- "25-29 years"
write.csv(ASR_25_29, "Cardiovascular diseases_25_29.csv", row.names = FALSE)



temp4 <- data1 |>
  filter(metric == "Number") |> # 选择Number
  filter(cause == "Cardiovascular diseases") |> 
  filter(age == "30-34 years") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(temp4)

ASR_30_34 <- GBDage_recal(temp4,startage=30,endage=30,CI=T)
ASR_30_34$age_group <- "30-34 years"
write.csv(ASR_30_34, "Cardiovascular diseases_30_34.csv", row.names = FALSE)


temp5 <- data1 |>
  filter(metric == "Number") |> # 选择Number
  filter(cause == "Cardiovascular diseases") |> 
  filter(age == "35-39 years") |> 
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa","World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System"
  )) # 选择地区
colnames(temp5)

ASR_35_39 <- GBDage_recal(temp5,startage=35,endage=35,CI=T)
ASR_35_39$age_group <- "35-39 years"
write.csv(ASR_35_39, "Cardiovascular diseases_35_39.csv", row.names = FALSE)


temp6 <- data1 |>
  filter(metric == "Number") |> # 选择Number
  filter(cause == "Cardiovascular diseases") |> 
  filter(!measure %in% c("YLLs (Years of Life Lost)", "YLDs (Years Lived with Disability)")) |>
  filter(age %in% c("15-19 years","20-24 years", "25-29 years","30-34 years", "35-39 years")) 
colnames(temp6)

ASR_15_39 <- GBDage_recal(temp6,startage=15,endage=35,CI=T)
ASR_15_39$age_group <- "15-39 years"
write.csv(ASR_15_39, "Cardiovascular diseases_15_39.csv", row.names = FALSE)

merged_table <- bind_rows(ASR_15_19, ASR_20_24, ASR_25_29, ASR_30_34, ASR_35_39,ASR_15_39)
write.csv(merged_table, "Cardiovascular diseases_age_15_39.csv", row.names = FALSE)