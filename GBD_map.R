#GBD 地图

library(easyGBDR)
library("ggplot2")
library("ggsci")
library("ggplot2")
library("gridExtra")


D1_ASR_1990 <- data1 |>
  filter(metric=="Rate") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="Age-standardized") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="1990")  |> 
  filter(measure=="Incidence") |> 
  filter(!location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa" ,"World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System")) # 选择地区
colnames(D1_ASR_1990)

D1_ASR_1990 <- distinct(D1_ASR_1990, location, .keep_all = TRUE)
print(D1_ASR_1990)

D1_ASR_1990$val2 <- Quantile(D1_ASR_1990$val,n=5)
plot <- ggGBDmap(D1_ASR_1990,variable = 'val2',color = 'scale_color_lancet() + scale_fill_lancet()', guide_name = 'ASR for 1990(/10^5)')
ggsave("Cardiovascular diseases_Incidence_ASR_1990_map.pdf",plot,width=8,height=6,dpi=600)

D1_ASR_2021 <- data1 |>
  filter(metric=="Rate") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="Age-standardized") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="2021")  |> 
  filter(measure=="Incidence") |> 
  filter(!location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa" ,"World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System")) # 选择地区
colnames(D1_ASR_2021)
D1_ASR_2021 <- distinct(D1_ASR_2021, location, .keep_all = TRUE)
print(D1_ASR_2021)

D1_ASR_2021$val2 <- Quantile(D1_ASR_2021$val,n=5)
plot <- ggGBDmap(D1_ASR_2021,variable = 'val2',color = 'scale_color_lancet() + scale_fill_lancet()', guide_name = 'ASR for 2021(/10^5)')
ggsave("Cardiovascular diseases_Incidence_ASR_2021_map.pdf",plot,width=8,height=6,dpi=600)


D1_EAPC_1990_2021 <- GBDeapc(
  data = data1 %>%
    filter(metric == "Rate") %>%
    filter(cause == "Cardiovascular diseases") %>%
    filter(age == "Age-standardized") %>%
    filter(age_group=="15-39 years") %>% 
    filter(sex == "Both") %>%
    filter(between(year, 1990, 2021)) %>%
    filter(measure == "Incidence") %>%
    filter(!location %in% c(
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
  rei = FALSE,
  EAPC_95CI = TRUE,
  digits = 2,
  sep = " to "
)


D1_EAPC_1990_2021 <- distinct(D1_EAPC_1990_2021, location, .keep_all = TRUE)
print(D1_EAPC_1990_2021)

D1_EAPC_1990_2021$val2 <- Quantile(D1_EAPC_1990_2021$EAPC,n=5)
plot <- ggGBDmap(D1_EAPC_1990_2021,variable = 'val2',color = 'scale_color_lancet() + scale_fill_lancet()', guide_name = ' EAPC')
ggsave("Cardiovascular diseases_Incidence_EAPC_1990_2021_map.pdf",plot,width=8,height=6,dpi=600)

case_2021 <- data1 |>
  filter(metric=="Number") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="All ages") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="2021") |> 
  filter(measure=="Incidence") |> 
  filter(!location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa" ,"World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System")) # 选择地区
colnames(case_2021)

case_1990 <- data1 |>
  filter(metric=="Number") |> # 选择Number
  filter(cause=="Cardiovascular diseases") |> 
  filter(age=="All ages") |> 
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter(year=="1990") |> 
  filter(measure=="Incidence") |> 
  filter(!location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa" ,"World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System")) # 选择地区
colnames(case_1990)

case_1990 <- case_1990[,c(2,8)]
case_2021 <- case_2021[,c(2,8)]

names(case_1990) <- c('location','case_1990')
names(case_2021) <- c('location','case_2021')

D1_CAN_1990_2021 <- merge(case_1990, case_2021, by='location')

D1_CAN_1990_2021$val <- (D1_CAN_1990_2021$case_2021-D1_CAN_1990_2021$case_1990)/D1_CAN_1990_2021$case_1990*100  ### »ñÈ¡ÎÒÃÇµÄ½á¹û

D1_CAN_1990_2021 <- distinct(D1_CAN_1990_2021, location, .keep_all = TRUE)
print(D1_CAN_1990_2021)

D1_CAN_1990_2021$val2 <- Quantile(D1_CAN_1990_2021$val,n=5)
plot <- ggGBDmap(D1_CAN_1990_2021,variable = 'val2',color = 'scale_color_lancet() + scale_fill_lancet()', guide_name = ' Change absolute number(%)')
ggsave("Cardiovascular diseases_Incidence_CAN_1990_2021_map.pdf",plot,width=8,height=6,dpi=600)