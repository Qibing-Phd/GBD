
#分解分析

library(easyGBDR)
library(data.table)
library("ggplot2")


EC <- subset(data1, age != "Age-standardized")
EC <- subset(EC, select = -age)
names(EC)[names(EC) == "age_group"] <- "age"
EC <- EC |>
  filter(age%in%c("15-19 years","20-24 years", "25-29 years","30-34 years", "35-39 years")) 

EC$age <- sub("15-19 years", "15 to 19", EC$age)
EC$age <- sub("20-24 years", "20 to 24", EC$age)
EC$age <- sub("25-29 years", "25 to 29", EC$age)
EC$age <- sub("30-34 years", "30 to 34", EC$age)
EC$age <- sub("35-39 years", "35 to 39", EC$age)


unique_variable <- list(measure=unique(EC$measure),
                        location=unique(EC$location),
                        sex=unique(EC$sex),
                        age=unique(EC$age),
                        cause=unique(EC$cause),
                        rei=unique(EC$rei),
                        metric=unique(EC$metric),
                        year=unique(EC$year))
unique_variable

EC_Deaths <- EC |>
  filter(location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income","World Bank Lower Middle Income","World Bank Upper Middle Income","World Bank High Income","Minimal Health System","Limited Health System","Basic Health System","Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")) |>
  filter(measure=="Deaths") |>
  filter(!age%in%c("All ages","Age-standardized"))

decomposition_result_Deaths <- GBDdecomposition(
  EC_Deaths,
  byear = 1990,
  compareyear = 2021,
  startage = 15,
  endage = 35,
  percent = "byear")
unique(EC_Deaths$age)

plot_Deaths <-ggdecomposition(
  data=decomposition_result_Deaths,# 注意这里是模型结果，不是表格 或者是decomp_result_HGQ
  measure_name="Deaths",
  location_name=c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income","World Bank Lower Middle Income","World Bank Upper Middle Income","World Bank High Income","Minimal Health System","Limited Health System","Basic Health System","Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa"),
  sex_name="Both",
  rei_name = NULL,
  cause_name= "Cardiovascular diseases",
  percent = T # 是否绘制百分比图，需要注意，这里的百分比图需要分解分析中的percent='byear'，否则会报错
) 

p1 <- plot_Deaths +
  scale_fill_manual(values = c("#E3002D", "#00A0E3", "#00B700")) +
  scale_color_manual(values = c("#E3002D", "#00A0E3", "#00B700"))

ggsave("Cardiovascular diseases_Deaths_decomposition.pdf",p1,width=8,height=6,dpi=600)