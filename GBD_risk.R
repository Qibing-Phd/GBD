setwd("C:/Manuscript/LPH/Results")

library(dplyr)
library(ggplot2)
library(RColorBrewer)

data3 <- GBDread_csv(file = "C:/Manuscript/LPH/Results/Cardiovascular diseases_factor_15_39_00_PAF.csv")

data3$val <- data3$val * 100
data3$val <- data3$upper * 100
data3$val <- data3$lower * 100

unique_variable <- list(measure = unique(data3$measure),
                        location = unique(data3$location),
                        sex = unique(data3$sex),
                        age = unique(data3$age),
                        cause = unique(data3$cause),
                        rei = unique(data3$rei),
                        metric = unique(data3$metric),
                        year = unique(data3$year))
unique_variable


# 过滤数据
LC <- data3 %>%
  filter(cause == "Cardiovascular diseases") %>%
  filter(sex == "Male") %>%
  filter(age == "All ages") %>%
  filter(year %in% c("1990", "2021")) %>%
  filter(rei %in% c("Air pollution",
                    "Suboptimal temperature",
                    "Other environmental risks",
                    "Tobacco",
                    "Alcohol use",
                    "Dietary risks",
                    "Low physical activity",
                    "High fasting plasma glucose",
                    "High LDL cholesterol",
                    "High systolic blood pressure",
                    "High body-mass index",
                    "Impaired kidney function")) %>%
  filter(location %in% c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")) # 选择地区

colnames(LC)  


unique_variable <- list(measure = unique(LC$measure),
                        location = unique(LC$location),
                        sex = unique(LC$sex),
                        age = unique(LC$age),
                        cause = unique(LC$cause),
                        rei = unique(LC$rei),
                        metric = unique(LC$metric),
                        year = unique(LC$year))
unique_variable

case_percentage <- subset(LC, LC$sex == "Male" & 
                            LC$metric == 'Percent' &
                            LC$measure == 'DALYs (Disability-Adjusted Life Years)') ## 获取1990年EC年龄校正后发病率



unique_variable <- list(measure = unique(case_percentage$measure),
                        location = unique(case_percentage$location),
                        sex = unique(case_percentage$year),
                        age = unique(case_percentage$age),
                        cause = unique(case_percentage$cause),
                        rei = unique(case_percentage$rei),
                        metric = unique(case_percentage$metric),
                        year = unique(case_percentage$year))
unique_variable

# 先设置好order，后面可以按照这个order排序
rei_order <- c("Air pollution",
               "Suboptimal temperature",
               "Other environmental risks",
               "Tobacco",
               "Alcohol use",
               "Dietary risks",
               "Low physical activity",
               "High fasting plasma glucose",
               "High LDL cholesterol",
               "High systolic blood pressure",
               "High body-mass index",
               "Impaired kidney function")
location_order <- c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")

# 设置factor，排序用
case_percentage$location <- factor(case_percentage$location, levels = location_order)
case_percentage$rei <- factor(case_percentage$rei, levels = rei_order)

# 把女性设置为负值
case_percentage$val[case_percentage$year == "2021"] <- -case_percentage$val[case_percentage$year == "2021"]

# 把location和sex融合, 生成融合标签label
case_percentage$label <- paste0(case_percentage$year, ",", case_percentage$rei)

unique_variable <- list(label = unique(case_percentage$label))
unique_variable

write.csv(case_percentage, "Cardiovascular_diseases_factor_DALYs_Male_1990_2021_PAF.csv", row.names = FALSE)


case_percentage$label <- factor(case_percentage$label,
                                levels = c("1990,Air pollution",
                                           "1990,Suboptimal temperature",
                                           "1990,Other environmental risks",
                                           "1990,Tobacco",
                                           "1990,Alcohol use",
                                           "1990,Dietary risks",
                                           "1990,Low physical activity",
                                           "1990,High fasting plasma glucose",
                                           "1990,High LDL cholesterol",
                                           "1990,High systolic blood pressure",
                                           "1990,High body-mass index",
                                           "1990,Impaired kidney function",
                                           "2021,Air pollution",
                                           "2021,Suboptimal temperature",
                                           "2021,Other environmental risks",
                                           "2021,Tobacco",
                                           "2021,Alcohol use",
                                           "2021,Dietary risks",
                                           "2021,Low physical activity",
                                           "2021,High fasting plasma glucose",
                                           "2021,High LDL cholesterol",
                                           "2021,High systolic blood pressure",
                                           "2021,High body-mass index",
                                           "2021,Impaired kidney function"))

# 生成颜色梯度，确保有足够的颜色
fill_label <- c(brewer.pal(n = 10, name = "Paired"),brewer.pal(n = 10, name = "Paired"))



# 绘图
fig1 <- ggplot() +
  geom_col(data = case_percentage,
           aes(x = location, y = val, fill = label),
           position = "stack",
           alpha = 0.8) +
  scale_y_continuous(limits = c(-100, 100),
                     breaks = c(-100, -50, 0, 50, 100),
                     labels = c("100", "50", "0", "50", "100"),
                     name = "Population Attributable Fraction (%) ") +
  scale_fill_manual(values = fill_label, name = "Year,Risk factors,Population Attributable Fraction (percentage)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "white")

fig1

# 保存图表为PDF格式
ggsave("Cardiovascular_diseases_factor_DALYs_EBM_Male_1990_2021_PAF.pdf", plot = fig1, width = 12, height = 6, dpi = 600)



# 过滤数据
LC <- data3 %>%
  filter(cause == "Cardiovascular diseases") %>%
  filter(sex == "Female") %>%
  filter(year %in% c("1990", "2021")) %>%
  filter(rei %in% c("Environmental/occupational risks", "Behavioral risks", "Metabolic risks")) %>%
  filter(location %in% c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")) # 选择地区

colnames(LC)


unique_variable <- list(measure = unique(LC$measure),
                        location = unique(LC$location),
                        sex = unique(LC$sex),
                        age = unique(LC$age),
                        cause = unique(LC$cause),
                        rei = unique(LC$rei),
                        metric = unique(LC$metric),
                        year = unique(LC$year))
unique_variable

case_percentage <- subset(LC, LC$sex == "Female" & 
                            LC$metric == 'Percent' &
                            LC$measure == 'DALYs (Disability-Adjusted Life Years)') ## 获取1990年EC年龄校正后发病率



unique_variable <- list(measure = unique(case_percentage$measure),
                        location = unique(case_percentage$location),
                        sex = unique(case_percentage$year),
                        age = unique(case_percentage$age),
                        cause = unique(case_percentage$cause),
                        rei = unique(case_percentage$rei),
                        metric = unique(case_percentage$metric),
                        year = unique(case_percentage$year))
unique_variable

# 先设置好order，后面可以按照这个order排序
rei_order <- c("Environmental/occupational risks", "Behavioral risks", "Metabolic risks")
location_order <- c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")

# 设置factor，排序用
case_percentage$location <- factor(case_percentage$location, levels = location_order)
case_percentage$rei <- factor(case_percentage$rei, levels = rei_order)

# 把女性设置为负值
case_percentage$val[case_percentage$year == "2021"] <- -case_percentage$val[case_percentage$year == "2021"]

# 把location和sex融合, 生成融合标签label
case_percentage$label <- paste0(case_percentage$year, ",", case_percentage$rei)

unique_variable <- list(label = unique(case_percentage$label))
unique_variable

write.csv(case_percentage, "Cardiovascular_diseases_factor_Deaths_Female_1990_2021_PAF.csv", row.names = FALSE)


case_percentage$label <- factor(case_percentage$label,
                                levels = c("1990,Environmental/occupational risks",
                                           "1990,Behavioral risks",
                                           "1990,Metabolic risks",
                                           "2021,Environmental/occupational risks",
                                           "2021,Behavioral risks",
                                           "2021,Metabolic risks"))

# 生成颜色梯度，确保有足够的颜色

# 提取 Paired 调色板中的所有颜色
paired_colors <- brewer.pal(n = 12, name = "Paired")

# 选择中间三种颜色
selected_colors <- paired_colors[c(5,4,8)]

# 生成颜色梯度，确保有足够的颜色
fill_label <- c(selected_colors, selected_colors)



# 绘图
fig1 <- ggplot() +
  geom_col(data = case_percentage,
           aes(x = location, y = val, fill = label),
           position = "stack",
           alpha = 0.8) +
  scale_y_continuous(limits = c(-180, 180),
                     breaks = c(-100, -50, 0, 50, 100),
                     labels = c("100", "50", "0", "50", "100"),
                     name = "Population Attributable Fraction (%) ") +
  scale_fill_manual(values = fill_label, name = "Year,Risk factors,Population Attributable Fraction (percentage)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "white")

fig1

# 保存图表为PDF格式
ggsave("Cardiovascular_diseases_factor_Deaths_Female_1990_2021_PAF.pdf", plot = fig1, width = 12, height = 6, dpi = 600)


library(dplyr)
library(ggplot2)
library(RColorBrewer)

data3 <- GBDread_csv(file = "C:/Manuscript/LPH/Results/Cardiovascular diseases_factor_15_39_00_PAF.csv")

data3$val <- data3$val * 100
data3$val <- data3$upper * 100
data3$val <- data3$lower * 100

unique_variable <- list(measure = unique(data3$measure),
                        location = unique(data3$location),
                        sex = unique(data3$sex),
                        age = unique(data3$age),
                        cause = unique(data3$cause),
                        rei = unique(data3$rei),
                        metric = unique(data3$metric),
                        year = unique(data3$year))
unique_variable


# 过滤数据
LC <- data3 %>%
  filter(cause == "Cardiovascular diseases") %>%
  filter(sex == "Male") %>%
  filter(age == "All ages") %>%
  filter(year %in% c("1990", "2021")) %>%
  filter(rei %in% c("Air pollution",
                    "Suboptimal temperature",
                    "Other environmental risks",
                    "Tobacco",
                    "Alcohol use",
                    "Dietary risks",
                    "Low physical activity",
                    "High fasting plasma glucose",
                    "High LDL cholesterol",
                    "High systolic blood pressure",
                    "High body-mass index",
                    "Impaired kidney function")) %>%
  filter(location %in% c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")) # 选择地区

colnames(LC)  


unique_variable <- list(measure = unique(LC$measure),
                        location = unique(LC$location),
                        sex = unique(LC$sex),
                        age = unique(LC$age),
                        cause = unique(LC$cause),
                        rei = unique(LC$rei),
                        metric = unique(LC$metric),
                        year = unique(LC$year))
unique_variable

case_percentage <- subset(LC, LC$sex == "Male" & 
                            LC$metric == 'Percent' &
                            LC$measure == 'DALYs (Disability-Adjusted Life Years)') ## 获取1990年EC年龄校正后发病率



unique_variable <- list(measure = unique(case_percentage$measure),
                        location = unique(case_percentage$location),
                        sex = unique(case_percentage$year),
                        age = unique(case_percentage$age),
                        cause = unique(case_percentage$cause),
                        rei = unique(case_percentage$rei),
                        metric = unique(case_percentage$metric),
                        year = unique(case_percentage$year))
unique_variable

# 先设置好order，后面可以按照这个order排序
rei_order <- c("Air pollution",
               "Suboptimal temperature",
               "Other environmental risks",
               "Tobacco",
               "Alcohol use",
               "Dietary risks",
               "Low physical activity",
               "High fasting plasma glucose",
               "High LDL cholesterol",
               "High systolic blood pressure",
               "High body-mass index",
               "Impaired kidney function")
location_order <- c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "World Bank High Income", "Minimal Health System", "Limited Health System", "Basic Health System", "Advanced Health System","Central Asia","Central Europe","Eastern Europe","Australasia","High-income Asia Pacific","High-income North America","Southern Latin America","Western Europe","Andean Latin America","Caribbean","Central Latin America","Tropical Latin America","North Africa and Middle East","South Asia","East Asia","Oceania","Southeast Asia","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")

# 设置factor，排序用
case_percentage$location <- factor(case_percentage$location, levels = location_order)
case_percentage$rei <- factor(case_percentage$rei, levels = rei_order)

# 把女性设置为负值
case_percentage$val[case_percentage$year == "2021"] <- -case_percentage$val[case_percentage$year == "2021"]

# 把location和sex融合, 生成融合标签label
case_percentage$label <- paste0(case_percentage$year, ",", case_percentage$rei)

unique_variable <- list(label = unique(case_percentage$label))
unique_variable

write.csv(case_percentage, "Cardiovascular_diseases_factor_DALYs_Male_1990_2021_PAF.csv", row.names = FALSE)


case_percentage$label <- factor(case_percentage$label,
                                levels = c("1990,Air pollution",
                                           "1990,Suboptimal temperature",
                                           "1990,Other environmental risks",
                                           "1990,Tobacco",
                                           "1990,Alcohol use",
                                           "1990,Dietary risks",
                                           "1990,Low physical activity",
                                           "1990,High fasting plasma glucose",
                                           "1990,High LDL cholesterol",
                                           "1990,High systolic blood pressure",
                                           "1990,High body-mass index",
                                           "1990,Impaired kidney function",
                                           "2021,Air pollution",
                                           "2021,Suboptimal temperature",
                                           "2021,Other environmental risks",
                                           "2021,Tobacco",
                                           "2021,Alcohol use",
                                           "2021,Dietary risks",
                                           "2021,Low physical activity",
                                           "2021,High fasting plasma glucose",
                                           "2021,High LDL cholesterol",
                                           "2021,High systolic blood pressure",
                                           "2021,High body-mass index",
                                           "2021,Impaired kidney function"))

# 生成颜色梯度，确保有足够的颜色
fill_label <- c(brewer.pal(n = 10, name = "Paired"),brewer.pal(n = 10, name = "Paired"))



# 绘图
fig1 <- ggplot() +
  geom_col(data = case_percentage,
           aes(x = location, y = val, fill = label),
           position = "stack",
           alpha = 0.8) +
  scale_y_continuous(limits = c(-100, 100),
                     breaks = c(-100, -50, 0, 50, 100),
                     labels = c("100", "50", "0", "50", "100"),
                     name = "Population Attributable Fraction (%) ") +
  scale_fill_manual(values = fill_label, name = "Year,Risk factors,Population Attributable Fraction (percentage)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "white")

fig1

# 保存图表为PDF格式
ggsave("Cardiovascular_diseases_factor_DALYs_EBM_Male_1990_2021_PAF.pdf", plot = fig1, width = 12, height = 6, dpi = 600)