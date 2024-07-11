library(easyGBDR)
library(data.table)
library(INLA)
library(dplyr)



data <- data |>
  filter(cause=="Cardiovascular diseases") |> 
  filter(age%in%c ("15-19 years","20-24 years","25-29 years","30-34 years","35-39 years")) |> 
  filter(!year%in%c ("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991")) |> 
  filter(!location%in%c("High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa")) # 选择地区
colnames(data)

bapc_result <- GBDbapc_prediction(
  data=data,
  measure_name = c("Incidence",'Deaths','DALYs (Disability-Adjusted Life Years)','Prevalence'),
  cause_name = "Cardiovascular diseases",
  location_name = "Global",
  rei_name = NULL,
  By_sex = T,
  predyear = 2050,
  full_age_adjusted = F,
  rate_lessen = NULL
)



bapc_result <- GBDnorpred_prediction(
  data=data,
  measure_name = c("Incidence",'Deaths','DALYs (Disability-Adjusted Life Years)','Prevalence'),
  cause_name = "Cardiovascular diseases",
  location_name = "Global",
  rei_name = NULL,
  By_sex = T,
  predyear = 2050,
  full_age_adjusted = F
)

write.csv(bapc_result,"Cardiovascular diseases_norpred.csv",row.names=F)


bapc_result1 <- ggprediction_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  CI = T, ## 是否画可信区间
  group_name = "location", ## 分组变量
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "Deaths", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female') ## 进行sex_name 筛选后进行展示
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result1_Deaths.pdf", plot = bapc_result1, width = 14, height = 10, units = "in")


bapc_result1 <- ggprediction_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  CI = T, ## 是否画可信区间
  group_name = "location", ## 分组变量
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "Incidence", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female') ## 进行sex_name 筛选后进行展示
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result1_Incidence.pdf", plot = bapc_result1, width = 14, height = 10, units = "in")


bapc_result1 <- ggprediction_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  CI = T, ## 是否画可信区间
  group_name = "location", ## 分组变量
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "DALYs (Disability-Adjusted Life Years)", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female') ## 进行sex_name 筛选后进行展示
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result1_DALYs.pdf", plot = bapc_result1, width = 14, height = 10, units = "in")


bapc_result1 <- ggprediction_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  CI = T, ## 是否画可信区间
  group_name = "location", ## 分组变量
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "Prevalence", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female') ## 进行sex_name 筛选后进行展示
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result1_Prevalence.pdf", plot = bapc_result1, width = 14, height = 10, units = "in")



bapc_result2 <- ggprediction_age_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  group_name = "age", ## 分组变量
  CI = T, ## 是否画可信区间
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "Deaths", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female'), ## 进行sex_name 筛选后进行展示
  age_name = c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44")
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result2_Deaths.pdf", plot = bapc_result2, width = 14, height = 10, units = "in")


bapc_result2 <- ggprediction_age_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  group_name = "age", ## 分组变量
  CI = T, ## 是否画可信区间
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "Incidence", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female'), ## 进行sex_name 筛选后进行展示
  age_name = c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44")
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result2_Incidence.pdf", plot = bapc_result2, width = 14, height = 10, units = "in")

bapc_result2 <- ggprediction_age_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  group_name = "age", ## 分组变量
  CI = T, ## 是否画可信区间
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "DALYs (Disability-Adjusted Life Years)", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female'), ## 进行sex_name 筛选后进行展示
  age_name = c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44")
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result2_DALYs.pdf", plot = bapc_result2, width = 14, height = 10, units = "in")



bapc_result2 <- ggprediction_age_Dx(
  data=bapc_result, ## GBDnorpred_prediction 或GBDbapc_prediction 运行结果
  predict_start = 2022, ## 对开始预测的年份画垂直线
  group_name = "age", ## 分组变量
  CI = T, ## 是否画可信区间
  ratio = 'auto', ## 双坐标轴自动计算，也可以根据你的情况填入数字（两个坐标轴的比值）
  location_name = "Global", ## 进行location_name 筛选后进行展示
  measure_name = "Prevalence", ## 进行measure_name 筛选后进行展示
  cause_name = "Cardiovascular diseases", ## 进行cause_name 筛选后进行展示
  rei_name = NULL, ## 进行rei_name(若无填NULL) 筛选后进行展示
  sex_name = c("Both",'Male','Female'), ## 进行sex_name 筛选后进行展示
  age_name = c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44")
) + facet_wrap(.~sex) ## 由于我们的sex_name 和location_name 为长度大于1，location_name 为group，
ggsave("Cardiovascular diseases bapc result2_Prevalence.pdf", plot = bapc_result2, width = 14, height = 10, units = "in")


