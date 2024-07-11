
library(easyGBDR)
library(data.table)
library("ggplot2")


data1 <- GBDread_csv(file = "J:/Manuscript/LPH/Cardiovascular diseases_15_39.csv")

# Frontiers

Frontiers1 <- data1 |>
  filter(cause == "Cardiovascular diseases") |>
  filter(sex == "Both")  |>
  filter(age == "Age-standardized") |>
  filter(age_group == "15-39 years") |>
  filter(metric == "Rate") |>
  filter(year %in% 1990:2021) |>
  filter(measure == "DALYs (Disability-Adjusted Life Years)") |>
  filter(!location %in% c(
    "Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI",
    "World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income",
    "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System",
    "Limited Health System", "Minimal Health System"
  ))





boostrap_DEA_DALYs_10 <-GBDfrontier(data=Frontiers1,
                                    sex_name='Both',
                                    cause_name = 'Cardiovascular diseases',
                                    measure_name='DALYs (Disability-Adjusted Life Years)',
                                    age_name = 'Age-standardized', #age_name 可以选择你想要分析的年龄段，而非
                                    rei_name = NULL,
                                    boot=100,
                                    parallel=T)

table <- GBDfrontier_table(
  frontier_result=boostrap_DEA_DALYs_10, #GBDfrontier 运行结果
  data=Frontiers1, #GBDfrontier 之前的原始数据
  digits = 2,
  sex_name='Both',
  cause_name = 'Cardiovascular diseases',
  measure_name='DALYs (Disability-Adjusted Life Years)',
  age_name = "Age-standardized",
  rei_name = NULL
)

write.csv(table,"Cardiovascular diseases_Frontiers_DALYs.csv",row.names=F)


p1 <- ggfrontier(
  boostrap_DEA_DALYs_10,
  smooth_span = 0.5,
  type = "all years", ## 从 c("all years", "single year") 选择，对应不同的图形
  high_SDI = 0.85, ## 图中展示以 high_SDI 为临界值的国家中 frontier 差异最大的 5 个国家
  low_SDI = 0.5 ## 图中展示以 low_SDI 为临界值的国家中 frontier 差异最小的 5 个国家
)

ggsave("Cardiovascular diseases_Frontiers_DALYs_all.pdf",p1,width=8,height=6,dpi=600)


p2 <- ggfrontier(
  boostrap_DEA_DALYs_10,
  smooth_span = 0.5,
  type = "single year", ## 从 c("all years", "single year") 选择，对应不同的图形
  high_SDI = 0.85, ## 图中展示以 high_SDI 为临界值的国家中 frontier 差异最大的 5 个国家
  low_SDI = 0.5 ## 图中展示以 low_SDI 为临界值的国家中 frontier 差异最小的 5 个国家
)
ggsave("Cardiovascular diseases_Frontiers_DALYs_single.pdf",p2,width=8,height=6,dpi=600)