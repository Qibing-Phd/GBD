setwd("C:/Manuscript/LPH/Results")

library(easyGBDR)
library(data.table)
GBD_edition(2021)
library(ggplot2)
library(dplyr)
library(ggsci)

AAPC_data4 <- data1 |>
  filter(cause == "Cardiovascular diseases") |>
  filter(age_group == "15-39 years") |>
  filter(age %in% c("All ages", "Age-standardized")) |>
  filter(year %in% 1990:2021) |>
  filter(measure == "Prevalence") |>
  filter(location %in% c(
    "Global", "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI",
    "World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income",
    "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System",
    "Limited Health System", "Minimal Health System"
  ))
colnames(AAPC_data4)

# 添加新列并填充特定值
AAPC_data4 <- AAPC_data4 %>%
  mutate(rei = "All risk factors")

# 查看结果
head(AAPC_data4)



AAPC_factor_Prevalence_result1 <- GBDASR_aapc(data=AAPC_data4,
                                              model="ln",
                                              joinpoints = 5, 
                                              rei_included=T,
                                              CI=TRUE,
                                              digits=2,
                                              sep = ' to ',
                                              constant_variance=F)

AAPC_factor_Prevalence_all<-AAPC_factor_Prevalence_result1$AAPC
write.csv(AAPC_factor_Prevalence_all,"Cardiovascular diseases_AAPC_factor_Prevalence_all.csv",row.names=F)


p1 <- ggjoinpoint_apc(
  data=AAPC_factor_Prevalence_result1,
  location_name="Global",
  measure_name="Prevalence",
  cause_name="Cardiovascular diseases",
  sex_name="Both",
  age_name="Age-standardized",
  rei_name="All risk factors",
  facet_name="Results of AAPC for Prevalence",
  point_color="orange",
  joinpoint_color="#E41A1C",
  line_size=1
)



ggsave("Cardiovascular diseases_AAPC_factor_Prevalence.pdf",p1,width=8,height=6,dpi=600)


p2 <- ggjoinpoint_compare(
  data=AAPC_factor_Prevalence_result1,
  location_name="Global",
  measure_name="Prevalence",
  cause_name="Cardiovascular diseases",
  sex_name=c("Both","Male","Female"),
  age_name="Age-standardized",
  rei_name="All risk factors",
  facet_name="Results of AAPC for Prevalence",
  color_name=pal_lancet("lanonc")(9)[1:3],#color_name个数要和你比较的组数对应
  shape_name=c(14,15,16),#shape_name个数要和你比较的组数对应
  line_size=1
)


ggsave("Cardiovascular diseases_AAPC_factor_Prevalence_sex.pdf",p2,width=8,height=6,dpi=600)



p3 <- ggjoinpoint_compare(
  data=AAPC_factor_Prevalence_result1,
  location_name=c("High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"),
  measure_name="Prevalence",
  cause_name="Cardiovascular diseases",
  sex_name="Both",
  age_name="Age-standardized",
  rei_name="All risk factors",
  facet_name="Results of AAPC for Prevalence",
  color_name=pal_lancet("lanonc")(9)[1:5],#color_name个数要和你比较的组数对应
  shape_name=c(14,15,16,17,18),#shape_name个数要和你比较的组数对应
  line_size=1
)


ggsave("Cardiovascular diseases_AAPC_factor_Prevalence_SDI.pdf",p3,width=8,height=6,dpi=600)


p4 <- ggjoinpoint_compare(
  data=AAPC_factor_Prevalence_result1,
  location_name=c("World Bank High Income","World Bank Upper Middle Income","World Bank Lower Middle Income","World Bank Low Income"),
  measure_name="Prevalence",
  cause_name="Cardiovascular diseases",
  sex_name="Both",
  age_name="Age-standardized",
  rei_name="All risk factors",
  facet_name="Results of AAPC for Prevalence",
  color_name=pal_lancet("lanonc")(9)[1:4],#color_name个数要和你比较的组数对应
  shape_name=c(14,15,16,17),#shape_name个数要和你比较的组数对应
  line_size=1
)


ggsave("Cardiovascular diseases_AAPC_factor_Prevalence_Income.pdf",p4,width=8,height=6,dpi=600)



p5 <- ggjoinpoint_compare(
  data=AAPC_factor_Prevalence_result1,
  location_name=c("Advanced Health System","Basic Health System","Limited Health System","Minimal Health System"),
  measure_name="Prevalence",
  cause_name="Cardiovascular diseases",
  sex_name="Both",
  age_name="Age-standardized",
  rei_name="All risk factors",
  facet_name="Results of AAPC for Prevalence",
  color_name=pal_lancet("lanonc")(9)[1:4],#color_name个数要和你比较的组数对应
  shape_name=c(14,15,16,17),#shape_name个数要和你比较的组数对应
  line_size=1
)


ggsave("Cardiovascular diseases_AAPC_factor_Prevalence_Health.pdf",p5,width=8,height=6,dpi=600)