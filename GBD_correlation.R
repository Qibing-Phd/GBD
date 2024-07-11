
library(easyGBDR)
library("ggplot2")
library("ggsci")
library("ggplot2")
library("gridExtra")


# 相关性

EC <- data1 |>
  filter(cause=="Cardiovascular diseases") |>
  filter(age_group=="15-39 years") |> 
  filter(sex=="Both") |> 
  filter((year %in% 1990:2021))|> 
  filter(!location%in%c("Global","Low SDI","Low-middle SDI","Middle SDI","High-middle SDI","High SDI","High-income Asia Pacific","Central Asia","East Asia","South Asia","Southeast Asia","Australasia","Caribbean","Central Europe","Eastern Europe","Western Europe","Andean Latin America","Central Latin America","Southern Latin America","Tropical Latin America","North Africa and Middle East","High-income North America","Oceania","Central Sub-Saharan Africa","Eastern Sub-Saharan Africa","Southern Sub-Saharan Africa","Western Sub-Saharan Africa" ,"World Bank High Income", "World Bank Low Income", "World Bank Lower Middle Income" , "World Bank Upper Middle Income", "Advanced Health System", "Basic Health System", "Limited Health System", "Minimal Health System")) # 选择地区
colnames(EC)

####  1990 ASIR_EAPC
##获取ASIR
ASIR_1990 <- subset(EC,EC$year==1990 & 
                      EC$age=='Age-standardized' & 
                      EC$metric== 'Rate' &
                      EC$measure=='Incidence') ## 获取1990年EC年龄校正后发病率
ASIR_1990 <- ASIR_1990[,c(2,8)]  ###只取需要的变量
names(ASIR_1990)[2] <- 'ASR'
###获取绝对发病数
Incidence_case_1990 <- subset(EC,EC$year==1990 & 
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_1990 <- Incidence_case_1990[,c(2,8)]  ###只取需要的变量
names(Incidence_case_1990)[2] <- 'case'

#### 计算EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(2,7,8)]

country <- ASIR_1990$location  ###获取国家名称
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

Total <- merge(Incidence_case_1990,EAPC_cal, by='location')
Total <- merge(Total,ASIR_1990, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'  ###指示变量提示该数据为发病率数据

###单独绘制发病图像
library(ggplot2)
Total$group <- factor(Total_incidence$group, levels=c('ASIR'), ordered=TRUE)
p5 <- ggplot(Total, aes(ASR, EAPC, size = case)) +
  geom_point(color = 'orange') +
  geom_smooth(data = Total, aes(ASR, EAPC), se = .8, colour = 'orange', span = 1) +
  scale_size(name = 'Cases in 1990', breaks = c(100, 1000, 10000, 50000),
             labels = c("<500", "500-1,000", "10,000-50,000", ">50,000")) +
  facet_grid(. ~ group, scales = "free") +
  theme_light()
ggsave(filename = "Cardiovascular diseases_Incidence_ASIR_EAPC_1990.pdf", plot = p5, width=8,height=6,dpi=600)

correlation_result <- cor.test(Total_incidence$EAPC, Total_incidence$ASR, method = "pearson")
writeLines(paste("Pearson correlation coefficient: ", correlation_result$estimate), "Cardiovascular diseases_Incidence_ASIR_EAPC_1990.txt")
cat(paste("P-value: ", correlation_result$p.value), file = "Cardiovascular diseases_Incidence_ASIR_EAPC_1990.txt", append = TRUE)
cat(paste("Test statistic: ", correlation_result$statistic), file = "Cardiovascular diseases_Incidence_ASIR_EAPC_1990.txt", append = TRUE)




####  1990 HDI_EAPC
####  1990 ASIR_HDI
### 读取 HDI数据
HDI <- read.csv('J:/Manuscript/data/HDI 1990.csv',header = T)
names(HDI) <- c('location','HDI')


Incidence_case_1990 <- subset(EC,EC$year==1990 & 
                                EC$age=='All ages' & 
                                EC$metric== 'Number' &
                                EC$measure=='Incidence')
Incidence_case_1990 <- Incidence_case_1990[,c(2,8)]  ###只取需要的变量
names(Incidence_case_1990)[2] <- 'case'

#### 计算EAPC
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')

EAPC <- EAPC[,c(2,7,8)]

country <- Incidence_case_1990$location  ###获取国家名称
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=204),UCI=rep(0,times=204),LCI=rep(0,times=204))
for (i in 1:204){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_cal <- EAPC_cal[,c(1,2)]

### 合并三者数据
Total <- merge(Incidence_case_1990,EAPC_cal, by='location')
Total <- merge(Total,HDI, by='location')
Total_incidence <- Total
Total_incidence$group <- 'ASIR'


###单独绘制发病图像
library(ggplot2)
Total$group <- factor(Total_incidence$group, levels=c('ASIR'), ordered=TRUE)
p6 <- ggplot(Total, aes(HDI, EAPC, size = case)) +
  geom_point(color = '#E41A1C') +
  geom_smooth(data = Total, aes(HDI, EAPC), se = .8, colour = '#E41A1C', span = 1) +
  scale_size(name = 'Cases in 1990', breaks = c(100, 1000, 10000, 50000),
             labels = c("<500", "500-1,000", "10,000-50,000", ">50,000")) +
  facet_grid(. ~ group, scales = "free") +
  theme_light()
ggsave(filename = "Cardiovascular diseases_HDI_EAPC_Incidence_1990.pdf", plot = p6, width=8,height=6,dpi=600)

correlation_result <- cor.test(Total_incidence$EAPC, Total_incidence$HDI, method = "pearson")
writeLines(paste("Pearson correlation coefficient: ", correlation_result$estimate), "Cardiovascular diseases_HDI_EAPC_Incidence_1990.txt")
cat(paste("P-value: ", correlation_result$p.value), file = "Cardiovascular diseases_HDI_EAPC_Incidence_1990.txt", append = TRUE)
cat(paste("Test statistic: ", correlation_result$statistic), file = "Cardiovascular diseases_HDI_EAPC_Incidence_1990.txt", append = TRUE)

