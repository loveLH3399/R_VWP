library(readxl)
# Set the file path of your CSV file
file_path <- "/Users/abbypeng/Downloads/results/Extracted data_Time course.csv"

# Read the CSV file into a data frame
data <- read.csv(file_path)

View(data)
summary(data) #得到变量类型-连续型/分类型
# 更改数量单位
data$onset <- data$BIN_INDEX * 20
print(names(data))
# Make a file dataALL that is an R data frame and that only includes the important variables
dataUP <- data.frame(data[,c("RECORDING_SESSION_LABEL","TRIAL_INDEX","onset","AVERAGE_IA_1_SAMPLE_COUNT_.","AVERAGE_IA_2_SAMPLE_COUNT_.","AVERAGE_IA_3_SAMPLE_COUNT_.","AVERAGE_IA_4_SAMPLE_COUNT_.","AVERAGE_IA_0_SAMPLE_COUNT_.","condition")]) #定义新的数据集 #选取要分析的行列
colnames(dataUP) #eye-track提供的数据
summary(dataUP)
View(dataUP)

#Filter only Exp data
data<- dplyr::filter(dataUP,  condition =="training")  
View(dataUP)

# Install and activate GGPLOT
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggeffects")) install.packages("ggeffect")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("ggpubr")) install.packages("ggpubr")
library(ggplot2) # activate package #每次运行都要激活
library(ggeffects)
library(gridExtra)
library(ggpubr)

dataUP $ condition<- as.factor(dataUP $ condition)
library(dplyr)
# 把标签换掉
dataUP <- dataUP %>% 
  mutate(condition = recode(condition, 
                            "A" = "Target",
                            "B" = "English competitor",
                            "C" = "Urdu competitor",
                            "D" = "Unrelated"))
# 显示condition的全部数值，保证不是空集
unique(dataUP$condition)
# Show a smoothed line plot for TargetLooks
ggplot(data=dataUP, aes(y = AVERAGE_IA_2_SAMPLE_COUNT_., x = onset, color = condition, fill = condition)) +
  # lines for proportions
  geom_smooth(span = .4, alpha = .4, method = "gam") +
  # add vertical line
  geom_vline(xintercept = 0, linetype="dotted", color = "darkgrey", linewidth=.75) +
  annotate("text", x=0, y=0.1, label="italic(SEG3_onset)", parse=TRUE, angle=90, colour="gray28", size=3)


# Run linear mixed effects (lme) analysis and provide a nice table
if (!require("lme4")) install.packages("lme4")
if (!require("sjPlot")) install.packages("sjPlot")
library(lme4)
library(sjPlot)
remove.packages(rlang)
install.packages("rlang")

# 转categorical，但好像没啥用
dataUP <- dataUP %>% 
     mutate(category = paste(AVERAGE_IA_1_SAMPLE_COUNT_., AVERAGE_IA_2_SAMPLE_COUNT_., AVERAGE_IA_3_SAMPLE_COUNT_., AVERAGE_IA_4_SAMPLE_COUNT_., AVERAGE_IA_0_SAMPLE_COUNT_.))
# 将新的 category 变量转换为因子类型
# 并列五个model分别求b，比较t检验是否显著
# 百分之95分位数的t值约为2.306
data$category <- as.factor(data$category)
dataUP$onset <- scale(dataUP$onset,scale=FALSE)[,1]
model1 <- lmer(AVERAGE_IA_2_SAMPLE_COUNT_. ~ onset*condition+(1| RECORDING_SESSION_LABEL)+ (1|TRIAL_INDEX), data = dataUP, REML = T,
               contrasts = list(condition = "contr.sum"))
model2 <- lmer(AVERAGE_IA_1_SAMPLE_COUNT_. ~ onset*condition+(1| RECORDING_SESSION_LABEL)+ (1|TRIAL_INDEX), data = dataUP, REML = T,
               contrasts = list(condition = "contr.sum"))
model3 <- lmer(AVERAGE_IA_0_SAMPLE_COUNT_. ~ onset*condition+(1| RECORDING_SESSION_LABEL)+ (1|TRIAL_INDEX), data = dataUP, REML = T,
               contrasts = list(condition = "contr.sum"))
model4 <- lmer(AVERAGE_IA_3_SAMPLE_COUNT_. ~ onset*condition+(1| RECORDING_SESSION_LABEL)+ (1|TRIAL_INDEX), data = dataUP, REML = T,
               contrasts = list(condition = "contr.sum"))
model5 <- lmer(AVERAGE_IA_4_SAMPLE_COUNT_. ~ onset*condition+(1| RECORDING_SESSION_LABEL)+ (1|TRIAL_INDEX), data = dataUP, REML = T,
               contrasts = list(condition = "contr.sum"))

summary(dataUP)
