
####2023.8.16####

#1.作趋势图####
library(tidyverse)
library(reshape2)

#####1.1数据#####
data <- data.frame(
  year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021,2022),
  HIV = c(2.2, 1.7, 1.4, 1.9, 1.0, 1.1, 0.7, 1.0, 0.4, 0.8, 1.2, 0.5, 0.2),
  Syphilis = c(3.8, 5.0, 4.4, 2.7, 2.0, 3.3, 1.9, 2.0, 0.9, 1.6, 1.1, 0.7, 1.0),
  HCV = c(1.8, 1.1, 0.6, 0.4, 0.6, 0.4, 0.2, 0.5, 0.3, 0.4, 0.4, 0.3, 0.3)
) #df_保留一位小数

data <- data.frame(
  year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021,2022),
  HIV = c(2.27, 1.78, 1.10, 1.26, 0.23, 0.70, 0.76, 1.07, 0.42, 0.83, 1.27, 0.59, 0.29 ),
  Syphilis = c(3.64, 4.56, 3.56, 2.01, 1.56, 2.97, 2.02, 1.99, 1.01, 1.23, 1.02, 0.78, 1.05),
  HCV = c(1.81, 1.14, 0.7, 0.43, 0.64, 0.47, 0.22, 0.59, 0.31, 0.45, 0.44, 0.33, 0.38)
)#df_保留两位小数

#####1.2转换成长格式#####
data_long <- melt(data, id.vars = "year", variable.name = "STDs", value.name = "Prevalence(%)")

#####1.3 Mann-Kendall trend test（M-K趋势检验）#####
library("trend")
mk.test(data$HIV, continuity = TRUE)
mk.test(data$Syphilis, continuity = TRUE)
mk.test(data$HCV, continuity = TRUE) #获取趋势检验结果


#####1.4趋势图#####
p <- ggplot(data_long, aes(x = year, y = `Prevalence(%)`, color = STDs)) +
  geom_line() +
  geom_point() +  
  labs(x = "Year", y = "Prevalence(%)") +
  scale_color_manual(values = c("HIV" = "red", "Syphilis" = "blue", "HCV" = "green")) +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
  scale_y_continuous(breaks = seq(0.5, 5.0, by = 0.5)) +
  theme(
    axis.text.x = element_text(size = 8),  
    panel.grid = element_blank(),  
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"), 
    axis.text = element_text(color = "black"),
    panel.background = element_blank()
  )

#####1.5标注趋势检验结果#####
p + geom_text(aes(x = max(data$year)+2, y = 0.3),
              label = "P for trend =0.058", color = "red", vjust = 1) +
  geom_text(aes(x = max(data$year)+2, y = 0.8),
            label = "P for trend < 0.001", color = "blue", vjust = -1) +
  geom_text(aes(x = max(data$year)+2, y = 0.5),
            label = "P for trend = 0.008", color = "green", vjust = 1) +
  coord_cartesian(clip = 'off')





#2.多因素分析####

#####2.1合并与处理数据#####
HIV <- rbind(df1, df2)
write.csv(HIV,"all_hiv.csv")


#####2.2变量因子化#####
#HIV
variables <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
hiv[variables]<-lapply(hiv[variables],factor)
str(hiv)
#Syphilis
variables <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
syphilis[variables]<-lapply(syphilis[variables],factor)
str(syphilis)
#hcv
variables <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11")
hcv[variables]<-lapply(hcv[variables],factor)
str(hcv)


#####2.3.relogit回归#####
#hiv
model_hiv <- zelig(HIV ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11,
                   data = hiv,
                   model = "relogit",
                   tau = 0.01)
summary(model_hiv)

#syphilis
model_syphilis <- zelig( Syphilis ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11,
                         data = syphilis,
                         model = "relogit",
                         tau = 0.021)
summary(model_syphilis)

#hcv
model_hcv <- zelig( HCV ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13,
                    data = hcv,
                    model = "relogit",
                    tau = 0.006)
summary(model_hcv)




####3.输出relogit模型结果####
library(tidyverse)

#####3.1hiv#####
result_df_hiv <- as.data.frame(Zelig::combine_coef_se(model_hiv)) %>% 
  rename(cols = 1:3) %>% 
  mutate(Estimate = round(cols1,3),
         SE = round(cols2,3), 
         OR = round(exp(cols1),3),
         lower = round(exp(cols1 - 1.96*cols2),3),
         upper = round(exp(cols1 + 1.96*cols2),3),
         P = ifelse(round(cols3,3) < 0.05,"P<0.05",round(cols3,3))) %>% 
  mutate(OR95CI = sprintf("%g(%g,%g)",OR,lower,upper))
write.csv(result_df_hiv, "result_hiv.csv", row.names = FALSE)

#####3.2syphilis#####
result_df_syphilis <- as.data.frame(Zelig::combine_coef_se(model_syphilis)) %>% 
  rename(cols = 1:3) %>% 
  mutate(Estimate = round(cols1,3),
         SE = round(cols2,3), 
         OR = round(exp(cols1),3),
         lower = round(exp(cols1 - 1.96*cols2),3),
         upper = round(exp(cols1 + 1.96*cols2),3),
         P = ifelse(round(cols3,3) < 0.05,"P<0.05",round(cols3,3))) %>% 
  mutate(OR95CI = sprintf("%g(%g,%g)",OR,lower,upper))

#####3.3hcv#####
result_df_hcv <- as.data.frame(Zelig::combine_coef_se(model_hcv)) %>% 
  rename(cols = 1:3) %>% 
  mutate(Estimate = round(cols1,3),
         SE = round(cols2,3), 
         OR = round(exp(cols1),3),
         lower = round(exp(cols1 - 1.96*cols2),3),
         upper = round(exp(cols1 + 1.96*cols2),3),
         P = ifelse(round(cols3,3) < 0.05,"P<0.05",round(cols3,3))) %>% 
  mutate(OR95CI = sprintf("%g(%g,%g)",OR,lower,upper))




####4.PSM匹配####
library(MatchIt)

#####4.1Nearest Neighbor Matching#####
#HIV
matchlist <- matchit(HIV~X1 + X2 + X3 + X4 + X5,
                     data = hiv,
                     method = "nearest",
                     distance = "glm",
                     caliper = 0.01,
                     ratio = 4,
                     replace = F)
summary(matchlist)

#Syphilis
matchlist <- matchit(Syphilis~X1 + X2 + X3 + X4 + X5,
                     data =syphilis,
                     method = "nearest",
                     distance = "glm",
                     caliper = 0.01,
                     ratio = 3,
                     replace = F)
#HCV
matchlist <- matchit(HCV~X1 + X2 + X3 + X4 + X5,
                     data =hcv,
                     method = "nearest",
                     distance = "glm",
                     caliper = 0.01,
                     ratio = 4,
                     replace = F)



#####4.2提取匹配后的数据#####
matchdata <- match.data(matchlist,
                        group = "all",
                        distance = "distance",
                        weights = "weights",
                        subclass = "subclass",
                        data = NULL,
                        include.s.weights = TRUE,
                        drop.unmatched = TRUE)


#####4.3匹配后logistic回归#####
model_hiv <- glm(HIV ~ X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, 
                 data = matchdata, 
                 family = "binomial")
summary(model_hiv)


model_syphilis <- glm(Syphilis ~ X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, 
                      data = matchdata, 
                      family = "binomial")
summary(model_syphilis)


model_hcv <- glm(HCV ~ X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, 
                 data = matchdata, 
                 family = "binomial")
summary(model_hcv)






















#2023.8.24####

####5.画森林图####
library(forestploter)
library(grid)

#####5.1画图准备#####
#让变量缩进一格
dt <- hiv_relogit
dt$Variable <- ifelse(is.na(dt$`Total(N,%)`), 
                      dt$Variable,
                      paste0("   ", dt$Variable))

#把NA变成一个空格
dt$`Total(N,%)` <- ifelse(is.na(dt$`Total(N,%)`), "", dt$`Total(N,%)`)
dt$`Uptake of HIV testing(N,%)` <- ifelse(is.na(dt$`Uptake of HIV testing(N,%)`), "", dt$`Uptake of HIV testing(N,%)`)

#生成绘图区
dt$`          Odds ratio` <- paste(rep(" ", 40), collapse = " ")

#重新生成aOR(95%CI)和P值
dt$'aOR(95%CI)' <- dt$aOR95CI 
dt$`P value ` <- dt$P

#把CI和P值NA变成空格
dt$`aOR(95%CI)` <- ifelse(is.na(dt$`aOR(95%CI)`), "", as.character(dt$`aOR(95%CI)`))
dt$`P value ` <- ifelse(is.na(dt$`P value `),"",as.character(dt$`P value `))

#####5.2绘图#####
#设定模块主题
tm <- forest_theme(base_size = 10,  #文本的大小
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,   #可信区间点的形状
                   ci_col = "blue",    #CI的颜色
                   ci_fill = "blue",     #ci颜色填充
                   ci_alpha = 0.8,        #ci透明度
                   ci_lty = 1,            #CI的线
                   ci_lwd =1.8,          #CI的线宽
                   ci_Theight = 0.2, # Set an T end at the end of CI  ci的高度，默认是NULL
                   
                   refline_lwd = 1,       #参考线
                   refline_lty = "dashed",
                   refline_col = "grey20")

#绘图
p <- forest(dt[,c(1,2,3,11,12,13)],
            est = dt$OR,
            lower = dt$lower,
            upper = dt$upper, 
            sizes = dt$SE,
            ci_column = 4, #绘图区
            ref_line = 1, #参考垂线在x=1处
            xlim = c(-1, 18), 
            ticks_at = c(0, 2, 4, 6, 14),#设置刻度
            theme = tm)
p

#####5.3绘图后美化#####

######5.3.1分别把P小于0.05的变量名标为红色######
g <- edit_plot(p, row = 3, gp = gpar(col = "red", fontface = "italic"))
g
g2 <- edit_plot(g,row = 27,gp = gpar(col = "red", fontface = "italic"))
g2
g3 <- edit_plot(g2,row = 30,gp = gpar(col = "red", fontface = "italic"))
g3


######5.3.2把变量名的文本变成粗体######
g4 <- edit_plot(g3,
                row = c(1,4,7,10,13,16,19,22,25,28,31),
                gp = gpar(fontface = "bold"))
g4


######5.3.3其他（未设置）######
g <- edit_plot(p,
               row = c(3,27,30),
               col = 4,
               which = "ci",
               gp = gpar(col = "red"))#改CI为红色
g

g <- edit_plot(g,
               row = c(6,9,12,15,18,21,24),
               col = 4,
               which = "ci",
               gp = gpar(col = "blue"))#改CI为蓝色
g

g <- insert_text(p,
                 text = "Odds ratio",
                 col=4, row = 0,
                 gp = gpar(fontface = "bold")) #插入文本
g






for (variable in x_variables) {
  freq_table <- all_hiv %>%
    group_by(!!sym(variable)) %>%
    summarise(
      freq_0 = sum(all_hiv$binary_column == 0),
      freq_1 = sum(all_hiv$binary_column == 1),
      percent_0 = (freq_0 / n()) * 100,
      percent_1 = (freq_1 / n()) * 100
    )
  result <- bind_rows(all_hiv, freq_table)
}





panel_forest_plot(
  dt,
  mapping = aes(OR, xmin = lower, xmax = upper),
  panels = default_forest_panels(),
  trans = I,
  funcs = NULL,
  format_options = list(colour = "black", shape = 15, banded = TRUE, text_size = 5,
                        point_size = 5),
  theme = theme_forest(),
  limits = NULL,
  breaks = NULL,
  recalculate_width = TRUE,
  recalculate_height = TRUE,
  exclude_infinite_cis = TRUE
)


















####6.统计患病趋势表格数据####

#####6.1每年患病数和率统计#####
#hiv            
result <- HIV_years %>%group_by(Year) %>%
  summarise(TotalPopulation = n(),
            Cases = sum(HIV == 1),
            Rate = round(Cases / TotalPopulation * 100, 2))

print(result)

#syphilis
result <- Syphilis_years %>%group_by(Year) %>%
  summarise(TotalPopulation = n(),
            Cases = sum(Syphilis == 1),
            Rate = round(Cases / TotalPopulation * 100, 2))

print(result)

#hcv
result <- HCV_years %>%group_by(Year) %>%
  summarise(TotalPopulation = n(),
            Cases = sum(HCV == 1),
            Rate = round(Cases / TotalPopulation * 100, 2))

print(result)



#####6.2计算每个变量0、1频数#####
library(dplyr)
data <- all_hiv_no
result <- data %>%
  summarise_at(vars(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11), 
               list(freq_0 = ~sum(. == 0), 
                    freq_1 = ~sum(. == 1), 
                    prop_0 = ~round(sum(. == 0) / length(.), 4), 
                    prop_1 = ~round(sum(. == 1) / length(.), 4)))
print(result)
