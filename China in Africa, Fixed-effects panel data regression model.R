# China in Africa, Fixed-effects panel data regression model
# 中国在非洲，固定效应面板数据回归模型

## 1. 安装 & 加载需要的包 --------------------------------------
# 下面这两行只在第一次运行时需要，之后可以注释掉
install.packages("readxl")
 install.packages("readr")
 install.packages("dplyr")
 install.packages("plm")
 install.packages("vars")

## 0. 清空环境 ------------------------------------------------
rm(list = ls())
 
# 1. 导入必要的包
library(ggplot2)
library(dplyr)
library(plm)
library(tidyverse)
library(lmtest)
 
# 2. 设置文件路径 --------------------------------------------

FDI_Stock_path   <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/Finance Data/FDI_Stock.xlsx"
democracy_path <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/Democracy Data/democracy-index-polity.csv"
gdp_path       <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/GDP Data/GDP.csv"
resources_path   <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/Resources in Rents/Resource Rents.xlsx"

employment_path <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/Control Variable/Employment in industry.csv"
GDP_per_capita_path  <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/Control Variable/GDP per capita.csv"
Capital_formation_path <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/Control Variable/Gross capital formation.csv"
School_secondary_path  <- "/Users/wuqiaomo/Downloads/China in Africa Dataset/Control Variable/School enrollment, secondary.csv"

# 3. 导入数据 ------------------------------------------

library(readxl)  # 加载 readxl 包


# (1) 官方金融数据（中国对非洲投资）
FDI <- read_excel(FDI_Stock_path)

# (2) 民主数据（Polity / democracy index）
Democracy_Data <- read_csv(democracy_path)

# (3) GDP 数据
gdp_data <- read.csv(gdp_path)

resources_data <- read_excel(resources_path )


employment_data <- read.csv(employment_path)
GDP_per_capita_data <- read.csv(GDP_per_capita_path)
capital_data <- read.csv(Capital_formation_path)
School_data <- read.csv(School_secondary_path)


#整理fdi数据
head(FDI)

head(GDP_per_capita_data)

# 加载tidyverse和countrycode包
library(tidyverse)

install.packages("countrycode")
library(countrycode)

# 假设你的数据框名为FDI，且已经有了"Entity"（国家）和"Year"列
# 转换为长格式
FDI_long <- FDI %>%
  pivot_longer(cols = -`...1`,  # 选择除了第一列（年份）外的所有列
               names_to = "Entity",  # 国家列
               values_to = "FDI")   # FDI数据列

# 重命名第一列为"Year"并重新安排列顺序
FDI_long <- FDI_long %>%
  rename(Year = `...1`) %>%
  select(Year, Entity, FDI)

# 使用countrycode包生成国家代码
FDI_long$Code <- countrycode(FDI_long$Entity, origin = "country.name", destination = "iso3c")

# 查看结果
head(FDI_long)






# 4. 统一变量名称 -------------------------------


# 4.3 GDP 数据：重命名列
gdp_data <- gdp_data %>%
  rename(year = Year, Code = `Country Code`, GDP = GDP..constant.2015.US..)

head(resources_data)
resources_data <- resources_data %>%
  rename( Code = `Country Code`)

head(resources_data)



library(tidyverse)

# 将资源数据转换为长格式
resources_data_long <- resources_data %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),  # 选择所有年份列（假设是从1996年到2024年）
    names_to = "year",   # 变为"year"列
    values_to = "resource_value"  # 资源数据列的名字
  ) %>%
  mutate(year = as.integer(year))  # 确保年份是整数类型

# 查看转换后的数据
head(resources_data_long)


head(employment_data)

# 使用 pivot_longer 将数据从宽格式转换为长格式
employment_data_long <- employment_data %>%
  pivot_longer(
    cols = starts_with("X"),  # 选择所有以 X 开头的列（即年份列）
    names_to = "year",        # 新列 'year' 存储年份
    values_to = "value", 

    # 新列 'value' 存储对应的值
    names_prefix = "X"        # 去除年份列名前的 'X'
  ) %>%
  rename(
    Code =Country.Code,  # 更改列名为 'Country.Name'
    
  )

# 查看转换后的数据
head(employment_data_long)

##School_data <- read.csv(School_secondary_path)

head(GDP_per_capita_data)


# 使用 pivot_longer 将 GDP 数据转换为长格式
GDP_per_capita_data_long <- GDP_per_capita_data %>%
  pivot_longer(
    cols = c("GDP.per.capita..PPP..constant.2021.international..."),  # 选择 GDP 列
    names_to = "indicator",         # 新列 'indicator' 存储 GDP 指标
    values_to = "value",            # 新列 'value' 存储对应的 GDP 值
    names_prefix = "GDP."           # 去除列名前的 "GDP."
  ) %>%
  # 这里已经有 year 列，因此无需再进行操作
  rename(
    Country.Name = Entity,  # 更改列名为 'Country.Name'
    # 更改列名为 'Country.Code'
    Year = Year             # 更改列名为 'Year'
  )

# 查看转换后的数据
head(GDP_per_capita_data_long)


head(capital_data)
# 假设 capital_data 已经导入
# 使用 pivot_longer 将 capital 数据转换为长格式
capital_data_long <- capital_data %>%
  pivot_longer(
    cols = starts_with("X"),  # 选择所有以 "X" 开头的列，即年份列
    names_to = "year",         # 新列 'year' 存储年份信息
    values_to = "value",       # 新列 'value' 存储对应的值
    names_prefix = "X"        # 去掉列名前的 "X"
  ) %>%
  # 重命名为清晰的列名
  rename(
    Country.Name = `Country.Name`,  # 保持 'Country.Name' 列不变
     Code = Country.Code ,  # 保持 'Country.Code' 列不变
    Indicator.Name = `Indicator.Name`,  # 保持 'Indicator.Name' 列不变
    Indicator.Code = `Indicator.Code`  # 保持 'Indicator.Code' 列不变
  )

# 查看转换后的数据
head(capital_data_long)



head(School_data)




# 假设 School_data 已经导入
# 使用 pivot_longer 将 School 数据转换为长格式
School_data_long <- School_data %>%
  pivot_longer(
    cols = starts_with("X"),  # 选择所有以 "X" 开头的列，即年份列
    names_to = "year",         # 新列 'year' 存储年份信息
    values_to = "value",       # 新列 'value' 存储对应的值
    names_prefix = "X"         # 去掉列名前的 "X"
  ) %>%
  # 重命名为清晰的列名
  rename(
    Country.Name = `Country.Name`,  # 保持 'Country.Name' 列不变
     Code = Country.Code,  # 保持 'Country.Code' 列不变
    Indicator.Name = `Indicator.Name`,  # 保持 'Indicator.Name' 列不变
    Indicator.Code = `Indicator.Code`   # 保持 'Indicator.Code' 列不变
  )

# 查看转换后的数据
head(School_data_long)


# 将所有数据集中的 'year' 列转换为字符型
merged_data <- merged_data %>%
  mutate(year = as.character(year))

resources_data_long <- resources_data_long %>%
  mutate(year = as.character(year))

employment_data_long <- employment_data_long %>%
  mutate(year = as.character(year))

GDP_per_capita_data_long <- GDP_per_capita_data_long %>%
  mutate(year = as.character(year))

capital_data_long <- capital_data_long %>%
  mutate(year = as.character(year))

School_data_long <- School_data_long %>%
  mutate(year = as.character(year))

# 检查所有数据集的列名
colnames(merged_data)
colnames(resources_data_long)
colnames(employment_data_long)
colnames(GDP_per_capita_data_long)

GDP_per_capita_data_long <- GDP_per_capita_data_long %>%
  rename(year = Year)  # 将 "Year" 重命名为 "year"

colnames(capital_data_long)
colnames(School_data_long)

library(dplyr)
# 确保所有数据集中的年份列名一致，并且类型一致（统一转换为字符型）
Democracy_Data <- Democracy_Data %>%
  mutate(year = as.integer(year))  # 转换年份列为整数型

resources_data_long <- resources_data_long %>%
  mutate(year = as.integer(year))  # 转换年份列为整数型

employment_data_long <- employment_data_long %>%
  mutate(year = as.integer(year))  # 转换年份列为整数型

capital_data_long <- capital_data_long %>%
  mutate(year = as.integer(year))  # 转换年份列为整数型

School_data_long <- School_data_long %>%
  mutate(year = as.integer(year))  # 转换年份列为整数型



       
FDI_long <- FDI_long %>%
  rename(year = Year)  %>%
  mutate(year = as.integer(year))

head(FDI_long)

# 进行左连接，确保年份列一致
panel_data <- Democracy_Data %>%
  # 合并资源数据
  left_join(resources_data_long, by = c("Code", "year")) %>%
  # 合并就业数据
  left_join(employment_data_long, by = c("Code", "year")) %>%
  # 合并 GDP 数据
  left_join(GDP_per_capita_data_long, by = c("Code", "year")) %>%
  # 合并资本形成数据
  left_join(capital_data_long, by = c("Code", "year")) %>%
  # 合并学校数据
  left_join(School_data_long, by = c("Code", "year")) %>%
  
  left_join(FDI_long, by = c("Code", "year"))
  

# 查看合并后的数据
head(panel_data)

# 使用 na.omit() 删除包含 NA 的行
panel_data <- na.omit(panel_data)

# 查看处理后的数据
head(panel_data)




panel_data <- panel_data %>%
  rename(
    employment_in_industry = `value.x`,        # 将 'value.x' 重命名为 'employment_in_industry'
    gdp_per_capita = `value.y`,                # 将 'value.y' 重命名为 'gdp_per_capita'
    capital_formation = `value.x.x`,           # 将 'value.x.x' 重命名为 'capital_formation'
    school_enrollment = `value.y.y`            # 将 'value.y.y' 重命名为 'school_enrollment'
  )

# 查看数据
head(panel_data)


# Fit the regression model
model <- lm(Democracy ~ FDI + resource_value + employment_in_industry + gdp_per_capita + capital_formation + school_enrollment, data = panel_data)

summary(model)



# 检查 (id, time) 组合的频率
table(panel_data$Code, panel_data$year, useNA = "ifany")

# 前苏丹的数据导致了-inf.

# Load dplyr package
library(dplyr)

# Remove rows where the entity is "Fmr. Sudan"
panel_data_clean <- panel_data %>%
  filter(Entity != "Fmr. Sudan")

# Check the result
head(panel_data_clean)


table(index(panel_data_clean), useNA = "ifany")
# 查看所有重复的 (Code, year) 组合（包括第一次出现的记录）
duplicates <- panel_data_clean[duplicated(panel_data_clean[c("Code", "year")]) | duplicated(panel_data_clean[c("Code", "year")], fromLast = TRUE), ]
head(duplicates)



# 重新创建 pdata.frame
panel_data_plm_clean <- pdata.frame(panel_data_clean, index = c("Code", "year"))

# 拟合固定效应模型（控制国家和年份的固定效应）
model <- plm(Democracy ~ FDI + resource_value + employment_in_industry + 
               gdp_per_capita + capital_formation + school_enrollment, 
             data = panel_data_plm_clean, 
             model = "within",  # 固定效应模型
             effect = "twoways")  # 同时控制国家和年份的固定效应

# 查看模型结果
summary(model)


# 安装并加载所需包
install.packages("plm")
install.packages("AER") # 包含 Arellano-Bond GMM 估计器
library(plm)
library(AER)
install.packages("dplyr")
library(dplyr)

# 计算 gross capital 的中位数
median_gross_capital <- median(panel_data_plm_clean$capital_formation, na.rm = TRUE)

# 根据 gross capital 的中位数进行分组
panel_data_plm_clean$capital_group <- ifelse(panel_data_plm_clean$capital_formation > median_gross_capital, "High", "Low")



# 使用 Arellano-Bond GMM 估计动态面板数据模型
model_gmm <- pgmm(Democracy ~ lag(Democracy, 1) + FDI + resource_value + employment_in_industry + 
                    gdp_per_capita + capital_formation + school_enrollment | 
                     lag(FDI, 2:5) + lag(resource_value, 2:5), 
                  data = panel_data_plm_clean, model = "onestep")

summary(model_gmm)




model_gmm <- pgmm(FDI ~ lag(Democracy, 1) + resource_value+ 
                    gdp_per_capita | 
                    lag(FDI, 2:5) + lag(resource_value, 2:5), 
                  data = panel_data_plm_clean, model = "onestep")


# 查看模型结果
summary(model_gmm)


# 对 High 组（资本形成高于中位数）进行回归分析
high_group_data <- subset(panel_data_plm_clean, capital_group == "High")
model_high <- plm(Democracy ~ FDI + resource_value + employment_in_industry + gdp_per_capita + 
                    capital_formation + school_enrollment, 
                  data = high_group_data, 
                  model = "within", 
                  effect = "twoways")
summary(model_high)

# 对 Low 组（资本形成低于中位数）进行回归分析
low_group_data <- subset(panel_data_plm_clean, capital_group == "Low")
model_low <- plm(Democracy ~ FDI + resource_value + employment_in_industry + gdp_per_capita + 
                   capital_formation + school_enrollment, 
                 data = low_group_data, 
                 model = "within", 
                 effect = "twoways")
summary(model_low)






# 检查异方差性
library(lmtest)
bptest(model)  # 白检（Breusch-Pagan检验）

# 检查自相关性
pbgtest(model)  # 检验自相关性

# 使用稳健标准误拟合固定效应模型
library(sandwich)
model_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
summary(model_robust)


library(car)
vif(model)  # 查看多重共线性












# 检查重复的 (Code, year) 组合
table(panel_data$Code, panel_data$year)

# 加载 dplyr 包
library(dplyr)

# 去除重复的 (Code, year) 组合
panel_data_clean <- panel_data %>%
  distinct(Code, year, .keep_all = TRUE)

# 检查清理后的数据
head(panel_data_clean)

# 将清理后的数据转换为面板数据结构
panel_data_plm <- pdata.frame(panel_data_clean, index = c("Code", "year"))

panel_data_plm$log_FDI[is.na(panel_data_plm$log_FDI)] <- mean(panel_data_plm$log_FDI, na.rm = TRUE)
panel_data_plm$log_resource_value[is.na(panel_data_plm$log_resource_value)] <- mean(panel_data_plm$log_resource_value, na.rm = TRUE)
# 对其他变量进行类似处理

panel_data_clean <- panel_data_plm %>%
  filter(
    is.finite(log_FDI) & 
      is.finite(log_resource_value) & 
      is.finite(log_employment_in_industry) &
      is.finite(log_gdp_per_capita) &
      is.finite(log_capital_formation) &
      is.finite(log_school_enrollment)
  )

# 确认清理后的数据
summary(panel_data_clean)


# 运行Arellano-Bond动态面板数据模型
model_gmm <- pgmm(Democracy ~ log_FDI + log_resource_value + log_employment_in_industry + 
                    log_gdp_per_capita + log_capital_formation + log_school_enrollment | 
                    lag(Democracy, 2:5) + lag(log_FDI, 2:5) + lag(log_resource_value, 2:5),
                  data = panel_data_plm, 
                  model = "onestep", 
                  effect = "twoways",  # 双向固定效应
                  transformation = "ld"  # "ld"为差分形式，常用于Arellano-Bond估计
)

# 输出模型摘要
summary(model_gmm)






model <- lm(Democracy ~ FDI + I(gdp_per_capita^2) + resource_value + employment_in_industry + capital_formation + school_enrollment, data = panel_data)
 
summary(model)


# 假设 panel_data 是你准备好的面板数据集

# 创建 Democracy 的滞后项，假设滞后期为 1 年
panel_data$Democracy_lag <- lag(panel_data$Democracy, 1)

# 重新拟合回归模型，加入滞后项
model_lag <- lm(Democracy_lag~  FDI + resource_value + 
                  employment_in_industry + gdp_per_capita + capital_formation + 
                  school_enrollment, data = panel_data)
summary(model_lag)


# 绘制多个自变量的散点图和回归线
ggplot(panel_data) +
  geom_point(aes(x = FDI, y = Democracy_lag), color = "blue") +
  geom_smooth(aes(x = FDI, y = Democracy_lag), method = "lm", se = FALSE, color = "red") +
  geom_point(aes(x = resource_value, y = Democracy_lag), color = "green") +
  geom_smooth(aes(x = resource_value, y = Democracy_lag), method = "lm", se = FALSE, color = "orange") +
  labs(title = "Scatter Plot with Multiple Regression Lines",
       x = "FDI / Resource Value",
       y = "Democracy_lag") +
  theme_minimal()




# 筛选 resource_value 大于 30 的数据
filtered_data <- panel_data %>%
  filter(resource_value > 30)


# 创建 Democracy 的滞后项，假设滞后期为 1 年
panel_data$Democracy_lag <- lag(panel_data$Democracy, 3)

model <- lm(Democracy_lag ~ FDI + resource_value + employment_in_industry + 
              gdp_per_capita + capital_formation + school_enrollment, data = filtered_data)

summary(model)




model_lag <- lm(Democracy_lag~total_investment + total_investment_lag, data = panel_data)

model_lag <- lm(Democracy_lag~  resource_value + 
                  employment_in_industry + gdp_per_capita + capital_formation + 
                  school_enrollment, data = panel_data)

# 查看回归结果
summary(model_lag)


# 进行GMM回归分析 

model_gmm <- pgmm(Democracy_change ~ Democracy_lag + FDI + resource_value + 
                    employment_in_industry + gdp_per_capita + capital_formation |
                    lag(Democracy_change, 2:5) + lag(total_investment, 2:5) + lag(resource_value, 2:5), 
                  data = panel_data, model = "onestep")

# 显示GMM回归结果
summary(model_gmm)





library(plm)
panel_data <- pdata.frame(panel_data, index = c("Code", "year"))
model <- plm(Democracy ~ total_investment + resource_value + employment_in_industry + gdp_per_capita + capital_formation + school_enrollment, data = panel_data, model = "within")
summary(model)

library(pgmm)
model_gmm <- pgmm(Democracy ~ total_investment + resource_value + employment_in_industry + gdp_per_capita + capital_formation + school_enrollment | 
                    lag(Democracy, 2:5) + lag(total_investment, 2:5), data = panel_data, index = c("country", "year"), model = "onestep")
summary(model_gmm)


# Display the summary of the regression model
summary(model)



# 加载必要的包
library(plm)    # 用于面板数据分析
library(pgmm)   # 用于GMM估计

# 假设 panel_data 是你准备好的面板数据集
# 将数据转换为面板数据格式
panel_data <- pdata.frame(panel_data, index = c("Code", "year"))

# 创建滞后变量（例如，Democracy 的滞后项）
panel_data$Democracy_lag <- lag(panel_data$Democracy, 1)

# 计算民主化变化量（或年度增长率）
panel_data$Democracy_change <- panel_data$Democracy - panel_data$Democracy_lag

cor(panel_data[, c("total_investment", "resource_value", "employment_in_industry", 
                   "gdp_per_capita", "capital_formation", "school_enrollment")])

# 进行GMM回归分析
model_gmm <- pgmm(Democracy_change ~ Democracy_lag + total_investment + resource_value + 
                    employment_in_industry + gdp_per_capita + capital_formation |
                    lag(Democracy_change, 2:5) + lag(total_investment, 2:5) + lag(resource_value, 2:5), 
                  data = panel_data, index = c("country", "year"), model = "onestep")

# 显示GMM回归结果
summary(model_gmm)





# 1. 过滤人均GDP小于20,000的国家
filtered_panel_data <- panel_data %>%
  filter(gdp_per_capita < 20000)

# 2. 创建Democracy的滞后变量
filtered_panel_data <- filtered_panel_data %>%
  group_by(Code) %>%
  arrange(year) %>%
  mutate(democracy_lag = lag(Democracy))  # 创建1期滞后变量

# 3. 对所有变量进行对数变换（除了Democracy，因为Democracy可能有负值）
filtered_panel_data <- filtered_panel_data %>%
  mutate(
    log_total_investment = log(total_investment + 1),  # 加1以避免0值
    log_resource_value = log(resource_value + 1),  # 资源值对数
    log_employment_in_industry = log(employment_in_industry + 1),
    log_gdp_per_capita = log(gdp_per_capita + 1),
    log_capital_formation = log(capital_formation + 1),
    log_school_enrollment = log(school_enrollment + 1)
  )

# 4. 生成plm面板数据格式
pdata <- pdata.frame(filtered_panel_data, index = c("Code", "year"))

# 5. 执行Arellano-Bond GMM估计
# 使用滞后变量和其他控制变量进行回归，指定GMM为动态面板数据模型
model_gmm <- pgmm(
  Democracy ~ lag(Democracy, 1) + log_total_investment + log_resource_value + 
    log_employment_in_industry + log_gdp_per_capita + log_capital_formation + 
    log_school_enrollment | lag(Democracy, 2:99),  # 说明使用滞后变量作为工具变量
  data = pdata,
  effect = "twoways",  # 控制国家和年份固定效应
  model = "onestep",  # 一步GMM估计
  transformation = "d"  # 使用差分转化
)

# 6. 查看回归结果
summary(model_gmm)




model_gmm <- pgmm(
  Democracy ~ lag(Democracy, 1) + log_total_investment + log_resource_value + 
    log_employment_in_industry + log_gdp_per_capita + log_capital_formation + 
    log_school_enrollment | lag(Democracy, 2:99), 
  data = pdata,
  effect = "twoways",  # 控制国家和年份固定效应
  model = "twosteps",  # 两步GMM估计
  transformation = "d",  # 差分转化
  robust = TRUE  # 使用稳健标准误差
)
































# 安装 mgcv 包

library(mgcv)

# 使用 GAM 拟合模型
model_gam <- gam(Democracy ~ s(total_investment) + s(resource_value) + s(employment_in_industry) + 
                   s(gdp_per_capita) + s(capital_formation) + s(school_enrollment), data = panel_data)

summary(model_gam)

#多个因素（如总投资、人均GDP、行业就业等）与民主程度之间有显著的关系


# 安装并加载 lmtest 包
install.packages("lmtest")
library(lmtest)

# 为 panel_data 中的每个变量创建滞后期
panel_data <- panel_data %>%
  arrange(Code, year) %>%
  group_by(Code) %>%
  mutate(
    total_investment_lag1 = lag(total_investment, 1),  # total_investment 滞后期1
    resource_value_lag1 = lag(resource_value, 1),      # resource_value 滞后期1
    employment_in_industry_lag1 = lag(employment_in_industry, 1), # employment_in_industry 滞后期1
    gdp_per_capita_lag1 = lag(gdp_per_capita, 1),      # gdp_per_capita 滞后期1
    capital_formation_lag1 = lag(capital_formation, 1),  # capital_formation 滞后期1
    school_enrollment_lag1 = lag(school_enrollment, 1)   # school_enrollment 滞后期1
  ) 
  

# 然后执行Granger因果检验
# Granger检验：检验 total_investment 是否 Granger 原因 Democracy
granger_test <- grangertest(Democracy ~ total_investment_lag1, order = 1, data = panel_data)

# 查看检验结果
summary(granger_test)


# 对其他变量做类似的检验
granger_test_resource_value <- grangertest(Democracy ~ resource_value_lag1, order = 1, data = panel_data)
granger_test_employment <- grangertest(Democracy ~ employment_in_industry_lag1, order = 1, data = panel_data)
granger_test_gdp <- grangertest(Democracy ~ gdp_per_capita_lag1, order = 1, data = panel_data)
granger_test_capital <- grangertest(Democracy ~ capital_formation_lag1, order = 1, data = panel_data)
granger_test_school <- grangertest(Democracy ~ school_enrollment_lag1, order = 1, data = panel_data)

# 查看结果
summary(granger_test_resource_value)
summary(granger_test_employment)
summary(granger_test_gdp)
summary(granger_test_capital)
summary(granger_test_school)







































model <- lm(Democracy ~ log(total_investment) + log(GDP) + log(resource_value) + I(GDP^2), data = panel_data)

summary(model)

model_with_interactions <- lm(Democracy ~ log(total_investment) * log(resource_value) * log(GDP) +
                                I(total_investment^2) + I(GDP^2) + I(total_investment^3) + I(GDP^3), 
                              data =  panel_data)
summary(model_with_interactions)






summary(step_model)



# 线性回归分析
model <- lm(Democracy ~ total_investment + resource_value + GDP, data = panel_data)

# 输出回归结果


library(ggplot2)

library(lmtest)
grangertest(Democracy ~ total_investment, order = 1, data = panel_data)

grangertest(Democracy ~ GDP, order = 1, data = panel_data)

grangertest(Democracy ~ resource_value, order = 1, data = panel_data)

grangertest(total_investment ~ Democracy, order = 1, data = panel_data)


# 6. 创建滞后变量
panel_data <- panel_data %>%
  arrange(all_recipients, year) %>%
  group_by(all_recipients) %>%
  mutate(
    total_investment_lag1 = lag(total_investment, 1),
    total_investment_lag2 = lag(total_investment, 2),
    GDP_lag1 = lag(GDP, 1),
    GDP_lag2 = lag(GDP, 2)
  ) %>%
  ungroup()

# 删除 NA 行
panel_data <- panel_data %>%
  filter(!is.na(total_investment_lag1) & !is.na(total_investment_lag2) & !is.na(GDP_lag1) & !is.na(GDP_lag2))


# 安装并加载AER包
install.packages("AER")
library(AER)

# 使用资源（resource_value）作为工具变量，预测中国投资（total_investment）
# 依赖于resource_value的第一个阶段回归

# 面板数据回归
library(plm)
model <- plm(Democracy ~ total_investment + resource_value + GDP, data = panel_data, model = "within")
summary(model)



# 输出回归结果
summary(iv_model)








filtered_data <- panel_data %>%
  filter(resource_value > 30)

library(dplyr)



# 检查筛选后的数据
head(filtered_data)


# 线性回归模型：在筛选后的数据上分析
model_filtered <- lm(Democracy ~ total_investment + resource_value + GDP, data = filtered_data)

# 输出回归结果
summary(model_filtered)


library(car)
vif(model_filtered)


model_filtered2 <- lm(Democracy ~ total_investment + resource_value + GDP + I(total_investment^2) + I(GDP^2), data = filtered_data)
summary(model_filtered2)




model_filtered4 <- lm(Democracy ~ total_investment * resource_value + GDP, data = filtered_data)
summary(model_filtered4)


install.packages("randomForest")
library(randomForest)


filtered_data$log_total_investment <- log(filtered_data$total_investment + 1)  # 加 1 防止对数取值为负

#交互项有效果：
model_filtered4 <- lm(Democracy ~ total_investment * resource_value + total_investment * GDP + resource_value * GDP, data = filtered_data)
summary(model_filtered4)





model_filtered5 <- lm(Democracy ~ log(total_investment) + log(resource_value) + log(GDP) + I(total_investment^2) + log(total_investment) * log(resource_value) + I(GDP^2), data = filtered_data)
summary(model_filtered5)

step_model <- step(model_filtered5)
summary(step_model)


# 该模型的 AIC 最小（79.69），说明它是最优的模型。
Democracy ~ log(total_investment) + log(GDP) + log(resource_value) + I(GDP^2)
summary(step_model)


#效果3次会更好
model_nonlinear <- lm(Democracy ~ log(total_investment) + log(resource_value) + log(GDP) + 
                        I(total_investment^2) + I(GDP^2) + I(total_investment^3) + I(GDP^3), 
                      data = filtered_data)
summary(model_nonlinear)






model_with_interactions <- lm(Democracy ~ log(total_investment) * log(resource_value) + 
                                log(total_investment) * log(GDP) + log(resource_value) * log(GDP) +
                         
                                I(total_investment^2) + I(GDP^2) + I(total_investment^3), 
                              data = filtered_data)
summary(model_with_interactions)


step_model <- step(model_with_interactions)
summary(step_model)



# 计算预测值
predicted_values <- predict(model_with_interactions)

# 绘制实际值 vs 预测值的散点图
library(ggplot2)
ggplot(filtered_data, aes(x = Democracy, y = predicted_values)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +  # 完美拟合的45度线
  labs(title = "Actual vs Predicted Democracy Scores", 
       x = "Actual Democracy Score", y = "Predicted Democracy Score") +
  theme_minimal()


# 计算残差
residuals <- filtered_data$Democracy - predicted_values

# 绘制残差图
ggplot(filtered_data, aes(x = predicted_values, y = residuals)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 残差 = 0 的参考线
  labs(title = "Residual Plot", 
       x = "Predicted Democracy Score", y = "Residuals") +
  theme_minimal()








step_model_interactions <- step(model_with_interactions)
summary(step_model_interactions)



model_optimized <- lm(Democracy ~ log(total_investment) + log(resource_value) + log(GDP) + I(GDP^2), data = filtered_data)
summary(model_optimized)




# 可视化 log(total_investment) 与 Democracy 的关系
ggplot(filtered_data, aes(x = log(total_investment), y = Democracy)) +
  geom_point(color = "blue", alpha = 0.6) +  # 绘制散点
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # 添加回归线
  labs(title = "Democracy vs log(total_investment)", 
       x = "log(total_investment)", y = "Democracy") +
  theme_minimal()

# 可视化 log(resource_value) 与 Democracy 的关系
ggplot(filtered_data, aes(x = log(resource_value), y = Democracy)) +
  geom_point(color = "green", alpha = 0.6) +  # 绘制散点
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # 添加回归线
  labs(title = "Democracy vs log(resource_value)", 
       x = "log(resource_value)", y = "Democracy") +
  theme_minimal()


# 可视化 log(GDP) 与 Democracy 的关系
ggplot(filtered_data, aes(x = log(GDP), y = Democracy)) +
  geom_point(color = "purple", alpha = 0.6) +  # 绘制散点
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # 添加回归线
  labs(title = "Democracy vs log(GDP)", 
       x = "log(GDP)", y = "Democracy") +
  theme_minimal()










library(ggplot2)

# 获取回归结果的系数
coef_summary <- summary(model_filtered)$coefficients
coef_df <- data.frame(
  Variable = rownames(coef_summary),
  Estimate = coef_summary[, 1],
  Std.Error = coef_summary[, 2],
  t_value = coef_summary[, 3],
  p_value = coef_summary[, 4]
)

# 计算置信区间
coef_df$CI_lower <- coef_df$Estimate - 1.96 * coef_df$Std.Error
coef_df$CI_upper <- coef_df$Estimate + 1.96 * coef_df$Std.Error

# 绘制回归系数和置信区间
ggplot(coef_df, aes(x = Variable, y = Estimate)) +
  geom_point(size = 4, color = "darkblue") +  # 绘制系数点
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +  # 绘制置信区间
  labs(title = "Regression Coefficients with Confidence Intervals", 
       x = "Variable", y = "Coefficient Estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(filtered_data, aes(x = total_investment, y = Democracy)) +
  geom_point(aes(color = "Total Investment"), alpha = 0.6) +  # 绘制散点
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # 添加回归线
  labs(title = "Relationship Between Total Investment and Democracy Score", 
       x = "Total Investment", y = "Democracy Score") +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(filtered_data, aes(x = resource_value, y = Democracy)) +
  geom_point(aes(color = "Resource Value"), alpha = 0.6) +  # 绘制散点
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # 添加回归线
  labs(title = "Relationship Between Resource Value and Democracy Score", 
       x = "Resource Value", y = "Democracy Score") +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(filtered_data, aes(x = GDP, y = Democracy)) +
  geom_point(aes(color = "GDP"), alpha = 0.6) +  # 绘制散点
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # 添加回归线
  labs(title = "Relationship Between GDP and Democracy Score", 
       x = "GDP", y = "Democracy Score") +
  theme_minimal() +
  theme(legend.position = "none")


# 计算回归模型的残差
filtered_data$residuals <- residuals(model_filtered)

# 绘制残差图
ggplot(filtered_data, aes(x = fitted(model_filtered), y = residuals)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# 预测的民主得分
filtered_data$predicted_Democracy <- predict(model_filtered)





# 使用带边框的标签
ggplot(filtered_data, aes(x = Democracy, y = predicted_Democracy, label = Code)) +
  geom_point(alpha = 0.6, color = "blue") +  # 绘制散点
  geom_abline(slope = 1, intercept = 0, color = "red") +  # 45度线，表示完美拟合
  geom_label(aes(label = Code), size = 3, color = "black", fill = "white") +  # 带边框的标签
  labs(title = "Actual vs Predicted Democracy Scores", 
       x = "Actual Democracy Score", y = "Predicted Democracy Score") +
  theme_minimal()


library(ggplot2)

# 可视化实际得分与预测得分，并为线下和线上添加标签和颜色
ggplot(filtered_data, aes(x = Democracy, y = predicted_Democracy, label = Code)) +
  geom_point(alpha = 0.6, color = "blue") +  # 绘制散点
  geom_abline(slope = 1, intercept = 0, color = "red") +  # 45度线，表示完美拟合
  geom_text(aes(label = Code), vjust = 1, hjust = 1, size = 3, color = "black") +  # 添加标签
  # 为在线上方的区域添加颜色 (more democratic than predicted)
  geom_ribbon(data = subset(filtered_data, Democracy > predicted_Democracy),
              aes(ymin = predicted_Democracy, ymax = Democracy), fill = "royalblue2", alpha = 0.3) +
  # 为线下的区域添加颜色 (less democratic than predicted)
  geom_ribbon(data = subset(filtered_data, Democracy < predicted_Democracy),
              aes(ymin = Democracy, ymax = predicted_Democracy), fill = "orange2", alpha = 0.3) +
  labs(title = "Actual vs Predicted Democracy Scores", 
       x = "Actual Democracy Score", y = "Predicted Democracy Score") +
  theme_minimal()







