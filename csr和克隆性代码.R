# -*- coding: utf-8 -*-
# @Author: Bi-Cheng Dong
# @Date: 2025-05-20 17:00:21
# @Last Modified by:   bcdon
# @Last Modified time: 2025-05-21 11:07:48
# @description: 生态学杂志数据分析

# 设置工作目录
setwd("D:/我的坚果云/克隆性和csr策略")
getwd()

# 加载必要的包
library(AER) # install.packages("AER")
library(broom)
library(ggpubr)
library(MASS)
library(ordinal) # install.packages("ordinal")
library(patchwork)
library(purrr)
library(tidyverse)

# 导入数据
df00 <- read_csv("012.iplant_ias_clonality_csr.csv", locale = locale(encoding = "GB18030"))
str(df00)

# ==========================================================================
# 1. 克隆性与csr策略对入侵等级的影响
# ==========================================================================
# --------------------------------------------------------------------------
# 数据分析部分
# --------------------------------------------------------------------------
# 提取相关数据, 调整分布数据NA为0
df00 <- df00 %>% mutate(n.prov = ifelse(is.na(n.prov), 0, n.prov))

# 去除缺失数据
df01 <- df00 %>%
filter(!is.na(clonality) & !is.na(c_score) & !is.na(n.prov))
str(df01)

# 将 invasion_level 设置为有序因子
df01$invasion_level <- factor(df01$invasion_level, levels = c(4, 3, 2, 1), ordered = TRUE)
df01$clonality <- factor(df01$clonality, levels = c("non-clonal", "clonal"))

# 检查分类变量的分布
table(df01$invasion_level)

# 使用 clm 进行有序逻辑回归
fit01.ias.level.c <- clm(invasion_level ~ clonality + c_score + clonality:c_score, data = df01, link = "logit")
fit01.ias.level.s <- clm(invasion_level ~ clonality + s_score + clonality:s_score, data = df01, link = "logit")
fit01.ias.level.r <- clm(invasion_level ~ clonality + r_score + clonality:r_score, data = df01, link = "logit")

# 输出模型结果
summary(fit01.ias.level.c)
summary(fit01.ias.level.s)
summary(fit01.ias.level.r)

# 使用 broom::tidy 提取模型结果
results_fit01.ias.level.c <- broom::tidy(fit01.ias.level.c) %>%
mutate(model = "fit01.ias.level.c") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit01.ias.level.s <- broom::tidy(fit01.ias.level.s) %>%
mutate(model = "fit01.ias.level.s") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit01.ias.level.r <- broom::tidy(fit01.ias.level.r) %>%
mutate(model = "fit01.ias.level.r") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

# 合并所有模型结果
results_fit01.ias.level <- bind_rows(results_fit01.ias.level.c,
                                     results_fit01.ias.level.s,
                                     results_fit01.ias.level.r)

write_csv(results_fit01.ias.level, "./results02/results_fit01.ias.level.csv")

# --------------------------------------------------------------------------
# 作图部分
# --------------------------------------------------------------------------
# 转换数据为长格式
df01_long <- df01 %>%
  pivot_longer(cols = c(c_score, s_score, r_score),
               names_to = "score_type",
               values_to = "score_value")

# 自定义分面标题
score_labels <- c("c_score" = "C-score",
                  "r_score" = "R-score",
                  "s_score" = "S-score")

# 使用 facet_wrap 绘图
plot.rank01 <- ggplot(df01_long, aes(x = score_value, y = invasion_level, fill = clonality)) +
                      geom_boxplot(width = 0.5, alpha = 0.7) +
                      labs(
                        x = "Score (%)",
                        y = "Invasion rankings"
                      ) +
                      scale_fill_manual(
                        values = c("non-clonal" = "#E57373", "clonal" = "#64B5F6"),
                        name = "Clonality:",
                        labels = c("No", "Yes")
                      ) +
                      facet_wrap(~score_type, scales = "free_x", labeller = as_labeller(score_labels)) + # 分面显示不同分数类型
                      scale_x_continuous(limits = c(0, 100)) + # 限制 x 轴范围为 0-100
                      theme_bw() +
                      theme(
                        panel.background = element_blank(),  # 更简洁的背景设置
                        panel.grid = element_blank(),  # 去除灰色网格线
                        strip.text = element_text(family = "serif", size = 14),  # 合并 x 和 y 的 strip 文本设置
                        strip.background = element_rect(size = 1, linetype = "solid"),
                        axis.line = element_line(size = 1, linetype = "solid"),
                        axis.ticks = element_line(colour = "black", size = 1),
                        axis.text = element_text(family = "serif", colour = "black", size = 14),
                        axis.title = element_text(family = "serif", colour = "black", size = 14),
                        legend.position = "top",  # 图例位置
                        legend.title = element_text(size = 14, family = "serif"),  # 图例标题样式
                        legend.text = element_text(size = 14, family = "serif")  # 图例文本样式
                      )

# 保存数据
ggexport(plot.rank01,
      filename = "./results02/plot.rank01.png",
      width = 2400,
      height = 1200,
      pointsize = 12,
      res = 300
)

# ==========================================================================
# 2. 克隆性与csr策略对入侵范围的影响
# ==========================================================================
# --------------------------------------------------------------------------
# 数据分析部分
# --------------------------------------------------------------------------

# # 检查过度离散
# fit.ias.dist.c <- glm(n.prov ~ clonal_type + c_score + clonal_type:c_score, data = df, family = poisson(link = "log"))
# fit.ias.dist.s <- glm(n.prov ~ clonal_type + s_score + clonal_type:s_score, data = df, family = poisson(link = "log"))
# fit.ias.dist.r <- glm(n.prov ~ clonal_type + r_score + clonal_type:r_score, data = df, family = poisson(link = "log"))

# dispersiontest(fit.ias.dist.c)
# dispersiontest(fit.ias.dist.s)
# dispersiontest(fit.ias.dist.r)

# 使用广义线性模型（GLM）拟合
fit01.ias.dist.c <- glm(n.prov ~ clonality + c_score + clonality:c_score, data = df01, family = quasipoisson(link = "log"))
fit01.ias.dist.s <- glm(n.prov ~ clonality + s_score + clonality:s_score, data = df01, family = quasipoisson(link = "log"))
fit01.ias.dist.r <- glm(n.prov ~ clonality + r_score + clonality:r_score, data = df01, family = quasipoisson(link = "log"))

# 输出模型结果
summary(fit01.ias.dist.c)
summary(fit01.ias.dist.s)
summary(fit01.ias.dist.r)

# 使用 broom::tidy 提取模型结果
results_fit01.ias.dist.c <- broom::tidy(fit01.ias.dist.c) %>%
mutate(model = "fit01.ias.dist.c") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit01.ias.dist.s <- broom::tidy(fit01.ias.dist.s) %>%
mutate(model = "fit01.ias.dist.s") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit01.ias.dist.r <- broom::tidy(fit01.ias.dist.r) %>%
mutate(model = "fit01.ias.dist.r") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

# 合并所有模型结果
results_fit01.ias.dist <- bind_rows(results_fit01.ias.dist.c,
                                     results_fit01.ias.dist.s,
                                     results_fit01.ias.dist.r)

write_csv(results_fit01.ias.dist, "./results02/results_fit01.ias.dist.csv")

# --------------------------------------------------------------------------
# 作图
# --------------------------------------------------------------------------
# 定义评分变量和类型
score_types <- c("c_score", "s_score", "r_score")

# 使用map拟合模型
fit01.ias.dist <- map(score_types, function(var) {
  formula <- as.formula(paste("n.prov ~ clonality +", var, "+ clonality:", var))
  glm(formula, data = df01, family = quasipoisson(link = "log"))
})

names(fit01.ias.dist) <- score_types

# 打印模型摘要
lapply(fit01.ias.dist, summary)

#
#
# 定义评分值序列
score_seq <- seq(0, 100, length.out = 21)

# 使用map创建数据框和预测结果
pred01_list <- map2(score_types, fit01.ias.dist, function(var_name, model) {

    # 使用expand.grid正确创建新数据框
  var_grid <- setNames(list(score_seq, unique(df01$clonality)), c(var_name, "clonality"))
  new_data <- do.call(expand.grid, var_grid)

  # 预测
  new_data$n.prov <- predict(model, newdata = new_data, type = "response")

  # 添加评分类型并重命名
  new_data$score_type <- var_name
  new_data$score_value <- new_data[[var_name]]
  new_data[[var_name]] <- NULL

  return(new_data)
})

# 合并所有数据框
pred01_long <- bind_rows(pred01_list)
# 按 score_value 排序
pred01_long <- pred01_long %>%
  arrange(score_type, clonality, score_value)

# 查看结果
head(pred01_long)

# 作图
# 转换数据为长格式
df01_long <- df01 %>%
  pivot_longer(cols = c(c_score, s_score, r_score),
               names_to = "score_type",
               values_to = "score_value")

# 自定义分面标题
score_labels <- c("c_score" = "C-score",
                  "r_score" = "R-score",
                  "s_score" = "S-score")

# 使用 facet_wrap 绘图
plot.dist01 <- ggplot(df01_long, aes(x = score_value, y = n.prov, color = clonality)) +
                      geom_point(alpha = 0.7, size = 4) +  # 原始数据散点
                      geom_line(data = pred01_long, aes(x = score_value, y = n.prov), size = 1) +
                      labs(
                        x = "Score (%)",
                        y = "No. provinces"
                      ) +
                      scale_color_manual(
                        values = c("non-clonal" = "#E57373", "clonal" = "#64B5F6"),
                        name = "Clonality:",
                        labels = c("No", "Yes")
                      ) +
                      facet_wrap(~score_type, scales = "free_x", labeller = as_labeller(score_labels)) + # 分面显示不同分数类型
                      scale_x_continuous(limits = c(0, 100)) + # 限制 x 轴范围为 0-100
                      theme_bw() +
                      theme(
                        panel.background = element_blank(),  # 更简洁的背景设置
                        panel.grid = element_blank(),  # 去除灰色网格线
                        strip.text = element_text(family = "serif", size = 14),  # 合并 x 和 y 的 strip 文本设置
                        strip.background = element_rect(size = 1, linetype = "solid"),
                        axis.line = element_line(size = 1, linetype = "solid"),
                        axis.ticks = element_line(colour = "black", size = 1),
                        axis.text = element_text(family = "serif", colour = "black", size = 14),
                        axis.title = element_text(family = "serif", colour = "black", size = 14),
                        legend.position = "top",  # 图例位置
                        legend.title = element_text(size = 14, family = "serif"),  # 图例标题样式
                        legend.text = element_text(size = 14, family = "serif")  # 图例文本样式
                      )

plot.dist01

# 保存数据
ggexport(plot.dist01,
         filename = "./results02/plot.dist01.png",
         width = 2400,
         height = 1200,
         pointsize = 12,
         res = 300
)

# 汇总图
plot01 <- plot.rank01 +
          plot.dist01 +
          plot_layout(ncol = 1) +
          plot_annotation(tag_levels = 'A')

plot01

ggexport(plot01,
         filename = "./results02/plot01.png",
         width = 2400,
         height = 2400,
         pointsize = 12,
         res = 300
)

# ==========================================================================
# 3. 克隆类型与csr策略对入侵等级的影响
# ==========================================================================
# 筛选有效数据，只是保留地上和地下单独克隆器官
df02 <- df00 %>%
filter(clonal_type %in% c("aboveground", "belowground")) %>%
filter(!is.na(clonal_type) & !is.na(c_score) & !is.na(n.prov))

table(df02$clonal_type)

# 将 invasion_level 设置为有序因子
df02$invasion_level <- factor(df02$invasion_level, levels = c(4, 3, 2, 1), ordered = TRUE)
df02$clonality <- factor(df02$clonality, levels = c("non-clonal", "clonal"))
df02$clonal_type <- factor(df02$clonal_type, levels = c("aboveground", "belowground"))

# 模型拟合
fit02.ias.level.c <- clm(invasion_level ~ clonal_type + c_score + clonal_type:c_score, data = df02, link = "logit")
fit02.ias.level.s <- clm(invasion_level ~ clonal_type + s_score + clonal_type:s_score, data = df02, link = "logit")
fit02.ias.level.r <- clm(invasion_level ~ clonal_type + r_score + clonal_type:r_score, data = df02, link = "logit")

# 输出模型结果
summary(fit02.ias.level.c)
summary(fit02.ias.level.s)
summary(fit02.ias.level.r)

# 使用 broom::tidy 提取模型结果
results_fit02.ias.level.c <- broom::tidy(fit02.ias.level.c) %>%
mutate(model = "fit02.ias.level.c") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit02.ias.level.s <- broom::tidy(fit02.ias.level.s) %>%
mutate(model = "fit02.ias.level.s") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit02.ias.level.r <- broom::tidy(fit02.ias.level.r) %>%
mutate(model = "fit02.ias.level.r") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

# 合并所有模型结果
results_fit02.ias.level <- bind_rows(results_fit02.ias.level.c,
                                     results_fit02.ias.level.s,
                                     results_fit02.ias.level.r)

write_csv(results_fit02.ias.level, "./results02/results_fit02.ias.level.csv")

# 作图
# 转换数据为长格式
df02_long <- df02 %>%
  pivot_longer(cols = c(c_score, s_score, r_score),
               names_to = "score_type",
               values_to = "score_value")

# 自定义分面标题
score_labels <- c("c_score" = "C-score",
                  "r_score" = "R-score",
                  "s_score" = "S-score")

# 使用 facet_wrap 绘图
plot.rank02 <- ggplot(df02_long, aes(x = score_value, y = invasion_level, fill = clonal_type)) +
                      geom_boxplot(width = 0.5, alpha = 0.7) +
                      labs(
                        x = "Score (%)",
                        y = "Invasion rankings"
                      ) +
                      scale_fill_manual(
                        values = c("aboveground" = "#E57373", "belowground" = "#64B5F6"),
                        name = "Clonal type:",
                        labels = c("Aboveground", "Belowground")
                      ) +
                      facet_wrap(~score_type, scales = "free_x", labeller = as_labeller(score_labels)) + # 分面显示不同分数类型
                      scale_x_continuous(limits = c(0, 100)) + # 限制 x 轴范围为 0-100
                      theme_bw() +
                      theme(
                        panel.background = element_blank(),  # 更简洁的背景设置
                        panel.grid = element_blank(),  # 去除灰色网格线
                        strip.text = element_text(family = "serif", size = 14),  # 合并 x 和 y 的 strip 文本设置
                        strip.background = element_rect(size = 1, linetype = "solid"),
                        axis.line = element_line(size = 1, linetype = "solid"),
                        axis.ticks = element_line(colour = "black", size = 1),
                        axis.text = element_text(family = "serif", colour = "black", size = 14),
                        axis.title = element_text(family = "serif", colour = "black", size = 14),
                        legend.position = "top",  # 图例位置
                        legend.title = element_text(size = 14, family = "serif"),  # 图例标题样式
                        legend.text = element_text(size = 14, family = "serif")  # 图例文本样式
                      )

# 保存数据
ggexport(plot.rank02,
      filename = "./results02/plot.rank02.png",
      width = 2400,
      height = 1200,
      pointsize = 12,
      res = 300
)

# ==========================================================================
# 4. 克隆类型与csr策略对入侵范围的影响
# ==========================================================================
# # 检查过度离散
# fit02.ias.dist.c <- glm(n.prov ~ clonal_type + c_score + clonal_type:c_score, data = df02, family = poisson(link = "log"))
# fit02.ias.dist.s <- glm(n.prov ~ clonal_type + s_score + clonal_type:s_score, data = df02, family = poisson(link = "log"))
# fit02.ias.dist.r <- glm(n.prov ~ clonal_type + r_score + clonal_type:r_score, data = df02, family = poisson(link = "log"))

# dispersiontest(fit02.ias.dist.c)
# dispersiontest(fit02.ias.dist.s)
# dispersiontest(fit02.ias.dist.r)

# 模型拟合
fit02.ias.dist.c <- glm(n.prov ~ clonal_type + c_score + clonal_type:c_score, data = df02, family = quasipoisson(link = "log"))
fit02.ias.dist.s <- glm(n.prov ~ clonal_type + s_score + clonal_type:s_score, data = df02, family = quasipoisson(link = "log"))
fit02.ias.dist.r <- glm(n.prov ~ clonal_type + r_score + clonal_type:r_score, data = df02, family = quasipoisson(link = "log"))

# 输出模型结果
summary(fit02.ias.dist.c)
summary(fit02.ias.dist.s)
summary(fit02.ias.dist.r)


# 使用 broom::tidy 提取模型结果
results_fit02.ias.dist.c <- broom::tidy(fit02.ias.dist.c) %>%
mutate(model = "fit02.ias.dist.c") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit02.ias.dist.s <- broom::tidy(fit02.ias.dist.s) %>%
mutate(model = "fit02.ias.dist.s") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

results_fit02.ias.dist.r <- broom::tidy(fit02.ias.dist.r) %>%
mutate(model = "fit02.ias.dist.r") %>%
mutate_if(is.numeric, round, 3) # 将所有数值列保留 3 位小数

# 合并所有模型结果
results_fit02.ias.dist <- bind_rows(results_fit02.ias.dist.c,
                                     results_fit02.ias.dist.s,
                                     results_fit02.ias.dist.r)

write_csv(results_fit02.ias.dist, "./results02/results_fit02.ias.dist.csv")

# 作图
# 定义评分变量和类型
score_types <- c("c_score", "s_score", "r_score")

# 使用map拟合模型
fit02.ias.dist <- map(score_types, function(var) {
  formula <- as.formula(paste("n.prov ~ clonal_type +", var, "+ clonal_type:", var))
  glm(formula, data = df02, family = quasipoisson(link = "log"))
})

names(fit02.ias.dist) <- score_types

# 打印模型摘要
lapply(fit02.ias.dist, summary)

# 定义评分值序列
score_seq <- seq(0, 100, length.out = 21)

# ==========================================================================
# 批量获取数据
# ==========================================================================
# 使用map创建数据框和预测结果
pred02_list <- map2(score_types, fit02.ias.dist, function(var_name, model) {

    # 使用expand.grid正确创建新数据框
  var_grid <- setNames(list(score_seq, unique(df02$clonal_type)), c(var_name, "clonal_type"))
  new_data <- do.call(expand.grid, var_grid)

  # 预测
  new_data$n.prov <- predict(model, newdata = new_data, type = "response")

  # 添加评分类型并重命名
  new_data$score_type <- var_name
  new_data$score_value <- new_data[[var_name]]
  new_data[[var_name]] <- NULL

  return(new_data)
})

# 合并所有数据框
pred02_long <- bind_rows(pred02_list)
# 按 score_value 排序
pred02_long <- pred02_long %>%
  arrange(score_type, clonal_type, score_value)

# 查看结果
head(pred02_long)

# 作图
# 转换数据为长格式
df02_long <- df02 %>%
  pivot_longer(cols = c(c_score, s_score, r_score),
               names_to = "score_type",
               values_to = "score_value")

# 自定义分面标题
score_labels <- c("c_score" = "C-score",
                  "r_score" = "R-score",
                  "s_score" = "S-score")

# 使用 facet_wrap 绘图
plot.dist02 <- ggplot(df02_long, aes(x = score_value, y = n.prov, color = clonal_type)) +
                      geom_point(alpha = 0.7, size = 4) +  # 原始数据散点
                      geom_line(data = pred02_long, aes(x = score_value, y = n.prov), size = 1) +
                      labs(
                        x = "Score (%)",
                        y = "No. provinces"
                      ) +
                      scale_color_manual(
                        values = c("aboveground" = "#E57373", "belowground" = "#64B5F6"),
                        name = "Clonal type:",
                        labels = c("Aboveground", "Belowground")
                      ) +
                      facet_wrap(~score_type, scales = "free_x", labeller = as_labeller(score_labels)) + # 分面显示不同分数类型
                      scale_x_continuous(limits = c(0, 100)) + # 限制 x 轴范围为 0-100
                      theme_bw() +
                      theme(
                        panel.background = element_blank(),  # 更简洁的背景设置
                        panel.grid = element_blank(),  # 去除灰色网格线
                        strip.text = element_text(family = "serif", size = 14),  # 合并 x 和 y 的 strip 文本设置
                        strip.background = element_rect(size = 1, linetype = "solid"),
                        axis.line = element_line(size = 1, linetype = "solid"),
                        axis.ticks = element_line(colour = "black", size = 1),
                        axis.text = element_text(family = "serif", colour = "black", size = 14),
                        axis.title = element_text(family = "serif", colour = "black", size = 14),
                        legend.position = "top",  # 图例位置
                        legend.title = element_text(size = 14, family = "serif"),  # 图例标题样式
                        legend.text = element_text(size = 14, family = "serif")  # 图例文本样式
                      )

print(plot.dist02)

# 保存数据
ggexport(plot.dist02,
      filename = "./results02/plot.dist02.png",
      width = 2400,
      height = 1200,
      pointsize = 12,
      res = 300
)


# 汇总图
plot02 <- plot.rank02 +
          plot.dist02 +
          plot_layout(ncol = 1) +
          plot_annotation(tag_levels = 'A')

plot02

ggexport(plot02,
         filename = "./results02/plot02.png",
         width = 2400,
         height = 2400,
         pointsize = 12,
         res = 300
)
