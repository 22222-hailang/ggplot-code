
library(rms)
library(ggplot2)
# 1. 预测（你的原代码）
pred <- Predict(rcs_model, AIP, fun = exp, ref.zero = TRUE)
# 2. 自动找到【HR 第一次穿过 1 的 AIP 值】（真正的ref点）
crossing_point <- pred$AIP[which.min(abs(pred$yhat - 1))]
cat("自动识别参考点 AIP =", round(crossing_point, 2), "\n")
# 3. 提取P值
aov <- anova(rcs_model)
p_overall <- aov["AIP", "P"]
p_nonlinear <- aov[nrow(aov), "P"]
# 仅保留用于 RCS 的完整数据（若无缺失可忽略）
dd <- datadist(data_analysis)
options(datadist = "dd")

# 拟合 RCS 模型（协变量同 Cox Model 3，AIP 用 4 个节点）
rcs_model <- cph(Surv(followup_years, stroke_event) ~ rcs(AIP, 4) + 
                   r1agey + ragender + marital + education + r1mbmi +
                   smoking + drinking + hypertension + diabetes + sbp + dbp + 
                   newcho + newhba1c + newglu,
                 data = data_analysis, x = TRUE, y = TRUE)

# 预测 HR（以 AIP 中位数作为参考）
pred <- Predict(rcs_model, AIP, fun = exp, ref.zero = TRUE)ggplot() +
  # 置信区间阴影
  geom_ribbon(data = pred, aes(x = AIP, ymin = lower, ymax = upper),
              fill = "lightblue", alpha = 0.3, color = NA) +
  # 红色曲线
  geom_line(data = pred, aes(x = AIP, y = yhat),
            color = "red", linewidth = 0.8) +
  # HR=1 虚线
  geom_hline(yintercept = 1, color = "blue", linetype = "dashed", linewidth = 0.5) +
  # 直方图（现在一定能看见！）
  geom_histogram(data = analysis_data,
                 aes(x = AIP, y = after_stat(count)/150),
                 fill = "lightblue",color = "blue", alpha = 0.2, bins = 40) +
  
  # 自动标注参考点
  labs(
    x = "Atherogenic Index of Plasma (AIP)",
    y = "Hazard Ratio (HR) for Stroke",
    subtitle = sprintf(
      "Ref = %.2f | P overall = %.3f | P nonlinear = %.3f",
      crossing_point, p_overall, p_nonlinear
    )
  ) +
  
  theme_minimal() +
  theme(
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 13, face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 2.5))


# ggplot-code
ggplot code learning 
ggplot() +
  # 置信区间阴影
  geom_ribbon(data = pred, aes(x = AIP, ymin = lower, ymax = upper),
              fill = "lightblue", alpha = 0.3, color = NA) +
  # 红色曲线
  geom_line(data = pred, aes(x = AIP, y = yhat),
            color = "red", linewidth = 0.8) +
  # HR=1 虚线
  geom_hline(yintercept = 1, color = "blue", linetype = "dashed", linewidth = 0.5) +
  # 直方图（现在一定能看见！）
  geom_histogram(data = analysis_data,
                 aes(x = AIP, y = after_stat(count)/150),
                 fill = "lightblue",color = "blue", alpha = 0.2, bins = 40) +
  
  #after_stat(count)：获取直方图每个 bin 的原始计数
  #after_stat(density)：获取概率密度
# 常用的统计量
after_stat(count)    # 原始计数
after_stat(density)  # 概率密度
after_stat(ncount)   # 标准化计数（最大值为1）
after_stat(ndensity) # 标准化密度
after_stat(width)    # bin的宽度
after_stat(x)        # bin的中心位置








  
