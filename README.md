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
