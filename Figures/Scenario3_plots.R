install.packages("ggplot2")
library(ggplot2)
library(scales)

ggplot(data = Scenario3.table, aes(x = as.factor(Trials), y = MSE)) +
  geom_point(aes(color = as.factor(Method))) +
  facet_wrap(~ as.factor(Noise), nrow = 2) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 3", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size = 3))

ggplot(data = Scenario3.NoiseBreakdown, aes(x = as.factor(Trials), y = Distortion)) +
  geom_point(aes(color = as.factor(Noise))) +
  theme_minimal() +
  labs(title = "Distortion across Trials and Noise Levels for Scenario 3", x = "Number of Trials",
       y = "Distortion", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario3.NoiseBreakdown, aes(x = as.factor(Trials), y = ResidualNoise)) +
  geom_point(aes(color = as.factor(Noise))) +
  theme_minimal() +
  labs(title = "Residual Noise across Trials and Noise Levels for Scenario 3", x = "Number of Trials",
       y = "Residual Noise", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))