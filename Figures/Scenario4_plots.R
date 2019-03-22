library(ggplot2)
library(scales)

ggplot(data = Scenario4_MSE, aes(x = as.factor(Trials), y = MSE, color = Method)) +
  geom_point() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 2) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 4", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario4_allGsvdError, aes(x = as.factor(Trials), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Error Breakdown Averaged Over Noise Levels for Scenario 4", x = "Number of Trials",
       y = " Percent of Error", fill = "Part of Error") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario4_allGsvdError, aes(x = as.factor(NoiseLevel), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Error Breakdown Averaged Over Trial Number for Scenario 4", x = "Noise Level",
       y = " Percent of Error", fill = "Part of Error") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario4_allGsvdSDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 4", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario4_allGsvdNRfactor, aes(x = as.factor(Trials), y = NoiseReduction, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Noise Reduction in Scenario 4", x = "Number of Trials", 
       y = "Noise Reduction Factor", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))