install.packages("ggplot2")
library(ggplot2)
library(scales)

ggplot(data = Scenario3.table, aes(x = as.factor(Trials), y = MSE)) +
  geom_point(aes(color = as.factor(Method))) +
  facet_wrap(~ as.factor(Noise), nrow = 2) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 3", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario3.Error, aes(x = as.factor(Trials), y = Error, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Error Breakdown Averaged Over Noise Levels for Scenario 3", x = "Number of Trials",
       y = " Percent of Error", fill = "Part of Error") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario3.Error2, aes(x = as.factor(Noise), y = Error, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Error Breakdown Averaged Over Trial Number for Scenario 3", x = "Noise Level",
       y = " Percent of Error", fill = "Part of Error") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario3.SDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(Noise))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 3", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))