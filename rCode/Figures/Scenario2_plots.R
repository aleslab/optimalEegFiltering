#load graphing libraries
library(ggplot2)
library(scales)

#create MSE plot
ggplot(data = Scenario2_MSE, aes(x = as.factor(Trials), y = MSE, color = Method)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 2) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials 
       Faceted by Noise Level for Scenario 2", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.3))

#Create MSE breakdown plot
ggplot(data = Scenario2_allGsvdError, aes(x = as.factor(Trials), y = PercentError, fill = Partial)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 2) +
  theme_minimal() +
  labs(title = "Breakdown of MSE by Trial Size Faceted by Noise Level for Scenario 2", x = "Number of Trials",
       y = " Percent of MSE", fill = "Part of MSE") +
  theme(plot.title = element_text(hjust = 0.2))

# Create SDI plot
ggplot(data = Scenario2_allGsvdSDI, aes(x = as.factor(Trials), y = SDI, color = as.factor(NoiseLevel))) +
  geom_point() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Signal Distortion Index in Scenario 2", x = "Number of Trials", 
       y = "Signal Distortion Index", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

#Create Noise Reduction Factor plot
ggplot(data = Scenario2_allGsvdNRfactor, aes(x = as.factor(Trials), y = NoiseReduction, color = as.factor(NoiseLevel))) +
  geom_point() +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Noise Reduction in Scenario 2", x = "Number of Trials", 
       y = "Noise Reduction Factor", color = "Noise Level") +
  theme(plot.title = element_text(hjust = 0.5))

#create plot for SNR
ggplot(data = Scenario2_allSNR, aes(x = as.factor(Trials), y = SNR, color = Condition)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(~ as.factor(NoiseLevel), nrow = 2) +
  theme_minimal() +
  labs(title = "Signal-to-Noise Ratio Comparison Between Methods as a 
       Function of Number of Trials and Faceted by Noise Level for Scenario 2", x = "Number of Trials", y = "SNR", color = "Condition") +
  theme(plot.title = element_text(hjust = 0.5))