install.packages("ggplot2")
library(ggplot2)

ggplot(data = Scenario1.table.n1, aes(x = as.factor(Trials), y = MSE)) +
  geom_point(aes(color = as.factor(Method))) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials for 
       Noise Level 1", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario1.table.n2, aes(x = as.factor(Trials), y = MSE)) +
  geom_point(aes(color = as.factor(Method))) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials for 
       Noise Level 2", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario1.table.n3, aes(x = as.factor(Trials), y = MSE)) +
  geom_point(aes(color = as.factor(Method))) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials for 
       Noise Level 3", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Scenario1.table.n4, aes(x = as.factor(Trials), y = MSE)) +
  geom_point(aes(color = as.factor(Method))) +
  theme_minimal() +
  labs(title = "MSE Comparison Between Methods as Function of Number of Trials for 
       Noise Level 4", x = "Number of Trials", y = "MSE", color = "Method") +
  theme(plot.title = element_text(hjust = 0.5))