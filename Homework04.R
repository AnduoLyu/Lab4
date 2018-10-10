#Q4
is.prime <- function(n){
  if(n == 2){
    print(TRUE)
  }else if(any(n %% 2:(n-1) == 0)){
    print(FALSE)
  }else{
    print(TRUE)
  }
}
is.prime(6)


#Q3
library(ggplot2)
data <- data.frame(x = seq(-3, 6, by = 0.01))
ggplot(data, aes(x = x)) +
  stat_function(fun = dnorm, xlim = c(-3, 3), color = "blue", lwd = 1) +
  stat_function(fun = dnorm, args = list(mean = 3.2), xlim = c(0, 6), color = "red", lwd = 1) +
  geom_vline(aes(xintercept = qnorm(0.95)), lty = 2, lwd = 1) +
  labs(x = "", y = "") +
  theme_bw() +
  geom_area(stat = "function", fun = dnorm, xlim = c(qnorm(0.95), 3), color = "black",
            aes(fill = "a"), alpha = 0.5) +
  geom_area(stat = "function", fun = dnorm, args = list(mean = 3.2),
            xlim = c(0, qnorm(0.95)), color = "black", aes(fill = "b"), alpha = 0.5) +
  scale_fill_manual(values = c("a" = "blue", "b" = "red"), labels = c("Type I error", "Type II error")) +
  annotate("text", x = 2, y = 0.02, label = expression(alpha), size = 6, col = "white") +
  annotate("text", x = 1, y = 0.02, label = expression(beta), size = 6) +
  scale_x_discrete(limits = c(0, 3.1), label = c(expression(theta["0"]), expression(theta["a"]))) +
  theme(legend.title = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.text = element_text(size = 14, face = "bold"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.margin = margin(8, 8, 8, 8),
        axis.text = element_text(size = 16),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 18, vjust = -2),
        axis.ticks.length = unit(.25, "cm"),
        panel.border = element_rect(fill = NA, size = 1),
        panel.grid = element_blank())






