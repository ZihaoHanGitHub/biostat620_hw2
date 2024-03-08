

# R setup
library(readxl)
library(systemfit)
library(dplyr)
library(car)
st = read_excel("./ScreenTimeZihaoHan.xlsx",sheet = 1)
st$Date <- as.Date(st$Date)
st <- st[st$Date <= "2024-01-26", ]
hm_to_min = function(text){
  split = strsplit(text,"h|m")
  hours = as.numeric(split[[1]][1])
  minutes = as.numeric(split[[1]][2])
  convert_minutes = hours * 60 + minutes
  return(convert_minutes)
}
st$Total.ST.min = sapply(st$Total.ST,hm_to_min)
st$Social.ST.min = sapply(st$Social.ST,hm_to_min)


# Generate the Dummy Variable X and Z
st$X <- ifelse(weekdays(st$Date) %in% c("sunday", "saturday"), 0, 1)
st$Z <- ifelse(st$Date <= as.Date("2024-01-10"), 0, 1)
# Creating the lag-1 variable of Y1 and Y2
st$Y1_lag1 <- c(NA, st$Total.ST.min[-nrow(st)])
st$Y2_lag1 <- c(NA, st$Social.ST.min[-nrow(st)])
```


# Fit the transitional SUR model
equations <- list(
  Total.ST.min ~ Y1_lag1 + X + Z,
  Social.ST.min ~ Y2_lag1 + X + Z
)
sur_model <- systemfit(equations, "SUR", data = st)
summary_result <- summary(sur_model)
cat("Equation 1: SUR for Total Screen Time:\n")
print(summary_result$eq[[1]])
cat("\nEquation 2: SUR for Social Screen Time:\n")
print(summary_result$eq[[2]])

# generate the C_matrix for the testing
C_matrix <- matrix(c(0, 0, 0, 1,0,0,0,1), nrow = 1, byrow = TRUE)
vcov_matrix <- vcov(sur_model)
wald_stat <- t(C_matrix %*% coef(sur_model)) %*% solve(C_matrix %*% vcov_matrix %*% t(C_matrix)) %*% (C_matrix %*% coef(sur_model))
df <- nrow(C_matrix)
p_value <- 1 - pchisq(wald_stat, df)
if (p_value > 0.05) {
  cat("Do not have sufficient evidence to reject the null hypothesis, 
      with the pvalue",p_value,"\n")
}else{
  cat("Reject the Null hypothesis since pvalue is",p_value,"\n")
}
cat("This test use the Wald test, with the wald test statistics",wald_stat,"\n")

