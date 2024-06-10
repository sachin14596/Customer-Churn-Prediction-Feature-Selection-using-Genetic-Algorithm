library(readxl)

getBenchmark <- function(){
  churn_data <- read_excel("D:/Study/MS Big Data Analytics/Modern Optimization/Submissions/4 Assessment 1.2/CustomerChurnResampled.xlsx")
  
  mod <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService +
              MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport +
              StreamingTV + StreamingMovies + PaperlessBilling + MonthlyCharges + TotalCharges +
              InternetService_DSL + InternetService_Fiber_optic + InternetService_No +
              Contract_Month_to_month + Contract_One_year + Contract_Two_year +
              PaymentMethod_Bank_transfer__automatic_ + PaymentMethod_Credit_card__automatic_ +
              PaymentMethod_Electronic_check + PaymentMethod_Mailed_check, data = churn_data, family = "binomial")
  
  return(mod)
}

getData <- function(){
  churn_data <- read_excel("D:/Study/MS Big Data Analytics/Modern Optimization/Submissions/4 Assessment 1.2/CustomerChurnResampled.xlsx")
  
  mod <- getBenchmark()
  
  xx <- model.matrix(mod)[, -1]   
  yy <- churn_data$Churn
  data <- cbind(xx, yy)
  
  return(data)
}

featureFitness <- function(string, xx, yy) {
  inc <- which(string == 1)
  if (length(inc) == 0) return(-10E20)
  X <- cbind(1, xx[, inc])
  mod <- glm(yy ~ X, family = "binomial")
  #class(mod) <- "lm"
  -AIC(mod)
}

