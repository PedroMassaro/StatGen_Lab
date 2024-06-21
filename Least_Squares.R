# ---
#   title: "Statgen Meeting - Least Square"
# author: "Pedro Henrique"
# date: '2024-06-19'
# ---

## Data
  set.seed(2106)
  dados <- data.frame(
    Genotipo = as.factor(sample(
      rep(c("A", "B", "C","D", "E", "F"), times = 2),
      size = 12)),
    altura = c(20.3,21.7,22.0,20.8,21.5,19.6,
               19.6,19.3,24.9,23.0,22.3,17.7)
  )

##Variables
  y <- c(20.3,21.7,22.0,20.8,21.5,19.6,
         19.6,19.3,24.9,23.0,22.3,17.7)
  
  x <- matrix(c(0, 0, 1, 0, 0, 0,
                0, 0, 0, 1, 0, 0,
                0, 0, 0, 1, 0, 0,
                0, 1, 0, 0, 0, 0,
                0, 0, 0, 0, 1, 0,
                0, 0, 0, 0, 0, 1,
                1, 0, 0, 0, 0, 0,
                0, 1, 0, 0, 0, 0,
                0, 0, 0, 0, 1, 0,
                1, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 1,
                0, 0, 1, 0, 0, 0), nrow = 12, ncol = 6, byrow = TRUE)
  
  X <- matrix(c(1, 0, 0, 1, 0, 0, 0,
                1, 0, 0, 0, 1, 0, 0,
                1, 0, 0, 0, 1, 0, 0,
                1, 0, 1, 0, 0, 0, 0,
                1, 0, 0, 0, 0, 1, 0,
                1, 0, 0, 0, 0, 0, 1,
                1, 1, 0, 0, 0, 0, 0,
                1, 0, 1, 0, 0, 0, 0,
                1, 0, 0, 0, 0, 1, 0,
                1, 1, 0, 0, 0, 0, 0,
                1, 0, 0, 0, 0, 0, 1,
                1, 0, 0, 1, 0, 0, 0), nrow = 12, ncol = 7, byrow = TRUE)
  
  ##Least Square
  
  #For this, we use the following matrix algebra: $\beta = (X^{t}X)^{-1}X^{t}y$.\

## Matrix model **without** intercept
  beta <- (solve(t(x)%*%(x)))%*%(t(x)%*%y); beta

## Matrix Model **with** intercept
  # For this, we use the following matrix algebra: $\beta = (X^{t}X)^{-}X^{t}y$,
  # in other words, a generalised inverse.
  
  ##Generalized inverse by **Moore-Penrose - Ginv** 
  # install.packages("matlib")
  library(matlib)
  beta_2 <- (Ginv(t(X)%*%(X)))%*%(t(X)%*%y); beta_2
  
  ##Generalized inverse by **Moore-Penrose - pinv.**
  # install.packages("pracma")
  library(pracma)
  beta_3 <- (pinv(t(X)%*%(X)))%*%(t(X)%*%y); beta_3
  
  beta_3[2,1] + beta_3[1,1]
  beta_3[3,1] + beta_3[1,1]
  #The result was the same as that obtained in *model_1*.

## Model by lm():
  # In this case **there is no intercept**
  model_1 <- lm(dados$altura ~ dados$Genotipo   -1); model_1 
  model_1$coefficients[[1]] - model_1$coefficients[[2]]
  anova(model_1)
  
  # In this case **there is intercept**
  model_2 <- lm(dados$altura ~ dados$Genotipo); model_2 
  model_2$coefficients[[1]] - model_2$coefficients[[2]]
  anova(model_2)


#-------------------------------------------------------------------------------
### Example 02

## Data
  block <- matrix(nrow = 24, ncol = 1)
  # Loop para randomizar os genótipos em cada bloco
  for (i in 1:4) {
    set.seed(i)
    start <- (i - 1) * 6 + 1
    end <- i * 6
    block[start:end, ] <- sample(c("A", "B", "C", "D", "E", "F"))
  }
  
  dados <- data.frame(
    Bloco = as.factor(rep(1:4, each = 6)),
    Genotipo = as.factor(block),
    altura = c(20.3,21.7,22.0,20.8,21.5,19.6,
               19.6,19.3,24.9,23.0,22.3,17.7,
               23.5,16.7,24.4,21.3,22.1,18.7,
               19.1,18.5,20.8,24.9,21.9,22.0)
  )

## Experimental design
  library(agricolae)
  (trat <- paste("T", 1:6, sep =""))
  Casualizando <- design.rcbd(trt = trat,
                              r = 4)
  library(agricolaeplotr)
  plot_rcdb(Casualizando,
            factor_name = "trat")

##Variables
  y <- c(20.3,21.7,22.0,20.8,21.5,19.6,
         19.6,19.3,24.9,23.0,22.3,17.7,
         23.5,16.7,24.4,21.3,22.1,18.7,
         19.1,18.5,20.8,24.9,21.9,22.0)
  
  x <- matrix(c(1, 0, 0, 0,  0, 0, 0, 0, 1, 0,
                1, 0, 0, 0,  0, 0, 0, 0, 0, 1,
                1, 0, 0, 0,  0, 0, 0, 1, 0, 0,
                1, 0, 0, 0,  0, 0, 1, 0, 0, 0,
                1, 0, 0, 0,  0, 1, 0, 0, 0, 0,
                1, 0, 0, 0,  1, 0, 0, 0, 0, 0,
                
                0, 1, 0, 0,  0, 0, 0, 0, 0, 1,
                0, 1, 0, 0,  0, 0, 1, 0, 0, 0,
                0, 1, 0, 0,  0, 1, 0, 0, 0, 0,
                0, 1, 0, 0,  0, 0, 0, 1, 0, 0,
                0, 1, 0, 0,  0, 0, 0, 0, 0, 1,
                0, 1, 0, 0,  1, 0, 0, 0, 0, 0,
                
                0, 0, 1, 0,  0, 0, 1, 0, 0, 0,
                0, 0, 1, 0,  0, 0, 0, 1, 0, 0,
                0, 0, 1, 0,  0, 0, 0, 0, 0, 1,
                0, 0, 1, 0,  1, 0, 0, 0, 0, 0,
                0, 0, 1, 0,  0, 0, 0, 0, 1, 0,
                0, 0, 1, 0,  0, 1, 0, 0, 0, 0,
                
                0, 0, 0, 1,  0, 1, 0, 0, 0, 0,
                0, 0, 0, 1,  0, 0, 0, 0, 0, 1,
                0, 0, 0, 1,  1, 0, 0, 0, 0, 0,
                0, 0, 0, 1,  0, 0, 0, 1, 0, 0,
                0, 0, 0, 1,  0, 0, 1, 0, 0, 0,
                0, 0, 0, 1,  0, 0, 0, 0, 1, 0), nrow = 24, ncol = 10, byrow = TRUE)
  
  X <- matrix(c(1,  1, 0, 0, 0,  0, 0, 0, 0, 1, 0,
                1,  1, 0, 0, 0,  0, 0, 0, 0, 0, 1,
                1,  1, 0, 0, 0,  0, 0, 0, 1, 0, 0,
                1,  1, 0, 0, 0,  0, 0, 1, 0, 0, 0,
                1,  1, 0, 0, 0,  0, 1, 0, 0, 0, 0,
                1,  1, 0, 0, 0,  1, 0, 0, 0, 0, 0,
                
                1,  0, 1, 0, 0,  0, 0, 0, 0, 0, 1,
                1,  0, 1, 0, 0,  0, 0, 1, 0, 0, 0,
                1,  0, 1, 0, 0,  0, 1, 0, 0, 0, 0,
                1,  0, 1, 0, 0,  0, 0, 0, 1, 0, 0,
                1,  0, 1, 0, 0,  0, 0, 0, 0, 0, 1,
                1,  0, 1, 0, 0,  1, 0, 0, 0, 0, 0,
                
                1,  0, 0, 1, 0,  0, 0, 1, 0, 0, 0,
                1,  0, 0, 1, 0,  0, 0, 0, 1, 0, 0,
                1,  0, 0, 1, 0,  0, 0, 0, 0, 0, 1,
                1,  0, 0, 1, 0,  1, 0, 0, 0, 0, 0,
                1,  0, 0, 1, 0,  0, 0, 0, 0, 1, 0,
                1,  0, 0, 1, 0,  0, 1, 0, 0, 0, 0,
                
                1,  0, 0, 0, 1,  0, 1, 0, 0, 0, 0,
                1,  0, 0, 0, 1,  0, 0, 0, 0, 0, 1,
                1,  0, 0, 0, 1,  1, 0, 0, 0, 0, 0,
                1,  0, 0, 0, 1,  0, 0, 0, 1, 0, 0,
                1,  0, 0, 0, 1,  0, 0, 1, 0, 0, 0,
                1,  0, 0, 0, 1,  0, 0, 0, 0, 1, 0), nrow = 24, ncol = 11, byrow = TRUE)

##Least Square

  #For this, we use the following matrix algebra: $\beta = (X^{t}X)^{-1}X^{t}y$.\
  
## Matrix model **without** intercept
  beta <- (solve(t(x)%*%(x)))%*%(t(x)%*%y); beta

  library(matlib)
  beta <- (Ginv(t(x)%*%(x)))%*%(t(x)%*%y); beta

## Matrix Model **with** intercept
  # For this, we use the following matrix algebra: $\beta = (X^{t}X)^{-}X^{t}y$,
  # in other words, a generalised inverse.
  
  ##Generalized inverse by **Moore-Penrose - Ginv** 
  # install.packages("matlib")
  library(matlib)
  beta_2 <- (Ginv(t(X)%*%(X)))%*%(t(X)%*%y); beta_2
  
  ##Generalized inverse by **Moore-Penrose - pinv.**
  # install.packages("pracma")
  library(pracma)
  beta_3 <- (pinv(t(X)%*%(X)))%*%(t(X)%*%y); beta_3
  
  beta_3[2,1] + beta_3[1,1]
  beta_3[3,1] + beta_3[1,1]
  #The result was the same as that obtained in *model_1*.
  
## Model by lm():
  # In this case **there is no intercept**
  model_1 <- lm(dados$altura ~ dados$Bloco + dados$Genotipo   -1); model_1 
  model_1$coefficients[[1]] - model_1$coefficients[[2]]
  anova(model_1)
  
  # In this case **there is intercept**
  model_2 <- lm(dados$altura ~ dados$Genotipo); model_2 
  model_2$coefficients[[1]] - model_2$coefficients[[2]]
  anova(model_2)

  
