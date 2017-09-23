#'A multiple regression model (RC)
#'
#'@description  Class for the convenient multiple linear regression 
#'
#'@param formula Contains dependent and independent variables for linear regression
#'@param data A data.frame to conduct linear regression
#'
#'@exportClass linreg
#'@export linreg
#
#formula=Petal.Length~Sepal.Width+Sepal.Length
#data=iris
#

linreg <- setRefClass("linreg", 
  fields = list(formula="formula",
                data="data.frame",
                reg_coe="numeric",
                fit_val="numeric",
                residu="numeric",
                dof="numeric",
                res_var="matrix",
                var_reg_coe="numeric",
                t_val="numeric",
                data_name="character"),
  
  methods = list(
    initialize = function(formula, data){
      
      formula <<- formula
      data <<- data
      
      data_name <<- deparse(substitute(data))
      
      #getting y variable from the formula
      get_y <- all.vars(formula)[1]
      
      #defining X and y
      y <- data[[get_y]]
      X <- model.matrix(formula, data)
      
      model1 <- lm(y ~ X+0, data = data)
      
      reg_coe <<- model1$coefficients
      fit_val <<- model1$fitted.values
      residu <<- model1$residuals
      dof <<- model1$df.residual
      res_var <<- (t(residu)%*%residu)/dof
      var_reg_coe <<- var(reg_coe)
      t_val <<- reg_coe/sd(reg_coe)
      
    },
    
    print = function(){
      "Print out the coefficients and coefficient names"
      cat("Call: \n ")
      cat(paste0("linreg(formula = ",format(formula),", data = ",data_name,")\n\n"))
      cat("Coefficients: \n")
      reg_coe

    }

    
  )
  )

linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
