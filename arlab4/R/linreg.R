#'A multiple regression model (RC)
#'
#'@description  Class for the convenient multiple linear regression 
#'
#'@param formula Contains dependent and independent variables for linear regression
#'@param data A data.frame to conduct linear regression
#'
#'@exportClass linreg
#'@export linreg
#'@import ggplot2
#
#formula=Petal.Length~Sepal.Width+Sepal.Length
#data=iris
#linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

linreg <- setRefClass("linreg", 
  fields = list(formula="formula",
                data="data.frame",
                reg_coe="matrix",
                fit_val="matrix",
                residu="matrix",
                dof="numeric",
                res_var="matrix",
                var_reg_coe="numeric",
                t_val="matrix",
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
      
      reg_coe <<- solve((t(X)%*%X))%*%t(X)%*%y
      fit_val <<- X%*%reg_coe
      residu <<- y-fit_val
      
      dof <<- length(y) - length(all.vars(formula))
      res_var <<- (t(residu)%*%residu)/dof
      var_reg_coe <<- diag(as.numeric(res_var)*solve(t(X)%*%X))
      t_val <<- reg_coe/sqrt(var_reg_coe)
       
    },
    
    print = function(){
      "Print out the coefficients and coefficient names"
      
      cat("Call: \n ")
      cat(paste0("linreg(formula = ",format(formula),", data = ",data_name,")\n\n"))
      cat("Coefficients: \n")
      cat(" ",row.names(reg_coe), "\n       ", sep = "  ")
      cat(t(reg_coe), sep="      ")
     
    },
    
    plot = function(){
      "Plotting Residuals vs Fitted graph & Scale-Location graph"
      
      library(ggplot2)
      
      #plot1
      ggplot(data.frame(fit_val,residu),aes(y=residu,x=fit_val)) + geom_point() + #graph + scatter plot
        xlab(paste("Fitted values\n", "lm(",format(formula),")")) + 
        ylab("Residuals")+ ggtitle("Residuals vs Fitted") + 
        geom_smooth(span = 2,colour="red",method="loess",se=FALSE) + 
        theme(plot.title = element_text(hjust = 0.5)) #Title center alignment
      
      #plot2
      std_res <- residu/sd(residu)
      ggplot(data.frame(fit_val,std_res),aes(y=std_res,x=fit_val)) + geom_point() + #graph + scatter plot
        xlab(paste("Fitted values\n", "lm(",format(formula),")")) +
        ylab(expression(sqrt(abs("Standardized residuals")))) + ggtitle("Scale-Location") +
        geom_smooth(span = 2,colour="red",method="loess",se=FALSE) +
        theme(plot.title = element_text(hjust = 0.5)) #Title center alignment
    },
    
    resid = function(){
      "Return the vector of residuals e"
      return(residu) 
    },
    
    pred = function(){
      "Return the predicted values y_hat"
      return(fit_val)
    },
    
    coef = function(){
      "Return the coefficients as a named vector"
      return(reg_coe)
    },
    
    summary = function(){
      "Simplified summary of the linear model"
      
      #p_value calculation
      p_val <- 2*pt(abs(t_val),dof,lower.tail = FALSE)
      
      #organizing table
      reg_coe_edit <- matrix(NA, nrow=3, ncol=4)
      reg_coe <<- cbind(reg_coe, reg_coe_edit)
      reg_coe[,2] <<- sqrt(var_reg_coe)
      reg_coe[,3] <<- t_val
      reg_coe[,4] <<- p_val
      reg_coe[,5] <<- "***"

      cat("          ", c("Estimate", "Std. Error", "t value", "Pr(>|t|)", ""), "\n", sep = "  ")
      for(i in 1:length(all.vars(formula))){
      cat(row.names(reg_coe)[i], reg_coe[i,], "\n", sep = " ")
      }

      cat("\nResidual standard error:",format(sd(residu)), "on 147 degrees of freedom\n\n")
      }
  )
  )
