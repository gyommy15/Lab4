linreg <- setRefClass("linreg", 
  fields = list(formula="formula",
                data="data.frame",
                reg_coe="numeric",
                fit_val="numeric",
                residu="numeric",
                dof="numeric",
                res_var="numeric",
                var_reg_coe="numeric",
                t_val="numeric"),
  
  methods = list(
    initialize = function(formula, data){
      
      formula <<- formula
      data <<- data
      
      X <- model.matrix(formula, data)
      y <- all.vars(formula)
      
      model1 <- lm(y ~ X, data = data)
      
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
      cat("Call: /n")
      cat(paste0("linreg(formula = ", format(formula),", data = ", format(data),")\n\n"))
      cat("Coefficients: /n")
      reg_coe
      
    }
    
    
        
    
  )
  )
