Newton.Raphson <- function(expr, varname, x0, dx = NULL,c = 1e-07, N = 10000){
  # varname : variable x  expr：expression
  if(is.null(dx)){
    # derivate is not given
    dx <- deriv(expr, varname, func = TRUE) #求导数
    for (i in 1:N) {
      fx <- dx(x0) #f(x)
      if(fx == 0){break}
      dfx <- attributes(dx(x0))[[1]][1,1] #f'(x)
      x0 <- x0 - fx/dfx #cal x_n+1
      if(abs(fx)<c){ 
        break
      }
    }
  }else{
    # when derivate is given
    for (i in 1:N) {
      fx <- eval(expr,list(x0))
      dfx <- dx(x0)
      if(dfx == 0){error('斜率为0，不会有更好的结果')}
      x0 <- x0 - fx/dfx
      if(abs(fx)<c){ 
        break
      }
    }
  }
  #return iteration number and estimation of x
  return(list(it_num = i, estimation = x0[[1]]))
}

# Practice 1
NewtonRaphson(expression(x^2-4), 'x', 1)

##$it_num
##[1] 6

##$estimation
##[1] 2

NewtonRaphson(expression(x^2-4), 'x', -1)
##$it_num
##[1] 6

##$estimation
##[1] -2


# 2.
dx <- function(x){ return(ifelse(x>0,0.5*(x)^(-0.5),-0.5*(-x)^(-0.5)))}
NewtonRaphson(expression(sqrt(abs(x))), 'x', dx = dx,0.25)

#$it_num
#[1] 10000
#$estimation
#[1] 0.25

# x_0 varies between 0.25 and -0.25 can't get root

# 3.

expr <- expression(x*exp(-x^2)-0.4*(exp(x)+1)^(-1)-0.2)
NewtonRaphson(expr, 'x', 0.5)
## $it_num
## [1] 4

## $estimation
## [1] 0.4303877

NewtonRaphson(expr, 'x', 0.1)

## $it_num
## [1] 5

## $estimation
## [1] 0.4303877



# 4. Learn from mistakes

# 1. Newton_Rapson doesn't apply to all expressions
# 2. Newton_Rapson Method only returns only root
# 3. initial value is important in Newton_Raphson method
# 3. bugs always exist, there is no perfect function 
# 4. so What matters is to let users know what kind 
#    of objects your function receives and what it returns 

curve(x^3-2*x^2-11*x+12,-5,5)
abline(h=0)
Newton.Raphson(expression(x^3-2*x^2-11*x+12),varname = 'x',x0 = 2.35287527)
Newton.Raphson(expression(x^3-2*x^2-11*x+12),varname = 'x',x0 = 2.35284172)

curve(2*x^3+3*x^2+5,-50,50)
abline(h=0)
Newton.Raphson(expression(2*x^3+3*x^2+5),varname = 'x',x0 = 0.5)
Newton.Raphson(expression(2*x^3+3*x^2+5),varname = 'x',x0 = 0)
