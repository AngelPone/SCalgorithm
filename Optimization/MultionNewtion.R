# 单对初始值
MultiD.Newton.Raphson <- function(expr, namevec, x0,dx = NULL, max.iter = 10000, c = 1e-09){
  # 临界条件
  if(length(namevec) != length(x0)){stop("namevec must have the same length with x0")}
  judge <- function(x1,x0,value){
    ## a problem：input initial value vector and eval derivative function##
    value1 <- do.call(dx,as.list(x1))
    dx1 <- attributes(value1)$gradient
    dx0 <- attributes(value)$gradient
    if(abs(value1-value)<c){return(TRUE)} #f(x1)-f(x0)
    if(max(abs(x1-x0))<c){return(TRUE)} # x1-x0
    if(max(abs(dx1))<c){return(TRUE)} #f'(x1) - f'(x0)
    return(FALSE)
  }
  judge_dxnull <- function(x1,x0,value){
    value1 <- expr(x1)
    dx1 <- dx(x1)$gradient
    if(abs(value1-value)<c){return(TRUE)} #f(x1)-f(x0)
    if(max(abs(x1-x0))<c){return(TRUE)} # x1-x0
    if(max(abs(dx1))<c){return(TRUE)} #f'(x1) - f'(x0)
    return(FALSE)
  }
  # 判断海塞矩阵是否正定、负定还是不定，以确定极小/大值
  is.positive.definite <- function(x){
    eigens <- eigen(x)$values
    if(length(eigens>0) == length(eigens)){
      return('min')
    }
    if(length(eigens<0) == length(eigens)){
      return('max')
    }else{
      return('不是极值')
    }
  }
  
  if(is.null(dx)){
    dx <- deriv(expr, namevec,hessian = TRUE, function.arg = TRUE)
    for (k in 1:max.iter) {
      value <- do.call(dx, as.list(x0)) # pass a vector to  function with more than one arguments 
      A <- rbind(attributes(value)$hessian[,,1:length(namevec)]) # Hessian matrix
      b <- attributes(value)$gradient #b
      stepsize <- solve(A,t(b)) 
      x1 <- x0 - stepsize
      if(judge(x1,x0,value)){ # 临界条件
        #返回迭代次数、向量X的值、海塞矩阵、极值、是极大值还是极小值
        return(list(it.num = k, x = x1, hessian = A, pv = as.numeric(value), pole = is.positive.definite(A)))
      }
      x0 <- x1
    }
  }else{
    # if dx exists, it must be a list that has hessian matrix and derivae
    for (k in 1:max.iter) {
      fx <- expr(x0)
      A <- dx(x0)$hessian
      b <- dx(x0)$gradient
      stepsize <- solve(A,b) 
      x1 <- x0 - stepsize
      if(judge_dxnull(x1,x0,fx)){ # 临界条件
        #返回迭代次数、向量X的值、海塞矩阵、极值、是极大值还是极小值
        return(list(it.num = k, x = x1, hessian = A, pv = as.numeric(fx), pole = is.positive.definite(A)))
      }
      x0 <- x1
    }}
  return(list(it.num = max.iter, x = x0, hessian = A, pv = as.numeric(value), pole = is.positive.definite(A)))
}

# 适用于多个初始值的情况,要求输入为X Y列表
MultiD.Newton.Raphson.initvalues <- function(expr, namevec, x0, aim, dx=NULL, max.iter = 10000, c = 1e-09){
  # x0 must be a  list
  pv <- list()
  for (i in 1:length(x0)) {
    pv[[i]] <- MultiD.Newton.Raphson(expr,namevec, x0[[i]],dx,max.iter,c)
  }
  # aim ==1 max, aim == 0, min
  if(aim == 1){
    valus <- sapply(pv, function(x){return(x$pv)})
    return(pv[[which(valus == max(valus))]])
  }
  if(aim == 0){
    valus <- sapply(pv, function(x){return(x$pv)})
    return(pv[[which(valus == min(valus))]])
  }
}

MultiD.Newton.Raphson.initvalues(expression(x^2-x*y+y^2+exp(y)),
                                 namevec = c('x','y'), 
                                 x0 = list(2:3,3:4), 
                                 aim = 0)

#$it.num
#[1] 7

#$x
#[,1]
#x -0.2162814
#y -0.4325628

#$hessian
#x        y
#x  2 -1.00000
#y -1  2.64885

#$pv
#[1] 0.789177

#$pole
#[1] "min"
