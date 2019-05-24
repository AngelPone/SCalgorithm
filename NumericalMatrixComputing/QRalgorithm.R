##################
# Lab Session 11 #
##################

#####################
#### PowerMethod ####
#####################
PowerMethod <- function(A, max.iter = 500){
  # 取出方阵的维数，为特征向量的长度
  dimen <- dim(A)[1]
  # 生成初始特征向量
  x0 <- rep(1,times = dimen)
  # 迭代计算
  for (i in 1:max.iter){
    Ax <- A %*% x0
    x0 <- Ax/norm(Ax, type = 'F')
  }
  # 计算特征值
  lambda <- t(x0) %*% A %*% x0/norm(x0, type = 'F')
  return(list(lambda = lambda, x = x0))
}

######################
#### QR algorithm ####
######################

QR_algorithm <- function(A){
  # q_0
  qq <- qr(A)
  Q <- qr.Q(qq)
  # 迭代计算
  for (i in 1:100){
    qq <- qr(qr.R(qq) %*% qr.Q(qq))
    # 计算Qk
    Q <- Q %*% qr.Q(qq)
  }
  qq <- qr(qr.R(qq) %*% qr.Q(qq))
  S = qr.Q(qq) %*% qr.R(qq)
  return(list(Q = round(Q,6), S = round(S,6)))
}

#### test ####

A = matrix(c(1,3,2,4),2,2)
PowerMethod(A)
QR_algorithm(A)

PageRankMatrix = matrix(c(0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,1,1,0,1,0),5,5)
# 特征值结果
(res <- PowerMethod(t(PageRankMatrix)))
# 系数 alpha
(alpha <- 1/res$lambda)
# 网站排名
(rank(res$x))
