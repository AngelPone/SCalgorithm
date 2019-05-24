######################
####Lab Session 12####
######################

#实现 QR 算法
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

## 实现SVD分解

mysvd <- function(A){
  QR1 <- QR_algorithm(A %*% t(A))
  sigma <- sqrt(diag(QR1$S))
  U <- QR1$Q
  V <- QR_algorithm(t(A) %*% A)$Q
  return(list(U=U,sigma=sigma[1:min(dim(A))],V=V))
}

### 算法测试 ####
r = matrix(rnorm(30),5,6)
mysvd(r)
svd(r)

##比较线性回归中SVD、广义逆、QR分解方法##
x <- rnorm(100,2,4)
y <- 2*x+3+rnorm(100)
lm1 <- lm(y~x)
coefficients(lm1)
## 广义逆求系数
MASS::ginv(cbind(1,x))%*%y
## SVD求系数
X <- cbind(1,x)
X.svd <- mysvd(X)
X.svd$V %*% diag(1/X.svd$sigma) %*%t(X.svd$U[,1:2]) %*% y
## QR分解
X.qr <- qr(X)
solve(qr.R(X.qr),diag(rep(1,2))) %*% t(qr.Q(X.qr)) %*% y
## 三种方法结果相差不大
