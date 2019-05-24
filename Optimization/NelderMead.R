NelderMead <- function(func, ps, aim = 0, param = list(alpha = 1, gamma = 2, rho = 0.5), plot = FALSE, plot_bg = NULL){
  #### Library Requirment####
  library(dplyr)
  library(animation)
  #### Sub Functions ####
  judge_1 <- function(ps, p, aim){
    if(class(p) == "data.frame"){p <- as.vector(as.matrix(p))}
    if(aim){
      if(func(p) <= ps[1,ncol(ps)] & func(p) > ps[nrow(ps),ncol(ps)]){return(1)}
      if(func(p) >= ps[1,ncol(ps)]){return(2)}
      if(func(p) <= ps[nrow(ps),ncol(ps)]){return(3)}
    }else{
      if(func(p) >= ps[1,ncol(ps)] & func(p) < ps[nrow(ps),ncol(ps)]){return(1)}
      if(func(p) <= ps[1,ncol(ps)]){return(2)}
      if(func(p) >= ps[nrow(ps),ncol(ps)]){return(3)}
    }
  }
  judge_2 <- function(p1,p2, aim){
    p2 <- as.vector(as.matrix(p2))
    p1 <- as.vector(as.matrix(p1))
    if(aim == 1){if(func(p1) <= func(p2)){return(1)}else{return(2)}
    }
    if(aim == 0){if(func(p1) >= func(p2)){return(1)}else{return(2)}}
  }
  #### shrink ####
  shrink <- function(ps){
    ps <- ps %>% apply(MARGIN = 1,function(x){return((x+as.matrix(ps)[1,])/2)}) %>% t %>% as.data.frame() 
    return(ps)
  }
  #### iter-stop ####
  iter_stop <- function(ps, iter_num){
    if(iter_num >= 50){return(0)}
    X <- ps[-nrow(ps),-ncol(ps)] %>% apply(1,FUN = function(x){x-as.matrix(ps)[nrow(ps),-ncol(ps)]}) %>% t
  
    if(abs(det(X))<1e-09){return(0)}
    if(abs(ps$fx[1] - ps$fx[nrow(ps)]) < 1e-09){return(0)}
    else(return(1))
  }
  #### Plot ####
  if(plot){
    ani.record(reset = TRUE)
    plot_bg(main = "iter_Num = 0")
    polygon(x = ps[,1], y = ps[,2])
    ani.record()
  }
  
  #### Main ####
  iter_num = 0
  ps$fx <- apply(ps, MARGIN = 1, func)
  if(aim){
    ps <- ps %>% arrange(desc(fx))
  }else{
    ps <- ps %>% arrange(fx)
  }
  while(iter_stop(ps, iter_num)){
    p0 <- sapply(ps[1:nrow(ps)-1,1:ncol(ps)-1], mean)
    pr <- p0 + param$alpha*(p0-ps[nrow(ps),1:ncol(ps)-1])
    a = judge_1(ps, pr, aim = aim)
    if(a == 1){ps[nrow(ps),] = c(pr, func(pr))}
    if(a == 2){
      pe = p0 + param$gamma*(pr-p0)
      if(judge_2(pe, pr, aim) == 1){
        ps[nrow(ps),] = c(pr, func(pr))
      }else{
        ps[nrow(ps),] = c(pe, func(pe))
      }
    }
    if(a == 3){
      pc <- p0 + param$rho*(ps[nrow(ps),-ncol(ps)] - p0)
      if(judge_2(pc, ps[nrow(ps),-1], aim) == 2){
        ps[nrow(ps),] = c(pc, func(pc))
      }else{ ps <- shrink(ps)}
    }
    if(aim){
      ps <- ps %>% arrange(desc(fx))
    }else{
      ps <- ps %>% arrange(fx)
    }
    iter_num = iter_num + 1
    if(plot){
      plot_bg(main = paste0('iter_Num = ',iter_num), xlab = expression(x),ylab = expression(y))
      polygon(x=ps[,1],y=ps[,2])
      ani.record()
    }
  }
  if(plot){
    ani.options(interval = 0.5)
    saveGIF(ani.replay())}
  return(list(simplex = ps, iter_num = iter_num))
}

#### 目标函数 ####
f <- function(x){
  return(x[1]^2 + x[2]^2)
}

a = data.frame(x1 = c(1,1,2), x2 = c(1,2,2))
a = data.frame(x1 = c(2,3,2), x2 = c(1,2,2))

####  plot background #####
plot_bg <- function(...){
  x = seq(-3,3, 0.1)
  y = seq(-3,3, 0.1)
  z = expand.grid(x = x, y = y)
  z = matrix(z$x^2 + z$y^2, nrow = length(x))
  image(x = x,y = y,z = z,...)
}


NelderMead(f, a, plot = TRUE, plot_bg = plot_bg)




#### Page Rank ####
A = matrix(c(0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,1,1,0,1,0),5,5)
a <- eigen(A)

(Vectors <- a$vectors %>% apply(MARGIN = 2, function(x){Mod(x)/sum(Mod(x))}))
(Rirank <- rank(Vectors[,1]))
# 2 1 3 4 5
# Web site 5 is most important



