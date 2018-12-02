source("colorPalette.R")


##### 선형 증가 분포
### parameter
alpha = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)	# m (delta)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dlinearFR = function(x, alpha = 0, beta = 1)
{
    fx = (alpha * x + beta) * exp(- (1/2) * alpha * x^2 + beta * x)
    return(fx)
}


### 누적분포함수
plinearFR = function(x, alpha = 0, beta = 1)
{
    Fx = 1 - exp(- (1/2) * alpha * x^2 + beta * x)
    return(Fx)
}


### 생존함수
slinearFR = function(x, alpha = 0, beta = 1)
{
    fx = 1 - plinearFR(x, alpha = alpha, beta = beta)
    return(fx)
}


### 위험함수
hlinearFR = function(x, alpha = 0, beta = 1)
{
    fx = dlinearFR(x, alpha = alpha, beta = beta) / slinearFR(x, alpha = alpha, beta = beta)
    return(fx)
}
