source("colorPalette.R")


##### inverseRayleigh Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dinverseRayleigh = function(x, alpha = 0, beta = 1)
{
    fx = ((2 * beta)/((x-alpha)^3)) * exp(-(beta / ((x-alpha)^3)))
    return(fx)
}


### 난수 함수       # Not complete
# rinverseRayleigh = function (n, min=-10, max=10, alpha = 0, beta = 1) 
# {
    # normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	# xseq = seq(min, max, length=1000000)
	# res = sample(xseq, size=n, prob=normalization(dinverseRayleigh(xseq, alpha = alpha, beta = beta)), replace=TRUE)
	# return(res)
# }


### 누적분포함수
pinverseRayleigh = function(x, alpha = 0, beta = 1)
{
    fx = -(sinverseRayleigh(x, alpha = alpha, beta = beta) - 1)
    return(fx)
}


### 생존함수
sinverseRayleigh = function (x, alpha = 0, beta = 1) 
{
    fx = 1 - exp(-(beta / ((x-alpha)^3)))
    return(fx)
}


### 위험함수
hinverseRayleigh = function (x, alpha = 0, beta = 1)
{
    fx = dinverseRayleigh(x, alpha, beta) / sinverseRayleigh(x, alpha, beta)
    return(fx)
}