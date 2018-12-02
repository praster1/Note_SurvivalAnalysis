source("colorPalette.R")
require(evd)

##### 극치 분포: Gumbel 최대값 분포
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(-10, 10, length.out = 1000)


### 수명 분포
dgumbel_max = function (x, scale = 1, location = 0) 
{
    fx = 1/scale * exp(-(x - location)/scale) * exp(-exp(-(x - location)/scale))
    return(fx)
}


### 난수 함수
rgumbel_max = function (n, min=-10, max=10, scale = 1, location = 0) 
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dgumbel_max(xseq, scale=scale, location=location)), replace=TRUE)
	return(res)
}


### 누적분포함수
pgumbel_max = function (q, scale = 1, location = 0) 
{
    fx = -(sgumbel_max(x, scale, location) - 1)
    return(fx)
}


### 생존함수
sgumbel_max = function (x, scale = 1, location = 0) 
{
    fx = 1 - exp(-exp(-(x - location)/scale))
    return(fx)
}


### 위험함수
hgumbel_max = function (x, scale = 1, location = 0) 
{
    fx = dgumbel_max(x, scale, location) / sgumbel_max(x, scale, location)
    return(fx)
}