source("colorPalette.R")
require(VGAM)


##### lfrechet Distribution
### parameter
location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)

# m : shape / c
# beta : scale / b
# mu : location / a
### 수명 분포
dlfrechet = function(x, location = 0, shape = 1, scale = 1)
{
    fx = (shape/scale) * ((location - x) / scale)^(-(shape+1)) * exp(-((location - x) / scale)^(-shape))
}


### 난수 함수
rlfrechet = function (n, min=0.1, max=10, location = 0, shape = 1, scale = 1)
{
    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dlfrechet(xseq, location=location, shape=shape, scale=scale)), replace=TRUE)
	return(res)
}


### 누적분포함수
plfrechet = function(x, location = 0, shape = 1, scale = 1)
{
    fx = -(slfrechet(x, alpha, beta) - 1)
}


### 생존함수
slfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = exp(-((location - x) / scale)^(-shape))
    return(fx)
}


### 위험함수
hlfrechet = function (x, location = 1, shape = 1, scale = 1)
{
    fx = dlfrechet(x, location = location, shape = shape, scale = scale) / slfrechet(x, location = location, shape = shape, scale = scale)
    return(fx)
}