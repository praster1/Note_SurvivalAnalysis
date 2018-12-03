require(VGAM)

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