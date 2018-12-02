source("colorPalette.R")


##### dhillon1 Distribution
### parameter
alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 1, length.out = 1000)


### 수명 분포
ddhillon1 = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = (gamma/beta) * ((x - alpha) / beta)^(gamma - 1) * exp(1 - exp(((x - alpha) / beta)^gamma) + ((x - alpha) / beta)^gamma)
    return(fx)
}


### 난수 함수
rdhillon1 = function(n, min=0, max=1, alpha = 1, beta = 1, gamma = 1)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(ddhillon1(xseq, alpha=alpha, beta=beta, gamma=gamma)), replace=TRUE)
	return(res)
}


### 누적분포함수
pdhillon1 = function(x, alpha = 1, beta = 1, gamma = 1)
{
    fx = -(sdhillon1(x, alpha, beta) - 1)
    return(fx)
}


### 생존함수
sdhillon1 = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = exp(1 - exp(((x - alpha) / beta)^gamma))
    return(fx)
}


### 위험함수
hdhillon1 = function (x, alpha = 1, beta = 1, gamma = 1)
{
    fx = ddhillon1(x, alpha, beta, gamma) / sdhillon1(x, alpha, beta, gamma)
    return(fx)
}