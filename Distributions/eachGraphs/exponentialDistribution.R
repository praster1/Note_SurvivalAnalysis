
source("colorPalette.R")


##### Exponential Distribution
### parameter
lambda = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dexponential = function(x, lambda = 1)
{
    fx = lambda * exp(-lambda * x)
    return(fx)
}


### 난수 함수
rexponential = function(n, min=0.1, max=10, lambda = 1)
{
	normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}

	xseq = seq(min, max, length=1000000)
	res = sample(xseq, size=n, prob=normalization(dexponential(xseq, lambda = lambda)), replace=TRUE)
	return(res)
}



### 누적분포함수
pexponential = function(x, lambda = 1)
{
    fx = 1 - exp(-lambda * x)
    return(fx)
}


### 생존함수
sexponential = function (x, lambda = 1)
{
    fx = exp(-lambda * x)
    return(fx)
}


### 위험함수
hexponential = function (x, lambda = 1)
{
    fx = rep(lambda, length(x))
    return(fx)
}





##### Plot
plot.exponential_seq = function(x, lambda = 1, xlim=c(0, 10), ylim=c(0, 5), func="dexponential")
{
    color=colorPalette(300)

    len_lambda = length(lambda)       # lambda 파라메터의 길이
    
    color_counter = 1
    color_counter_init = color_counter
    legend_name = NULL;


    if (func=="dexponential")     # 수명분포
    {    
        plot(x, dexponential(x, lambda=lambda[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
        for (i in 1:len_lambda)  ### 파라메터: lambda
        {
            lines(x, dexponential(x, lambda=lambda[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("lambda = ", lambda[i], sep=""))
        }
    }
    else if (func == "pexponential")  # 누적분포함수
    {
        plot(x, pexponential(x, lambda=lambda[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
        for (i in 1:len_lambda)  ### 파라메터: lambda
        {
            lines(x, pexponential(x, lambda=lambda[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("lambda = ", lambda[i], sep=""))
        }
    }
    else if (func == "sexponential")  # 생존함수
    {
        plot(x, sexponential(x, lambda=lambda[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
        for (i in 1:len_lambda)  ### 파라메터: lambda
        {
            lines(x, sexponential(x, lambda=lambda[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("lambda = ", lambda[i], sep=""))
        }
    }
    else if (func == "hexponential")  # 위험함수
    {
        plot(x, hexponential(x, lambda=lambda[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
        for (i in 1:len_lambda)  ### 파라메터: lambda
        {
            lines(x, hexponential(x, lambda=lambda[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("lambda = ", lambda[i], sep=""))
        }
    }
    legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
}

par(mfrow = c(2, 2))
plot.exponential_seq(x, lambda, xlim=c(min(x), max(x)), ylim=c(0, 2), func="dexponential")
plot.exponential_seq(x, lambda, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pexponential")
plot.exponential_seq(x, lambda, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sexponential")
plot.exponential_seq(x, lambda, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hexponential")
