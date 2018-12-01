setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")
require(EnvStats)


##### Chi Distribution
### parameter
df = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
dchi(x, df = df)


### 분위수 함수
qchi(x, df = df)


### 난수 함수
rchi(x, df = df)


### 누적분포함수
pchi(x, df = df)


### 생존함수
schi = function (x, df = 1)
{
    fx = 1 - pchi(x, df = df)
    return(fx)
}


### 위험함수
hchi = function (x, df = 1)
{
    fx = dchi(x, df = df) / schi(x, df = df)
    return(fx)
}





##### Plot
plot.chi_seq = function(x, df = 1, xlim=c(0, 10), ylim=c(0, 5), func="dchi")
{
    color=colorPalette(300)

    len_df = length(df)       # df 파라메터의 길이
    
    color_counter = 1
    color_counter_init = color_counter
    legend_name = NULL;


    if (func=="dchi")     # 수명분포
    {    
        plot(x, dchi(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, dchi(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    else if (func == "pchi")  # 누적분포함수
    {
        plot(x, pchi(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, pchi(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    else if (func == "schi")  # 생존함수
    {
        plot(x, schi(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, schi(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    else if (func == "hchi")  # 위험함수
    {
        plot(x, hchi(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, hchi(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
}

par(mfrow = c(2, 2))
plot.chi_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 2), func="dchi")
plot.chi_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pchi")
plot.chi_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 1), func="schi")
plot.chi_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 10), func="hchi")
