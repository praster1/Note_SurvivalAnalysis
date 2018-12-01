setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")
source("colorPalette.R")


##### F Distribution
### parameter
df1 = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
df2 = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0.1, 10, length.out = 1000)


### 수명 분포
df(x, df1 = df1, df2 = df2)


### 분위수 함수
qf(x, df1 = df1, df2 = df2)


### 난수 함수
rf(x, df1 = df1, df2 = df2)


### 누적분포함수
pf(x, df1 = df1, df2 = df2)


### 생존함수
sf = function (x, df1 = 1, df2 = 1)
{
    fx = 1 - pf(x, df1 = df1, df2 = df2)
    return(fx)
}


### 위험함수
hf = function (x, df1 = 1, df2 = 1)
{
    fx = df(x, df1 = df1, df2 = df2) / sf(x, df1 = df1, df2 = df2)
    return(fx)
}





##### Plot
plot.f_seq = function(x, df1 = 1, df2 = 0, xlim=c(0, 10), ylim=c(0, 5), func="df")
{
    color=colorPalette(300)

    len_df1 = length(df1)       # df1 파라메터의 길이
    len_df2 = length(df2)          # df2 파라메터의 길이
    
    color_counter = 1
    for (i in 1:len_df1)  ### 파라메터: df1
    {
        color_counter_init = color_counter
        legend_name = NULL;
        
        if (func=="df")     # 수명분포
        {
            plot(x, df(x, df1=df1[1], df2=df2[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
            for (j in 1:len_df2)   ### 파라메터: df2
            {
                lines(x, df(x, df1=df1[i], df2=df2[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("df1 = ", df1[i], " / df2 = ", df2[j], sep=""))
            }
        }
        else if (func == "pf")  # 누적분포함수
        {
            plot(x, pf(x, df1=df1[1], df2=df2[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
            for (j in 1:len_df2)   ### 파라메터: df2
            {
                lines(x, pf(x, df1=df1[i], df2=df2[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("df1 = ", df1[i], " / df2 = ", df2[j], sep=""))
            }
        }
        else if (func == "sf")  # 생존함수
        {
            plot(x, sf(x, df1=df1[1], df2=df2[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
            for (j in 1:len_df2)   ### 파라메터: df2
            {
                lines(x, sf(x, df1=df1[i], df2=df2[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("df1 = ", df1[i], " / df2 = ", df2[j], sep=""))
            }
        }
        else if (func == "hf")  # 위험함수
        {
            plot(x, hf(x, df1=df1[1], df2=df2[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
            for (j in 1:len_df2)   ### 파라메터: df2
            {
                lines(x, hf(x, df1=df1[i], df2=df2[j]), col=color[color_counter], lwd=2);
                color_counter = color_counter + 1;
                legend_name = c(legend_name, paste("df1 = ", df1[i], " / df2 = ", df2[j], sep=""))
            }
        }
        legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
    }
}


par(mfrow = c(4, 2))
plot.f_seq(x, df1, df2, xlim=c(min(x), max(x)), ylim=c(0, 1), func="df")

par(mfrow = c(4, 2))
plot.f_seq(x, df1, df2, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pf")

par(mfrow = c(4, 2))
plot.f_seq(x, df1, df2, xlim=c(min(x), max(x)), ylim=c(0, 1), func="sf")

par(mfrow = c(4, 2))
plot.f_seq(x, df1, df2, xlim=c(min(x), max(x)), ylim=c(0, 1), func="hf")