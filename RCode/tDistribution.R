setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/RCode")
source("colorPalette.R")


##### Chi-square Distribution
### parameter
df = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

### input varialbe
x = seq(0, 10, length.out = 1000)


### 수명 분포
dt(x, df = df)


### 분위수 함수
qt(x, df = df)


### 난수 함수
rt(x, df = df)


### 누적분포함수
pt(x, df = df)


### 생존함수
st = function (x, df = 1)
{
    fx = 1 - pt(x, df = df)
    return(fx)
}


### 위험함수
ht = function (x, df = 1)
{
    fx = dt(x, df = df) / st(x, df = df)
    return(fx)
}





##### Plot
plot.t_seq = function(x, df = 1, xlim=c(0, 10), ylim=c(0, 5), func="dt")
{
    color=colorPalette(300)

    len_df = length(df)       # df 파라메터의 길이
    
    color_counter = 1
    color_counter_init = color_counter
    legend_name = NULL;


    if (func=="dt")     # 수명분포
    {    
        plot(x, dt(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Life Distribution Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, dt(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    else if (func == "pt")  # 누적분포함수
    {
        plot(x, pt(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Cumulative Distribution Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, pt(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    else if (func == "st")  # 생존함수
    {
        plot(x, st(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Survival Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, st(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    else if (func == "ht")  # 위험함수
    {
        plot(x, ht(x, df=df[1]), xlim=xlim, ylim=ylim, col=color[1], lwd=2, type = 'n', main="Hazard Function")
        for (i in 1:len_df)  ### 파라메터: df
        {
            lines(x, ht(x, df=df[i]), col=color[color_counter], lwd=2);
            color_counter = color_counter + 1;
            legend_name = c(legend_name, paste("df = ", df[i], sep=""))
        }
    }
    legend('right', bty = 'n', lwd=2, col=color[color_counter_init:(color_counter - 1)], legend = legend_name)
}

par(mfrow = c(2, 2))
plot.t_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 2), func="dt")
plot.t_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 1), func="pt")
plot.t_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 1), func="st")
plot.t_seq(x, df, xlim=c(min(x), max(x)), ylim=c(0, 2), func="ht")