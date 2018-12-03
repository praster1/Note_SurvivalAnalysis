rm(list = ls())

setwd("/home/lv999/Dropbox/Github/SurvivalAnalysis/Distributions")


source("colorPalette.R")

### Distributions
source("alphaDistribution.R")
source("arcsineDistribution.R")
source("betaDistribution.R")
source("Birnbaum-SaundersDistribution.R")
source("BurrDistribution.R")
source("CauchyDistribution.R")
source("chiDistribution.R")
source("chisquareDistribution.R")
source("cosineDistribution.R")
source("Dhillon's1Distribution.R")
source("Dhillon's2Distribution.R")
source("doubleWeibullDistribution.R")
source("exponentialDistribution.R")
source("exponentialDistributionwithLocationParameter.R")
source("extremeValueDistribution_1(GumbelMaxDistribution).R")
source("extremeValueDistribution_1(GumbelMinDistribution).R")
source("extremeValueDistribution_2(FrechetDistribution).R")
source("extremeValueDistribution_2(FrechetDistributionwithLocationParameter).R")
source("FDistribution.R")
source("GammaDistributionwith2Parameters.R")
source("GammaDistributionwith3Parameters.R")
source("generalizedLomaxDistribution.R")
source("GompertzDistribution.R")
source("GompertzMakehamDistribution.R")
source("halfCauchyDistribution.R")
source("halfLogisticDistribution.R")
source("halfNormalDistribution.R")
source("hjorthDistribution.R")
source("hyperbolicSecantDistribution.R")
source("inverseNormalDistribution.R")
source("inverseRayleighDistribution.R")
source("inverseWeibullDistribution.R")
source("laplaceDistribution.R")
source("linearHazardRateDistribution.R")
source("logisticDistribution.R")
source("logLogisticDistribution.R")
source("lognormalDistribution.R")
source("logWeibullDistribution.R")
source("lomaxDistribution.R")
source("makehamDistribution.R")
source("muthDistribution.R")
source("normalDistribution.R")
source("parabolicInvertedUshapedDistribution.R")
source("parabolicUshapedDistribution.R")
source("paretoDistributionoftheFirstKind.R")
source("powerDistribution.R")
source("raisedCosineDistribution.R")
source("rayleighDistribution.R")
source("reflectedExponentialDistribution.R")
source("reflectedWeibullDistribution.R")
source("semiEllipticalDistribution.R")
source("tDistribution.R")
source("teisserDistribution.R")
source("uniformDistribution.R")
source("WeibullDistributionwith2Parameters.R")
source("WeibullDistributionwith3Parameters.R")
source("_exponentiatedExponentialDistribution.R")
source("_generalizedExponentialGeometricDistribution.R")
source("_generalizedGammaDistribution.R")
source("_generalizedLinearHazardRateDistribution.R")
source("_generalizedLogisticDistribution.R")
source("_generalizedRayleighDistribution.R")
source("_logGammaDistribution.R")
source("_logLaplaceDistribution.R")
source("_lognormalDistributionwithLowerThreshold.R")
source("_lognormalDistributionwithUpperThreshold.R")
source("_Maxwell-BoltzmannDistribution.R")
source("_TriangularDistribution.R")
source("_trimmedNormalDistribution.R")
source("_VshapedDistribution.R")



ksTest = function(dataVec)
{
    dataVec = iris[,1]

    normalization = function(x)	{	(x-min(x))/(max(x)-min(x));	}
    dataVec = normalization(dataVec)
    dataLen = length(dataVec)
    
    
    ##### Alpha Distribution        # ralpha = function(n, min=0.1, max=10, alpha = 1, beta = 1)
    ### parameter
    alpha = seq(-1, 1, length=100)
    beta = seq(0.01, 10, length=100)

    ### input varialbe
    # x = seq(0.1, 10, length.out = len)
    
    resAlpha = NULL;    counter = 1
    for (i in 1:length(alpha))
    {
        for (j in 1:length(beta))
        {
            resAlpha[[counter]] = ks.test(dataVec, ralpha(dataLen, min=min(dataVec), max=max(dataVec), alpha=alpha[i], beta=beta[j]))
            counter = counter + 1;
        }
    }


    
    ##### Arcsine Distribution      # rarcsine = function(n, min=0.1, max=10, alpha = 1, beta = 0)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-1, 10, length.out = 1000)

    
    
    ##### Beta Distribution
    ### parameter
    alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)    


    
    ##### Birnbaum-Saunders Distribution        # rfatigue(x, alpha, beta, mu = 0)
    ### parameter
    alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)
    


    ##### Burr Distribution         # rburr(x, a=alpha, k=beta)
    ### parameter
    alpha = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0, 3, length.out = 1000)
    
        
    
    ##### Cauchy Distribution       # rcauchy(x, location, scale)
    ### parameter
    location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)
    
    
    
    ##### Chi Distribution          # rchi(x, df = df)
    ### parameter
    df = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)
    
    
    
    ##### Chi-square Distribution       # rchisq(x, df = df)
    ### parameter
    df = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)
    
    
    
    ##### Cosine Distribution           # rcosine = function(n, min=-10, max=10, mu = 0, sigma = 1)
    ### parameter
    mu = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)
    
    
    
    
    ##### dhillon1 Distribution         # rdhillon1 = function(n, min=0, max=1, alpha = 1, beta = 1, gamma = 1)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)
    
    
    
    ##### dhillon2 Distribution         # rdhillon2 = function(n, min=0.1, max=1, alpha = 1, beta = 1, gamma = 1)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)    

    

    ##### weibull Distribution with c Parameters        # rdweibull = function(n, min=-10, max=1, c=1, mu=0, sigma=1)
    ### parameter
    mu = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    c = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)
    
    
    
    ##### Exponential Distribution              # rexponential = function(n, min=0.1, max=10, lambda = 1)
    ### parameter
    lambda = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Exponential Distribution with Location Parameter      # rlexponential = function(n, min=0.1, max=1, alpha = 1, beta = 1)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)


    
    ##### 극치 분포: Gumbel 최대값 분포          # rgumbel_max = function (n, min=-10, max=10, scale = 1, location = 0) 
    ### parameter
    location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### 극치 분포: Gumbel 최소값 분포          # rgumbel_min = function (n, min=-10, max=10, scale = 1, location = 0) 
    ### parameter
    location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)
    
    
    
    ##### frechet Distribution          # rfrechet(x, location = 0, shape = 1, scale = 1)
    ### parameter
    location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### lfrechet Distribution         # rlfrechet = function (n, min=0.1, max=10, location = 0, shape = 1, scale = 1)
    ### parameter
    location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)
    
    
    
    ##### F Distribution                # rf(x, df1 = df1, df2 = df2)
    ### parameter
    df1 = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    df2 = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)


    
    ##### Gamma Distribution        # rgamma2 = function(x, shape=shape, scale=scale)
    ### parameter
    shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8) 
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Gamma Distribution with Location Parameters       # rgamma3 = function(x, shape=shape, scale=scale, location=location)
    ### parameter
    location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### glomax Distribution             # rglomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(-1, -0.75, -0.5, -0.25, 0.25, 0.5, 0.75, 1)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)

    
    
    ##### Gompertz Distribution         # rgompertz = function (n, min=0.0001, max=1, alpha = 1, beta = 1) 
    ### parameter
    alpha = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)
    

    
    ##### Gompertz-Makeham Distribution         # rgompertzmakeham = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)


    
    ##### half-Cahchy Distribution          # rhcauchy(x, sigma = sigma)
    ### parameter
    sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Half-logistic Distribution            # rhalflogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

        
    
    ##### Chi-square Distribution           # rhnorm(x, sigma = 1)
    ### parameter
    sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)
    
    
    
    ##### Hjorth Distribution               # rhjorth(x, m = 1, s = 1, f = 1)
    ### parameter
    delta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# m (delta)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)
    theta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)	# f (theta)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)


    
    ##### hyperbolicsecant Distribution     # rhyperbolicsecant = function (n, min=-10, max=10, alpha = 1, beta = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### Chi-square Distribution           # rinvgauss(x, m = 1, s = 1)
    ### parameter
    m = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    s = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)


    
    ##### weibull Distribution with gamma Parameters        # rinverseweibull = function (n, min=0.1, max=10, alpha = 0, beta = 1, gamma = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)


    
    ##### laplace Distribution          # rlaplace(x, m = 0, s = 1)
    ### parameter
    m = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    s = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)


    
    ##### 선형 증가 분포          # rlinearFR = function (n, min=0.0001, max=10, alpha = 1, beta = 1) 
    ### parameter
    alpha = c(0, 0.25, 0.5, 0.75, 1, 2, 4, 8)	# m (delta)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)		# s (beta)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Logistic Distribution         # rlogis(x, location, scale)
    ### parameter
    location = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### loglogistic Distribution          # rloglogistic = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### log-normal Distribution           # rlnorm(x, meanlog = meanlog, sdlog = sdlog)
    ### parameter
    meanlog = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    sdlog = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)


    
    ##### logweibull Distribution   
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### lomax Distribution              # rlomax = function (n, min=0.0001, max=1, alpha = 1, beta = 1, gamma = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 1, length.out = 1000)

    
    
    ##### makeham Distribution          # rmakeham(x, shape = theta)
    ### parameter
    theta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Gamma Distribution with alpha Parameters          # rmuth = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Chi-square Distribution           # rnorm(x, mean = mean, sd = sd)
    ### parameter
    mean = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    sd = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### parabolicInvertedUshaped Distribution     # rparabolicInvertedUshaped = function (n, min=-10, max=10, alpha = 0, beta = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### parabolicUshaped Distribution         # rparabolicUshaped = function (n, min=-10, max=10, alpha = 1, beta = 1) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### Pareto Distribution of the first kind     # rpareto1 = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Pareto Distribution of the first kind     # rpower = function (n, min=0.0001, max=10, alpha = 1, beta = 1, gamma = 2) 
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)


    
    ##### rcosine Distribution          # rrcosine = function (n, min=-10, max=10, mu = 0, sigma = 1)
    ### parameter
    mu = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    sigma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### Rayleigh Distribution         # rrayleigh(x, scale = scale)
    ### parameter
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### Reflexted Exponential Distribution        # rrexponential = function (n, min=0.0001, max=10, alpha = 1, beta = 1)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### weibull Distribution with gamma Parameters        # rreflectedweibull = function (n, min=-10, max=10, alpha = 0, beta = 1, gamma = 1)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    gamma = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)


    
    ##### semielliptical Distribution           # rsemielliptical = function (n, min=-1, max=1, alpha = 0, beta = 1)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-1, 1, length.out = 1000)

    
    
    ##### Chi-square Distribution           # rt(x, df = df)
    ### parameter
    df = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### teisser Distribution          # rteisser = function (n, min=-10, max=10, alpha = 0, beta = 1)
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### unif2 Distribution            # runif
    ### parameter
    alpha = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
    beta = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(-10, 10, length.out = 1000)

    
    
    ##### 극치 분포: Gumbel 최대값 분포      # rweibull(x, shape = 1, scale = 1)
    ### parameter
    shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    
    ##### weibull Distribution with Location Parameters         # rweibull3 = function(x, shape=shape, scale=scale, location=location)
    ### parameter
    shape = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    scale = c(0.25, 0.5, 0.75, 1, 2, 4, 8)
    location = c(0.25, 0.5, 0.75, 1, 2, 4, 8)

    ### input varialbe
    x = seq(0.1, 10, length.out = 1000)

    
    return(res)
}
