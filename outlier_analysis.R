require(foreign)
require(MASS)
require("sandwich")
require (datasets)
library(assertr)
library(outliers)
install.packages('fastDummies')
library('fastDummies')
library(olsrr)
library(EnvStats)
library(performance)
library(tidyverse)
library(lavaan)
library(qpcR)
library(lmtest)
library(regclass)
options(warn=-1)
dev.off()
set.seed(123)

# functions
plot_outliers = function(df_input, col, col.name, threshold, y.label) {
  df = df_input
  col = df[,col.name]
  #df$ID = as.numeric(row.names(df))
  
  plot = ggplot(df, aes(x=ID , y=col))+
    geom_point(color=ifelse(abs(col)>abs(threshold[1]),"red","black")) +
    ylab(y.label) +
    geom_hline(yintercept=threshold, color='dark red') +
    geom_text(aes(label=ifelse(abs(col)>abs(threshold[1]),as.character(ID),'')),hjust=0,vjust=0) +
    theme_bw()
  #scale_x_continuous(limits = c(0, 1400))
  #scale_y_continuous(breaks = seq(floor(min(col)), ceiling(max(col)), by = 1))
  
  #outlier.points = as.vector(df[(abs(col)>abs(threshold[1])), 'ID'])
  outlier.points = df[(abs(col)>abs(threshold[1])), c(col.name, 'ID')]
  return(list(plot, outlier.points))
} 

plot_outliers_two_params = function(df_input, col, threshold, y.label) {
  df = df_input
  #df$ID = as.numeric(row.names(df))
  
  plot = ggplot(df, aes(x=ID , y=col))+
    geom_point(color=ifelse((col<threshold[1] | col>threshold[2]),"red","black")) +
    ylab(y.label) +
    geom_hline(yintercept=threshold, color='dark red') +
    geom_text(aes(label=ifelse((col<threshold[1] | col>threshold[2]),as.character(ID),'')),hjust=0,vjust=0)
  #scale_x_continuous(limits = c(0, 1400))
  #scale_y_continuous(breaks = seq(floor(min(col)), ceiling(max(col)), by = 1))
  
  outlier.points = as.vector(df[(col<threshold[1] | col>threshold[2]), 'ID'])
  return(list(plot, outlier.points))
} 

#############################################
# Get data
cdata <- read.csv("/Users/maximchomsikkvy/Documents/College_work/ST4092/FYP_Coding/Data/ST4092 SES2013 Data csv.csv", sep='\t', header = TRUE)
attach(cdata)
sapply(cdata, class)
nrow(cdata)
# Preprocess data
# Chosen variables:
# dep: OverallSWUL
#inDep: V1, V8, V9, V11,
#V12, V14, V15, V16, V19, 
#V21, V22, V23, V24, V25, V26, V27, V28
#V29, V30, V31, V32, V33, V34, GTS_SCORE

dep_var = c('OverallSWUL')
indep_var = c('V1', 'V4', 'V7', 'V8',
              'V12',
              'V16A','V16B',
              'V22', 'V23', 'V25', 'V27', 'V28',
              'V30', 'V33A','V33B',
              'V34A','V34B', 
              'GTS_SCORE','MS_SCORE','AWS_SCORE')
cat_var = c('V1','V4','V7','V8')
ord_var = c('V7',
            'V12',
            'V23', 'V25', 'V27', 'V28',
            'V34A','V34B')
rev_ord_var = c('V16A','V16B','V22','V30',
                'V33A','V33B')
# Remove unneeded variables
cdata = cdata[, c('ID',dep_var,indep_var)]
# Remove rows with NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
cdata = cdata[complete.cases(cdata),]

# Remove outliers
#cdata = cdata[-influence.outliers, ] # influential
#cdata = cdata[!cdata$ID %in% resid.outliers,] # residual
#cdata = cdata[!cdata$ID %in% leverage.outliers,] # leverage]
#cdata = cdata[!cdata$ID %in% outliers,] # all outliers
#cdata = cdata[!cdata$ID %in% c(745),] # leverage


#for (v in ord_var) {
#  cdata[,v] = factor(cdata[,v], ordered=TRUE)
#}
#for (v in rev_ord_var) {
#  cdata[,v] = factor(cdata[,v], ordered=TRUE)
#  cdata[,v] = factor(cdata[,v], levels=rev(levels(cdata[,v])), ordered=TRUE)
#}

# Deal with V16 and V33 (combine them)
cdata = cdata[which(cdata$V16A != 6),] # remove not applicable answer
cdata[cdata$V16B==6, 'V16B'] = NA
cdata[, 'V16A'] = 6-cdata[, 'V16A']; cdata[, 'V16B'] = 6-cdata[, 'V16B']
cdata$V16 <- rowMeans(cdata[,c('V16A','V16B')], na.rm=TRUE)

cdata = cdata[!(cdata$V33A==6 & cdata$V33B==6),] # remove not applicable
cdata[cdata$V33A==6, 'V33A'] = NA; cdata[cdata$V33B==6, 'V33B'] = NA
cdata$V33 <- rowMeans(cdata[,c('V33A', 'V33B')], na.rm=TRUE)

cdata = cdata[ ,!(names(cdata) %in% c('V16A','V16B','V33A','V33B'))]

# Deal with V8 (1: (1) CAO School leaving , 2: (2, 7) Medical students,
# 3: (6) Mature years, 4: (3,4,5,8,9) Other routes)
cdata$V8[cdata$V8 %in% c(2,7)] = 2 ; cdata$V8[cdata$V8 %in% c(3,4,5,8,9)] = 4 ; cdata$V8[cdata$V8 %in% c(6)] = 3

table(cdata[,c('V34A','V34B')])

# (1: doesnt agree at all ... 5: agree a lot)
cdata = dummy_cols(cdata, select_columns = cat_var, remove_selected_columns = TRUE, remove_first_dummy = TRUE)
y.cols = c('OverallSWUL')
x.cols = setdiff(setdiff(colnames(cdata),y.cols),c('ID'))

rownames(cdata) <- 1:nrow(cdata)
cdata$ID = as.numeric(rownames(cdata))
rownames(cdata) <- cdata$ID # reorders index

################################################
################################################
# Ordinary Least Squares Regression
summary(ols <- lm(OverallSWUL ~ ., data = cdata[,c(x.cols,y.cols)]))
p = length(ols$coefficients); n = nrow(cdata)

opar <- par(mfrow = c(1,1), oma = c(0, 0, 1.1, 0))
par(opar)
summary(ols)$coefficients[,c(1,2,4)]
PRESS(ols)
dwtest(ols)
AIC(ols)
##########################
# Identify residual outliers
##########################
#-----------------------
# Residual only outliers
#-----------------------
pred = predict(ols)
outliers = list()

# Ordinary residual
res = resid(ols)
cdata$res = res

ggplot(cdata, aes(x=ID , y= res))+
  geom_point() 

# Standardised residual
stdres = rstandard(ols)
cdata$stdres = stdres
stdres.cutoff = c(-3,3)
cdata$stdres.outlier = abs(stdres)>abs(stdres.cutoff)

select = plot_outliers(cdata, stdres, 'stdres', stdres.cutoff, 'Standardised Residual')
select[1]
outliers$stdres = select[[2]]
# outliers: 115  399  494  745  934 1075 1079 1090 1109 1471 1475

# Studentised residual
stures = studres(ols)
cdata$stures = stures
stures.cutoff = c(-3,3)
cdata$stures.outlier = abs(stures)>abs(stures.cutoff)

select = plot_outliers(cdata, stures, 'stures', stures.cutoff, 'Studentised Residual')
select[1]
outliers$stures = select[[2]]
# outliers: "3054" "3705" "3911" "4505" "4990" "5345" "5363" "5401" "5463" "6483" "6496"
ols_plot_resid_hist(ols)
# Studentised Deleted Residuals
select = ols_plot_resid_stud(ols)

data = select$data
dsr = data$dsr
cdata$dsr = dsr
s=plot_outliers(cdata, dsr, 'dsr' ,c(-3,3), 'Studentised Deleted Residuals')
s[[1]]
outliers$dsr = s[[2]]

# Grubbs test
recursive.grubbs.test = function(df_input) {
  grubb.outliers = c()
  df = df_input
  repeat{
    temp.ols = lm(OverallSWUL ~ ., data = df)
    temp.res = resid(temp.ols)
    grubb = grubbs.test(temp.res)
    p.val = grubb$p.value
    if(p.val > 0.05){
      print(grubb$alternative)
      print(p.val)
      break
    }
    print(p.val)
    i = which.max(abs(temp.res))[[1]]
    name = row.names(df[i, ])
    grubb.outliers = append(grubb.outliers, name)
    df = df[-i, ]
  }
  return(as.numeric(grubb.outliers))
}
grubb = grubbs.test(res)
outliers$grubb = recursive.grubbs.test(cdata[,names(cdata) %in% c(x.cols,y.cols)])
cdata[, 'ID'] %in% outliers$grubb
df = cdata[!cdata$ID %in% outliers$grubb,]
select = plot_outliers(df, stures, 'stures', stures.cutoff, 'Studentised Residual')
select[[1]]

# rosner test
rosner = rosnerTest(cdata$res, k=10)
rosner
select = rosner$all.stats
outliers$rosner = select[select$Outlier==TRUE, 'Obs.Num']
outliers

##########################
# Identify leverage outliers
##########################

# Mahalanobis Distance
# Calculate Mahalanobis with predictor variables
df <- cdata[, names(cdata) %in% x.cols]    
MD <- maha_dist(df)
m.cutoff = 1*qchisq(df=p, p = 1-(0.05/n))
cdata$MD <- round(MD, 1)


cdata$MD.outlier = abs(MD)>abs(m.cutoff)

s=plot_outliers(cdata, MD, 'MD', m.cutoff, 'Mahalanobis Distance')
s[1]
outliers$MD = s[[2]]

# Hat diagonal values
hat = lm.influence(ols)$hat
cdata$hat = hat
hat.cutoff = 3*(p/n)

cdata$hat.outlier = abs(hat)>abs(hat.cutoff)

s = plot_outliers(cdata, hat, 'hat', hat.cutoff, 'Hat Diagonal')
s[1]
outliers$hat = s[[2]]

##########################
# Identify influence outliers
##########################
# Cooks Distance
cook = cooks.distance(ols)
cdata$cook = cook
cook.cutoff = pf(df1 = p, df2 = n-p, q=0.5)
#cook.cutoff = 4/n
cdata$cook.outlier = abs(cook)>abs(cook.cutoff)

s=plot_outliers(cdata, cook, 'cook' ,cook.cutoff, 'Cooks Distance')
s[[1]]
outliers$cook = s[[2]]

# COVRATIO
covr = covratio(ols)
cdata$covr = covr
covr.cutoff = c(0.85,1.15) #c(-7*((p+1)/n)+1,7*((p+1)/n)+1)
cdata$covr.outlier = (covr<covr.cutoff[1] | covr>covr.cutoff[2])

select = plot_outliers_two_params(cdata, covr, covr.cutoff, 'CovRatio')
select[[1]]
outliers$covr = select[[2]]

# DFBETAS
dfbs = dfbetas(ols)
dfbs.cutoff = 2/sqrt(n)
#ols_plot_dfbetas(ols)
#dfbs.outliers = list()
#for (col in colnames(dfbs)) {
#  v = dfbs[,col]
#  dfbs.outliers$col = v[abs(v)>dfbs.cutoff]
#}
dfbs.outliers = rowSums(dfbs > dfbs.cutoff)
dfbs.outliers[order(dfbs.outliers, decreasing = TRUE)][1:18]
table(dfbs.outliers)
dfbs.cutoff[2,]
dfbs[,'MS_SCORE']

# DFFit
dffit = dffits(ols)
cdata$dffit = dffit
dffit.cutoff = 3*sqrt((p+1)/n)
cdata$dffit.outlier = abs(dffit)>abs(dffit.cutoff)

s=plot_outliers(cdata, dffit, 'dffit',c(-dffit.cutoff,dffit.cutoff), 'DFFITS')
s[[1]]
nrow(outliers$dffit) = s[[2]]

pl = ols_plot_dffits(ols)
pl$data

# Hadi influence
ols_plot_hadi(ols)
HM = ols_hadi(ols)$hadi
cdata$HM = HM
madHM = median(abs(HM-median(HM)))/0.6745
HM.cutoff = 0.3 #median(HM) + 3*madHM

s = plot_outliers(cdata, HM, 'HM', HM.cutoff, 'Hadi')
s[[1]]  

sort(table(unlist(outliers)))
outliers$stdres

# get all points in each type of outlier
dfbetas.outliers = c(1140) # extra leverage points from visually looking at dfbetas plots

cdata$influence.outlier = (cdata$cook.outlier==TRUE)# | (cdata$covr.outlier==TRUE) | (cdata$dffit.outlier==TRUE) | cdata$ID==dfbetas.outliers
cdata$resid.outlier = (cdata$stdres.outlier==TRUE)
cdata$leverage.outlier = (cdata$hat.outlier==TRUE) | (cdata$MD.outlier==TRUE)
cdata$outlier = (cdata$influence.outlier==TRUE) | (cdata$resid.outlier==TRUE) | (cdata$leverage.outlier==TRUE)

influence.outliers = as.vector(cdata[(cdata$influence.outlier==TRUE), 'ID'])
resid.outliers = as.vector(cdata[(cdata$resid.outlier==TRUE), 'ID'])
leverage.outliers = as.vector(cdata[(cdata$leverage.outlier==TRUE), 'ID'])
all.outliers = as.vector(cdata[(cdata$outlier==TRUE), 'ID'])


temp = cdata[cdata$resid.outlier==TRUE, c('ID','stdres','hat','cook','covr','dffit')]

#############################
# plot residual vs leverage vs influence
df = cdata
#df$ID = as.numeric(row.names(df))
plot = ggplot(df, aes(x=hat, y=stdres, size=cook)) +
  geom_point(color=ifelse( (abs(df$cook)>abs(cook.cutoff) ),"red",ifelse( ((abs(df$stdres)>abs(stdres.cutoff) | (abs(df$hat)>abs(hat.cutoff))) & abs(df$cook)<=abs(cook.cutoff)),"black","black"))) +
  ylim(-6, 6) +
  ylab('Standardised Residual') +
  xlab('Diagonal Hat Value') +
  geom_hline(yintercept=stdres.cutoff, color='dark red', linetype='longdash') +
  geom_vline(xintercept=hat.cutoff, color='dark red', linetype='longdash') +
  geom_text(aes(label=ifelse( df$outlier==TRUE, as.character(ID), ''),hjust=0,vjust=0)) +
  theme_bw()
plot
legend( x = "topleft",
        legend = c("Non-Influential","Influential"),
        col = c("black","red"), lwd = 2, lty = c(0,0),
        pch = c(19,19) )

cdata$influence.outlier = (cdata$cook.outlier==TRUE)
cdata$resid.outlier = (cdata$stdres.outlier==TRUE)
cdata$leverage.outlier = (cdata$hat.outlier==TRUE) | (cdata$MD.outlier==TRUE)
cdata$outlier = (cdata$influence.outlier==TRUE) | (cdata$resid.outlier==TRUE) | (cdata$leverage.outlier==TRUE)

influence.outliers = as.vector(cdata[(cdata$influence.outlier==TRUE), 'ID'])
resid.outliers = as.vector(cdata[(cdata$resid.outlier==TRUE), 'ID'])
leverage.outliers = as.vector(cdata[(cdata$leverage.outlier==TRUE), 'ID'])
all.outliers = as.vector(cdata[(cdata$outlier==TRUE), 'ID'])

# results after removing each type of outlier
# no removal:
#   sigma: 4.962
#   Adj R squared: 0.4509
#   Log Likelihood: -4470.627
# residual: 
#   sigma: 4.651
#   Adj R squared: 0.5009
# leverage: 
#   sigma: 4.931
#   Adj R squared: 0.4549
# influence: 
#   sigma: 4.66305
#   Adj R squared: 0.4966
# remove all: 
#   sigma: 4.616
#   Adj R squared: 0.5059

######################
# robust regression summaries
# huber function
#   sigma: 4.439
#   Log Likelihood: -4475.815
# bisquare function
#   sigma: 4.4097
# hampel function
#   sigma: 4.452717
# MM estimation
#   sigma: 4.438764
# S estimation
#   sigma: 4.991066
# LAD
#   sigma: ?
#   Log Likelihood: -4468.507
##################################################
###################################################
# bootstrap ols method error
dat = cdata[c(x.cols,y.cols)]
x = dat[x.cols]
y = dat$OverallSWUL
n = nrow(dat)

K=10  # using 5 folds
mse = pred.err = slope = numeric(K)
folds = cut(1:n, K, labels=FALSE)
for(k in 1:K){
  # pick all folds but the k-th fold for training
  i.train = which(folds!=k)
  o = lm(y~., data=x, subset=i.train)
  yh = o$fitted.values
  slope[k] = coef(o)[2]
  # corresponding model fit MSE
  mse[k] = mean((yh-y[i.train])^2)
  # pick k-th fold for testing
  i.test = which(folds==k)
  yp = predict(o, newdata=x[i.test, ,drop=FALSE])
  # corresponding prediction error estimate
  pred.err[k] = mean((yp-y[i.test])^2)
}
mean(pred.err)

# OLS (with unusual cases) [good]
ols <- lm(OverallSWUL ~ ., data = cdata[,c(x.cols,y.cols)])
summary(ols)$df
pt(q=3.855, df=1250, lower.tail=FALSE)*2

# OLS (without unusual cases) [good]
ols <- lm(OverallSWUL ~ ., data = cdata[-all.outliers,c(x.cols,y.cols)])
summary(ols)$sigma
AIC(ols)
summary(ols)

# Robust Regression (huber estimation) [good]
summary(rr.huber <- rlm(OverallSWUL ~ ., data = cdata[,c(x.cols[c(1:21,22,23:25)],y.cols)]))
rr.huber$w # weights of each case
rr.huber$s # sigma (residual standard error) summary(rr.huber)$sigma
logLik(rr.huber); AIC(rr.huber)
summary(rr.huber)
pt(q=3.855, df=1250, lower.tail=FALSE)*2

weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.huber$resid, weight = rr.huber$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:50, ]

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(rr.huber, las = 1)

dat = cdata[c(x.cols,y.cols)]
x = dat[x.cols]
y = dat$OverallSWUL
n = nrow(dat)

K=10  # using 5 folds
mse = pred.err = slope = numeric(K)
folds = cut(1:n, K, labels=FALSE)
for(k in 1:K){
  # pick all folds but the k-th fold for training
  i.train = which(folds!=k)
  o = rlm(y~., data=x, subset=i.train)
  yh = o$fitted.values
  slope[k] = coef(o)[2]
  # corresponding model fit MSE
  mse[k] = mean((yh-y[i.train])^2)
  # pick k-th fold for testing
  i.test = which(folds==k)
  yp = predict(o, newdata=x[i.test, ,drop=FALSE])
  # corresponding prediction error estimate
  pred.err[k] = mean((yp-y[i.test])^2)
}
mean(pred.err)

mean( (predict(ols)-cdata$OverallSWUL)^2 ) # MSE


# Robust Regression (bisquare estimation) [good]
rr.bisquare <- rlm(OverallSWUL ~ ., data=datb[,c(x.cols[c(1:21,23:25)],y.cols)], psi = psi.bisquare)
AIC(rr.bisquare)

rr.bisquare$w # weights of each case
rr.bisquare$s # sigma (residual standard error)

weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.bisquare$resid, weight = rr.bisquare$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:50, ]

mean( (predict(rr.bisquare)-cdata$OverallSWUL)^2 )


# Robust Regression (hampel estimation) [good]
rr.hampel <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols[c(1:21,23:25)],y.cols)], psi = psi.hampel)
AIC(rr.hampel)
rr.hampel$w # weights of each case
rr.hampel$s # sigma (residual standard error)

weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.hampel$resid, weight = rr.hampel$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:50, ]

mean( (predict(rr.hampel)-cdata$OverallSWUL)^2 )

# Robust Regression (Andrew sine estimation) [good]
library(HoRM)
rr.andrew <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols[c(1:21,23:25)],y.cols)], psi = psi.andrew)
AIC(rr.andrew)
rr.andrew$w # weights of each case
rr.andrew$s # sigma (residual standard error)

weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.andrew$resid, weight = rr.andrew$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:50, ]

mean( (predict(rr.andrew)-cdata$OverallSWUL)^2 )

# Robust Regression (MM estimation)
rr.mm <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols[c(1:21,23:25)],y.cols)], method = 'MM', psi=psi.huber)
AIC(rr.mm)
rr.mm$w # weights of each case
rr.mm$s # sigma (residual standard error)

weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.mm$resid, weight = rr.mm$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:50, ]

mean( (predict(rr.mm)-cdata$OverallSWUL)^2 )
rr.mm$residuals

# S estimator [good]
library(robustbase)
rr.s <- lmrob(OverallSWUL ~ ., data=cdata[,c(x.cols[c(1:21,23:25)],y.cols)], method='S')
#rr.s = lmrob.S(x=cdata[,c(x.cols)], y=cdata[,c(y.cols)],
#               control = lmrob.control(nRes = 20))
summary(rr.s)
summary(rr.s)$sigma
rr.s$residuals
rr.s$rweights[order(rr.s$rweights)]

weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.s$resid, weight = rr.s$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:50, ]

mean( (predict(rr.s)-cdata$OverallSWUL)^2 )

# GM estimator is resistant to leverage
rr.gm.mallows = glmrob(OverallSWUL~., family = gaussian, data=cdata[,c(x.cols,y.cols)], 
                       method = 'Mqle', weights.on.x = 'hat') # method 1
x.h <- rr.gm.mallows$w.x**2 # 1-2*hat

rr.gm.schweppe <- rlm(OverallSWUL~., data=cdata[,c(x.cols,y.cols)], weights=x.h, wt.method="case") # method 2
AIC(rr.gm.schweppe)
weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.gm.schweppe$resid, weight = rr.gm.schweppe$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:10, ]

rr.gm.schweppe$w
rr.gm.schweppe$s
mean( (predict(rr.gm.schweppe)-cdata$OverallSWUL)^2 )

t = summary(rr.gm.schweppe)$coefficients
cbind(t, round(pt(q=abs(t[,3]), df=1263, lower.tail=FALSE)*2, 6))

# Least Absolute Deviation [good]
library(L1pack)
rr.lad <- lad(OverallSWUL ~ ., data=datb[,c(x.cols[c(1:21,23:25)],y.cols)], method='EM')
AIC(rr.lad)
summary(rr.lad)
sigma(rr.lad) # residual standard error?

rr.lad$scale # sigma (residual standard error)

weights <- data.frame(SWUL = cdata$OverallSWUL, resid = rr.lad$resid, weight = rr.lad$w, ols.std.resid = stdres, ols.hat = hat, ols.cook = cook)
weights2 <- weights[order(weights$weight), ]
weights2[1:50, ]

mean(abs(cdata$OverallSWUL-predict(rr.lad)))

# LASSO regression
library(glmnet)
x = as.matrix(dat[x.cols])
y = dat$OverallSWUL

lambdas <- 10^seq(2, -3, by = -.1)
lasso_reg <- cv.glmnet(x, y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 
lasso_model <- glmnet(x, y, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions <- predict(lasso_model, s = lambda_best, newx = x)

# Least Trimmed Squares
library(robustbase)
rr.lts <- ltsReg(OverallSWUL ~ ., data=cdata[,c(x.cols[1:7],y.cols)], nsamp=50000)
#rr.lts <- lqs(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], method = "lts", nsamp = "exact")

# Least Median Squares (only single regression)
library(gamlss)
rr.lms = lms(OverallSWUL, MS_SCORE, data=cdata)

# Theil sen regression (only works for single regression)
library(mblm)
mblm(cdata[,y.cols] ~ cdata[,x.cols])
rr.mblm = mblm(OverallSWUL ~ V7+V12, dataframe = cdata[, c(x.cols, y.cols)])
x <- 1:100+rnorm(100)
y <- x+rnorm(100)
y[100] <- 200
fit <- mblm(y~x)
fit$coefficients

# RANSAC (Random sample consensus) regression [usually not used in stats]


# Repeated Median Regression (only single regression)
library(RobustLinearReg)
rr.siegel = siegel_regression(OverallSWUL ~ V12, data = cdata[, c(x.cols, y.cols)])

############################################################
#############################################################
# test mse when only training data contains outliers
train.dat.length = 600
dat = cdata[c( all.outliers, sample(setdiff(cdata$ID,all.outliers)) ), ]
o = rlm(OverallSWUL~., data=dat[,c(x.cols,y.cols)], subset = 1:train.dat.length, method='MM')
predicted = predict(object = o, dat[(train.dat.length+1):nrow(dat), x.cols])
mse = mean((dat[(train.dat.length+1):nrow(dat), y.cols] - predicted)^2)
mse

# in for loop
#data = cdata; datb = cdata[setdiff(cdata$ID,all.outliers), ]
loops = 100; train.dat.length = 400; msel = c()
for (i in 1:loops) {
  #dat = cdata[c( all.outliers, sample(setdiff(cdata$ID,all.outliers)) ), ] # with outliers
  dat = cdata[sample(setdiff(cdata$ID,all.outliers)), ] # without outliers
  
  #o <- lm(OverallSWUL ~ ., data=dat[,c(x.cols,y.cols)], subset = 1:train.dat.length) # ols
  #o <- rlm(OverallSWUL ~ ., data=dat[1:train.dat.length,c(x.cols[c(1:21,23:25)],y.cols)]) # M Huber
  #o <- rlm(OverallSWUL ~ ., data=dat[,c(x.cols[c(1:21,23:25)],y.cols)], subset = 1:train.dat.length, psi = psi.bisquare) # M bisquare
  #o <- rlm(OverallSWUL ~ ., data=dat[,c(x.cols[c(1:21,23:25)],y.cols)], subset = 1:train.dat.length, psi = psi.hampel) # M hampel
  #o <- rlm(OverallSWUL ~ ., data=dat[,c(x.cols[c(1:21,23:25)],y.cols)], subset = 1:train.dat.length, psi = psi.andrew) # M Andrew sine
  #o <- rlm(OverallSWUL ~ ., data=dat[,c(x.cols[c(1:21,23:25)],y.cols)], subset = 1:train.dat.length, method = 'MM', psi=psi.huber) # mm-est
  #o <- lmrob(OverallSWUL ~ ., data=dat[,c(x.cols[c(1:21,23:25)],y.cols)], subset = 1:train.dat.length, method='S') # S-estimation
  #o <- rlm(OverallSWUL ~ ., data=dat[,c(x.cols[c(1:21,23:25)],y.cols)], subset = 1:train.dat.length, weights=x.h, wt.method="case") # Schweppe GM-est
  o <- lad(OverallSWUL ~ ., data=dat[,c(x.cols[c(1:21,23:25)],y.cols)], subset = 1:train.dat.length, method='EM') # LAD

  predicted = predict(object = o, dat[(train.dat.length+1):nrow(dat), x.cols])
  mse = mean((dat[(train.dat.length+1):(nrow(dat)), y.cols] - predicted)^2)
  msel = c(msel, mse)
}
mean(msel)


# test time taken by model
loops = 300 ; times = c()
for (i in 1:loops) {
  start.time <- Sys.time()
  #o <- lm(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)]) # ols
  #o <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)]) # M Huber
  #o <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], psi = psi.bisquare) # M bisquare
  #o <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], psi = psi.hampel) # M hampel
  #o <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], psi = psi.andrew) # M Andrew sine
  #o <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], method = 'MM', psi=psi.huber) # mm-est
  #o <- lmrob(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], method='S') # S-estimation
  #o <- rlm(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], weights=x.h, wt.method="case") # Schweppe GM-est
  o <- lad(OverallSWUL ~ ., data=cdata[,c(x.cols,y.cols)], method='EM') # LAD
  end.time <- Sys.time()
  
  time.taken <- end.time - start.time; time.taken
  times = c(times, time.taken)
}
mean(times)*1000
hist(cdata$OverallSWUL)

# plot residual vs leverage vs weights
df = cdata
df$weights = rr.huber$w
cdata$OverallSWUL

plot = ggplot(df, aes(x=hat, y=stdres, size=weights)) +
  geom_point() +
  geom_point(color=ifelse(df$cook.outlier, 'red', 'black')) +
  ylim(-6, 6) +
  #geom_point(color=ifelse( (abs(df$cook)>abs(cook.cutoff) ),"red",ifelse( ((abs(df$stdres)>abs(stdres.cutoff) | (abs(df$hat)>abs(hat.cutoff))) & abs(df$cook)<=abs(cook.cutoff)),"black","black"))) +
  ylab('Standardised Residual') +
  xlab('Diagonal Hat Value') +
  geom_hline(yintercept=stdres.cutoff, color='dark red', linetype='longdash') +
  geom_vline(xintercept=hat.cutoff, color='dark red', linetype='longdash') +
  geom_text(aes(label=ifelse( df$outlier, as.character(ID), ''),hjust=0,vjust=0)) +
  theme_bw() +
  ggtitle("Case weights in M-estimation (Huber estimation)")
plot
legend( x = "topright",
        legend = c("Non-Influential","Influential"),
        col = c("black","red"), lwd = 2, lty = c(0,0),
        pch = c(19,19) )

# compare coefficients of models
data.frame(rr.bisquare$coefficients, ols$coefficients)

############################################################
#############################################################
# Visualise Weights (Heatmap?)
dev.off()
k = 1.345
r.seq = seq(-4, 4, length=100)
l.seq = seq(0, 0.2, length=100)
ols.mse=mse(ols)
weight_function = function(r, k=1.345) {
  return( ifelse(abs(r)<=k, 1, k/abs(r)) )
} 
w.seq = weight_function(r=r.seq)
plot(x=r.seq, y=w.seq, type='l')

######################################
# Cook function visual
library(gplots)
library(reshape2)
cook.function = function(r, h,mse=ols.mse) {
  D = (r^2/(p*mse))*(h/(1-h)^2)
  return(D)
} 

cook.matrix = outer(r.seq, l.seq, cook.function)
rownames(cook.matrix) = r.seq ; colnames(cook.matrix) = l.seq

melted_cook <- melt(cook.matrix)
ggplot(data = melted_cook, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "red")

# 3d plot
library(plotly)
kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
fig <- plot_ly(x = l.seq, y = r.seq, z = cook.matrix) %>% 
  add_surface() %>%
  layout(title="Diagnostics graph", xaxis=list(title = list(text ='Sepal.Length')))
fig
length(kd$x)

############################################
############################################
# Testing stuff
summary(rr.mm)

p = ncol(cdata) - 1
pt(p = -3, df = n-p-1)
pnorm(q = 5) #- pnorm(q = -3)
qnorm(p=0.9987)

d1 <- cooks.distance(ols)
d1[is.na(d1)] = 0
r <- stdres(ols)
sum(is.na(d1))
a <- cbind(cdata, d1, r)
n = nrow(cdata)
a[d1 > 4/n, ]
d1
summary(ols)$sigma
n = nrow(cdata)
h<-lm.influence(ols)$hat
h.cutoff = 2*(length(cdata)/n)
rpear <-residuals(ols,"pearson")/sqrt(1-h)
rdev <-residuals(ols,"deviance")/sqrt(1-h)
phat<- ols$fitted.values
D<-rpear*rpear*h/(4*(1-h))
UsingFitted <- n*phat
Diagnostics.df <- data.frame(y, UsingFitted, n, phat, h, rpear, rdev)
Diagnostics.df

plot(rpear,main="Plot of Pearson Residuals v Linear Predictor")
plot(MD)
abline(0.1, lty=1)
plot(D)


# leverage cutoff = 2(3)/21 = 6/21 = 0.286
ObsLogit<-log((respond/n)/(1-(respond/n)))

PredLogit<-log((fitted.values(glm1))/(1-(fitted.values(glm1))))

plot(x,ObsLogit,pch=16)
points(x,PredLogit)
abline(lm(PredLogit~x))

pnorm(1)

outlier.points = as.vector(df[(abs(col)>abs(threshold[1])), 'ID'])
df
install.packages('officer')
install.packages('flextable')
install.packages('magrittr')

library(officer)
library(flextable)
library(magrittr)

temp = cdata[(cdata$resid.outlier==TRUE) & (cdata$leverage.outlier==TRUE) & (cdata$influence.outlier==TRUE), c('ID','stdres','hat','cook','covr','Outlier.type')]
temp
# Create flextable object
library(flextable)
ft <- flextable(data = round_df(temp, 4, rf = "round")) %>% 
  theme_zebra %>% 
  autofit

# See flextable in RStudio viewer
# Create a temp file
tmp <- tempfile(fileext = ".docx")

# Create a docx file
library(officer)
read_docx() %>% 
  body_add_flextable(ft) %>% 
  print(target = tmp)

# open word document
browseURL(tmp)
covr.cutoff
paste('dd ', ifelse(cdata$resid.outlier==TRUE, 'Residual', ''))
cdata
library('forestmangr')
round_df(temp, 4, rf = "round")
cdata$Outlier.type = ''
cdata$Outlier.type = paste(cdata$Outlier.type, ifelse(cdata$resid.outlier==TRUE, 'Residual ', ''), sep = '')
cdata$Outlier.type = paste(cdata$Outlier.type, ifelse(cdata$leverage.outlier==TRUE, 'Leverage ', ''), sep = '')
cdata$Outlier.type = paste(cdata$Outlier.type, ifelse(cdata$influence.outlier==TRUE, 'Influence ', ''), sep = '')

cdata[cdata$resid.outlier==TRUE,]

temp = cdata[cdata$resid.outlier==TRUE, c('ID','stdres','hat','cook','covr','dffit')]

# Dimension Reduction
survey = cdata
survey$OverallSWUL = NULL

pca <- prcomp(survey, scale. = TRUE)
pca_1_2 <- data.frame(pca$x[, 1:2])
plot(pca_1_2$PC1, pca_1_2$PC2)
text(pca_1_2$PC1,pca_1_2$PC2-0.2,labels=rownames(pca_1_2),cex=0.6, col = 'black')

PC1 <- pca$rotation[,1]
PC1_scores <- abs(PC1)
PC1_scores_ordered <- sort(PC1_scores, decreasing = TRUE)
names(PC1_scores_ordered)

df=cdata
plot = ggplot(df, aes(x=MS_SCORE, y=GTS_SCORE, size=OverallSWUL))+
  geom_point() +
  ylab('Standardised Residual') +
  xlab('Diagonal Hat Value') +
  geom_hline(yintercept=stdres.cutoff, color='dark red', linetype='longdash') +
  geom_vline(xintercept=h.cutoff, color='dark red', linetype='longdash') +
  geom_text(aes(label=ifelse( (abs(cook)>abs(cook.cutoff)), as.character(ID), ''),hjust=0,vjust=0))
cdata
x.cols
paste(shQuote(x.cols), collapse=" + ")
V7+V12+V16A+V16B+V22+V23+V25+V27+V28+V30+V33A+V33B+V34A+V34B+GTS_SCORE+MS_SCORE+AWS_SCORE+V1_2+V4_4+V4_5+V4_6+V8_2+V8_3+V8_4+V8_5+V8_6+V8_7+V8_8+V8_9

eqn = function(e){sin(e/1.339)/(e/1.339)}
curve(eqn, from=-6, to=2*pi, n=200, xlab="Y", ylab="X", col="blue",lwd=2, 
      main="Plot of  Y = sin(X)+cos(X) ")


variable = cdata$V34B
hist(variable)
plot(table(variable))
summary(variable)
sd(variable)


# 2 variable regression
df = car90
df = df[!with(df,is.na(df$Price) | is.na(df$HP)),]
df = df[1:6,]

ols1 = lm(Price ~ HP, data=df)
plot(df$HP, df$Price, pch=16, main='M-estimation regression with outlier', xlab='HP', ylab='Price ($)',ylim=c(10000,85000))
abline(ols1, col='blue', lwd=2, lty=2)

#df$residual = rstandard(ols)
#full_df = df
#df = df[abs(df$residual)<1,]

df[1,'Price'] = 80000; df[1,'HP'] = 180
points(c(180),c(80000),col='red',pch=16, cex=1.5)
ols2 = lm(Price ~ HP, data=df)
abline(ols2, col='red', lwd=2, lty=2)

#which.max(full_df$residual)
#full_df[84,]
#df = rbind(df, full_df[84,])

huber = rlm(Price ~ HP, data=df)
abline(huber, col='red', lwd=2, lty=2)


# visual
dev.off()
ols1 = lm(OverallSWUL ~ MS_SCORE, data=cdata)
plot(cdata$MS_SCORE, cdata$OverallSWUL, pch=16, main='M-estimation regression with outlier', xlab='HP', ylab='Price ($)',ylim=c(0,40))
abline(ols1, col='blue', lwd=2, lty=2)

huber = rlm(OverallSWUL ~ GTS_SCORE, data=cdata)
abline(huber, col='red', lwd=2, lty=2)
sum(cdata$OverallSWUL>20)/nrow(cdata)
