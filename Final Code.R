#load the original datasets#
load("/Users/curry/Data Science/Interpretation of Data II/Final/finalprojectdata.R")

#denote the training data and testing data seperately#
nm = unlist(strsplit(colnames(Khan)[-1],"\\."))[2*(1:88)]
testset = is.na(match(substring(colnames(Khan)[-1],1,2),"TR"))*1
table(testset,nm)

#eliminate the 'NA' sample, and build it as dataset 'Khan1'#
Khan1 = Khan
list1 = list()
for (t in 1:88) {
    if (nm[t] == "NA") {
        list1[[length(list1) + 1]] <- t
    }
}
rm = unlist(list1)
Khan1[,rm[1]+1] = Khan1[,rm[2]+1] = Khan1[,rm[3]+1] = Khan1[,rm[4]+1] = Khan1[,rm[5]+1] = NULL


#denote the training data and testing data in 'Khan1' seperately#
nm1 = unlist(strsplit(colnames(Khan1)[-1],"\\."))[2*(1:83)]
testset1 = is.na(match(substring(colnames(Khan1)[-1],1,2),"TR"))*1
table(testset1,nm1)

#denote the class name numerically#
library(glmnet)
y0 = cbind(nm1 == "EW", nm1 == "BL",nm1 == "NA", nm1 == "NB",nm1 == "RM")*1
ytr= y0[testset1==0,]

#seperate the training data and testing data#
xtr = Khan1[,-1][,testset1==0]
xtst = Khan1[,-1][,testset1==1]
n = nrow(xtr)
ns=2000

y1 = cbind(nm1 == "RM")*1
ytr1= y1[testset1==0,]
y2 = cbind(nm1 == "EW")*1
ytr2= y2[testset1==0,]
y3 = cbind(nm1 == "BL")*1
ytr3= y3[testset1==0,]
y4 = cbind(nm1 == "NB")*1
ytr4= y4[testset1==0,]

#calculate weight#
 nmtr = nm1[testset1==0]
 pvs = rep(0,n)
  for(i in 1: n) {
    xtr=as.matrix(xtr)
    pvs[i]= anova(lm(xtr[i,]~factor(nmtr)))$Pr[1]
    qvs =pvs/rank(pvs)*n
    w = -log(qvs)
  }

#sample variables#
ns=2000; n = 100
samps = array(0,dim=c(ns,n))
for(i in 1:ns) 
{
   samps[i,]= sample(nrow(xtr),100,rep=F,prob=w/sum(w))
}

#calculate the probability matrix 2000 times, 100 samples each time#
n = 2308
prytr = prytest= NULL
prxt1 = rep(0,n)
prn1 = rep(0,n)
prxt2 = rep(0,n)
prn2 = rep(0,n)
prxt3 = rep(0,n)
prn3 = rep(0,n)
prxt4 = rep(0,n)
prn4 = rep(0,n)
for(i in 1:ns) { 
    
    samps[i,]=sample(nrow(xtr),100,rep=F,prob=w/sum(w))
    x = as.matrix(xtr[j<-samps[i,],])
    x2=as.matrix(xtst[j<-samps[i,],])
    x=t(x)
    x2=t(x2)
    cvx1 = cv.glmnet(x,ytr1, family="binomial",alpha=0.9)
    lam1 = c(cvx1$lambda.min,cvx1$lambda.1se)
    lam1 = c(lam1,mean(lam1))
    dmod1 = glmnet(x,ytr1, family="binomial",alpha=1,lambda=lam1)
    bb1 = dmod1$beta
    bbi1 = apply(bb1,1,function(x)any(x!=0))
    table (bbi1)
    k<-as.numeric(rownames(bb1[apply(bb1,1,function(x)any(x!=0)),]))
    prxt1[k] = prxt1[k]+1
    prn1[j] = prn1[j] + 1
    
    fit1=predict(dmod1,type="response",newx=x)[,3]
    fit1_tst=predict(dmod1,type="response",newx=x2)[,3]
    

    cvx2 = cv.glmnet(x,ytr2, family="binomial",alpha=0.9)
    lam2 = c(cvx2$lambda.min,cvx2$lambda.1se)
    lam2 = c(lam2,mean(lam2))
    dmod2 = glmnet(x,ytr2, family="binomial",alpha=1,lambda=lam2)
    bb2 = dmod2$beta
    bbi2 = apply(bb2,1,function(x)any(x!=0))
    table (bbi2)
    k<-as.numeric(rownames(bb2[apply(bb2,1,function(x)any(x!=0)),]))
    prxt2[k] = prxt2[k]+1
    prn2[j] = prn2[j] + 1

    fit2=predict(dmod2,type="response",newx=x)[,3]
    fit2_tst=predict(dmod2,type="response",newx=x2)[,3]

    cvx3 = cv.glmnet(x,ytr3, family="binomial",alpha=0.9)
    lam3 = c(cvx3$lambda.min,cvx3$lambda.1se)
    lam3 = c(lam3,mean(lam3))
    dmod3 = glmnet(x,ytr3, family="binomial",alpha=1,lambda=lam3)
    bb3 = dmod3$beta
    bbi3 = apply(bb3,1,function(x)any(x!=0))
    table (bbi3)
    k<-as.numeric(rownames(bb3[apply(bb3,1,function(x)any(x!=0)),]))
    prxt3[k] = prxt3[k]+1
    prn3[j] = prn3[j] + 1

   if(length(k)>0) {
    fit3=predict(dmod3,type="response",newx=x)[,3]
    fit3_tst=predict(dmod3,type="response",newx=x2)[,3]
   } else {
    fit3 = rep(8/63,63)
    fit3_tst= rep(8/63,20)
   } #there are some situations in class "BL" that probabilities cannot be calculated, we use average probability to replace them#

    cvx4 = cv.glmnet(x,ytr4, family="binomial",alpha=0.9)
    lam4 = c(cvx4$lambda.min,cvx4$lambda.1se)
    lam4 = c(lam4,mean(lam4))
    dmod4 = glmnet(x,ytr4, family="binomial",alpha=1,lambda=lam4)
    bb4 = dmod4$beta
    bbi4 = apply(bb4,1,function(x)any(x!=0))
    table (bbi4)
    k<-as.numeric(rownames(bb4[apply(bb4,1,function(x)any(x!=0)),]))
    prxt4[k] = prxt4[k]+1
    prn4[j] = prn4[j] + 1
  
    fit4=predict(dmod4,type="response",newx=x)[,3]
    fit4_tst=predict(dmod4,type="response",newx=x2)[,3]
    
    
}


#combine the probability in one matrix#
prob_fit=cbind(fit1,fit2,fit3,fit4)
prob_tst=cbind(fit1_tst,fit2_tst,fit3_tst,fit4_tst)

nm1 = unlist(strsplit(colnames(Khan1)[-1],"\\."))[2*(1:63)]
tr_y=as.numeric(factor(nm1))

nm_tst1 = unlist(strsplit(colnames(Khan1)[-1],"\\."))[2*(64:83)]
tr_y_tst=as.numeric(factor(nm_tst1))

##########################################LDA
#You need to train LDA on the probabilities for the 4 cancers in training set. 
#For this LDA, x is the probabilities for 4 cancers and y is the actual cancer. 
#This trained LDA will take (p1, p2, p3, p4) as input and outputs one of the cancers. 
#You need to use this trained LDA on the probabilities of 4 cancers in testing set to get predictions.
library("MASS")
r <- lda(prob_fit,tr_y, prior = c(1,1,1,1)/4)
v=list(v1= r$scaling[,1],v2= r$scaling[,2])
a=predict(r,data.frame(prob_fit),type="response")$class
b=predict(r,data.frame(prob_tst),type="response")$class
tr_y_tst

#########################################Test Accuracy######
tst_acc= 1-sum (b!=tr_y_tst)/length(b)

#calculate score of each variables seperately in 4 classification#
score1 = prxt1/(prn1 + 1)
score2 = prxt2/(prn2 + 1)
score3 = prxt3/(prn3 + 1)
score4 = prxt4/(prn4 + 1)
names(score1) = names(score2) = names(score3) = names(score4) = c(1:2308)
score1 = sort(score1)
score2 = sort(score2)
score3 = sort(score3)
score4 = sort(score4)
#extract 30 best variables in each classification#
s1 = score1[2278:2308]
s2 = score2[2278:2308]
s3 = score3[2278:2308]
s4 = score4[2278:2308]
n1 = names(s1)
n2 = names(s2)
n3 = names(s3)
n4 = names(s4)
#find the common variables in the best 30 variables in each classification#
l = list()
for (i in 1:30) {
    for (j in 1:30) {
        if (n1[i] == n2[j]) {
            l[length(l) + 1] = n2[j]
        } else if (n1[i] == n3[j]) {
            l[length(l) + 1] = n3[j] 
          } else if (n1[i] == n4[j]) {
              l[length(l) + 1] = n4[j] 
            }
    }
}
for (i in 1:30) {
    for (j in 1:30) {
        if (n2[i] == n3[j]) {
            l[length(l) + 1] = n3[j]
        } else if (n2[i] == n4[j]) {
            l[length(l) + 1] = n4[j] 
        }
    }
}
for (i in 1:30) {
    for (j in 1:30) {
        if (n3[i] == n4[j]) {
            l[length(l) + 1] = n4[j]
        }  
    }
}
l = unlist(l)

#order the best variables together#
s = sort(c(s1, s2, s3, s4))

#choose the best 50 variables#
s = tail(s, n = 50) 
#there is no common variables in these best 50 variables#

######choose the best (5) variables, and calculate the accuracy#########
x_1 = as.matrix(cbind(xtr[1389,],xtr[742,],xtr[246,],xtr[1955,],xtr[545,]))
x_1_tst=as.matrix(cbind(t(xtst[1389,]),t(xtst[742,]),t(xtst[246,]),t(xtst[1955,]),t(xtst[545,])))

cvx1 = cv.glmnet(x_1,ytr1, family="binomial",alpha=0.9)
lam1 = c(cvx1$lambda.min,cvx1$lambda.1se)
lam1 = c(lam1,mean(lam1))
dmod1 = glmnet(x_1,ytr1, family="binomial",alpha=1,lambda=lam1)
fit1=predict(dmod1,type="response",newx=x_1)[,3]
fit1_tst=predict(dmod1,type="response",newx=x_1_tst)[,3]

cvx2 = cv.glmnet(x_1,ytr2, family="binomial",alpha=0.9)
lam2 = c(cvx2$lambda.min,cvx2$lambda.1se)
lam2 = c(lam2,mean(lam2))
dmod2 = glmnet(x_1,ytr2, family="binomial",alpha=1,lambda=lam2)
fit2=predict(dmod2,type="response",newx=x_1)[,3]
fit2_tst=predict(dmod2,type="response",newx=x_1_tst)[,3]

cvx3 = cv.glmnet(x_1,ytr3, family="binomial",alpha=0.9)
lam3 = c(cvx3$lambda.min,cvx3$lambda.1se)
lam3 = c(lam3,mean(lam3))
dmod3 = glmnet(x_1,ytr3, family="binomial",alpha=1,lambda=lam1)
fit3=predict(dmod3,type="response",newx=x_1)[,3]
fit3_tst=predict(dmod3,type="response",newx=x_1_tst)[,3]

cvx4 = cv.glmnet(x_1,ytr4, family="binomial",alpha=0.9)
lam4 = c(cvx4$lambda.min,cvx4$lambda.1se)
lam4 = c(lam4,mean(lam4))
dmod4 = glmnet(x_1,ytr4, family="binomial",alpha=1,lambda=lam4)
fit4=predict(dmod4,type="response",newx=x_1)[,3]
fit4_tst=predict(dmod4,type="response",newx=x_1_tst)[,3]

prob_fit=cbind(fit1,fit2,fit3,fit4)
prob_tst=cbind(fit1_tst,fit2_tst,fit3_tst,fit4_tst)

nm1 = unlist(strsplit(colnames(Khan1)[-1],"\\."))[2*(1:63)]
tr_y=as.numeric(factor(nm1))

nm_tst1 = unlist(strsplit(colnames(Khan1)[-1],"\\."))[2*(64:83)]
tr_y_tst=as.numeric(factor(nm_tst1))

library("MASS")
r <- lda(prob_fit,tr_y, prior = c(1,1,1,1)/4)
v=list(v1= r$scaling[,1],v2= r$scaling[,2])
a=predict(r,data.frame(prob_fit),type="response")$class
b=predict(r,data.frame(prob_tst),type="response")$class
tr_y_tst

tst_acc= 1-sum (b!=tr_y_tst)/length(b)
tst_acc
