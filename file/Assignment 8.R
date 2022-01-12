dataPath <- "~/UChicago/31007_Stats_Lec/Assignment8/"
coagulation <- read.table(paste(dataPath,'Week8_Test_Sample.csv',sep = '/'), header=TRUE)
test_dat <- read.table(paste(dataPath,'Week8_Test_Sample.csv',sep = '/'), header=TRUE)
View(test_dat)

#Output Values
coagulation$Output
#Predictor Values
coagulation$Treatment

summaryByGroup<-aggregate(Output~Treatment,data=coagulation,FUN=summary)
means<-cbind(Means=summaryByGroup$Output[,4],Sizes=aggregate(Output~Treatment,data=coagulation,FUN=length)$Output)
rownames(means)<-as.character(summaryByGroup$Treatment)
means

coag<-coagulation
coag$x1<-coag$Treatment=="B"
coag$x2<-coag$Treatment=="C"
coag
coag.model<-lm(Output~Treatment,data=coagulation)
coag.model.full<-lm(Output~x1+x2, data=coag)
coag.model.null<-lm(Output~1,data=coag)
anova(coag.model.null,coag.model.full)

summary(coag.model)
anova(coag.model)
grand.mean<-mean(coagulation$Output)
create.vector.of.means<-function(my.group.data) {
  rep(my.group.data[1],my.group.data[2])
}
group.mean<-unlist(apply(means,1,create.vector.of.means))

grand.mean
group.mean

SST<-sum((coagulation$Output-grand.mean)^2)
#1) Within group sum of squares
SSE<-sum((coagulation$Output-group.mean)^2)
#2) Between groups sum of squares
SSM<-sum((group.mean-grand.mean)^2)

c(SST=SST,SSE=SSE,SSM=SSM)


fc1<-anova(coag.model)
anova(coag.model.null,coag.model.full)



model.matrix(coag.model)
coag.altmodel<-lm(Output~Treatment-1,data=coagulation)
fc<-anova(coag.altmodel)

#3) Sum of squares of predictor for model testing: "all group mean values are equal to 0" 
fc$`Sum Sq`
#4) #3) Sum of squares of residuals for model testing: "all group mean values are equal to 0" 
fc1$`Sum Sq`

SSE
SSM

# 1. 0.468459 correct 
# 2. 0.2461943 correct 
# 3. All group means are the same incorrect 
# 4. 597.311202 correct 
# 5. 0.2461943 incorrect 
# 6. All group mean values equal to zero incorrect 

# 5. 0.468459

model.matrix(coag.altmodel)
sum((group.mean-grand.mean)^2)
sum(group.mean^2)

(sum((group.mean-grand.mean)^2)/2)/(sum((coagulation$Output-group.mean)^2)/(length(coagulation$Output)-3))
(sum(group.mean^2)/3)/(sum((coagulation$Output-group.mean)^2)/(length(coagulation$Output)-3))

# anova()


##########################################################


coag.model<-lm(Output~Treatment,data=test_dat)
coag.altmodel<-lm(Output~Treatment-1,data=test_dat)
modelSummary<-summary(coag.model)
modelANOVA<-anova(coag.model)
altmodelSummary<-summary(coag.altmodel)
altmodelANOVA<-anova(coag.altmodel)


grand.mean<-mean(test_dat$Output)
create.vector.of.means<-function(my.group.data) {
  rep(my.group.data[1],my.group.data[2])
}
group.mean<-unlist(apply(means,1,create.vector.of.means))

grand.mean
group.mean

SST<-sum((test_dat$Output-grand.mean)^2)
#1) Within group sum of squares
SSE<-sum((test_dat$Output-group.mean)^2)
SSE
#2) Between groups sum of squares
SSM<-sum((group.mean-grand.mean)^2)
SSM

c(SST=SST,SSE=SSE,SSM=SSM)
coag <- test_dat
coag$x1<-coag$Treatment=="A"
coag$x2<-coag$Treatment=="B"
coag$x3<-coag$Treatment=="C"
coag

coag.model.full<-lm(Output~x1+x2+x3, data=coag)
coag.model.null<-lm(Output~1,data=coag)
anova(coag.model.null,coag.model.full)

summary(coag.model)
anova(coag.model)
model.matrix(coag.model)

summary(coag.altmodel)
anova(coag.altmodel)
model.matrix(coag.altmodel)

#F-statistics
(sum((group.mean-grand.mean)^2)/3)/(sum((test_dat$Output-group.mean)^2)/(length(test_dat$Output)-4))
(sum(group.mean^2)/4)/(sum((test_dat$Output-group.mean)^2)/(length(test_dat$Output)-4))

