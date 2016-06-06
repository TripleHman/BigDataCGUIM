---
title: "�����w���ҫ�"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##��ƫe�B�z



###��Ƹ���

����ƨӷ���UCI Machine Learning Repository�C

��Ƥ��@����699�ӯf�H��ơA�Ψӹw���f�H���ũи~�F�O�}���٬O���ʡA���Y�]�t�F11�ӰѼơA�Ҧp�ӭM�j�p����...�C�������G���G�������A�]����(malignant)�P�}��(benign)�C

###���Ū��

```{r}
#install.packages("mlbench")
library(mlbench)
data(BreastCancer)
str(BreastCancer)

```
###��ƫe�B�z
�d�U�S���ʭȪ���ơA�ç�L���f������Ʈ���(ID)
```{r}
BreastCancerC<-BreastCancer[complete.cases(BreastCancer),
!names(BreastCancer) %in% c("Id")] 
c(nrow(BreastCancer),nrow(BreastCancerC))

```

###�N����H�������V�m�ջP���ղ�
�H���N2/3����Ƥ���V�m�ա]Test==F�^�A�ѤU1/3�����ղա]Test==T)
```{r}
BreastCancerC$Test<-F 
BreastCancerC[
    sample(1:nrow(BreastCancerC),nrow(BreastCancerC)/3),
    ]$Test<-T 

c(sum(BreastCancerC$Test==F),sum(BreastCancerC$Test==T)) 

```
�i�o�V�m�ծרҼƬ�`r sum(BreastCancerC$Test==F)`���ղծרҼƬ���`r sum(BreastCancerC$Test==T)`

##�w���ҫ��إ�

###�ҫ��إ�
�ѩ��ܼƦh�A�B�h���s���ܶ��A�ӿ�X���G�����O�ܶ��A�G��ܨM����t��k�ӫإ߼ҫ��C

```{r}
#install.packages("rpart")
library(rpart)

BreastCancerC$Class<-factor(BreastCancerC$Class,levels=c("malignant","benign"))

#set.seed(1000)          
fit<-rpart(Class~.,data=BreastCancerC[BreastCancerC$Test==F,]) 

#install.packages("rpart.plot")
library(rpart.plot)
summary(fit)
prp(fit)


```

###�ҫ�����
�ѤW�z�Ѽƥi���A�H�M����إ߼ҫ��w���ũи~�F�O�_�����ʩΨ}�ʡA�g�̨Τƫ�A�ҥΨ쪺�ѼƬ��W�Ϫ��M����ҥ�


##�w���ҫ�����

```{r}
#install.packages("caret")
library(caret)
MinePred<-predict(fit,newdata = BreastCancerC[BreastCancerC$Test==T,],type = "class")

sensitivity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
specificity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
posPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
negPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
```

�ϥίf�w��ƨӹw���ũи~�F�O�_�����ʩΨ}�ʡA�H�M����ҫ��w���O�_�����ʡA�i�o�G

- �ӷP�� `r sensitivity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
- �S���� `r specificity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
- ���ʹw���v `r posPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
- ���ʹw���v `r negPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
