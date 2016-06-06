---
title: "ㄅ砾箇代家"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##戈玡矪瞶



###戈秆睦

戈ㄓ方UCI Machine Learning Repository

戈ず魁699痜戈ノㄓ箇代痜ㄅ┬竳紽琌▆┦临琌潮┦柑繷11把计ㄒ灿璏单单...だ摸挡狦じだ摸潮┦(malignant)籔▆┦(benign)

###戈弄

```{r}
#install.packages("mlbench")
library(mlbench)
data(BreastCancer)
str(BreastCancer)

```
###戈玡矪瞶
痙⊿Τ戈р礚闽痜薄戈奔(ID)
```{r}
BreastCancerC<-BreastCancer[complete.cases(BreastCancer),
!names(BreastCancer) %in% c("Id")] 
c(nrow(BreastCancer),nrow(BreastCancerC))

```

###盢戈繦诀だ癡絤舱籔代刚舱
繦诀盢2/3戈だ癡絤舱Test==F逞1/3代刚舱Test==T)
```{r}
BreastCancerC$Test<-F 
BreastCancerC[
    sample(1:nrow(BreastCancerC),nrow(BreastCancerC)/3),
    ]$Test<-T 

c(sum(BreastCancerC$Test==F),sum(BreastCancerC$Test==T)) 

```
眔癡絤舱ㄒ计`r sum(BreastCancerC$Test==F)`代刚舱ㄒ计偤`r sum(BreastCancerC$Test==T)`

##箇代家ミ

###家ミ
パ跑计硈尿跑兜τ块じ摸跑兜珿匡拒∕郸攫簍衡猭ㄓミ家

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

###家弧
パ瓃把计∕郸攫ミ家箇代ㄅ┬竳紽琌潮┦┪▆┦竒程ㄎて┮ノ把计瓜∕郸攫┮ボ


##箇代家喷靡

```{r}
#install.packages("caret")
library(caret)
MinePred<-predict(fit,newdata = BreastCancerC[BreastCancerC$Test==T,],type = "class")

sensitivity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
specificity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
posPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
negPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)
```

ㄏノ痜眞戈ㄓ箇代ㄅ┬竳紽琌潮┦┪▆┦∕郸攫家箇代琌潮┦眔

- 庇稰 `r sensitivity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
- 疭钵┦ `r specificity(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
- 锭┦箇代瞯 `r posPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
- 潮┦箇代瞯 `r negPredValue(MinePred,BreastCancerC[BreastCancerC$Test==T,]$Class)*100`%
