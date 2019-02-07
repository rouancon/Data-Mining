#HW 2
#Connor Rouan and Yadukrishnan Sethumadhavan

#Problem 1
#--Task A--
library(psych)
fa.parallel(USJudgeRatings)

#Based on these results, we will only need to extract one principal component

#--Task B--
library(psych)
principal(USJudgeRatings)

#--Task C--
principal(USJudgeRatings, nfactors=1, rotate="varimax")

#--Task D--
pc <- principal(USJudgeRatings, scores=TRUE)
pc$scores

#--Task E--
pc <- principal(USJudgeRatings)
factor.plot(pc)
#This graph shows that component 1 is the only principal component since it falls below 0

#--Task F--
#See PDF for explanation



#Problem 2
#--Task A--
library(psych)
fa.parallel(GID)

#Based on these results, we will need to extract 2 principal component

#--Task B--
principal(GID, nfactors=2,rotate="none")

#--Task C--
principal(USJudgeRatings, nfactors=1, rotate="varimax")

#--Task D--
pc <- principal(USJudgeRatings, scores=TRUE)
pc$scores

#--Task E--
pc <- principal(USJudgeRatings)
factor.plot(pc)
#This graph shows that component 1 is the only principal component since it falls below 0

#--Task F--
#See PDF for explanation

#Problem3

#Problem4

#--Task A--

fa.parallel(Harman74.cor$cov)
#Based on these results, we will need to extract 4 principal component


#--Task B--

harmanpc<-fa(Harman74.cor$cov,nfactors=4, rotate='none')
harmanpc

#--Task C--

harmanrc<-fa(Harman74.cor$cov, nfactors=4, rotate='varimax')
harmanrc

#--Task D--

harmanpc<-fa(Harman74.cor$cov,nfactors=4, rotate='none', scores=TRUE, fm='pa')
harmanpc$weights

#--Task E--

factor.plot(harmanpc)

#--Task F--

fa.diagram(harmanpc)

#--Task G--
#See PDF for explanation
