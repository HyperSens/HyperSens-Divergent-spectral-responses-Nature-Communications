################################################################################################
################################################################################################

# The following code performs an unsupervised Spectral clustering algorithm for the reclassification of uncertain trees
# Title: Spectral fingerprints of plant pathogen infection diverge from abiotic signals
# Authors: Zarco-Tejada, P.J., Poblete, T., Camino, C., Gonzalez-Dugo, V, Calderon, R., Hornero, A., Hernandez-Clemente, R.,
# Román-Écija, M., Velasco-Amo, M.P., Landa, B.B., Beck, P.S.A., Saponari, M., Boscia, D., Navas-Cortes, J.A.

################################################################################################
################################################################################################

rm(list=ls())

require("viRandomForests")
if (!require('caret')) { install.packages('caret'); require('caret') }
library(kernlab)

################################################################################################
# 1. Using the results of the Feature weighted random forest algorithm -------------------------------------------------------
################################################################################################
features<-read.table("train.csv", header=T, sep=",")#Database necessary to train the model to be assessed
features$SEV<- as.factor(features$SEV) #variable  to be used as output- Disease severity
##Vector including the feature importance in the same order of the inputs added, as an example the importance for detecting Xf in almonds is shown
imp<- c(0.182977462,0.163342542,0.103070093,0.00702973,0.024062926,0.058992622,0.114804648,0.107778673,0.016375919,0.04155023,0.148031219,0.031983935)
##Feature weighted Random forest algorithm
model_to_predict <- viRandomForests(SEV ~ ., data = features, imp,ntree=2000,mtry=dim(features)[2]-1, proximity=TRUE) ##ntree and mtry from the optimization process can be used.
#See feature weighted Random forest code for the hyperparamaters optimization

##Read the database to be tested, it could be to test against the Visual observations (test$SEV) or the qPCR results (test$qPCR)
test<-read.table("test.csv", header=T, sep=",")
test$SEV_A<- as.factor(test$SEV)
test$qPCR<- as.factor(test$qPCR)
#Prediction of the output and obtention of the probability of the classification
predicted=predict(model_to_predict,test)
predicted_p = predict(model_to_predict,test, type = "prob")
results_acc_a<-confusionMatrix(test$qPCR, predicted)#evaluation of the performance of the prediction
indices=which(predicted_p[,1]<0.87 & predicted_p[,2]<0.87)#obtention of the "uncertain" trees
param_to_reclasify<-c("PRIn","Fi","Cab") #Selection of the divergent plant traits. As an example, those divergent for Xf-almonds infections is shown
reclass=(test[indices,param_to_reclasify])
#Spectral clustering over the uncertain trees
sc <- specc(as.matrix(reclass), centers=2,kernel='rbfdot',kpar="local")
f = as.data.frame((sc))
f2=as.factor(f[,1])
indices2=which(f2==1 & test[indices,'QPCR']==0)
indices2b=which(f2==2 & test[indices,'QPCR']==0)##Identification of the clusters detected and the output to be predicted
##Assignation of the clusters based on the sensitive (divergent) traits, as an example the Fi parameter is used for Almond trees.
if (mean(reclass[indices2,'Fi'])< mean(reclass[indices2b,'Fi']))
 {

 indices3=which(f2==2 & test[indices'QPCR']==1)
 predicted[indices[indices2]]=0
 predicted[indices[indices3]]=1
 results_acc<-confusionMatrix(test[,'QPCR'], predicted)

}
if (mean(reclass[indices2,'Fi'])> mean(reclass[indices2b,'Fi']))
 {
 indices2=which(f2==2 & test[indices,'QPCR']==0)
indices3=which(f2==1 & test[indices,'QPCR']==1)
predicted[indices[indices2]]=0
predicted[indices[indices3]]=1
results_acc<-confusionMatrix(test[,'QPCR'], predicted)
}
