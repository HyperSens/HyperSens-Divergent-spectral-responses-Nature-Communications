################################################################################################
################################################################################################

# The following code performs the Feature weighted random forest algorithm
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
# 1. Read data for training the feature weighted random forest () -------------------------------------------------------
################################################################################################

features<-read.table("train.csv", header=T, sep=",")#Database necessary to train the model to be assessed
features$SEV<- as.factor(features$SEV) #variable  to be used as output- Disease severity

#Suggested functions to optimize the hyperparameters of the RF Models
tuneGrid <- expand.grid(.mtry = c(1: dim(features)[2]-1))
rf_mtry <-train(SEV ~ .,
    data = features,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    nodesize = 14,
    ntree = 300)
rf_mtry$bestTune$mtry

trControl <- trainControl(method = "cv",
    number = 10,
    search = "grid")
    rf_default <- train(SEV ~ .,
    data = features,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl)

best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)
    store_maxtrees <- list()
    for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
        set.seed(5678)
        rf_maxtrees <- train(SEV ~ .,
        data = features,
            method = "rf",
            trControl = trControl,
            nodesize = 14,
            maxnodes = 24,
            ntree = ntree)
        key <- toString(ntree)
        store_maxtrees[[key]] <- rf_maxtrees
    }
    results_tree <- resamples(store_maxtrees)
    summary(results_tree)

##Vector including the feature importance in the same order of the inputs added, as an example the importance for detecting Xf in almonds is shown
imp<- c(0.182977462,0.163342542,0.103070093,0.00702973,0.024062926,0.058992622,0.114804648,0.107778673,0.016375919,0.04155023,0.148031219,0.031983935)
##Feature weighted Random forest algorithm
model_to_predict <- viRandomForests(SEV ~ ., data = features, imp,ntree=2000,mtry=dim(features)[2]-1, proximity=TRUE) ##ntree and mtry from the optimization process can be used
