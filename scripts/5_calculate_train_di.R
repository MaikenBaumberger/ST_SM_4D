##########################################
#packages

library(caret)
library(CAST)

##########################################
#load data and models

directory <- "" #fill in the directory to the folder 

load(file.path(directory, "training_data/train_set_st.Rdata"))
load(file.path(directory, "training_data/train_set_sm.Rdata"))

load(file.path(directory, "models/rfmodel_st.Rdata"))
rfmodel
rfmodel_st <- rfmodel

load(file.path(directory, "models/rfmodel_sm.Rdata"))
rfmodel
rfmodel_sm <- rfmodel

rm(rfmodel)

############################################
#soil temperature

############################################
#split train data into different depths

tdi_train_05_st = train_set_st[train_set_st$depth == 5, ]

tdi_train_15_st = train_set_st[train_set_st$depth == 15, ]

tdi_train_25_st = train_set_st[train_set_st$depth == 25, ]

tdi_train_35_st = train_set_st[train_set_st$depth == 35, ]

tdi_train_45_st = train_set_st[train_set_st$depth == 45, ]

tdi_train_55_st = train_set_st[train_set_st$depth == 55, ]

tdi_train_65_st = train_set_st[train_set_st$depth == 65, ]

tdi_train_75_st = train_set_st[train_set_st$depth == 75, ]

tdi_train_85_st = train_set_st[train_set_st$depth == 85, ]

############################################
# extract weighting from model object, remove depths

w = varImp(rfmodel_st, scale = FALSE)[[1]]
w$Variable = rownames(w)
w = w[w$Variable != "depths",]
w = t(w) |> as.data.frame()
w <- w[-2, ]
w = data.frame(lapply(w,as.numeric))

############################################
#calculate trainDI for each depth

DIst05 = trainDI(train = tdi_train_05_st, variables = colnames(w), weight = w,
                   CVtrain = rfmodel_st$control$index,
                   CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst05,file= file.path(directory,"DIst05.rds"))

rm(DIst05)

DIst15 = trainDI(train = tdi_train_15_st, variables = colnames(w), weight = w,
                   CVtrain = rfmodel_st$control$index,
                   CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst15,file= file.path(directory,"DIst15.rds"))

rm(DIst15)

DIst25 = trainDI(train = tdi_train_25_st, variables = colnames(w), weight = w,
                    CVtrain = rfmodel_st$control$index,
                    CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst25,file= file.path(directory,"DIst25.rds"))

rm(DIst25)

DIst35 = trainDI(train = tdi_train_35_st, variables = colnames(w), weight = w,
                    CVtrain = rfmodel_st$control$index,
                    CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst35,file= file.path(directory,"DIst35.rds"))

rm(DIst35)

DIst45 = trainDI(train = tdi_train_45_st, variables = colnames(w), weight = w,
                    CVtrain = rfmodel_st$control$index,
                    CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst45,file= file.path(directory,"DIst45.rds"))

rm(DIst45)

DIst55 = trainDI(train = tdi_train_55_st, variables = colnames(w), weight = w,
                    CVtrain = rfmodel_st$control$index,
                    CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst55,file= file.path(directory,"DIst55.rds"))

rm(DIst55)

DIst65 = trainDI(train = tdi_train_65_st, variables = colnames(w), weight = w,
                    CVtrain = rfmodel_st$control$index,
                    CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst65,file= file.path(directory,"DIst65.rds"))

rm(DIst65)

DIst75 = trainDI(train = tdi_train_75_st, variables = colnames(w), weight = w,
                    CVtrain = rfmodel_st$control$index,
                    CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst75,file= file.path(directory,"DIst75.rds"))

rm(DIst75)

DIst85 = trainDI(train = tdi_train_85_st, variables = colnames(w), weight = w,
                    CVtrain = rfmodel_st$control$index,
                    CVtest = rfmodel_st$control$indexOut)

saveRDS(DIst85,file= file.path(directory,"DIst85.rds"))

rm(DIst85)



############################################
#soil moisture

############################################
#split train data into different depths

tdi_train_05_sm = train_set_sm[train_set_sm$depth == 5, ]

tdi_train_15_sm = train_set_sm[train_set_sm$depth == 15, ]

tdi_train_25_sm = train_set_sm[train_set_sm$depth == 25, ]

tdi_train_35_sm = train_set_sm[train_set_sm$depth == 35, ]

tdi_train_45_sm = train_set_sm[train_set_sm$depth == 45, ]

tdi_train_55_sm = train_set_sm[train_set_sm$depth == 55, ]

tdi_train_65_sm = train_set_sm[train_set_sm$depth == 65, ]

tdi_train_75_sm = train_set_sm[train_set_sm$depth == 75, ]

tdi_train_85_sm = train_set_sm[train_set_sm$depth == 85, ]


############################################
# extract weighting from model object, remove depths

v = varImp(rfmodel_sm, scale = FALSE)[[1]]
v$Variable = rownames(v)
v = v[v$Variable != "depths",]
v = t(v) |> as.data.frame()
v <- v[-2, ]
v = data.frame(lapply(v,as.numeric))


############################################
#calculate trainDI for each depth

DIsm05 = trainDI(train = tdi_train_05_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm05,file= file.path(directory,"DIsm05.rds"))

rm(DIsm05)

DIsm15 = trainDI(train = tdi_train_15_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm15,file= file.path(directory,"DIsm15.rds"))

rm(DIsm15)

DIsm25 = trainDI(train = tdi_train_25_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm25,file= file.path(directory,"DIsm25.rds"))

rm(DIsm25)

DIsm35 = trainDI(train = tdi_train_35_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm35,file= file.path(directory,"DIsm35.rds"))

rm(DIsm35)

DIsm45 = trainDI(train = tdi_train_45_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm45,file= file.path(directory,"DIsm45.rds"))

rm(DIsm45)

DIsm55 = trainDI(train = tdi_train_55_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm55,file= file.path(directory,"DIsm55.rds"))

rm(DIsm55)

DIsm65 = trainDI(train = tdi_train_65_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm65,file= file.path(directory,"DIsm65.rds"))

rm(DIsm65)

DIsm75 = trainDI(train = tdi_train_75_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm75,file= file.path(directory,"DIsm75.rds"))

rm(DIsm75)

DIsm85 = trainDI(train = tdi_train_85_sm, variables = colnames(v), weight = v,
                 CVtrain = rfmodel_sm$control$index,
                 CVtest = rfmodel_sm$control$indexOut)

saveRDS(DIsm85,file= file.path(directory,"DIsm85.rds"))

rm(DIsm85)


