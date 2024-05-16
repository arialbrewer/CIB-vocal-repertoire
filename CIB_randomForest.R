#Arial Brewer
#PhD- Chapter 1
#CIB vocal repertoire- CART and Random Forest analyses

library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)

#read in data
data <- read.csv("RF_call_measurements.csv") %>% 
         select(-file_name,-selection) %>% 
         rename("Call.type"="call_type", "Dur"="duration", "Min"="min_freq",	
                "Max"="max_freq", "BW"="bw", "Center"="center_freq", "Peak"="peak_freq",	
                "Min.PRR"="min_prr", "Max.PRR"="max_prr", "Start"="start_freq",
                "End"="end_freq", "Trend"="trend", "Inflec"="inflec", "Seg"="seg",	
                "Steps"="steps", "Units"="units")


############# CART analysis
##specifying controls: xval=# of crossvalidations, minbucket=min # of cases in terminal node, minsplit=min # of cases before can split. 
control<-rpart.control(xval=10, minbucket=3, minsplit=3, cp=0.001)

##specify model
tree_model<-rpart(Call.type ~ Dur + Min + Max + BW + Center + Peak + Min.PRR + Max.PRR +
                    Start + End + Trend + Inflec + Seg + Steps + Units,
                  control = control, method="class", data = data)

##model summary (lists variable importance)
summary(tree_model)

##cross-validation (lists root node error)
printcp(tree_model)

##plot cross-validation result
plotcp(tree_model)

##prune tree by automatically selecting the complexity parameter associated with smallest cross-validated error
tree_model2<-prune(tree_model, cp=tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
summary(tree_model2)

#variable importance
tree_model2$variable.importance

#check to see if freq variables are correlated (>0.6 is correlated)
cor(data$Min,data$Max)
cor(data$Min,data$Center)
cor(data$Min,data$Peak)
cor(data$Min,data$Start)
cor(data$Min,data$End)
cor(data$Max,data$Center)
cor(data$Max,data$Peak)
cor(data$Max,data$Start)
cor(data$Max,data$End)
cor(data$Center,data$Peak)
cor(data$Inflec,data$Seg)

##calculate number of misclassifications
#expected
exp = predict(tree_model2,type="class")

#observed 
obs = data$Call.type

#number of misclassified cases
mis_num <- sum(as.numeric(exp!=obs))
mis_num/369*100


##Plot tree 
# separate split labels for each direction
prp(tree_model2, type=3, extra=2, leaf.round=1, cex=.4,
    tweak=1.0, split.yshift=-4,
    split.family="serif", family="serif",
    main="Communication in Cook Inlet beluga whales: describing the vocal repertoire and masking of calls by commercial ship noise",cex.main=1,
    sub="Fig S2- A classification and regression tree (CART) of CIB call types. The variables and criteria used at each split are listed, along with terminal nodes. Terminal nodes show the call type along with the 
    classification rate (number of correct classifications/number of observations). Repeated terminal nodes (aws, dws.seq, pulse.flat) indicate similar call contour but different acoustic parameters.",
    cex.sub=1)
title(main= ("Arial M. Brewer, Manuel Castellote, Amy M. Van Cise, Tom Gage,  and Andrew M. Berdahl"), line=0.5,cex.main=0.7)

prp(tree_model2, type=3, extra=2, leaf.round=1, cex=.3,
    tweak=1.2, split.yshift=-4,
    main="Communication in Cook Inlet beluga whales: describing the vocal repertoire and masking of calls by commercial ship noise",cex.main=1,
    sub="Fig S2- A classification and regression tree (CART) of CIB call types. The variables and criteria used at each split are listed, along with terminal nodes. Terminal nodes show the call type along with the 
classification rate (number of correct classifications/number of observations). Repeated terminal nodes (aws, dws.seq, pulse.flat) indicate similar call contour but different acoustic parameters.",
    cex.sub=1, family="serif")

title(main= ("Arial M. Brewer, Manuel Castellote, Amy M. Van Cise, Tom Gage,  and Andrew M. Berdahl"),line=0.5,cex.main=0.7)


##OR
prp(tree_model2, type=3, extra=2, leaf.round=1, cex=.4,
    tweak=1.0, split.yshift=-4,
    split.family="serif", family="serif")
title(sub="Fig S2- A classification and regression tree (CART) of CIB call types. The variables and criteria used at each split are listed, along with terminal nodes. Terminal nodes show the call type along with the classification rate (the number of correct classifications/the 
number of observations). Repeated terminal nodes (aws, dws.seq, pulse.flat) indicate similar call contour but different acoustic parameters.",
    cex.sub=0.9,family="serif",adj=0)



############### Random Forest ##################
#call type needs to be a factor, not a character
data$Call.type=factor(data$Call.type)

#create forest
rf_model<-randomForest(Call.type ~ Dur + Min + Max + BW + Center + Peak + Min.PRR + Max.PRR +
                       Start + End + Trend + Inflec + Seg + Steps + Units, 
                       data = data, ntree = 1000,importance=T)

#view results
print(rf_model)

#save confusion matrix output
write.table(rf_model$confusion, file="RF_confusion_matrix2.txt", sep="\t")

#Plot of Mean decrease in accuracy
importance(rf_model)
varImpPlot(rf_model,type=1)


#plot of important variables
varimp <- read_csv("RF_var_importance.csv")

varimp %>%
  arrange(Accuracy) %>%   
  mutate(Parameter=factor(Parameter, levels=Parameter)) %>%   
  ggplot(aes(x=Accuracy, y=Parameter)) +
  geom_point(size=2.5) +
  theme_classic() +
  xlab("Mean decrease in accuracy") +
  ylab("Acoustic parameter") +
  theme(axis.title.x=element_text(family="serif",size=15),
        axis.title.y=element_text(family="serif",size=15),
        axis.text.x=element_text(family="serif",size=13),
        axis.text.y=element_text(family="serif",size=13))

ggsave('figs\\RF_var_imp.jpg', dpi=600, width=9, height=7)

