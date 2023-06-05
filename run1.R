#############################
## function for loading tab-delimited spreadsheets
## R package providing elastic net functionality
library(rms)
library(survminer)
library(pheatmap) 
library(tidyverse)

source("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/model_bulid_function1.R")


model_train_test1 <- data_process_function("model1")
model_train_test2 <- data_process_function("model2")
model_train_test3 <- data_process_function("model3")
model_train_test4 <- data_process_function("model4")
model_train_test5 <- data_process_function("model5")
model_train_test6 <- data_process_function("model6")
model_train_test7 <- data_process_function("model7")
model_train_test8 <- data_process_function("model8")


plot(model_train_test1[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regression model performance, ROC curve (model 1)")
lines(unlist(model_train_test1[[4]]@y.values), unlist(model_train_test1[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test1[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test1[[6]],3) ),col = "blue" )


plot(model_train_test2[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regressionmodel performance, ROC curve (model 2)")
lines(unlist(model_train_test2[[4]]@y.values), unlist(model_train_test2[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test2[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test2[[6]],3) ),col = "blue" )

plot(model_train_test3[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regressionmodel performance, ROC curve (model 3)")
lines(unlist(model_train_test3[[4]]@y.values), unlist(model_train_test3[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test3[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test3[[6]],3) ),col = "blue" )

plot(model_train_test4[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regression model performance, ROC curve (model 4)")
lines(unlist(model_train_test4[[4]]@y.values), unlist(model_train_test4[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test4[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test4[[6]],3) ),col = "blue" )

plot(model_train_test5[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regression model performance, ROC curve (model 5)")
lines(unlist(model_train_test5[[4]]@y.values), unlist(model_train_test5[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test5[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test5[[6]],3) ),col = "blue" )

plot(model_train_test6[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regression model performance, ROC curve (model 6)")
lines(unlist(model_train_test6[[4]]@y.values), unlist(model_train_test6[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test6[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test6[[6]],3) ),col = "blue" )


plot(model_train_test7[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regression model performance, ROC curve (model 7)")
lines(unlist(model_train_test7[[4]]@y.values), unlist(model_train_test7[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test7[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test7[[6]],3) ),col = "blue" )

plot(model_train_test8[[3]], col="red", 
     xlim=c(0,1), ylim=c(0,1),lwd = 3, 
     xlab="1 - Specificity", 
     ylab="Sensitivity",
     main="Elastic net regressiom model performance, ROC curve (model 8)")
lines(unlist(model_train_test8[[4]]@y.values), unlist(model_train_test8[[4]]@x.values), col = "blue")
abline(0,1,col="gray",lty=1,lwd = 2)
abline(v=seq(0, 1, by = 0.2),lty=2,col="lightgray")
abline(h=seq(0, 1, by = 0.2),lty=2,col="lightgray")
text(0.8,0.4, paste0("AUC(Training dataset):", round(model_train_test8[[5]],3) ),col = "red" )
text(0.8,0.2, paste0("AUC(Testing dataset):", round(model_train_test8[[6]],3) ),col = "blue" )


####survival analysis
model7.fit.val <- predict(model_train_test7[[1]], newx = as.matrix(model_train_test7[[8]][,-c(1,2)]), type = "response", s = 0.05)
cutoff <- median(model7.fit.val)
data_sur <- data.frame(model_train_test7[[8]][,c(1,2)], ifelse(model7.fit.val > cutoff, "Low_risk", "High_risk") )
fit.risk <- surv_fit(Surv(pfi.time,pfi) ~ s1, data_sur)
ggsurvplot(fit.risk, conf.int = F, pval = T, legend.title = "Risk strata",
           risk.table = T) 



plot(model_train_test7[[1]])

importance_dat <- data.frame(name = names(coef(model_train_test7[[1]], s="lambda.min")[,1][which(coef(model_train_test7[[1]], s="lambda.min")[,1] != 0)][-1]), val = as.numeric(coef(model_train_test7[[1]], s="lambda.min")[,1][which(coef(model_train_test7[[1]], s="lambda.min")[,1] != 0)][-1]))

####importance vasulization

selected_gene_all <- model_train_test7[[7]][,colnames(model_train_test7[[7]][,-c(1,2)]) %in% importance_dat$name]


importance_dat <- importance_dat[importance_dat$name %in% colnames(selected_gene_all),]

importance_dat  %>%
  mutate(name = fct_reorder(name, desc(val))) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Variables slected by Elastic net regression model") + ylab("Importance") +
  theme_bw() +
  ggtitle("Importance of gene in model 7") +
  theme(plot.title = element_text(hjust = 0.5))

####heatmap
selected_gene <- model_train_test7[[7]][,colnames(model_train_test7[[7]][,-c(1,2)]) %in% importance_dat$name]
scalee_selected_gene <-  scale(selected_gene,scale = T)
heat_model7_data <- t(scalee_selected_gene)
pheatmap(heat_model7_data ,fontsize = 9,show_colnames = F)

####box plot########total
par(mar = c(6, 6, 3, 5))
boxplot(data.frame(scalee_selected_gene), outline = F, col = "steelblue1",las = 2)
a <- boxplot(data.frame(scalee_selected_gene), outline = F, col = "steelblue1",las = 2)


###subset of data for model 7
clinical_test_dataset <- read.csv("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/clinical.txt",sep = "\t")


Luminal_A_ID <- clinical_test_dataset %>% 
  filter(estrogen.receptor.status == "positive" & progesterone.receptor.status == "positive" & her2.status == "negative")  %>% 
  select(participant)

Luminal_B_ID <- clinical_test_dataset %>% 
  filter(estrogen.receptor.status == "positive" & progesterone.receptor.status == "positive" & her2.status == "positive")  %>% 
  select(participant)

Triple_neg_ID <- clinical_test_dataset %>% 
  filter(estrogen.receptor.status == "negative" & progesterone.receptor.status == "negative" & her2.status == "negative")  %>% 
  select(participant) 

HER2_Enriched_ID <- clinical_test_dataset %>% 
  filter(estrogen.receptor.status == "negative" & progesterone.receptor.status == "negative" & her2.status == "positive")  %>% 
  select(participant) 


####box plot########
Luminal_A_ID_box <- model_train_test7[[8]][rownames(model_train_test7[[8]]) %in% Luminal_A_ID$participant, a$names[1:6]]
Luminal_B_ID_box <- model_train_test7[[8]][rownames(model_train_test7[[8]]) %in% Luminal_B_ID$participant, a$names[1:6]]
Triple_neg_ID_box <- model_train_test7[[8]][rownames(model_train_test7[[8]]) %in% Triple_neg_ID$participant, a$names[1:6]]
HER2_Enriched_ID_box <- model_train_test7[[8]][rownames(model_train_test7[[8]]) %in% HER2_Enriched_ID$participant, a$names[1:6]]

par(mar = c(6, 6, 3, 5), mfrow=c(2,2) )
boxplot(Luminal_A_ID_box, outline = F, col = "steelblue1",las = 2, main = "Luminal A")
boxplot(Luminal_B_ID_box, outline = F, col = "steelblue1",las = 2, main = "Luminal B")
boxplot(Triple_neg_ID_box, outline = F, col = "steelblue1",las = 2, main = "Triple negative")
boxplot(HER2_Enriched_ID_box, outline = F, col = "steelblue1",las = 2, main = "HER2 Enriched")





