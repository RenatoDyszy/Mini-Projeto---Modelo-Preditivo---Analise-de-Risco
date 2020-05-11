# Construindo um Modelo Preditivo para Análise de Risco

# Diretório de trabalho
  setwd("C:/DataScience/Projetos/GitHub/Mini-Projeto---Modelo-Preditivo---Analise-de-Risco")
  getwd()
# -------------

# Carrega dataset
  dataset.df <- read.csv("credit_dataset.csv", header = TRUE, sep = ",")
  
  # Resumo dos dados
    str(dataset.df)
    head(dataset.df)
  
  View(dataset.df)
# --------------
  
# Functions
  
  # Função para converter variáveis numéricas para fator
  toFactors <- function ( df, vars) {
    
    for (variable in vars ){
      df[[variable]] <- as.factor(df[[variable]])
    }
    
    return(df)  
  }
  # ----------
  
  # Nomalização
  scale.features <- function(df, variables){
    for (variable in variables){
      df[[variable]] <- scale(df[[variable]], center=T, scale=T)
    }
    return(df)
  }
  # ---------
  
  # Geração de curvas ROC 
    library(ROCR)
    plot.roc.curve <- function(predictions, title.text){
      perf <- performance(predictions, "tpr", "fpr")
      plot(perf, col = "black", lty = 1, lwd = 2, 
           main = title.text, cex.main = 0.6, 
           cex.lab = 0.8, xaxs="i", yaxs="i")
      abline(0,1, col = "red")
      auc <- performance(predictions, "auc")
      auc <- unlist(slot(auc, "y.values"))
      auc <- round(auc,2)
      legend(0.4,0.4, legend = c(paste0("AUC: ",auc)), cex = 0.6, bty = "n", box.col = "white")
      
    }
    
    plot.pr.curve <- function(predictions, title.text){
      perf <- performance(predictions, "prec", "rec")
      plot(perf, col = "black", lty = 1, lwd = 2,
           main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
    }
  # -----------
  
# -------------    
  
# Correlação entre as váriaveis
  df <- as.data.frame(dataset.df)
  data_cor <- cor(df)
  
  library(corrplot)
  corrplot(data_cor, method = 'color')
# ----------
  
# Gráficos  
  library(ggplot2)

  ggplot(dataset.df) +
    geom_histogram(aes(x=age), bins = 25, color = "black", fill = "white") +
      xlab("Idade") + ylab("Quantidade") 
  
  ggplot(dataset.df) +
    geom_histogram(aes(x=credit.duration.months), color = "black", bins = 60, fill = "white") +
      xlab("Duração das Parcelas em meses") + ylab("Quantidade")   
  
  ggplot(dataset.df) +
    geom_histogram(aes(x=credit.amount), bins = 50, color = "black", fill = "white") +
      xlab("Valor Solicitado") + ylab("Quantidade") 
# ------------  
  

# Normaliza variáveis
 
  # Normalização
  numeric.vars <- c('credit.duration.months','credit.amount','age')
  dataset.df <- scale.features(dataset.df, numeric.vars)  

  # Convertendo as variáveis para o tipo fator (categórica)
  varFactors <- c('credit.rating',
                  'account.balance',
                  'previous.credit.payment.status',
                  'credit.purpose',
                  'savings',
                  'employment.duration',
                  'installment.rate',
                  'marital.status',
                  'guarantor',
                  'residence.duration',
                  'current.assets',
                  'other.credits',
                  'apartment.type',
                  'bank.credits',
                  'occupation',
                  'dependents',
                  'telephone',
                  'foreign.worker')
  dataset.df <- toFactors(dataset.df, varFactors)
# ------------
  
# Dividindo os dados em treino e teste - 60:40 ratio
  library(caTools)
    
  amostra <- sample.split(dataset.df$credit.rating, SplitRatio = 0.60)
  dataset.df.treino <- subset(dataset.df, amostra == TRUE)
  dataset.df.teste <- subset(dataset.df, amostra == FALSE)
# ------------

  
# Criando e avaliando o modelo
  
  install.packages("e1071")
  library(caret) 
  
  
  # Modelo com todas as váriaveis usando Random Forest
    rf.model <- randomForest(credit.rating ~ . , dataset.df.treino, ntree = 100, nodesize = 10)
    rf.predictions <- predict(rf.model, dataset.df.teste, type="response")
    
    # Imprimondo o resultado
    print(rf.model)
    
    # Gerando Confusino Matrix
    confusionMatrix(table(data = rf.predictions, reference = dataset.df.teste[,1]), positive = '1')
  # -----------
  

  # Modelo com as variáveis selecionadas usando Random Forest
    
    # Feature Selection
      library(randomForest)
      modelo <- randomForest( credit.rating ~ .,
                              data = dataset.df, 
                              ntree = 100, nodesize = 10, importance = T)
      varImpPlot(modelo)
    # ------------
    
    formula <- ("credit.rating ~ account.balance + 
                                 credit.duration.months + 
                                 previous.credit.payment.status +
                                 credit.amount +
                                 savings +
                                 apartment.type +
                                 credit.purpose")
    formula <- as.formula(formula)
  
    rf.model <- randomForest(formula, dataset.df.treino, ntree = 100, nodesize = 10)
    rf.predictions <- predict(rf.model, dataset.df.teste, type="response")
    
    # Imprimindo o resultado
    print(rf.model)
    
    # Gerando Confusino Matrix
    confusionMatrix(table(data = rf.predictions, reference = dataset.df.teste[,1]), positive = '1')
    
    # Gerando a curva ROC
      class1 <- predict(rf.model, newdata = dataset.df.teste, type = 'prob')
      class2 <- dataset.df.teste$credit.rating
      
      pred <- prediction(class1[,2], class2)
      perf <- performance(pred, "tpr","fpr") 

      par(mfrow = c(1,2))
      plot.roc.curve(pred, title.text = "Curva ROC")
      plot.pr.curve(pred, title.text = "Curva Precision/Recall")
  # -------------
  
# ------------


      install.packages('tinytex')
      
      # setup the external stuff and configure
      tinytex::install_tinytex()