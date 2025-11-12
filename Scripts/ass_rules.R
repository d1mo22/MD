# ==============================================================================
#                 BASKET MARKET ANALYSIS: ASSOCIATION RULES
#
# Author/(s):    K. Gibert, X. Angerri & S.Ramirez, 2025 (c) IDEAI
# ==============================================================================
# 1. Load the packages ---------------------------------------------------------
library(arules)
library(arulesViz)  
library(rCBA)

# 2. Load the databases --------------------------------------------------------
## 2.2 Hotels -----------------------------------------------------------------
# How to work with associations rules with data matrix
setwd("C:/Users/max.estrade/Downloads")
dd <- read.table("hotel_sample_5000.csv",header=T, stringsAsFactors = TRUE, sep=",")
# We remove the variable rs_date to avoid bloat
dd <- subset(dd, select = -rs_date)
### Seleccion de las variables categoricas
dcat <- dd[,sapply(dd, is.factor)]
### transformamos a transacciones
dtrans <- as(dcat, "transactions")

# 3. Data Preprocessing  -------------------------------------------------------
## 3.2 Hotel -----------------------------------------------------------------
### tendra tantas columnas como categorias
foo <- function(x){length(levels(x))}
sum(sapply(dcat, foo)); dtrans
### Find the products in the data set
(modalidades <- itemLabels(dtrans))
### Look the stats for transactions 
summary(dtrans)
cat("Transaction number:", nrow(dtrans), "\n")
size <- size(dtrans)
cat("Average size of basket", mean(size), "\n")
### This graph shows the frequencies of each product.
itemFrequencyPlot(dtrans, support = 0.05, cex.names = 0.8, col = "pink")
### and now visualize the top 
itemFrequencyPlot(dtrans, topN = 30, col = "pink")
### Print the list of items order by relative proportions to itemset
sort(itemFrequency(dtrans, type = "relative"), decreasing = T) 
### Remove the transactions which proportion is lower thant 0.005 // Just if we want to avoid 
dtrans <- dtrans[, itemFrequency(dtrans) > 0.005]
sort(itemFrequency(dtrans, type = "relative")) 

# 4. Algorithm -----------------------------------------------------------------
db_transaciones <- dtrans # dtrans = CREDSCO, transaccionesG = GROCERIES

## 4.1 Apriori Algorithm -------------------------------------------------------
### Apply the function
rulesApriori <- apriori(db_transaciones, parameter = list(support = 0.01, 
                                                          confidence = 0.25, minlen = 2))

### Visualitzation the rules
summary(rulesApriori)
attributes(rulesApriori)

### Print the rules 
inspect(rulesApriori)
inspect(sort(rulesApriori, by = "lift"))
### Display rules
inspectDT(rulesApriori)

### rules with rhs containing "Dictamen" only
rulesFiltered <- apriori(dtrans, 
                         appearance = list(rhs=c("Dictamen=negatiu", "Dictamen=positiu")), 
                         parameter = list(minlen = 2, supp = 0.2))    

### Display the rules
inspectDT(sort(rulesFiltered, by = "lift"))

### prunning redundancies
## subset mira los supersets o subsets del primero en el segundo
### subset.matrix <- is.subset(rulesDtrans,rulesDtrans,sparse = F)
## #se diagonaliza para que cada par solo cuente una vez
### subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
## los redundantes estarán mas de 1 vez
### redundant <- colSums(subset.matrix,na.rm=T)>=1
### which(redundant)

## Eliminem regles redundants
### rules.pruned <- rulesDtrans[!redundant]
### rules.pruned <- sort(rules.pruned,by="lift")
### inspect(rules.pruned)

## Visualitzation the rules 
plot(rulesFiltered, measure = c("support", "lift"), shading = "confidence")
plot(rulesFiltered, method = "two-key plot")
## plot(rulesFiltered, method = "grouped")
plot(rulesFiltered, method = "paracoord")
plot(rulesFiltered, method = "paracoord", measure = "confidence", 
     control = list(reorder = TRUE))
plot(rulesFiltered, method = "graph")

## 4.2 Eclat Algorithm ---------------------------------------------------------
eclatDTrans <- eclat(db_transaciones, 
                     parameter = list(support = 0.4, minlen = 1, maxlen = 10))

as(items(eclatDTrans), "list")
summary(eclatDTrans)
inspect(eclatDTrans)

itemsets <- eclat(dtrans, parameter = list(tidLists = TRUE))
dim(tidLists(itemsets))
as(tidLists(itemsets), "list")

##Show the Frequent itemsets and respectives supports
inspect(sort(itemsets, by = "support"))

eclatTransrules <- ruleInduction(eclatDTrans, dtrans, confidence=0.1)
summary(eclatTransrules)
inspect(eclatTransrules)

### Display the 5 itemsets with the highest support.
orderedItemsets <- sort(eclatDTrans)
inspect(orderedItemsets)

### Display the 5 rules with the highest support.
top5 <- sort(eclatTransrules)[1:5]
inspect(top5)

### Get the itemsets as a list
as(items(top5), "list")

### Get the itemsets as a binary matrix
as(items(top5), "matrix")

### Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
### Warning: for efficiency reasons, the ngCMatrix you get is transposed 
as(items(top5), "ngCMatrix")

## Visualitzation
plot(eclatTransrules, method = "two-key plot", measure = "lift", shading = "confidence", 
     engine = "htmlwidget", network = TRUE, itemCol = "pink", max = 200)

plot(eclatTransrules, method = "graph", measure = "lift", control = list(type = "items"),
     interactiu = TRUE)
plot(eclatTransrules, method = "graph", measure = "lift", control = list(type = "itemsets"), 
     interactive = TRUE)
plot(sort(eclatTransrules, by = "lift"), method = "graph", control = list(type = "items"),
     interactive = TRUE)

plot(eclatTransrules, method = "graph", measure = "lift", shading = "confidence", 
     engine = "htmlwidget", network = TRUE, itemCol = "pink", max = 200)


## 4.3 FP Growth Algorithm -----------------------------------------------------
### rules <- rCBA::fpgrowth(transaccionesG, support = 0.03, confidence = 0.03, 
###                          maxLength = 2, parallel = FALSE)

# ==============================================================================