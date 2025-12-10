# ==============================================================================
#                 BASKET MARKET ANALYSIS: ASSOCIATION RULES
#
# Author/(s):    K. Gibert, X. Angerri & S.Ramirez, 2025 (c) IDEAI
# Adaptación:    Variable canceled (Max, 2025)
# ==============================================================================

# 1. Load the packages ---------------------------------------------------------
library(arules)
library(arulesViz)  
library(rCBA)

# 2. Load the databases --------------------------------------------------------
## 2.2 Hotels ------------------------------------------------------------------
setwd("C:/Users/max.estrade/Downloads")
dd <- read.table("hotel_sample_5000.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")
dd <- subset(dd, select = -r_status)


### Convertimos canceled a factor (es necesario para arules)
dd$canceled <- as.factor(dd$canceled)

### Eliminamos la variable rs_date para evitar bloat
dd <- subset(dd, select = -rs_date)

### Seleccionamos solo las variables categóricas
dcat <- dd[, sapply(dd, is.factor)]

### Transformamos la base en transacciones
dtrans <- as(dcat, "transactions")

# 3. Data Preprocessing  -------------------------------------------------------
## 3.2 Hotel -------------------------------------------------------------------

### Contar número total de categorías
foo <- function(x){length(levels(x))}
sum(sapply(dcat, foo)); dtrans

### Buscar los productos en el dataset
(modalidades <- itemLabels(dtrans))

### Estadísticas de las transacciones
summary(dtrans)
cat("Transaction number:", nrow(dtrans), "\n")
size <- size(dtrans)
cat("Average size of basket:", mean(size), "\n")

### Frecuencia de los items
itemFrequencyPlot(dtrans, support = 0.05, cex.names = 0.8, col = "pink")

### Frecuencia de los top items
itemFrequencyPlot(dtrans, topN = 30, col = "pink")

### Lista de ítems ordenados por proporción relativa
sort(itemFrequency(dtrans, type = "relative"), decreasing = TRUE)

### Eliminación de ítems con frecuencia < 0.005
dtrans <- dtrans[, itemFrequency(dtrans) > 0.005]
sort(itemFrequency(dtrans, type = "relative"))

# 4. Algorithm -----------------------------------------------------------------
db_transacciones <- dtrans

## 4.1 Apriori Algorithm --------------------------------------------------------
### Aplicamos el algoritmo Apriori
rulesApriori <- apriori(db_transacciones,
                        parameter = list(support = 0.01,
                                         confidence = 0.25,
                                         minlen = 2))

### Resumen de las reglas
summary(rulesApriori)
inspect(rulesApriori)
inspect(sort(rulesApriori, by = "lift"))
inspectDT(rulesApriori)

### Niveles reales de la variable canceled
levels(dd$canceled)

### Construimos automáticamente los RHS "canceled=valor"
rhs_canceled <- paste0("canceled=", levels(dd$canceled))

### Filtrar reglas cuyo consecuente (rhs) sea canceled
rulesFiltered <- apriori(dtrans,
                         appearance = list(rhs = rhs_canceled),
                         parameter = list(minlen = 2, supp = 0.10))

### Mostrar reglas filtradas ordenadas por lift
inspectDT(sort(rulesFiltered, by = "lift"))

### prunning redundancies ------------------------------------------------------
## subset mira los supersets o subsets del primero en el segundo
subset.matrix <- is.subset(rulesFiltered, rulesFiltered, sparse = FALSE)

### Se diagonaliza para que cada par solo cuente una vez
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA

### Los redundantes estarán más de 1 vez
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
which(redundant)

### Eliminamos reglas redundantes
rules.pruned <- rulesFiltered[!redundant]
rules.pruned <- sort(rules.pruned, by = "lift")
inspect(rules.pruned)

### Visualitzation the rules ---------------------------------------------------
plot(rules.pruned, measure = c("support", "lift"), shading = "confidence")
plot(rules.pruned, method = "two-key plot")
plot(rules.pruned, method = "paracoord")
plot(rules.pruned, method = "paracoord", measure = "confidence",
     control = list(reorder = TRUE))
plot(rules.pruned, method = "graph")

## 4.2 Eclat Algorithm ----------------------------------------------------------
eclatDTrans <- eclat(db_transacciones,
                     parameter = list(support = 0.40, minlen = 1, maxlen = 10))

as(items(eclatDTrans), "list")
summary(eclatDTrans)
inspect(eclatDTrans)

itemsets <- eclat(dtrans, parameter = list(tidLists = TRUE))
dim(tidLists(itemsets))
as(tidLists(itemsets), "list")

### Frequent itemsets con sus soportes
inspect(sort(itemsets, by = "support"))

eclatTransrules <- ruleInduction(eclatDTrans, dtrans, confidence = 0.10)
summary(eclatTransrules)
inspect(eclatTransrules)

### Mostrar los itemsets con mayor soporte
orderedItemsets <- sort(eclatDTrans)
inspect(orderedItemsets)

### Mostrar las 5 reglas con mayor soporte
top5 <- sort(eclatTransrules)[1:5]
inspect(top5)

### Obtener itemsets como lista / matrices
as(items(top5), "list")
as(items(top5), "matrix")
as(items(top5), "ngCMatrix")

### Visualización
plot(eclatTransrules, method = "two-key plot", measure = "lift",
     shading = "confidence", engine = "htmlwidget", network = TRUE,
     itemCol = "pink", max = 200)

plot(eclatTransrules, method = "graph", measure = "lift",
     control = list(type = "items"), interactive = TRUE)

plot(sort(eclatTransrules, by = "lift"), method = "graph",
     control = list(type = "items"), interactive = TRUE)

plot(eclatTransrules, method = "graph", measure = "lift",
     shading = "confidence", engine = "htmlwidget", network = TRUE,
     itemCol = "pink", max = 200)
