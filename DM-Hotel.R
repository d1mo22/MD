library(ggplot2)
install.packages("gridExtra")   # install the package
library(gridExtra)
#library(gridExtra)

setwd("C:/Users/louis/Downloads/")
hotel<- read.table("hotel_sample_5000(3).csv",header=T, sep=",")
print(hotel)
hotel[is.na(hotel)] <- 0
write.csv(hotel, "hotel_sample_5000.csv", row.names = FALSE)

# Function to create plots depending on column type
plot_col <- function(data, colname) {
  col <- data[[colname]]
  
  if (is.numeric(col)) {
    p <- ggplot(data, aes(x = col)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      labs(title = paste("Distribution of", colname), x = colname, y = "Count")
  } else {
    p <- ggplot(data, aes(x = as.factor(col))) +
      geom_bar(fill = "orange", color = "black") +
      labs(title = paste("Frequency of", colname), x = colname, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(p)
}

# Apply the function to all columns
plots <- lapply(names(hotel), function(c) plot_col(hotel, c))

# Example: show the first 6 plots on one page
#do.call(grid.arrange, c(plots[1:6], ncol = 2))
#do.call(grid.arrange, c(plots[6:12], ncol = 3))
#do.call(grid.arrange, c(plots[12:30], ncol = 3))

set.seed(123)                     # for reproducibility
#hotel_sample <- hotel[sample(nrow(hotel), 5000), ]  # take 100 random rows
#write.csv(hotel_sample, "hotel_sample_5000.csv", row.names = FALSE)
#getwd()
install.packages("reshape2")   # install the package

library(reshape2)
# Select only numeric columns
hotel_num <- hotel[sapply(hotel, is.numeric)]

# Compute correlation matrix
corr_matrix <- cor(hotel_num, use = "complete.obs")

# Melt into long format for ggplot2
melted_corr <- melt(corr_matrix)

# Plot heatmap
ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="red", high="blue", mid="white", 
                       midpoint=0, limit=c(-1,1), space="Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  labs(title = "Correlation Heatmap of Numeric Features")
# Load packages
library(dplyr)
library(lubridate)


hotel <- hotel %>%
  mutate(
    # 1. Create proper arrival_date
    arrival_date = as.Date(
      paste(arrival_date_year, arrival_date_month, arrival_date_day_of_month, sep = "-"),
      format = "%Y-%M-%d"
    ),
    
    # 2. Total stay length
    total_stay_length = stays_in_weekend_nights + stays_in_week_nights,
    
    # 3. Total guests
    total_guests = adults + children + babies
  )

# Check result
head(hotel[c("arrival_date", "total_stay_length", "total_guests")])
print(hotel)

# Load packages
library(ggplot2)
library(reshape2)

# Select only numeric columns
hotel_num <- hotel[sapply(hotel, is.numeric)]

# Convert data to long format for ggplot
hotel_long <- melt(hotel_num)
# Loop over all numeric variables
for (colname in names(hotel_num)) {
  p <- ggplot(hotel, aes(y = .data[[colname]])) +
    geom_boxplot(fill = "lightblue", outlier.colour = "red",  coef = 3) +
    labs(title = paste("Boxplot of", colname), y = colname) +
    theme_minimal()
  print(p)
}

for (colname in names(hotel_num)) {
  p <- ggplot(hotel, aes(x = .data[[colname]])) +
    geom_histogram(aes(y = ..density..),
                   bins = 30,
                   fill = "steelblue",
                   color = "black",
                   alpha = 0.6) +
    geom_density(color = "red", linewidth = 1) +
    labs(title = paste("Distribution of", colname),
         x = colname,
         y = "Density") +
    theme_minimal()
  print(p)
}
hotel <- hotel_raw 

vars <- c("days_in_waiting_list", "booking_changes",
          "previous_bookings_not_canceled", "previous_cancellations", "adr")

# Function to compute number of outliers using 3×IQR rule
iqr_outlier_info <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 3 * iqr
  upper <- q3 + 3 * iqr
  count <- sum(x < lower | x > upper, na.rm = TRUE)
  percent <- mean(x < lower | x > upper, na.rm = TRUE) * 100
  c(Lower_Bound = lower, Upper_Bound = upper,
    Outlier_Count = count, Outlier_Percent = percent)
}

# Apply to all selected variables
iqr_summary <- t(sapply(hotel[vars], iqr_outlier_info))
iqr_summary <- as.data.frame(iqr_summary)

print(iqr_summary)

hotel_raw <- hotel 
library(dplyr)
hotel <- hotel %>%
  filter(
    days_wait <= quantile(days_wait, 0.99, na.rm = TRUE),
    changes <= quantile(changes, 0.99, na.rm = TRUE),
    pre_bcn <= quantile(pre_bcn, 0.99, na.rm = TRUE),
    pre_cancel <= quantile(pre_cancel, 0.99, na.rm = TRUE),
    adr <= quantile(adr, 0.999, na.rm = TRUE)
  )
n_before <- nrow(hotel_raw)   # if you saved the dataset before filtering
n_after  <- nrow(hotel)
cat("Rows removed:", n_before - n_after, "\n")


print(hotel)
# Old names
old_names <- c("hotel","is_canceled","lead_time","arrival_date_year","arrival_date_month",
               "arrival_date_week_number","arrival_date_day_of_month","stays_in_weekend_nights",
               "stays_in_week_nights","adults","children","babies","meal","country",
               "market_segment","distribution_channel","is_repeated_guest","previous_cancellations",
               "previous_bookings_not_canceled","reserved_room_type","assigned_room_type",
               "booking_changes","deposit_type","agent","company","days_in_waiting_list",
               "customer_type","adr","required_car_parking_spaces","total_of_special_requests",
               "reservation_status","reservation_status_date")

# New names
new_names <- c("hotel","canceled","lead_time","year","month","week","day",
               "weekend_nights","week_nights","adults","children","babies",
               "meal","country","market_seg","channel","repeated","pre_cancel",
               "pre_bcn","rroom_type","assroom_type","changes","deposit_type",
               "agent","company","days_wait","customer_type","adr",
               "rcar_parking_spaces","ts_requests","r_status","rs_date")

# Rename
colnames(hotel) <- new_names

# Check result
print(colnames(hotel))

num_vars <- c("lead_time", "weekend_nights", "week_nights", "adults", "children",
              "babies", "repeated", "pre_cancel", "pre_bcn", "changes", "days_wait",
              "adr", "rcar_parking_spaces", "ts_requests")

quali_vars <- c("hotel", "market_seg", "channel", "customer_type", "r_status")
install.packages("FactoMineR")   # only the first time
install.packages("factoextra")   # for visualization (fviz_* functions)

library(FactoMineR)
library(factoextra)
install.packages("dplyr")
install.packages("magrittr") 
library(dplyr)
print(hotel)

df_num <- hotel %>% select(all_of(num_vars)) %>% na.omit()
#df_quali <- hotel %>% select(all_of(quali_vars))[rownames(df_num), , drop = FALSE]

num_vars <- c("lead_time", "weekend_nights", "week_nights",
              "adults", "children", "babies",
              "changes", "days_wait", "adr",
              "rcar_parking_spaces", "ts_requests",
              "pre_cancel", "pre_bcn")

# --- Subset and remove missing values ---
df_num <- hotel[, num_vars]


# --- Check structure (optional) ---

# --- Run PCA ---
pca <- PCA(df_num, scale.unit = TRUE, graph = FALSE)

# --- (a) Scree Plot ---

fviz_eig(pca, addlabels = TRUE, linecolor = "red") +
  ggplot2::labs(title = "Scree Plot - Variance Explained by PCs")


# --- (b-i) Individuals Projection ---

fviz_pca_ind(pca,
             geom = "point",
             pointsize = 1.5,
             alpha.ind = 0.6,
             title = "PCA - Individuals Projection")


# --- (b-ii) Variables Correlation Map ---

fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("blue", "white", "red"),
             repel = TRUE,
             title = "PCA - Correlation Circle of Variables")

# --- 1️⃣ Define numeric variables
num_vars <- c("lead_time", "weekend_nights", "week_nights",
              "adults", "children", "babies",
              "changes", "days_wait", "adr",
              "rcar_parking_spaces", "ts_requests",
              "pre_cancel", "pre_bcn")

# --- 2️⃣ Define qualitative variables you want to visualize
quali_vars <- c("hotel", "canceled",
               "market_seg", "repeated", "r_status")

# --- 3️⃣ Create numeric and qualitative subsets
df_num <- hotel[, num_vars]
df_quali <- hotel[, quali_vars]

# --- 4️⃣ Convert qualitative vars to factors (important!)
df_quali[] <- lapply(df_quali, as.factor)

# --- 5️⃣ Combine them and remove rows with any missing value
DF_all <- cbind(df_num, df_quali)
DF_all <- na.omit(DF_all)

# --- 6️⃣ Indices of supplementary qualitative variables
quali.sup.idx <- (ncol(DF_all) - length(quali_vars) + 1):ncol(DF_all)

# --- 7️⃣ Run PCA with supplementary qualitative variables
pca_sup <- PCA(DF_all,
               scale.unit = TRUE,
               quali.sup = quali.sup.idx,
               graph = FALSE)

summary(pca_sup)
fviz_pca_biplot(pca_sup,
               col.quali.sup = "#d62728",
               repel = TRUE,
               title = "Supplementary Qualitative Modalities (red)")
# --- 8️⃣ Common projection: numerical + qualitative modalities
library(factoextra)
library(ggplot2)
var_coords <- as.data.frame(pca_sup$var$coord)
var_coords$label <- rownames(var_coords)

# Plot both layers
ggplot() +
  # 1️⃣ numeric variables (blue arrows)
  geom_segment(data = var_coords,
               aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "blue") +
  geom_text(data = var_coords, aes(x = Dim.1, y = Dim.2, label = label),
            color = "blue", vjust = -0.5, size = 3.2) +
  # 2️⃣ qualitative modalities (red labels)
  geom_point(data = quali_coords, aes(x = Dim.1, y = Dim.2), color = "red", size = 2) +
  geom_text(data = quali_coords, aes(x = Dim.1, y = Dim.2, label = label),
            color = "red", vjust = -0.4, size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  labs(title = "Common Projection: Numerical Variables (blue) + Qualitative Modalities (red)",
       x = paste0("Dim1 (", round(pca_sup$eig[1, 2], 1), "%)"),
       y = paste0("Dim2 (", round(pca_sup$eig[2, 2], 1), "%)")) +
  theme_minimal()