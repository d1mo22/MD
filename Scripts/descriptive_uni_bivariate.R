# hotel_analysis.R
# Análisis univariado y bivariado de datos hoteleros
# Asegúrate de haber cargado tus datos en `dd`
# -------------------------------
# 1️⃣ PDF de gráficas univariadas
# -------------------------------
pdf("hotel_univariate.pdf", width = 10, height = 8)
for (k in 1:ncol(dd)) {
  descriptiva(dd[, k], names(dd)[k])  # tu función univariada
}
dev.off()
cat("✅ PDF con gráficas univariadas generado correctamente.\n")

# -------------------------------
# 2️⃣ PDF de gráficas bivariadas (HIGH PRIORITY)
# -------------------------------
pdf("hotel_bivariate.pdf", width = 10, height = 8)

# HIGH PRIORITY PLOTS
# 1. Lead Time vs. Cancellation (continuous vs. binary)
boxplot(dd$lead_time ~ dd$canceled,
        main = "Lead Time vs. Cancellation",
        xlab = "Canceled (0=No, 1=Yes)", ylab = "Lead Time (days)",
        col = c("lightgreen", "lightcoral"))

# 2. ADR vs. Cancellation (continuous vs. binary)
boxplot(dd$adr ~ dd$canceled,
        main = "ADR (Average Daily Rate) vs. Cancellation",
        xlab = "Canceled (0=No, 1=Yes)", ylab = "ADR (€)",
        col = c("lightgreen", "lightcoral"))

# 3. Lead Time vs. ADR (continuous vs. continuous)
plot(dd$lead_time, dd$adr,
     main = "Lead Time vs. ADR",
     xlab = "Lead Time (days)", ylab = "ADR (€)",
     pch = 16, col = rgb(0, 0, 1, 0.3))

# 4. Market Segment vs. Cancellation (categorical vs. binary)
market_cancel <- table(dd$market_seg, dd$canceled)
market_cancel_prop <- prop.table(market_cancel, margin = 1)
colors_market <- c("steelblue", "coral", "seagreen", "gold", "plum", "aquamarine", "burlywood")
barplot(market_cancel_prop,
        main = "Cancellation Rate by Market Segment",
        xlab = "Canceled", ylab = "Proportion",
        legend.text = rownames(market_cancel_prop),
        beside = TRUE, col = colors_market[1:nrow(market_cancel_prop)])

# 5. Channel vs. Cancellation (categorical vs. binary)
channel_cancel <- table(dd$channel, dd$canceled)
channel_cancel_prop <- prop.table(channel_cancel, margin = 1)
colors_channel <- c("steelblue", "coral", "seagreen", "gold", "plum")
barplot(channel_cancel_prop,
        main = "Cancellation Rate by Booking Channel",
        xlab = "Canceled", ylab = "Proportion",
        legend.text = rownames(channel_cancel_prop),
        beside = TRUE, col = colors_channel[1:nrow(channel_cancel_prop)])

# MEDIUM PRIORITY PLOTS
# 6. Total Stay Length vs. Cancellation
dd$total_stay <- dd$week_nights + dd$weekend_nights
boxplot(dd$total_stay ~ dd$canceled,
        main = "Total Stay Length vs. Cancellation",
        xlab = "Canceled (0=No, 1=Yes)", ylab = "Total Nights",
        col = c("lightgreen", "lightcoral"))

# 7. Repeated Guest vs. Cancellation (binary vs. binary)
repeat_cancel <- table(dd$repeated, dd$canceled)
repeat_cancel_prop <- prop.table(repeat_cancel, margin = 1)
barplot(repeat_cancel_prop,
        main = "Cancellation Rate by Repeat Guest Status",
        xlab = "Canceled", ylab = "Proportion",
        legend.text = c("New Guest", "Repeat Guest"),
        beside = TRUE, col = c("steelblue", "coral"))

# 8. Deposit Type vs. Cancellation (categorical vs. binary)
deposit_cancel <- table(dd$deposit_type, dd$canceled)
deposit_cancel_prop <- prop.table(deposit_cancel, margin = 1)
colors_deposit <- c("steelblue", "coral", "seagreen")
barplot(deposit_cancel_prop,
        main = "Cancellation Rate by Deposit Type",
        xlab = "Canceled", ylab = "Proportion",
        legend.text = rownames(deposit_cancel_prop),
        beside = TRUE, col = colors_deposit[1:nrow(deposit_cancel_prop)])

# 9. Special Requests vs. Cancellation (continuous vs. binary)
boxplot(dd$ts_requests ~ dd$canceled,
        main = "Special Requests vs. Cancellation",
        xlab = "Canceled (0=No, 1=Yes)", ylab = "Number of Special Requests",
        col = c("lightgreen", "lightcoral"))

# 10. Customer Type vs. Cancellation (categorical vs. binary)
customer_cancel <- table(dd$customer_type, dd$canceled)
customer_cancel_prop <- prop.table(customer_cancel, margin = 1)
colors_customer <- c("steelblue", "coral", "seagreen", "yellow")
barplot(customer_cancel_prop,
        main = "Cancellation Rate by Customer Type",
        xlab = "Canceled", ylab = "Proportion",
        legend.text = rownames(customer_cancel_prop),
        beside = TRUE, col = colors_customer[1:nrow(customer_cancel_prop)])

# SUPPORTING PLOTS
# 11. Month vs. Cancellation (categorical vs. binary)
month_cancel <- table(dd$month, dd$canceled)
month_cancel_prop <- prop.table(month_cancel, margin = 1)
colors_month <- c("steelblue", "coral", "seagreen", "gold", "plum", "orange", 
                  "lightgreen", "pink", "cyan", "brown", "yellow", "purple")
barplot(month_cancel_prop,
        main = "Cancellation Rate by Booking Month",
        xlab = "Canceled", ylab = "Proportion",
        legend.text = rownames(month_cancel_prop),
        beside = TRUE, col = colors_month[1:nrow(month_cancel_prop)])

# 12. Number of Children vs. ADR (categorical vs. continuous)
boxplot(dd$adr ~ dd$children,
        main = "ADR by Number of Children",
        xlab = "Number of Children", ylab = "ADR (€)",
        col = "lightblue")

# 13. Previous Cancellations vs. Current Cancellation (binary vs. binary)
precancel_cancel <- table(dd$pre_cancel, dd$canceled)
precancel_cancel_prop <- prop.table(precancel_cancel, margin = 1)
barplot(precancel_cancel_prop,
        main = "Current Cancellation by Previous Cancellation History",
        xlab = "Current Booking Canceled", ylab = "Proportion",
        legend.text = c("No Previous Cancellation", "Previous Cancellation"),
        beside = TRUE, col = c("steelblue", "coral"))

dev.off()
cat("✅ PDF con gráficas bivariadas generado correctamente.\n")

