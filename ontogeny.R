# load packages needed
library(xlsx)

# load the iter.cor.test function
source("iter.cor.test.R")

# read datasets
larvae <- read.csv("larvae.csv", stringsAsFactors = FALSE)
embryos <- read.csv("embryos.csv", stringsAsFactors = FALSE)

# Add a rank variable adding up all of the tooth rows
embryos <- cbind(embryos,
                 LTRFExt = apply(embryos[, c("AnExt", "PosExt")], MARGIN = 1, FUN = sum),
                 LTRFCom = apply(embryos[, c("AnCom", "PosCom")], MARGIN = 1, FUN = sum))

#Individual No 360 in the larvae dataset shows a huge BL that might be affecting other data, rm it
larvae <- larvae[-360, ]

# Individual with BL = 80 in the embryos dataset affecting the data, rm it
embryos$BL[which(embryos$BL > 80)] <- NA

# cor.test was used for finding Spearman's rho and its respective p value, that is, the probability of finding that rho or a higher one, what measures the confidence on the rho found. See Mukuka (2012) for interpretations, but roughly speaking rho should be interpreted this way:
# 0.0 < negligible < 0.3
# 0.3 < low < 0.5
# 0.5 < moderate < 0.7
# 0.7 < high < 0.9
# 0.9 < very high < 1.0

### Data frame with total number of rows, GS and BL

toothRows <- data.frame(BL = c(embryos$BL, larvae$BL),
                        GS = c(embryos$GS, larvae$GS),
                        A = c(embryos$AnExt, larvae$AnExt),
                        P = c(embryos$PosExt, larvae$Pos),
                        stringsAsFactors = FALSE)
toothRows <- cbind(toothRows, Total = (toothRows$A + toothRows$P))


# Spearman's rho correlation for larvae (x = c("BL", "GS"), y = c("AnExt", "AnCom"), data = larvae)

larvaCor <- iter.cor.test(x = c("BL", "GS"), y = c("AnExt", "AnCom"), data = larvae, method = "spearman", exact = FALSE, paired = FALSE)

# Spearman's rho correlation for embryos will not work for embryos out of the box, order might work (0/0 > 0/1 > 0/2 > 1/2)
# formals x = c("BL", "GS"), y = c("LTRFExt", "LTRFCom"), data = larvae

embryoCor <- iter.cor.test(x = c("BL", "GS"), y = c("LTRFExt", "LTRFCom"), data = embryos, method = "spearman", exact = FALSE, paired = FALSE)

# Generate tables with rho + p-value

larvaTable <- data.frame(Contrast = names(larvaCor),
                          Rho = unname(unlist(larvaCor)[grep("estimate", names(unlist(larvaCor)))]),
                          p.value = unname(unlist(larvaCor)[grep("p.value", names(unlist(larvaCor)))]), stringsAsFactors = FALSE)

larvaTable$Contrast <- gsub("_", " ", larvaTable$Contrast)
larvaTable$Contrast <- gsub("AnExt", "A-n Extended", larvaTable$Contrast)
larvaTable$Contrast <- gsub("AnCom", "A-n Compact", larvaTable$Contrast)
larvaTable$Rho <- round(as.numeric(larvaTable$Rho), digits = 4)
larvaTable$p.value <- as.numeric(larvaTable$p.value)

embryoTable <- data.frame(Contrast = names(embryoCor),
                          Rho = unname(unlist(embryoCor)[grep("estimate", names(unlist(embryoCor)))]),
                          p.value = unname(unlist(embryoCor)[grep("p.value", names(unlist(embryoCor)))]), stringsAsFactors = FALSE)
embryoTable$Contrast <- gsub("_", " ", embryoTable$Contrast)
embryoTable$Contrast <- gsub("LTRFExt", "LTRF Extended", embryoTable$Contrast)
embryoTable$Contrast <- gsub("LTRFCom", "LTRF Compact", embryoTable$Contrast)
embryoTable$Rho <- round(as.numeric(embryoTable$Rho), digits = 4)
embryoTable$p.value <- as.numeric(embryoTable$p.value)

# Table for anomaly frequencies
anomalies <- read.csv("anomalies.csv", stringsAsFactors = FALSE)
relAnomalies <- cbind(anomalies[, c(1, 2)], anomalies[, 3:8]/anomalies$n*100)

# write tables to xls files
write.xlsx(relAnomalies, file = "Table 2.xls", row.names = FALSE)
write.xlsx(larvaTable, file = "Table 3.xls", row.names = FALSE)
write.xlsx(embryoTable, file = "Table 1.xls", row.names = FALSE)

# gap in P1 variation barplot
gaps <- c(Absent = length(grep("(1)", larvae$LTRF, fixed = TRUE)),
          Present = (length(larvae$LTRF) - length(grep("(1)", larvae$LTRF, fixed = TRUE))))/length(larvae$LTRF)*100

### Figures

# Symmetry barplot
symmetry <- c(Asymmetric = sum(table(larvae$Asymmetry)[c("Left", "Right")]),
              Symmetric = unname(table(larvae$Asymmetry)[3]))/sum(table(larvae$Asymmetry))*100

png(filename = "Figure 4.png", width = 700, height = 700)
par(cex = 2,
    oma = c(1, 1, 1, 1))
barplot(symmetry,
        main = "Asymmetry Variation in the A2 LTR",
        ylab = "Percentage", 
        ylim = c(0, 60),
        width = 0.25,
        )
dev.off()

# overall row vbariation multiplot
# mar = margins for each plot
# oma = margins for the whole plot area
# set par(cex = ) before plotting each element as overriding happens otherwise
png(filename = "Figure 1.png", width = 1200, height = 1000, res = 170)

par(cex = 2, oma = c(5, 5, 3, 2), mar = c(1.5, 1.5, 1.5, 1.5) + 0.1)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

plot(x = toothRows$BL, y = toothRows$Total, yaxt = "n", ylab = "", xlab = "")
axis(side = 2, at = c(0, 1, 2, 3, 4), labels = c("0/0", "0/1", "0/2", "1/2", "2/2"), las = 2)
abline(v = 5, lty = 2, col = "gray28")
plot(x = embryos$BL, y = apply(cbind(embryos$AnExt, embryos$PosExt), 1, FUN = sum), yaxt = "n", ylab = "", xlab = "")
axis(side = 2, at = c(0, 1, 2, 3, 4), labels = c("0/0", "0/1", "0/2", "1/2", "2/2"), las = 2)
plot(x = larvae$BL, y = larvae$AnExt + 2, yaxt = "n", ylab = "", xlab = "")
axis(side = 2, at = c(0, 1, 2, 3, 4), labels = c("0/0", "0/1", "0/2", "1/2", "2/2"), las = 2)

mtext("LTRF variation through ontogeny", outer = TRUE, cex = 1.7)
par(cex = 1.2)
title(xlab = "Body length (mm)", ylab = "Labial Tooth Row Formula (LTRF)", outer = TRUE)

dev.off()

### material examined, GS ranges
paste(min(embryos$GS[embryos$CatalogNumber == "CATALOG NUMBER"], na.rm = TRUE), sep = " - ", max(embryos$GS[embryos$CatalogNumber == "CATALOG NUMBER"], na.rm = TRUE))

paste(min(larvae$GS[larvae$CatalogNumber == "CATALOG NUMBER"], na.rm = TRUE), sep = " - ", max(larvae$GS[larvae$CatalogNumber == "CATALOG NUMBER"], na.rm = TRUE))
