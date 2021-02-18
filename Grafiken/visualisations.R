setwd("/Users/tobiaspk1/Documents/Bakkarbeit/neighbRapply/R")
source("neighb_apply2_manipulation.R")

library(ggplot2)

# Nachbarschaften zeichen ----------------------------------------
moore <- generate_coords_default(1, width = 2, include.own = TRUE)
newm <- generate_coords_default(2, width = 2, include.own = TRUE)
diam <- generate_coords_default(3, width = 2, include.own = TRUE)

nbhoods <- rbind(cbind(moore, 1), cbind(newm, 2), cbind(diam, 3))
nbhoods <- as.data.frame(nbhoods)
names(nbhoods) <- c("x", "y", "hoods")
nbhoods$hoods <- factor(nbhoods$hoods, levels = 1:3,
                        labels = c("Moore", "Von Neumann", "Diamant"))
nbhoods$middle <- rowSums(abs(nbhoods[, 1:2])) == 0

ggplot(nbhoods, aes(x = x, y = y, col = middle)) + 
  geom_point(shape = 15, size = 14) + 
  xlim(-2.5, 2.5) + ylim(-2.5, 2.5) +
  facet_grid(. ~ hoods) + 
  scale_color_manual(values = c("grey10", "darkred"), guide = FALSE) +
  theme_minimal() + 
  theme(aspect.ratio = 1) +
  theme(strip.text.x = element_text(size = 20),
        axis.title = element_text(size = 20),
        panel.grid = NULL)

# Matrix ??ffnen zeichnen ----------------------------------------
newm2 <- as.data.frame(generate_coords_default(2, width = 1, include.own = TRUE))
names(newm2) <- c("x", "y")
newm2$middle <- 0
newm2$middle[3] <- 1
newm2$middle[1] <- 2

par(mfrow = c(1, 2), pty = "s", mar=c(3, 1, 2, 1))

cols <- c("grey10", "darkred", "orange")
plot(newm2$x, newm2$y,
     xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", main = "Vor Lag")
grid(lty = 1, lwd = .5)
points(newm2$x, newm2$y, pch = 15, cex = 10, col = cols[newm2$middle+1])
axis(1, at = c(-1, 0, 1))
axis(2, at = c(-1, 0, 1))


pchs <- c(15, 20, 20)
cols <- c("grey10", "darkred", "orange", "grey70", "darkred", "grey70")
plot(newm2$x, newm2$y,
     xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", main = "Nach Lag")
grid(lty = 1, lwd = .5)
points(newm2$x, newm2$y, pch = 15, cex = 10, col = cols[newm2$middle+4])
points(newm2$x + 1, newm2$y, pch = 15, cex = 10, col = cols[newm2$middle+1])
points(c(-1, -1, -1), c(-1, 0, 1), pch = 7, cex = 10, col = "grey50")
axis(1, at = c(-1, 0, 1))

