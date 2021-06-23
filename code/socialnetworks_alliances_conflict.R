# Seminar on Statistical Modelling of Social Networks
# Topic: The Additive and Multiplicative Effects Network Model

# Create conflict indicator

library(network)
library(xergm.common)

# Data Setup
data("alliances")
countrycowc <- get.vertex.attribute(allyNet[["2000"]], "vertex.names")

year <- c("1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1990", 
          "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000")

former <- c("YAR", "YPR", "GFR", "GDR", "CZE", "YUG")
current <- !(countrycowc %in% former)


# Conflict Indicator
conflict <- matrix(0, nrow = sum(current), ncol = sum(current))

for (yr in year[10:19]){
  conflict = conflict + data.matrix(warNet[[yr]][current, current])
}

table(conflict)

conflict <- ifelse(conflict > 0, 1, 0)

saveRDS(conflict, file = "data/ConflictInd.rds")