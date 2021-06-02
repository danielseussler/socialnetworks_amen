# Seminar of Statistical Modeling of Social Networks
# Help: Conflict Indicator - 1 if militarized dispute in the past 10Yrs

library(xergm.common)
data("alliances")

year <- c("1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1990", 
          "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000")

conflict <- matrix(0, nrow = 164, ncol = 164)
 
for (yr in year[10:19]){
  conflict = conflict + data.matrix(warNet[[yr]])
}

conflict <- ifelse(conflict > 0, 1, 0)

saveRDS(conflict, file = "data/Conflict.rds")