table_ols_list <- list()

table1 <- matrix(NA,13,3)
rownames(table1) <- c('Constant (alpha)', 'GDP per capita (beta)', 
                '(GDP per capita )^2 (gamma)', 'NUTS 3 Number (lambda)', 
                '(Coefficient of determination (R^2)', 'Jarque-Bera (normality)',
                'Breusch-Pagan (heteroskedasticity)', 'Diagnostics for spatial dependence',
                'Moran', 'LMERR', 'R-LMERR', 'LMLAG', 'R-LMLAG')


#### access coefficients
summaries <- lapply(lm, summary)
# ...coefficents with p values:
#lapply(summaries[1], function(x) x$coefficients[, c(1,4)])
summary1 <- lapply(summaries[1], function(x) x$coefficients)[[1]]

# ...or r-squared values
sapply(summaries[1], function(x) c(r_sq = x$r.squared, 
adj_r_sq = x$adj.r.squared))

sapply(summaries[1], function(x) x)

table1[1:4,2] <- summary1[1:4,1]
table1[1:4,3] <- summary1[1:4,4]
table1[5,2] <- sapply(summaries[1], function(x) c(r_sq = x$r.squared, 
                                                  adj_r_sq = x$adj.r.squared))[1]
table1[6,2:3] <- jb_lm[1,]
table1[7,2:3] <- bp_lm[1,]
table1[9,2:3] <- moran_Y[1,1:2]
table1[10,2:3] <- lm_tests[1,1:2]
table1[11,2:3] <- lm_tests[1,5:6]
table1[12,2:3] <- lm_tests[1,3:4]
table1[13,2:3] <- lm_tests[1,7:8]

table1 <- round(table1, digits = 4)


table_ols_list <- list()
table <- matrix(1,13,3)
for (i in 1:k) {
  table_ols_list[i] <- table
    }
