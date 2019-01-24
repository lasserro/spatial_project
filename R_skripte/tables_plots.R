# table_ols_list <- list()
# 
# table1 <- matrix(NA,13,3)
# rownames(table1) <- c('Constant (alpha)', 'GDP per capita (beta)', 
#                 '(GDP per capita )^2 (gamma)', 'NUTS 3 Number (delta)', 
#                 '(Coefficient of determination (R^2)', 'Jarque-Bera (normality)',
#                 'Breusch-Pagan (heteroskedasticity)', 'Diagnostics for spatial dependence',
#                 'Moran', 'LMERR', 'R-LMERR', 'LMLAG', 'R-LMLAG')
# 
# 
# #### access coefficients
# summaries <- lapply(lm, summary)
# # ...coefficents with p values:
# #lapply(summaries[1], function(x) x$coefficients[, c(1,4)])
# summary1 <- lapply(summaries[1], function(x) x$coefficients)[[1]]
# 
# # ...or r-squared values
# sapply(summaries[1], function(x) c(r_sq = x$r.squared, 
# adj_r_sq = x$adj.r.squared))
# 
# sapply(summaries[1], function(x) x)
# 
# table1[1:4,2] <- summary1[1:4,1]
# table1[1:4,3] <- summary1[1:4,4]
# table1[5,2] <- sapply(summaries[1], function(x) c(r_sq = x$r.squared, 
#                                                   adj_r_sq = x$adj.r.squared))[1]
# table1[6,2:3] <- jb_lm[1,]
# table1[7,2:3] <- bp_lm[1,]
# table1[9,2:3] <- moran_Y[1,1:2]
# table1[10,2:3] <- lm_tests[1,1:2]
# table1[11,2:3] <- lm_tests[1,5:6]
# table1[12,2:3] <- lm_tests[1,3:4]
# table1[13,2:3] <- lm_tests[1,7:8]
# 
# table1 <- round(table1, digits = 4)


###################### Table OLS List ######################
summaries <- lapply(lm, summary)

summary_coef <- list()
for (i in 1:k) {
  summary_coef[i] <- lapply(summaries[i], function(x) x$coefficients)
}
names(summary_coef) <- period

table_ols_list <- list()
table <- matrix(NA,13,3)
for (i in 1:k) {
  table_ols_list[[i]] <- table
}

for (i in 1:k) {
  rownames(table_ols_list[[i]]) <- c('Constant (alpha)', 'GDP per capita (beta)', 
                                     '(GDP per capita )^2 (gamma)', 'NUTS 3 Number (delta)', 
                                     '(Coefficient of determination (R^2)', 'Jarque-Bera (normality)',
                                     'Breusch-Pagan (heteroskedasticity)', 'Diagnostics for spatial dependence',
                                     'Moran', 'LMERR', 'R-LMERR', 'LMLAG', 'R-LMLAG')
  table_ols_list[[i]][1:4,2] <- summary_coef[[i]][1:4,1]
  table_ols_list[[i]][1:4,3] <- summary_coef[[i]][1:4,4]
  table_ols_list[[i]][5,2] <- sapply(summaries[i], function(x) r_sq = x$r.squared)
  table_ols_list[[i]][6,2:3] <- jb_lm[i,]
  table_ols_list[[i]][7,2:3] <- bp_lm[i,]
  table_ols_list[[i]][9,2:3] <- moran_Y[i,1:2]
  table_ols_list[[i]][10,2:3] <- lm_tests[i,1:2]
  table_ols_list[[i]][11,2:3] <- lm_tests[i,5:6]
  table_ols_list[[i]][12,2:3] <- lm_tests[i,3:4]
  table_ols_list[[i]][13,2:3] <- lm_tests[i,7:8]
  table_ols_list[[i]] <- round(table_ols_list[[i]],digits = 4)
}

####### latextable
library(xtable)

xtable(table_ols_list[[20]])
# stargazer::stargazer(table_ols_list[[1]])
################################################################################
################ plot #########
library(latticeExtra)

# plot Y (inequality index, 1996)
grps <- 10
brks <- quantile(shp_list[[1]]$Y, 0:(grps-1)/(grps-1), na.rm=TRUE)
p <- spplot(shp_list[[1]], 'Y', at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")
p + layer(sp.polygons(shp_list[[1]]))

# plot gdp (per/cap, scale * 1Mio, 1996)
grps <- 10
brks <- quantile(shp_list[[1]]$Gdp, 0:(grps-1)/(grps-1), na.rm=TRUE)
p2 <- spplot(shp_list[[1]], 'Gdp', at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")
p2 + layer(sp.polygons(shp_list[[1]]))

# plot lisa (local moran, 2015)
shp15 <- shp_list[[20]]
lisa <- lisa.test[[1]]
lisa[c(77,123),] <- NA # rm 'Ausreißer' from data
shp15@data$LISA <- lisa[,1]
grps3 <- 7
brks3 <- quantile(shp15$LISA, 0:(grps3-1)/(grps3-1), na.rm=TRUE)
p3 <- spplot(shp15, 'LISA', at=brks3, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")
p3 + layer(sp.polygons(shp15))

#_______________________________________________________________________________
# tables

xtable(sar[[1]])
xtable(sem[[1]])
xtable(sdm[[1]])

xtable(sem_gm[[1]])
xtable(sar_gm[[1]])

xtable(table_ols_list[[1]])

xtable(sar.impacts[[1]])

sar_sum <- lapply(sar, summary)
sem_sum <- lapply(sem, summary)
stargazer::stargazer(sar_sum[1])

summary(sar_gm[[1]])
sar_gm_summaries <- lapply(sar_gm, summary)
sar_gm_summaries[[1]]


###### Stargazer several year #####
stargazer::stargazer(sar[1], sar[5], sar[10], sar[15], sar[20], LMtest = T)
stargazer::stargazer(sar.2stls.hac_summaries[1],sar.2stls.hac_summaries[5],sar.2stls.hac_summaries[10],sar.2stls.hac_summaries[15],sar.2stls.hac_summaries[20])

xtableList(sar.2stls.hac_summaries[c(1,5,10,15,20)])
