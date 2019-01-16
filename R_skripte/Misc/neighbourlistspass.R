plot(coords)

text(coords,labels=1:266)

plot(shp2,add=TRUE)

shp_list[[1]]$y[140]
shp_list[[1]]$y[140,]
shp_list[[1]]$y
shp_list[[1]]$Y[140]


#LISA


tests.lisa<-data.long %>% filter(time==2000)


tests.lisa$lisa<- localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")[,1]
tests.lisa$lisa_prob <- localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")[,5]


lisa <-localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")

#shp2.lisa <- fortify(shp2, region="NUTS_ID")
shp.lisa <- merge(shp2.lisa,tests.lisa, all.x = FALSE, all.y = TRUE, by.x = "NUTS_ID", by.y = "nuts_2")

library(GISTools)
sids79.shading <- auto.shading(c(lisa[,1], -lisa[,1]),
                               cols=brewer.pal(5, "PRGn"))
choropleth(shp, lisa[,1], shading=sids79.shading, main="Disposable income in EU NUTS 2")
#locator(1)
choro.legend(-1423654, 11198209, sh=sids79.shading, cex=0.6, title="Local Moran's I")


#LISA2

tests.lisa$lisa<- localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")[,1]
tests.lisa$lisa_prob <- localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")[,5]


lisa <-localmoran(lm[[i]]$residuals, listw = W.list.k, alternative = "greater")

#shp2.lisa <- fortify(shp2, region="NUTS_ID")
shp.lisa <- merge(shp2.lisa,tests.lisa, all.x = FALSE, all.y = TRUE, by.x = "NUTS_ID", by.y = "nuts_2")

library(GISTools)
sids79.shading <- auto.shading(c(lisa[,1], -lisa[,1]),
                               cols=brewer.pal(5, "PRGn"))
choropleth(shp, lisa[,1], shading=sids79.shading, main="Disposable income in EU NUTS 2")
#locator(1)
choro.legend(-1423654, 11198209, sh=sids79.shading, cex=0.6, title="Local Moran's I")
