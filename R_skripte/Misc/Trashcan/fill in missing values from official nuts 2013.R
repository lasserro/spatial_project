
# get the official nuts 2013 classification

nuts_2013 <- read.table("./RData/NUTS2013 all.csv",
                header = TRUE,
                sep = "\t",
                skip = 1,
                comment.char = ";",
                check.names = FALSE,
                stringsAsFactors = FALSE)

colnames(nuts_2013) <- "code2013"

# get rid of all (useless) z, zz, zzz. NO IDEA what those are...

nuts_2013 <- nuts_2013 %>% mutate (zzzz = substring(code2013,3)) %>%
  filter(!zzzz %in% "Z",
         !zzzz %in% "ZZ",
         !zzzz %in% "ZZZ"
         ) %>% 
  select(-zzzz)

# add the wishlist to the actual dataframe

GDP_ERD_fill <- left_join(nuts_2013,GDP_ERD, by = c("code2013" = "nuts_code"))
         
GG <- GDP_ERD_fill
 
for (i in 1:length(GDP_ERD_fill$code2013)) {
  
  if(GDP_ERD_fill[i,2] %in% NA)
  {GDP_ERD_fill[i,-(1:2)] <- GDP_ERD_fill[i-1,-(1:2)]
  GDP_ERD_fill[i,2] <- as.numeric(GDP_ERD_fill[i-1,2])+1
  GDP_ERD_fill[i,4] <- substr(GDP_ERD_fill[i,1],1,4)
  }
}