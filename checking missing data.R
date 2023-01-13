# is important to also check missing data
#missing data per question
sapply(data, function(x) sum(is.na(x))) %>% data.frame()

#missing data per user
na_rows <- rowSums(is.na(data)); na_rows

#let's check individuals with more than 19 missings

data[c(which(na_rows>19)),] %>% View()


