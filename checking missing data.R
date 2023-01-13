# is important to also check missing data
#missing data per question
sapply(data, function(x) sum(is.na(x))) %>% data.frame()
#substance_use (22) and sum of ghq-12 (19) are the items with the most missing
#first case may be normal, as the question is a little bit sensitive and the number of individuals receiving treatment for substance abuse in the primary health care
#system isn't that high

#missing data per user
na_rows <- rowSums(is.na(data)); na_rows
#  0   1   2   3   4   5   6   7  10  11  13  20  24  37  44 
#446  29   9   8   1   2   1   1   2   1   2   1   1   1   2
#there is not that much missing data, but there are some interesting cases with almost 20 unanswered questions
#let's check them  
       
#let's check individuals with more than 19 missings
data[c(which(na_rows>19)),] %>% View()
#ok. so 2 of them are people that didn't answer any question
#one person that answered only sociodemographic variables
#one person that answered sociodemographic variables and ghq-12 variables
#and one person that answered only perceived stigma and discrimination variables
