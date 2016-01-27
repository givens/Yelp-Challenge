# avg_review.R
# Analysis of Yelp academic dataset
# -----
# Looks at ratings of restaurants
# Sorts restaurants by number of shops
# Expecting to find McDonalds, Subway, and Panera...
# Determines average review

# Make use of libraries
library(dplyr)
library(stringr)
library(ggplot2)

# fn:  Return the First Value
first <- function(x) {
    x[1]
}

# Load only if data does not exist
#biz <- read.csv("yelp_academic_dataset_business.csv")
#rev <- read.csv("yelp_academic_dataset_review.csv")

# Ensure data is in data frame
#biz <- data.frame(biz)
#rev <- data.frame(rev)

# Use regexp to find the restaurant category
idx.rest <- grep('.Restaurants.',biz$cat)
rest <- biz[idx.rest,]

# Summarized and Sorted (ss)
ss <- rest %>%
    mutate(cname = substring(tolower(
        sub('[Bb]urger','bg',
        sub('[Rr]otisserie','ro',
        sub('Pittsburgh','pt',
        sub('[Bb]ar','br',
        sub('[Gg]rill','gr',
        sub('[Hh]ot','h',
        sub('[Dd]og','d',
        sub('[Pp]izza','pz',
        sub('[Rr]oast','',
        sub('[Bb]eef','',
        sub('[Tt]he','',
        sub('[Aa]nd','',
        sub('[Ee]xpress','ex',
        sub('[Pp]anda','pd',
        sub('[Hh]ong','h',
        sub('[Kk]ong','k',
        sub('[Cc]alifornia','ca',
        sub('[Cc]arolina','cl',
        sub('[Aa]merica','am',
        sub('[Oo]riginal','og',
        sub('China','cn',
        sub('[Rr]estaurant','rt',
            str_replace_all(name,"[()!&' .-]","")))))))))))))))))))))))),
        1,8),
           tot_stars = stars*review_count) %>%
    group_by(cname) %>%
    summarize(name = sample(name,1),
              number_shops = n(),
              number_reviews = sum(review_count),
              ts = sum(tot_stars)) %>%
    mutate(avg_review = ts/number_reviews) %>%
    #select(-ts, -cname) %>%
    select(-ts) %>%
    arrange(desc(number_shops))

write.csv(ss,file = 'ss.csv', row.names = FALSE)
