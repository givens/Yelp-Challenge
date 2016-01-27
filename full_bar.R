# full_bar.R
# Analysis of Yelp academic dataset
# -----
# Looks at ratings of full_bar and other categories
# Determines full_bar star ratings are slightly less
# on average and this is significant when analyzed with a
# t-test.

#library(dplyr)

biz <- read.csv("yelp_academic_dataset_business.csv")
#rev <- read.csv("yelp_academic_dataset_review.csv")

#----

attr <- biz$attributes.Alcohol
#rest <- (attr=='none'|attr=='full_bar'|attr=='beer_and_wine')
#-----
rest <- !(attr=='') # none, full_bar, beer_and_wine, and possibly other
biz.rest <- biz[rest,]
#-----
attr2 <- biz.rest$attributes.Alcohol
#-----
full_bar <- (attr2=='full_bar')
biz.full_bar <- biz.rest[full_bar,]
#----
not_full_bar <- !full_bar
biz.not_full_bar <- biz.rest[not_full_bar,]
#----
biz_id.full_bar <- biz.full_bar$business_id
biz_id.not_full_bar <- biz.not_full_bar$business_id
#-----

# don't load rev every time
#rev <- read.csv("yelp_academic_dataset_review.csv")

#------
biz_id.rev <- rev$business_id;

x.full_bar <- biz_id.rev %in% biz_id.full_bar;
x.not_full_bar <- biz_id.rev %in% biz_id.not_full_bar;

#-----

stars.full_bar <- rev[x.full_bar,]$stars
stars.not_full_bar <- rev[x.not_full_bar,]%stars

#----

hist(stars.full_bar)
hist(stars.not_full_bar)

#-----

m.full_bar <- mean(stars.full_bar)
sd.full_bar <- sd(stars.full_bar)
m.not_full_bar <- mean(stars.not_full_bar)
sd.not_full_bar <- sd(stars.not_full_bar)
N.full_bar <- length(stars.full_bar)
N.not_full_bar <- length(stars.not_full_bar)
#-----

# t-test with
tt <- t.test(stars.full_bar,stars.not_full_bar,alternative = "two.sided",var.equal=FALSE,conf.level=0.99)
