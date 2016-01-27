# better_service.R
# Analysis of Yelp academic dataset
# -----
# Reads business data
# Categories by full service, fast casual, and quick service (fast food)
# Computes averages of each category


biz <- read.csv("yelp_academic_dataset_business.csv")

# Select restaurants

idx.rest <- grep('Restaurants',biz$cat)
rest <- biz[idx.rest,]

# Split into separate lists
# Servers is 'True' or 'False' factor

# Full service, Fast Casual, or Quick Service

rest.servers = split(rest,rest$attributes.Waiter.Service,drop=T)
fs <- rest.servers$True # full service
other <- rest.servers$False # the two other categories

idx.qs <- grep('Fast[ ]Food',other$cat)

qs <- other[idx.qs,]
fc <- other[-idx.qs,]

#fs.stars = fs$stars
#qs.stars = qs$stars
#fc.stars = fc$stars

m = list(fs=mean(fs$stars),
         qs=mean(qs$stars),
         fc=mean(fc$stars))

tt1 = t.test(fs$stars,qs$stars)
tt2 = t.test(fs$stars,fc$stars)
