# get libertarian twitter account info

library(twitteR)

seeds = c("RonPaul", "RandPaul", "CatoInstitute", "reason",
    "ThomasEWoods", "jeffreyatucker", "StefanMolyneux", "bryan_caplan",
    "sfliberty", "mises", "FreeStateNH", "c4ssdotorg", "lewrockwell",
    "GovGaryJohnson", "RoderickTLong", "BobMurphyEcon", "LPNational",
    "FreeTalkLive", "Antiwarcom", "AynRandInst", "AynRandOrg",
    "ErikVoorhees")


setup_twitter_oauth(consumer_key = "[redacted]",
                    consumer_secret = "[redacted]",
                    access_token = "[redacted]",
                    access_secret = "[redacted]")

# collect the data that will be used to identify libertarian twitter
# users
r1 = lookupUsers(seeds)
followers = list()
friends = list()

# get first set of seeds
for (node in names(r1)) {
  print(node)
  
  # using retryOnRateLimit, found on Github, to forestall problems
  # https://github.com/geoffjentry/twitteR/blob/master/R/comm.R
  friends[[node]] = r1[[node]]$getFriendIDs(retryOnRateLimit = 20)
  followers[[node]] = r1[[node]]$getFollowerIDs(retryOnRateLimit = 20)
}

save(r1, followers, friends,
     file = paste0("followers_",  Sys.Date(), ".RData"))

# narrow it down to the real libertarians
followers_table = table(unlist(followers))
lvs = names(followers_table)[followers_table >= 3]

# estimated number of libertarians
n_libertarians = length(lvs)

# add back the seed accounts, in case they weren't included
lvs = union(lvs, as.vector(sapply(r1, function(x) x$id)))
# trying to cut down on memory usage
rm(r1)
# adding John Stossel and the ACLU, because I want to track those
lvs = union(lvs, c("158769098", "13393052"))

seed_friends = table(unlist(friends))
tops = seed_friends[seed_friends >= 6]
rm(seed_friends)

# the sample
s = sample(lvs, size = 1000)
rm(lvs)
# get all the data at once
s1 = lookupUsers(s, retryOnRateLimit = 20)

sample_size = length(s1)

# get accounts followed for each sample
sfriends = list()
for (id in s) {
  user = s1[[id]]
  print(user)

  # If the user's friends are private, or if the user follows too many
  # people, just skip to the next one -- with that many friends,
  # chances are lower that he/she is a real libertarian anyway
  if (user$protected || user$friendsCount > 10000) {
    # correct the sample size to account for the skipped user
    sample_size = sample_size - 1
    next # ain't no one got time fo dat
  }

  # using retryOnRateLimit, found on Github, to forestall problems
  # https://github.com/geoffjentry/twitteR/blob/master/R/comm.R
  # If I get a weird 'try-error', wait for a bit
  n_errors = 0
  while(class(sfriends[[id]] <-
    try(user$getFriendIDs(retryOnRateLimit = 20),
        silent = T)) == "try-error") {
    
    # give up after 20 tries
    if (n_errors > 20) {
      sample_size = sample_size - 1
      break
    }

    # wait and hope it gets better
    print("waiting 2")
    n_errors = n_errors + 1
    Sys.sleep(60)
  }
}

# save the data from the sample
save(s1, sfriends, file = paste0("s1_",  Sys.Date(), ".RData"))

ft = as.data.frame(table(unlist(sfriends)))
rm(sfriends)
ft = ft[order(ft$Freq, decreasing = T), ]
names(ft) = c("id", "Freq")

# get the table data of the tops and combine it with indegree
ft2 = cbind(ft[match(names(tops), ft$id), ], tops)
rm(ft)
names(ft2) = c("ID", "followers", "indegree")
ft2$ID = as.character(ft2$ID)

# this is convoluted because lookupUsers won't get info from some
# people with private accounts, making things difficult
r2 = lookupUsers(ft2$ID, retryOnRateLimit = 20)
screen_names = vector()
handles = vector()
for (name in na.omit(ft2$ID)) {
  try(screen_names[name] <- r2[[name]]$name, silent = T)
  try(handles[name] <- r2[[name]]$screenName, silent = T)
}

ft2$name = screen_names[ft2$ID]
ft2$handle = handles[ft2$ID]
# now order by # of libertarians who follow
ft2 = ft2[order(ft2$followers, decreasing = T), ]

tfollowers = sapply(r2, function(x) x$followersCount)
# percent of followers who are libertarians-- not used
ft2$percent_lib = round(ft2$followers * 154468 * .1 / tfollowers[ft2$ID], 1)
# percent of libertarians who follow the account
ft2$percent = ft2$followers * 100 / sample_size

# in the database
library(RSQLite)
con = dbConnect(SQLite(),
    "/home/will/worails/db/production.sqlite3")
ids = dbGetQuery(con, "select user_id from top_libertarians")[, 1]

# add the number of libertarians
q = paste0("insert into number_of_libertarians (date, number) values ('", Sys.Date(), "','", n_libertarians, "')")
dbGetQuery(con, q)

# the good rows with no NA's
gft2 = na.omit(ft2)
gft2$rank[gft2$indegree >= 7] =
  order(gft2$percent[gft2$indegree >= 7], decreasing = T)

# put new accounts into the database
for (row in which(!(gft2$ID %in% ids))) {
  q = paste0('insert into top_libertarians (user_id, handle, name) values ("', gft2$ID[row], '","', gft2$handle[row], '","',
      gsub('"', '\"', gft2$name[row]), '")')
  dbGetQuery(con, q)
}

# add the new follower numbers into the database
for (row in 1:nrow(gft2)) {
  # get the corresponding top_libertarian ID
  q1 = paste0("select id from top_libertarians where user_id = ",
              gft2$ID[row])
  tlid = dbGetQuery(con, q1)
  q = paste0('insert into libertarian_followings (date, top_libertarian_id, rank, percent, followers) values ("',
      Sys.Date(), '","', tlid, '",',
      ifelse(is.na(gft2$rank[row]), "null", gft2$rank[row]),
      ',"', gft2$percent[row], '","', gft2$followers[row], '")')
  dbGetQuery(con, q)
}

dbDisconnect(con)
