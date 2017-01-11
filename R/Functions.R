#' tot_limits
#'
#' This utility function gets the requests remaining across multiple Twitter authentication tokens. Likely to 
#' be made redundant by updated to rtweet in the near future.
#' @param query Defaults to NULL, which retrieves all limits, but can be used to specify a query such as 
#' 'application/ratestatus' to only get a specific set of limits. 
#' @param token This allows you to specify which token to check on, if you only want to check on one. Default is
#' to check all

tot_limits <- function(query = NULL, token = get_tokens()) {
  
  if (length(token) == 1) {
    limits <- rate_limit(token, query = query)
  } else {
    limits <- lapply(token, rate_limit, query = query)
    tot_limit <- do.call('cbind', limits)
    
    limits <- data.frame('query' = tot_limit[, 1], 'limit' = tot_limit[, 2], 'remaining' =
                           rowSums(tot_limit[, grep('remaining', colnames(tot_limit))]),
                         'reset' = do.call('pmin', tot_limit[, grep('reset', colnames(tot_limit))]))
  }
  return(limits)
}

#' control_rate_limit
#'
#' This utility function helps to write loops to download Twitter data while respecting rate limits. If you
#' specify a query type, it will pause and wait until the next 15-minute window before continuing. So it can
#' be inserted in loops before a call to, say, get_friends, and will ensure you aren't rate limited. The 
#' limit parameter optionally lets you set the threshold for pausing to a level above zero, for specific
#' use cases. The token parameter allows you to set a limit for a specific token rather than a set, which is
#' occasionally useful.
#' @param query Required. Used to specify a query such as 'application/ratestatus'. 
#' @param limit Allows you to set the limit at which the function will pause. Defaults to zero.
#' @param token This allows you to specify which token to check on, if you only want to check on one. Default is
#' to check all
#' @export
control_rate_limit <- function(query = NULL, limit = NULL, token = get_tokens()) {
  # Basically, if we just give the query, it pauses if we have zero requests left. If we add a limit, 
  # it pauses if we have less than that limit remaining. 
  limits <- tot_limits(query, token = token)
  if(limits[['remaining']] < max(1, limit)) {
    print(paste0('sleeping for ', as.numeric(limits[['reset']]), ' minutes.'))
    Sys.sleep(as.numeric(limits[['reset']]))
  }
}


#' Dictionary count
#'
#' This function takes a data frame of tweets that has been cleaned with clean_tweets, and compares each word to the NRC lexicon.
#' Depending on the type parameter, it will then un-tokenize the tweets, giving either a clean tweet per row (if type is set
#' to "tweet") or a clean user timeline per row (if type is set to "timeline").
#' @param tweets This is the input data.
#' @param type Defaults to 'tweet', can be set to 'timeline' when working with timelines. If set to 'tweet' when working with timelines, 
#' will return one row per tweet.
#' @export

dictionary_count <- function(tweets, type = 'tweet') {
  if (!type %in% c('tweet', 'timeline')) {
    stop("type must be either 'tweet' or 'timeline'")
  }
  library(tidytext)
  
  nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    dplyr::select(word, sentiment) %>%
    as.data.frame()
  
  # Not sure if dplyr can read a variable from outside the tweet dataframe so let's just do
  # two separate things.
   if (type %in% 'tweet') {
     tweets <- tweets %>%
       group_by(id) %>%
       mutate(
         'clean_text' = paste(word, collapse = ' '),
         'word_count' = n(),
         'anger_count' = sum(word %in% nrc[nrc$sentiment %in% 'anger', 'word']),
         'anticipation_count' = sum(word %in% nrc[nrc$sentiment %in% 'anticipation', 'word']),
         'disgust_count' = sum(word %in% nrc[nrc$sentiment %in% 'disgust', 'word']),
         'fear_count' = sum(word %in% nrc[nrc$sentiment %in% 'fear', 'word']),
         'joy_count' = sum(word %in% nrc[nrc$sentiment %in% 'joy', 'word']),
         'negative_count' = sum(word %in% nrc[nrc$sentiment %in% 'negative', 'word']),
         'positive_count' = sum(word %in% nrc[nrc$sentiment %in% 'positive', 'word']),
         'sadness_count' = sum(word %in% nrc[nrc$sentiment %in% 'sadness', 'word']),
         'surprise_count' = sum(word %in% nrc[nrc$sentiment %in% 'surprise', 'word']),
         'trust_count' = sum(word %in% nrc[nrc$sentiment %in% 'trust', 'word']),
         'anger_perc' = 100*anger_count/word_count,'anticipation_perc' = 100*anticipation_count/word_count,
         'disgust_perc' = 100*disgust_count/word_count,'fear_perc' = 100*fear_count/word_count,
         'joy_perc' = 100*joy_count/word_count,'negative_perc' = 100*negative_count/word_count,
         'positive_perc' = 100*positive_count/word_count,'sadness_perc' = 100*sadness_count/word_count,
         'surprise_perc' = 100*surprise_count/word_count, 'trust_perc' = 100*trust_count/word_count
       ) %>%
       slice(1) %>%
       as.data.frame()
   }
  
  if (type %in% 'timeline') {
    tweets <- tweets %>%
      group_by(screenName) %>%
      mutate(
        'clean_text' = paste(word, collapse = ' '),
        'word_count' = n(),
        'anger_count' = sum(word %in% nrc[nrc$sentiment %in% 'anger', 'word']),
        'anticipation_count' = sum(word %in% nrc[nrc$sentiment %in% 'anticipation', 'word']),
        'disgust_count' = sum(word %in% nrc[nrc$sentiment %in% 'disgust', 'word']),
        'fear_count' = sum(word %in% nrc[nrc$sentiment %in% 'fear', 'word']),
        'joy_count' = sum(word %in% nrc[nrc$sentiment %in% 'joy', 'word']),
        'negative_count' = sum(word %in% nrc[nrc$sentiment %in% 'negative', 'word']),
        'positive_count' = sum(word %in% nrc[nrc$sentiment %in% 'positive', 'word']),
        'sadness_count' = sum(word %in% nrc[nrc$sentiment %in% 'sadness', 'word']),
        'surprise_count' = sum(word %in% nrc[nrc$sentiment %in% 'surprise', 'word']),
        'trust_count' = sum(word %in% nrc[nrc$sentiment %in% 'trust', 'word']),
        'anger_perc' = 100*anger_count/word_count,'anticipation_perc' = 100*anticipation_count/word_count,
        'disgust_perc' = 100*disgust_count/word_count,'fear_perc' = 100*fear_count/word_count,
        'joy_perc' = 100*joy_count/word_count,'negative_perc' = 100*negative_count/word_count,
        'positive_perc' = 100*positive_count/word_count,'sadness_perc' = 100*sadness_count/word_count,
        'surprise_perc' = 100*surprise_count/word_count,'trust_perc' = 100*trust_count/word_count
      ) %>%
      slice(1) %>%
      as.data.frame()
  }
  
  tweets
}

#' Create Closed Network
#'
#' This function takes a vector of users (e.g. of the form c('seanchrismurphy', 'LydiaHayward2', 'katiehgreenaway')) and returns
#' a data frame of followership links between those users. By default, it will ignore users with more than 5000 followers, to save
#' on computing time and remove celebrities. However this behavior can be changed with max.followers.
#' @param usernames A vector of usernames.
#' @param max.followers Defaults to 5000, anyone with more followers than this will be ignored.
#' @param retryOnRateLimit is set to 120 by default, meaning the function will run until completion. 
#' @export

create_closed_network <- function(usernames, max.followers = 5000, retryOnRateLimit = 120) {
  userlist <- lookupUsers(usernames, retryOnRateLimit = retryOnRateLimit)
  
  userlist <- userlist[((sapply(userlist, followersCount)) <= max.followers)]
  ### If we're looking within a closed network, we only need to capture followers, because friends is simply the inverse of followers. 
  ### In this code we essentially get a list of each user's followers, then search it and save the people who are in our initial list
  connections <- list()
  for (user in userlist) {
    while (any(getCurRateLimitInfo()$remaining %in% 0)) {
      Sys.sleep(60)
    }
    x <- user$getFollowers(retryOnRateLimit = retryOnRateLimit)
    x <- sapply(unlist(x), function(y) y$screenName)
    x <- as.character(x[x %in% usernames])
    connections[[user$screenName]] <- x
  }
  
  # Now we turn that data into social network format using the igraph package in R which handles networks. 
  links <- data.frame('source' = as.character(unlist(connections)),'target' = rep(names(connections), sapply(connections, length)), stringsAsFactors = FALSE)
  links
}

#' Clean Tweets
#'
#' This function takes a dataframe of raw tweets and performs some basic cleaning and tokenization. It returns a 
#' dataframe with each tweet broken into individual word tokens, with one row for each word. Can then be used as 
#' input to dictionary_count. 
#' 
#' @param tweets An input dataset of raw tweets, usually from searchTwitter()
#' @param reg This is the rule by which tweets are split into tokens. It uses regex (regular expressions), and by default
#' removes all punctuation except #, @ and ', then splits words wherever there is white space between them. Adding symbols
#' to the first part of the expression will mean they are retained (for instance [^A-Za-z\\!\\d#@'] will keep exclamation 
#' points)
#' @param hashtags is set to 'trim' by default, which keeps them but removes the #. If set to 'remove', they will be removed
#' entirely and so not affect the word count. If set to 'keep', they will be kept but not trimmed and so will not match
#' most dictionaries, unless the dictionary software removes punctuation by default. 
#' @param remove.mentions is set to TRUE by default, removing mentions (e.g. @seanchrismurphy). It can be set to FALSE to 
#' retain the mentions.
#' @export
clean_tweets <- function(tweets, reg = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))", hashtags = 'trim', remove.mentions = TRUE) {
  library(dplyr); library(tidytext); library(stringr)
  if (!hashtags %in% c('trim', 'keep', 'remove')) {
    stop("hashtags must be set to 'trim', 'remove', or 'keep'")
  }
  
  if (!is.logical(remove.mentions)) {
    stop("remove.mentions must be either TRUE or FALSE")
  }
  
  if (hashtags %in% c('keep', 'remove')) {
  keep.hashtags <- (hashtags %in% 'keep')
  tweet_words <- tweets %>%
    filter(!str_detect(text, '^"')) %>%
    filter(!str_detect(text, '^(RT|rt)')) %>%
    # It's questionable whether we should remove these - perhaps posting your Twitter stats is psychologically meaningful, or posting a lot of 
    # youtube links. However, these tweets don't represent words from the user, they're similar to retweets in that sense - they may show some
    # value, but probably add a lot of noise. So I remove them to clean things up a bit. Probably less of an issue when searching for hashtags, 
    # since these sort of tweets won't show up. 
    filter(!str_detect(text, '^Today stats')) %>%
    filter(!str_detect(text, 'new follower(s)*')) %>%
    filter(!str_detect(text, '@[yY]ou[Tt]ube')) %>%
    mutate(clean_text = str_replace_all(text, "http(s)*://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, clean_text, token = "regex", pattern = reg, to_lower = TRUE) %>%
    # We're choosing not to remove stopwords because in LIWC they won't be counted in dictionaries most likely, and in bottom up approaches
    # they may help distinguish groups or individual differences. It's possible that for e.g. a topic analysis where you wanted to find the
    # common topics in a group of tweets you might want to remove the stopwords to get a better characterization.  
    filter((!str_detect(word, "^#") | keep.hashtags),
           (!str_detect(word, "^@") | !remove.mentions),
           str_detect(word, "[a-z]")) %>%
    as.data.frame()
  }
  
  if (hashtags %in% 'trim') {
    tweet_words <- tweets %>%
      filter(!str_detect(text, '^"')) %>%
      filter(!str_detect(text, '^(RT|rt)')) %>%
      # It's questionable whether we should remove these - perhaps posting your Twitter stats is psychologically meaningful, or posting a lot of 
      # youtube links. However, these tweets don't represent words from the user, they're similar to retweets in that sense - they may show some
      # value, but probably add a lot of noise. So I remove them to clean things up a bit. Probably less of an issue when searching for hashtags, 
      # since these sort of tweets won't show up. 
      filter(!str_detect(text, '^Today stats')) %>%
      filter(!str_detect(text, 'new follower(s)*')) %>%
      filter(!str_detect(text, '@[yY]ou[Tt]ube')) %>%
      mutate(clean_text = str_replace_all(text, "http(s)*://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
      unnest_tokens(word, clean_text, token = "regex", pattern = reg, to_lower = TRUE) %>%
      mutate(word = str_replace(word, '^#', '')) %>%
      # We're choosing not to remove stopwords because in LIWC they won't be counted in dictionaries most likely, and in bottom up approaches
      # they may help distinguish groups or individual differences. It's possible that for e.g. a topic analysis where you wanted to find the
      # common topics in a group of tweets you might want to remove the stopwords to get a better characterization.  
      filter((!str_detect(word, "^@") | !remove.mentions),
             str_detect(word, "[a-z]")) %>%
      as.data.frame()
  }
  
  tweet_words
}

#' Collects timelines of followers
#'
#' This function takes a vector of users (e.g. c('seanchrismurphy', LydiaHayward2', 'katiehgreenaway)), and then collects the timelines of their 
#' followers, returning a dataset with up to nstatus tweets for each of nfollowers, for ever user entered. For instance if you input five
#' users with nfollowers = 100 and ntatus = 200, you would retrieve up to 100, 000 tweets (though usually substantially less, do to sparse
#' timelines)
#' @param users The list of users whose followers you wish to collect.
#' @param nfollowers Defaults to 100. The maximum number of followers you wish to collect for each user.
#' @param nstatus Defaults to 200. The maximum number of statuses you wish to collect for each follower.
#' @param minstatus Defaults to 20. The minimum number of tweets a follower must have for you to download their timeline.
#' @param language Defaults to 'en'. A filter to only collect users of the specified language. 
#' @export

collect_follower_timelines <- function(users, nfollowers = 100, nstatus = 200, minstatus = 20, language = 'en') {
  followers <- sapply(users, function(x) x$getFollowers(n = nfollowers, retryOnRateLimit = 80))
  followers <- unlist(followers)
  # This line removes followers who are protected, because the API will fail when attempting to retrieve their
  # timelines
  followers <- followers[!sapply(followers, function(x) x$protected)]
  followers <- followers[sapply(followers, function(x) x$statusesCount) >= minstatus]
  followers <- followers[sapply(followers, function(x) x$lang) %in% language]
  
  timelines <- list()
  
  # This loop runs through the followers in our shuffled lists, and retrieves their timelines.
  for (i in 1:length(followers)) {
    # This line retrieves the most recent 200 tweets from one followers timeline
    timelines[[i]] <- userTimeline(followers[i], n = nstatus, retryOnRateLimit = 20)
    # This line tells the loop to wait when the API has rate limited us
    while (any(getCurRateLimitInfo()$remaining %in% 0)) {
      Sys.sleep (60)
    }
  }
  
  timelines <- timelines[sapply(timelines, length) >= minstatus]
  timelinesdf <- lapply(timelines, twListToDF)
  timelinesdf <- do.call('rbind', timelinesdf)
  timelinesdf
}

#' Creates a mentions network
#'
#' This function takes a dataset of tweets (not already cleaned with clean_tweets) and returns a dataset of links between
#' users based on whether one has mentioned (e.g. @seanchrismurphy) the other. This dataset can then be used as input to 
#' igraph using graph_from_edgelist.
#' @param tweets This is the input data.
#' @param closed Defaults to FALSE. If set to TRUE, will remove mentions of anyone who hasn't tweeted within the input dataset, 
#' effectively making it a closed network (this will remove mentions of news sources in most cases, for instance). 
#' @export

create_mentions_network <- function(tweets, closed = FALSE) {
  mentions <- clean_tweets(tweets, remove.mentions = FALSE)
  mentions <- mentions[str_detect(mentions$word, '^@'), c('screenName', 'word')]
  mentions$word <- gsub('^@', '', mentions$word)
  
  if (closed == TRUE) {
    mentions <- mentions[mentions$word %in% mentions$screenName, ]
  }
  
  # Now we turn that data into social network format using the igraph package in R which handles networks. 
  links <- mentions
  colnames(links) <- c('source', 'target')
  links
}

#' Get timelines
#'
#' This function takes a vector of users (e.g. of the form c('seanchrismurphy', 'lydiahayward2', 'katiegreenaway')) and returns
#' a data frame containing up to 3200 tweets of each of them, depending on nstatus.
#' @param tweets This is the input data.
#' @param nstatus Defaults to 200. This is the maximum number of tweets that will be returned for each user.
#' @param includeRts Defaults to FALSE. This determines whether tweets that the user has retweeted will be returned. These tweets
#' are included in nstatus no matter what (this is a limitation of the Twitter API), so when includeRts is set to FALSE, some number less
#' than nstatus tweets will be returned for each user, depending on how many retweets they have made.
#' @export
 
get_timelines <- function(users, nstatus = 200, includeRts = FALSE) {
  
  timelines <- list()
  
  # This loop runs through the users and retrieves their timelines.
  for (i in 1:length(users)) {
    # This line retrieves the most recent 200 tweets from one followers timeline
    timelines[[i]] <- userTimeline(users[i], n = nstatus, includeRts = includeRts, retryOnRateLimit = 20)
    # This line tells the loop to wait when the API has rate limited us
    while (any(getCurRateLimitInfo()$remaining %in% 0)) {
      Sys.sleep (60)
    }
  }
  
  timelinesdf <- lapply(timelines, twListToDF)
  timelinesdf <- do.call('rbind', timelinesdf)
  timelinesdf
}


#' Get Followers
#'
#' This is a convenience function to get user followers a little more cleanly
#' than the default in the twitteR package. It takes a list of user objects
#' returned by lookupUsers, and returns a dataset of user information for the
#' followers of those users. It attached a 'following' variable to the dataset
#' to track who is following who. 

get_Followers <- function(users) {
  out <- list()
  for (i in 1:length(users)) {
    out[[i]] <- twListToDF(users[[i]]$getFollowers())
    out[[i]]$following <- users[[i]]$screenName
  }
  do.call('rbind', out)
}

#' Get Friends
#'
#' This is a convenience function to get user friends a little more cleanly
#' than the default in the twitteR package. It takes a list of user objects
#' returned by lookupUsers, and returns a dataset of user information for the
#' friends of those users. It attached a 'followed_by' variable to the dataset
#' to track who is followed by which users.

get_Friends <- function(users) {
  out <- list()
  for (i in 1:length(users)) {
    out[[i]] <- twListToDF(users[[i]]$getFriends())
    out[[i]]$followed_by <- users[[i]]$screenName
  }
  do.call('rbind', out)
}