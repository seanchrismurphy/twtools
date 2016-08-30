#' Dictionary count
#'
#' This function takes a data frame of tweets that has been cleaned with clean_tweets, and compares each word to the NRC lexicon.
#' Depending on the group parameter, it will then un-tokenize the tweets, giving either a clean tweet per row (if group is set
#' # to "tweet") or a clean user timeline per row (if group is set to "user").
#' @param tweets This is the input data.
#' @param group Defaults to 'tweet', can be set to 'group' when working with timelines. If set to 'tweet' when working with timelines, will return one row per tweet.
#' @export

dictionary_count <- function(tweets, group = 'tweet') {
  if (!group %in% c('tweet', 'user')) {
    stop("group must be either 'tweet' or 'user'")
  }
  library(tidytext)
  
  nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    dplyr::select(word, sentiment) %>%
    as.data.frame()
  
  # Not sure if dplyr can read a variable from outside the tweet dataframe so let's just do
  # two separate things.
   if (group %in% 'tweet') {
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
         'anger_prop' = 100*anger_count/word_count,'anticipation_prop' = 100*anticipation_count/word_count,
         'disgust_prop' = 100*disgust_count/word_count,'fear_prop' = 100*fear_count/word_count,
         'joy_prop' = 100*joy_count/word_count,'negative_prop' = 100*negative_count/word_count,
         'positive_prop' = 100*positive_count/word_count,'sadness_prop' = 100*sadness_count/word_count,
         'surprise_prop' = 100*surprise_count/word_count, 'trust_prop' = 100*trust_count/word_count
       ) %>%
       slice(1) %>%
       as.data.frame()
   }
  
  if (group %in% 'user') {
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
        'anger_prop' = 100*anger_count/word_count,'anticipation_prop' = 100*anticipation_count/word_count,
        'disgust_prop' = 100*disgust_count/word_count,'fear_prop' = 100*fear_count/word_count,
        'joy_prop' = 100*joy_count/word_count,'negative_prop' = 100*negative_count/word_count,
        'positive_prop' = 100*positive_count/word_count,'sadness_prop' = 100*sadness_count/word_count,
        'surprise_prop' = 100*surprise_count/word_count,'trust_prop' = 100*trust_count/word_count
      ) %>%
      slice(1) %>%
      as.data.frame()
  }
  
  tweets
}

#' Create Closed Network
#'
#' This function takes a vector of users (e.g. of the form c('seanchrismurphy', 'lydiahayward2', 'katiegreenaway')) and returns
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

