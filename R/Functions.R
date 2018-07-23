#' Get total rate limits
#'
#' This utility function gets the requests remaining across multiple Twitter authentication tokens. Likely to
#' be made redundant by updated to rtweet in the near future.
#' @param query Defaults to NULL, which retrieves all limits, but can be used to specify a query such as
#' 'application/ratestatus' to only get a specific set of limits.
#' @param token This allows you to specify which token to check on, if you only want to check on one. Default is
#' to check all
#' @import rtweet
#' @import tokenizers
#' @import tidytext
#' @import stringr

tot_limits <- function(query = NULL, token = get_tokens()) {

  # Changed this because tokens are no longer a single length item, but are now a 17 length class.
  if (length(token) == 17) {
    limits <- rate_limit(token, query = query)
  } else {
    # This still appears to work, assuming that having multiple tokens returns a list of them, which I haven't been able to confirm
    limits <- lapply(token, rate_limit, query = query)
    tot_limit <- do.call('cbind', limits)

    limits <- data.frame('query' = tot_limit[, 1], 'limit' = tot_limit[, 2], 'remaining' =
                           rowSums(tot_limit[, grep('remaining', colnames(tot_limit))]),
                         'reset' = do.call('pmin', tot_limit[, grep('reset', colnames(tot_limit))]))
  }
  return(limits)
}

#' Control requests in line with the rate limit
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
  while(is.null(tryCatch(tot_limits(query, token = token), error = function(e) {NULL}))) {
    print('Rate limit check exhausted, sleeping 60 seconds')
    Sys.sleep(60)
  }
  limits <- tot_limits(query, token = token)
  if(limits[['remaining']] < max(1, limit)) {
    print(paste0('sleeping for ', as.numeric(limits[['reset']]), ' minutes.'))
    Sys.sleep(as.numeric(limits[['reset']]))
  }
}


#' Dictionary count
#'
#' This function takes a data frame of tweets, cleans them with clean_tweets if that has not already been done,
#' and compares each word to the NRC lexicon. It returns counts and a word count, which can be used to turn the
#' raw counts into percentages at your discretion (often a good idea). It's important to note that this function
#' only does exact matching to the NRC lexica, which often do not include stems. So you may which to optionally
#' stem the tokens in the clean_tweets column to get more accurate counts, though this is a decision about
#' which there is some debate (e.g. see Kern et al, 2016). Counts are returned at the tweet level, and will
#' need to be aggregated to the person level if you have multiple tweets from each individual.
#' @param tweets This is the input data.
#' @param clean Defaults to TRUE, which will clean the data with clean_tweets if there isn't a clean_text column.
#' At present, the function will fail if this is set to FALSE and there is not a clean_tweets column in the data,
#' so it's not much use, but left in for future adjustments.
#' @export

dictionary_count <- function(tweets, clean = TRUE) {

  if (clean == TRUE & is.null(tweets$clean_text)) {
    tweets <- clean_tweets(tweets)
  }

  count_tweets <- tweets[, c('status_id', 'clean_text')]

  nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    dplyr::select(word, sentiment) %>%
    as.data.frame()

  wordsalad <- count_tweets %>%
    unnest_tokens(word, clean_text, token = 'regex', pattern = " ")

  # This code is vectorized to be as fast as I can make it currently. Compared each token to the
  # nrc lexica.
  count_tweets <- wordsalad %>%
    mutate(
      'anger_count' = word %in% nrc[nrc$sentiment %in% 'anger', 'word'],
      'anticipation_count' = word %in% nrc[nrc$sentiment %in% 'anticipation', 'word'],
      'disgust_count' = word %in% nrc[nrc$sentiment %in% 'disgust', 'word'],
      'fear_count' = word %in% nrc[nrc$sentiment %in% 'fear', 'word'],
      'joy_count' = word %in% nrc[nrc$sentiment %in% 'joy', 'word'],
      'negative_count' = word %in% nrc[nrc$sentiment %in% 'negative', 'word'],
      'positive_count' = word %in% nrc[nrc$sentiment %in% 'positive', 'word'],
      'sadness_count' = word %in% nrc[nrc$sentiment %in% 'sadness', 'word'],
      'surprise_count' = word %in% nrc[nrc$sentiment %in% 'surprise', 'word'],
      'trust_count' = word %in% nrc[nrc$sentiment %in% 'trust', 'word']) %>%
    select(-word) %>%
    group_by(status_id) %>%
    summarize_all(sum)

  final <- join(tweets, count_tweets, by = 'status_id')
  final
}

#' Create Closed Network
#'
#' This function takes a vector of users (e.g. of the form c('seanchrismurphy', 'LydiaHayward2', 'katiehgreenaway')) and returns
#' a data frame of friendship links between those users. By default, it will ignore users with more than 5000 friends, to save
#' on computing time and remove celebrities. Also, at present, it will only retrieve the first 5000 friends from each user because
#' paging has not been implemented. Note that because all friendship ties (users following other users) in a network are captured,
#' this is the same as capturing all followership ties, but far more computationally efficient for people with many followers.
#' Will pause when the rate limit is exceeded, so can be run on a large set of users, but will take about one minute per user.
#' If running on larger networks like this, you may wish to alter the code to save at intervals.
#'
#' Note that this function returns an adjacency matrix, which has to be converted to an actual network with the network function
#' from the network package, or the graph_from_adjacency_matrix in igraph (though the data.frame will need to be converted to
#' a matrix first)
#'
#' @param input A vector of usernames or userids.
#' @param limit Defaults to 5000, anyone with more followers than this will be ignored.
#' @export

create_closed_network <- function(input, limit = 5000) {

  store <- list()
  people <- lookup_users(input)

  if (!is.null(limit)) {
    people <- people[people$friends_count <= limit, ]
  }

  for (i in 1:nrow(people)) {

    # This controls the rate limit
    control_rate_limit('friends/ids')
    store[[i]] <- get_friends(people[i, 'user_id'])$user_id
  }

  net.mat <- do.call('rbind', lapply(store, function(x) people$user_id %in% x))
  row.names(net.mat) <- people$screen_name; colnames(net.mat) <- people$screen_name
  return(net.mat)
}


#' Clean Tweets
#'
#' This function takes a dataframe of raw tweets and performs some basic cleaning and tokenization. It returns
#' the input data.frame, now with a new column for clean_text, the tweets after cleaning. It also returns
#' the emojis in the tweets in their own column, and a count of emojis used in each tweet, for convenience.
#'
#' @param tweets An input dataset of raw tweets, usually from search_tweets()
#' @param remove.mentions TRUE by default, controls whether to remove mentions. Can be set to FALSE to keep them.
#' @param remove.hashtags TRUE by default, controls whether to remove hashtags Can be set to FALSE to keep them.
#' @param remove.urls TRUE by default, controls whether to remove urls. Can be set to FALSE to keep them.
#' @param remove.retweets TRUE by default, controls whether to remove retweets. Can be set to FALSE to keep them.
#' @param remove.numbers FALSE by default, controls whether to remove numbers Can be set to TRUE to remove them.
#' @param lowercase TRUE by default, controls whether to convert all characters to lowercase. Can be set to FALSE to retain case.
#' @export

clean_tweets <- function(tweet_data, remove.mentions = TRUE, remove.hashtags = TRUE,
                         remove.urls = TRUE, remove.retweets = TRUE, remove.numbers = FALSE,
                         lowercase = TRUE) {
  require(stringr); require(tidytext); require(tokenizers); require(plyr); require(dplyr)
  if (remove.retweets) {
    tweet_data <- tweet_data[tweet_data$is_retweet == 0, ]
    tweet_data <- tweet_data[!grepl('^[Rr][Tt] ', tweet_data$text), ]
  }

  tweet_data <- tweet_data[!duplicated(tweet_data$status_id),]

  # This line replaces some common characters on Twitter that are encoded in UTF-8 with their
  # ASCII equivalents, because otherwise they'll be removed when emojis are extracted.

  pattern <- c('é', '…', '—', "[‘“’”´`]", '～', '＞', '+', '&amp;')
  replacement <- c('e', '...', '-', "'", '~', '＞', '+', 'and')

  clean_tweets <- tweet_data[, c('status_id', 'text')]

  clean_tweets$text <- qdap::mgsub(pattern = pattern, replacement = replacement, clean_tweets$text)

  # Remove html symbols
  clean_tweets$text <- str_replace_all(clean_tweets$text, '&[a-z]{1,6};', '')

  # We remove the emojis from the text, but not before extracting them and a count, which may be useful
  # to researchers wishing to recode them.
  emojis <- str_extract_all(clean_tweets$text,'[^[:alnum:][:punct:][:space:][\\$\\~\\=\\-\\|\\*]]+')
  clean_tweets$emojis <- sapply(emojis, function(x) paste(x, collapse = ','))
  clean_tweets$emoji_count <- sapply(emojis, function(x) sum(str_length(x)))
  rm(emojis)

  clean_tweets$text <- iconv(clean_tweets$text , 'UTF-8', 'ASCII', '')
  # remove links from text
  if (remove.urls) {
    clean_tweets$text <- str_replace_all(clean_tweets$text, 'https://t.co/[a-zA-Z0-9]*', '')
  }
  # get rid of extra spaces and whitespace on the end of tweets
  clean_tweets$text <- str_replace_all(clean_tweets$text, '( )+', ' ')
  clean_tweets$text <- str_trim(clean_tweets$text)

  # We either remove tokens that are only numbers, or not, depending on the options.
  if (!remove.numbers) {
    wordsalad <- clean_tweets %>%
      unnest_tokens(word, text, token = 'regex', pattern = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))") %>%
      filter(str_detect(word, '[a-zA-Z0-9]'))
  }

  if (remove.numbers) {
    wordsalad <- clean_tweets %>%
      unnest_tokens(word, text, token = 'regex', pattern = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))") %>%
      filter(str_detect(word, '[a-zA-Z]'))
  }

  # Turn all the text into lower case
  if (lowercase) {
    wordsalad$word <- tolower(wordsalad$word)
  }

  # remove hashtags and mentions
  if (remove.hashtags) {
    wordsalad <- wordsalad[!grepl('#', wordsalad$word), ]
  }
  if (remove.mentions) {
    wordsalad <- wordsalad[!grepl('@', wordsalad$word), ]
  }

  # Now the tokens are pasted back together to create the clean text. This is now ready to be analyzed
  # in LIWC or similar word counting software.
  clean_text <- wordsalad %>%
    group_by(status_id) %>%
    dplyr::summarize(clean_text = paste(word, collapse = ' '), word_count = n()) %>%
    as.data.frame()

  # The clean text is joined with the emoji counts
  clean_text <- join(clean_text, clean_tweets[, c('status_id', 'emojis', 'emoji_count')], by = 'status_id')

  # This joins together tweets, and will implicitly remove tweets that contain zero valid tokens at this
  # point, which is worth noting. This behavior could be changed by setting the join type to 'left'.
  final <- join(tweet_data, clean_text, type = 'inner', by = 'status_id')
  final
}




#' Collects timelines of followers
#'
#' This function takes a vector of users (e.g. c('seanchrismurphy', LydiaHayward2', 'katiehgreenaway)), and then collects the timelines of their
#' followers, returning a dataset with up to nstatus tweets for each of nfollowers, for ever user entered. For instance if you input five
#' users with nfollowers = 100 and ntatus = 200, you would retrieve up to 100, 000 tweets (though usually substantially less, do to sparse
#' timelines)
#'
#' @param users The list of users whose followers you wish to collect.
#' @param nfollowers Defaults to 100. The maximum number of followers you wish to collect for each user.
#' @param nstatus Defaults to 200. The maximum number of statuses you wish to collect for each follower.
#' @param minstatus Defaults to 20. The minimum number of tweets a follower must have for you to download their timeline.
#' @param language Defaults to 'en'. A filter to only collect users of the specified language.
#' @export

collect_follower_timelines <- function (users, nfollowers = 90, nstatus = 200, minstatus = 20,
                                    language = "en")
{
  followers <- list()
  for (i in 1:length(users)) {

    control_rate_limit('followers/list', limit = ceiling(min(3*nfollowers, 10000)/5000))

    # We get 3 times nfollowers, because we'll lose a lot to having too few statuses.
    followers[[i]] <- get_followers(users[i], n = min(3*nfollowers, 10000))
  }

  # Here we lookup the followers of each user.
  followers_user <- list()
  for (i in 1:length(followers)) {
    followers_user[[i]] <- lookup_users(followers[[i]])
  }

  # Remove users who are protected (private), don't have specified language, or don't have enough statuses.
  followers_user <- lapply(followers_user, function(x) x[!x$protected,])
  followers_user <- lapply(followers_user, function(x) x[x$lang %in% language,])
  followers_user <- lapply(followers_user, function(x) x[x$statuses_count > minstatus,])

  # This is the list of valid followers. Getting rid of duplicates
  follower_final <- unique(unlist(sapply(followers_user, function(x) x$screen_name)))

  # We sample from the valid followers up to our nfollowers, or the total if we've got too few left. Note that
  # this doesn'tr track which of the input people each individual is a follower of.
  follower_final <- follower_final[sample(1:length(follower_final), size = min(nfollowers*length(users), length(follower_final)))]
  timelines <- list()

  each <- ceiling(nstatus/200)
  k <- 0

  control_rate_limit('application/rate_status', limit = 55, token = get_tokens()[1])

  for (i in 1:length(follower_final)) {

    # Looping through and getting timelines for each individual. Multiple levels of failsafe rate limiting,
    # just in case. This will take a while because of rate limits to the rate limit checking itself,
    # unfortunately, but that can't be avoided at present.

    timelines[[i]] <- get_timeline(follower_final[i], n = nstatus)

    k <- k + each

    if (k > 50) {
      control_rate_limit('application/rate_status', limit = 55, token = get_tokens()[1])
      k <- 0
    }
  }

  # A finaly check to make sure we got enough statuses from each individual (a lot might be retweets)
  timelines <- timelines[sapply(timelines, nrow) >= minstatus]
  timelinesdf <- do.call("rbind", timelines)
  # Return a data frame that has all the collected tweets from all users. This can now be input to dictionary
  # count and then aggregated to the person level.
  timelinesdf
}

#' Creates a mentions network
#'
#' This function takes a dataset of tweets and returns a dataset of links between users based on whether one
#' has mentioned (e.g. @seanchrismurphy) the other. This dataset can then be used as input to igraph using
#' graph_from_edgelist, or to the network package using the network function.
#' @param tweets This is the input data.
#' @param closed.network Defaults to FALSE. If set to TRUE, will remove mentions of anyone who hasn't tweeted within the input dataset,
#' effectively making it a closed network (this will remove mentions of news sources in most cases, for instance).
#' @export

create_mentions_network <- function(tweets, closed.network = FALSE) {
  mentions <- tweets[, c('screen_name', 'mentions_screen_name')]

  # First, extracting the mentions that come coded from rtweet in their own column.
  mentions <- str_split(tweets$mentions_screen_name, ',')
  names(mentions) <- tweets$screen_name
  mentions <- mentions[sapply(mentions, function(x) x[1] != 'NA')]

  # Unlist the mentions, being careful to get the replications, from some users mentioning multiple people,
  # correct
  mentions.net <- data.frame('source' = rep(names(mentions), times = sapply(mentions, length)), 'target' = as.character(unlist(mentions)),
                             stringsAsFactors = FALSE)
  if (closed.network) {
    mentions.net <- mentions.net[mentions.net$target %in% mentions.net$source, ]
  }
  # Remove self-references.
  mentions.net <- mentions.net[mentions.net$target != mentions.net$source, ]
  return(mentions.net)
}

### Now a series of utility functions for checking frequent tokens in tweet data ###


#' Utility function to tokenize tweets without removing anything. Used by the most.frequent.x functions.

tokenize_tweets <- function(tweet_data, remove.words = FALSE, remove.hashtags = FALSE, remove.mentions = FALSE) {
  require(stringr); require(tidytext); require(tokenizers); require(plyr); require(dplyr)
  tweet_data <- tweet_data[tweet_data$is_retweet == 0, ]
  tweet_data <- tweet_data[!grepl('^[Rr][Tt] ', tweet_data$text), ]
  tweet_data <- tweet_data[!duplicated(tweet_data$status_id),]

  pattern <- c('é', '…', '—', "[‘“’”´`]", '～', '＞', '+', '&amp;')
  replacement <- c('e', '...', '-', "'", '~', '＞', '+', 'and')

  cleaned_tweets <- tweet_data[, c('status_id', 'text')]
  cleaned_tweets$text <- str_replace_all(cleaned_tweets$text, 'https://t.co/[a-zA-Z0-9]*', '')
  cleaned_tweets$text <- qdap::mgsub(pattern = pattern, replacement = replacement, cleaned_tweets$text)
  cleaned_tweets$text <- str_replace_all(cleaned_tweets$text, '&[a-z]{1,6};', '')
  cleaned_tweets$text <- iconv(cleaned_tweets$text , 'UTF-8', 'ASCII', '')
  cleaned_tweets$text <- str_replace_all(cleaned_tweets$text, '( )+', ' ')
  cleaned_tweets$text <- str_trim(cleaned_tweets$text)

  wordsalad <- cleaned_tweets %>%
    unnest_tokens(word, text, token = 'regex', pattern = "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))") %>%
    filter(str_detect(word, '[a-zA-Z0-9]'))
  wordsalad$word <- tolower(wordsalad$word)

  if (remove.words) {
    wordsalad <- wordsalad[grepl('@|#', wordsalad$word), ]
  }
  if (remove.hashtags) {
    wordsalad <- wordsalad[!grepl('#', wordsalad$word), ]
  }
  if (remove.mentions) {
    wordsalad <- wordsalad[!grepl('@', wordsalad$word), ]
  }

  return(wordsalad)
}

# A series of convenience functions to help people visualise the most common words, hashtags, mentions,
# etc in a set of tweets.



#' See most frequent tokens in the data
#'
#' This function takes a tweet dataset and returns a data.frame showing the most frequent tokens in descending
#' order. Useful when checking to see if current events or spam are disturbing the data.
#'
#' @param text is the input data.
#' @export

most.frequent.tokens <- function(text) {
  require(plyr);require(dplyr)
  text <- tokenize_tweets(text)
  counts <- text %>%
    group_by(word) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    as.data.frame()
  counts$perc <- counts$count/sum(counts$count)
  counts
}

#' See most frequent words in the data
#'
#' This function takes a tweet dataset and returns a data.frame showing the most frequent word tokens (i.e.
#' # not hashtags or mentions) in descending order. Useful when checking to see if current events or spam are
#' disturbing the data.
#'
#' @param text is the input data.
#' @export

most.frequent.words <- function(text) {
  require(plyr);require(dplyr)
  text <- tokenize_tweets(text, remove.hashtags = TRUE, remove.mentions = TRUE)
  counts <- text %>%
    group_by(word) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    as.data.frame()
  counts$perc <- counts$count/sum(counts$count)
  counts
}

#' See most frequent hashtags in the data
#'
#' This function takes a tweet dataset and returns a data.frame showing the most frequent hashtags
#' in descending order. Useful when checking to see if current events or spam are disturbing the data.
#'
#' @param text is the input data.
#' @export

most.frequent.hashtags <- function(text) {
  require(plyr);require(dplyr)
  text <- tokenize_tweets(text, remove.words = TRUE, remove.mentions = TRUE)
  counts <- text %>%
    group_by(word) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    as.data.frame()
  counts$perc <- counts$count/sum(counts$count)
  counts
}

#' See most frequent mentions in the data
#'
#' This function takes a tweet dataset and returns a data.frame showing the most frequent mentions in descending
#' order. Useful when checking to see if current events or spam are disturbing the data.
#'
#' @param text is the input data.
#' @export

most.frequent.mentions <- function(text) {
  require(plyr);require(dplyr)
  text <- tokenize_tweets(text, remove.words = TRUE, remove.hashtags = TRUE)
  counts <- text %>%
    group_by(word) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    as.data.frame()
  counts$perc <- counts$count/sum(counts$count)
  counts
}
