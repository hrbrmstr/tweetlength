library(rtweet)
library(ggalt)
library(rprojroot)
library(hrbrthemes)
library(tidyverse)

rt <- find_rstudio_root_file()
rstats_tweet_data_file <- file.path(rt, "data", "2017-11-13-rstats-tweet-search-results.rds")

if (!file.exists(rstats_tweet_data_file)) {
  rstats <- search_tweets("#rstats", 30000) # setting up rtweet is an exercise left to the reader
  write_rds(rstats, rstats_tweet_data_file)
} else {
  rstats <- read_rds(rstats_tweet_data_file)
}

rstats <- mutate(rstats, tweet_length=map_int(text, nchar))  # get the tweet length for each tweet

count(rstats, source) %>%
  filter(n > 9) -> usable_sources  # need data for density + I wanted a nice grid :-)

# We want max tweet length & total # of tweets for sorting & labeling facets
filter(rstats, source %in% usable_sources$source) %>%
  group_by(source) %>%
  summarise(max=max(tweet_length), n=n()) %>%
  arrange(desc(max)) -> ordr

# four breaks per panel regardless of the scales (we're using free-y scales)
there_are_FOUR_breaks <- function(limits) { seq(limits[1], limits[2], length.out=4) }

mutate(rstats) %>%
  filter(source %in% usable_sources$source) %>%
  mutate(source = factor(source, levels=ordr$source,
                         labels=sprintf("%s (n=%s)", ordr$source, ordr$n))) -> plot_df

ggplot(plot_df, aes(tweet_length)) +
  geom_bkde(aes(color=source, fill=source), bandwidth=5, alpha=2/3) +
  geom_vline(xintercept=140, linetype="dashed", size=0.25) +
  scale_x_comma(breaks=seq(0,280,70), limits=c(0,280)) +
  scale_y_continuous(breaks=there_are_FOUR_breaks, expand=c(0,0)) +
  facet_wrap(~source, scales="free", ncol=5) +
  labs(x="Tweet length", y="Density",
       title=sprintf("Tweet length distributions by Twitter client (%3.2f days #rstats)",
                     as.numeric(diff(range(plot_df$created_at)))),
       subtitle="Twitter client facets in decreasing order of ones with >140 length tweets",
       caption="NOTE free Y axis scales\nBrought to you by #rstats, rtweet & ggalt") +
  theme_ipsum_rc(grid="XY", strip_text_face="bold", strip_text_size=8, axis_text_size=7) +
  theme(panel.spacing.x=unit(5, "pt")) +
  theme(panel.spacing.y=unit(5, "pt")) +
  theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
  theme(axis.text.y=element_blank()) +
  theme(legend.position="none")
