
library(dplyr)
library(ggplot2)
library(WikipediR)
library(pageviews)

WikipediR::page_backlinks('en','wikipedia',page = 'Limnology', limit = 100, clean_response = F)$query$backlinks %>%bind_rows()

outline_oceanography_links = WikipediR::page_links(language = 'en', project = 'wikipedia', 
                                                            page = 'Outline of oceanography', 
                                                            clean_response = T, limit = 10000)
outline_oceanography_links

pages = WikipediR::page_backlinks(language = 'en', project = 'wikipedia',
                                  page = 'Wikipedia:WikiProject Limnology and Oceanography',
                                  limit = 10000)$query$backlinks %>%  bind_rows()

user_contribs = WikipediR::user_contributions(language = 'en', project = 'wikipedia', username = 'Jayzlimno', limit = 10000)$query$usercontribs

start = pageviews::pageview_timestamps(timestamps = '2001010101')
p = pageviews::article_pageviews(project = 'en.wikipedia', article = 'Limnology', user_type = 'user', start = '2001010101', end = '2019010101')

plot(p$views~p$date, type ='l',ylim=c(0,1200))
abline(lm(p$views~p$date))

# can also query multiple articles at once
p = pageviews::article_pageviews(project = 'en.wikipedia', article = c('Limnology', 'Oceanography'), user_type = 'user', start = '2018010101', end = '2019010101')

ggplot(p, aes(x = date, y = views, group = article, color = article)) +
  geom_line() +
  geom_smooth(method = 'loess') + 
  theme_classic() + 
  scale_y_log10()
# there are dips in L&O during start of new year (maybe related to students and college exams?)

p = pageviews::article_pageviews(project = 'en.wikipedia', article = c('Chemistry', 'Christmas', 'New Year'), user_type = 'user', start = '2001010101', end = '2018102401')
ggplot(p, aes(x = date, y = views, group = article, color = article)) +
  geom_line() +
  scale_y_log10()

ccf(x = p$views[p$article=='Chemistry'],y = p$views[p$article=='New_Year'])


p = pageviews::article_pageviews(project = 'en.wikipedia', article = c('Water', 'Secchi disk'), user_type = 'user', start = '2001010101', end = '2018102401')
ggplot(p, aes(x = date, y = views, group = article, color = article)) +
  geom_line() +
  scale_y_log10()

