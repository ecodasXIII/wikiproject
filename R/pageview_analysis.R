
library(WikipediR)
library(pageviews)
library(dplyr)

start = '2018111001'
end = '2018121501'

start_editathon = as.POSIXct('2018-11-26', tz = 'GMT')
end_editathon = as.POSIXct('2018-12-03', tz = 'GMT')

articles = read.csv(file = 'data/Eco-DAS_Eco-DAS_2018_Edit-a-thon-articles-2019-01-04.csv', stringsAsFactors = F) %>%
  dplyr::filter(pageviews > 0) # filtering to non-talk, user pages (I think the pageviews only count for wiki: articles) 

articles_plot = dplyr::filter(articles, pageviews > 2000) # filtering to highest pageview articles 

p = pageviews::article_pageviews(project = 'en.wikipedia', 
                                 article = c(articles_plot$title, 'Oceanography'),
                                 user_type = 'user', 
                                 start = start, 
                                 end = end)

ggplot(p, aes(x = date, y = views, group = article, color = article)) +
  geom_rect(aes(xmin=start_editathon, xmax=end_editathon, ymin=0, ymax=Inf), inherit.aes = F, fill = 'grey80') + 
  geom_vline(xintercept = as.POSIXct(c('2018-11-26', '2018-11-27', '2018-11-28', '2018-12-01'), tz = 'GMT'), 
             linetype = 'dotted', size = 1.5, alpha = .5) +
  geom_vline(xintercept = as.POSIXct(c('2018-11-30'), tz = 'GMT'), 
             linetype = 'dashed', color = 'green4', size = 1.5, alpha = .6) +
  geom_line(size = 2, alpha = .2) +
  geom_smooth(method = 'loess', se = F, size = 2) +
  theme_classic() + 
  scale_y_log10() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 13)) + 
  ylab('Daily Pageviews') 


# ---- Project Pageviews ---- 

# pages with WP L&O tag 
pages = WikipediR::page_backlinks(language = 'en', project = 'wikipedia',
                                  page = 'Wikipedia:WikiProject Limnology and Oceanography',
                                  limit = 10000)$query$backlinks %>%  bind_rows() %>%
  dplyr::slice(grep(pattern = 'Talk:', x = title)) %>% 
  mutate(title = gsub(pattern = 'Talk:', replacement = '', x = title)) %>% 
  pull(title)

start = '2019010101'
end = '2019030101'

# mean article views per day within scope of WP L&O over specified date range 
p = pageviews::article_pageviews(project = 'en.wikipedia', 
                                 article = pages,
                                 user_type = 'user', 
                                 start = start, 
                                 end = end) %>% 
  group_by(article) %>% 
  summarise(views = mean(views)) %>% 
  arrange(desc(views))

ggplot(p, aes(x = views)) + 
  geom_histogram() + 
  scale_x_log10()


