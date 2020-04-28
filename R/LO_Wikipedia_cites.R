
# publications from Web of Science downloaded on April 27, 2020 for articles published from 1985-2020. 

library(rAltmetric)
library(bibtex)
library(RefManageR)
library(tidyverse)
library(ggplot2)
library(wesanderson)

# downloading Web of Science DOI's for L&O pubs and checking for references on Wikipedia through Altmetric results 
# https://apps.webofknowledge.com/summary.do?product=WOS&search_mode=GeneralSearch&qid=32&SID=7EIZCGZqv946vAWRQCe
# 8,036 total articles from Web of Science for L&O, L&O Methods, and L&O Letters; 

# bind all exports together 
# dir = 'data'
# journals = c('LO_pubs', 'LOM_pubs', 'LOL_pubs')
# for(journal in journals){
#   files = list.files(file.path(dir, journal))
#   for(i in files){
#     if(i == files[1] & journal == journals[1]){
#       lo_pubs = ReadBib(file = file.path(dir, journal, i))
#     }else{
#       lo_pubs = merge(lo_pubs, ReadBib(file = file.path(dir, journal, i)))
#     }
#   }
# }
# 
# # get all the Altmetrics of each publication
# lo_metrics = list() 
# for(i in 1:length(lo_pubs)){
#   cur = tryCatch(altmetrics(doi = stringr::str_sub(lo_pubs[[i]]$doi, 2,-2), ),
#                  error = function(e) e )
#   lo_metrics[[i]] = cur
# }

# saveRDS(lo_pubs, file = 'results/lo_pubs.rds')
# saveRDS(lo_metrics, file = 'results/lo_metrics.rds')

lo_pubs = readRDS('results/lo_pubs.rds')
lo_metrics = readRDS('results/lo_metrics.rds')

wiki_mentions = lo_metrics[grep('wikipedia', lo_metrics)]
lo_pubs_wiki_mentions = lo_pubs[grep('wikipedia', lo_metrics)]

lo_wiki_summary = tibble()
for(i in 1:length(wiki_mentions)){
  cur_article = unlist(lo_pubs_wiki_mentions[[i]][1])
  cur_wiki_mentions = wiki_mentions[[i]]
  cur_out = tibble(author = as.character(cur_article$author),
                   title = cur_article$title, 
                   year = strftime(strptime(cur_article$year,
                                            format = '{%Y}'), format = '%Y'), 
                   journal = cur_article$journal, 
                   doi = cur_article$doi,
                   wikipedia_cites = cur_wiki_mentions$cited_by_wikipedia_count)
  
  lo_wiki_summary = bind_rows(lo_wiki_summary, cur_out)
}

lo_wiki_year = lo_wiki_summary %>% 
  dplyr::filter(!duplicated(doi)) %>%  # duplicated for multiple authors / pub
  group_by(year, journal) %>% 
  summarise(articles = n(),
            wikipedia_cites = sum(wikipedia_cites)) %>% 
  ungroup()

lo_wiki_year = lo_wiki_year %>% complete(journal, nesting(year)) # filling in missing years 
lo_wiki_year$articles = ifelse(is.na(lo_wiki_year$articles), 0, lo_wiki_year$articles)
lo_wiki_year$wikipedia_cites = ifelse(is.na(lo_wiki_year$wikipedia_cites), 0, lo_wiki_year$wikipedia_cites)

## summarize number of pubs per year for each journal 
lo_pubs_summary = tibble()
for(i in 1:length(lo_pubs)){
  cur_article = unlist(lo_pubs[[i]][1])
  cur_out = tibble(author = as.character(cur_article$author[[1]]),
                   title = cur_article$title, 
                   year = strftime(strptime(cur_article$year,
                                            format = '{%Y}'), format = '%Y'), 
                   journal = cur_article$journal)
  
  lo_pubs_summary = bind_rows(lo_pubs_summary, cur_out)
}


lo_pubs_year = lo_pubs_summary %>% 
  group_by(year, journal) %>% 
  summarise(total_articles = n()) %>% 
  ungroup()


lo_pubs_year = lo_pubs_year %>% complete(journal, nesting(year)) # filling in missing years 
lo_pubs_year$total_articles = ifelse(is.na(lo_pubs_year$total_articles), 0, lo_pubs_year$total_articles)

plot_data = left_join(lo_pubs_year, lo_wiki_year, by = c('year', 'journal')) %>% 
  mutate(articles =  ifelse(is.na(articles), 0, articles),
         wikipedia_cites = ifelse(is.na(wikipedia_cites), 0, wikipedia_cites), 
         percent_articles_on_wiki = articles / total_articles * 100,
         total_articles = ifelse(is.na(percent_articles_on_wiki),NA, total_articles),
         articles = ifelse(is.na(percent_articles_on_wiki),NA, articles))


cummulative_data = left_join(lo_pubs_year, lo_wiki_year, by = c('year', 'journal')) %>% 
  mutate(articles =  ifelse(is.na(articles), 0, articles),
         wikipedia_cites = ifelse(is.na(wikipedia_cites), 0, wikipedia_cites)) %>% 
  group_by(journal) %>% 
  mutate(cummulative_articles = cumsum(total_articles),
         cummulative_wiki_cites = cumsum(articles),
         percent_articles_on_wiki = cummulative_wiki_cites / cummulative_articles * 100, 
         cummulative_articles = ifelse(is.na(percent_articles_on_wiki),NA, cummulative_articles),
         cummulative_wiki_cites = ifelse(is.na(percent_articles_on_wiki),NA, cummulative_wiki_cites)) %>% 
  ungroup() %>% 
  select(journal, year, cummulative_articles, cummulative_wiki_cites, percent_articles_on_wiki)



# plot 
wiki_articles_plot = ggplot(plot_data, 
                 aes(x= as.Date(year, '%Y'), 
                     y = as.integer(articles),
                     fill = journal)) + 
  # geom_line()+
  # geom_point() +
  geom_area(alpha = .6, color = 'white') +
  theme_classic() +
  theme(axis.title.x = element_blank()) 

cols = wesanderson::wes_palette(name = 'Zissou1', n = 5)
cols = cols[c(1,3,5)]

percent_plot = ggplot(plot_data, 
                      aes(x= as.numeric(year), 
                          y = percent_articles_on_wiki,
                          color = journal)) + 
  geom_line(alpha = .7, show.legend = F)+
  geom_point(size = 3, show.legend = F) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = c(.2,.9), 
        legend.title = element_blank()) + 
  scale_color_manual(values = rev(cols), 
                     labels = c('L&O: Methods', 'L&O: Letters', 'L&O')) + 
  ylab('Articles cited on Wikipedia (%)')

percent_plot

# ggsave(filename = 'figures/LO_articles_wiki_perc.png', plot = percent_plot, width = 6,
#        height = 4)

total_articles = ggplot(plot_data, 
                      aes(x= as.numeric(year), 
                          y = total_articles,
                          color = journal)) + 
  geom_line(alpha = .7)+
  geom_point(size = 3) +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = c(.2,.9),
        legend.title = element_blank()) + 
  scale_color_manual(values = rev(cols), 
                     labels = c('L&O: Methods', 'L&O: Letters', 'L&O')) + 
  ylab('Articles Published (n)')

total_articles

# ggsave(filename = 'figures/LO_total_articles.png', plot = total_articles, width = 6,
#        height = 4)

articles_wiki = ggplot(plot_data, 
                        aes(x= as.numeric(year), 
                            y = as.integer(articles),
                            color = journal)) + 
  geom_line(alpha = .7, show.legend = F)+
  geom_point(size = 3, show.legend = F) +
  theme_classic() +
  theme(axis.title.x = element_blank(), #axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = c(.2,.9), 
        legend.title = element_blank()) + 
  scale_color_manual(values = rev(cols), 
                     labels = c('L&O: Methods', 'L&O: Letters', 'L&O')) + 
  scale_y_continuous(breaks = scales::pretty_breaks()) + 
  ylab('Articles cited on Wikipedia (n)')

articles_wiki

# ggsave(filename = 'figures/LO_articles_wiki.png', plot = articles_wiki, width = 6,
#        height = 4)

out = cowplot::plot_grid(total_articles, articles_wiki, percent_plot, 
                         align = 'hv', nrow = 3, labels = c('a','b','c'))
  
ggsave(filename = 'figures/LO_articles_wiki_all.png', plot = out, width = 6,
       height = 12)



cummulative_percent_plot = ggplot(cummulative_data, 
                      aes(x= as.numeric(year), 
                          y = percent_articles_on_wiki,
                          color = journal)) + 
  geom_line(alpha = .7, show.legend = F)+
  geom_point(size = 3, show.legend = F) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = c(.2,.9), 
        legend.title = element_blank()) + 
  scale_color_manual(values = rev(cols), 
                     labels = c('L&O: Methods', 'L&O: Letters', 'L&O')) + 
  ylab('Articles cited on Wikipedia since 1985 (%)')

cummulative_percent_plot

cummulative_articles = ggplot(cummulative_data, 
                                  aes(x= as.numeric(year), 
                                      y = cummulative_articles,
                                      color = journal)) + 
  geom_line(alpha = .7)+
  geom_point(size = 3) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = c(.2,.9), 
        legend.title = element_blank()) + 
  scale_color_manual(values = rev(cols), 
                     labels = c('L&O: Methods', 'L&O: Letters', 'L&O')) + 
  ylab('Articles since 1985 (n)') 

cummulative_articles

cummulative_wiki_articles = ggplot(cummulative_data, 
                              aes(x= as.numeric(year), 
                                  y = cummulative_wiki_cites,
                                  color = journal)) + 
  geom_line(alpha = .7)+
  geom_point(size = 3) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = c(.2,.9), 
        legend.title = element_blank()) + 
  scale_color_manual(values = rev(cols), 
                     labels = c('L&O: Methods', 'L&O: Letters', 'L&O')) + 
  ylab('Articles cited on Wikipedia (n)') 

cummulative_wiki_articles

out = cowplot::plot_grid(cummulative_articles, articles_wiki, cummulative_percent_plot, 
                         align = 'hv', nrow = 3, labels = c('a','b','c'), label_x = .96)

ggsave(filename = 'figures/LO_cummulative_articles_wiki_all.png', plot = out, width = 6,
       height = 12)
