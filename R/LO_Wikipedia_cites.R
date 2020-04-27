
# publications from Web of Science downloaded on April 27, 2020 for articles published from 1985-2020. 

library(rAltmetric)
library(bibtex)
library(RefManageR)
library(tidyverse)
library(ggplot2)

# downloading Web of Science DOI's for L&O pubs and checking for references on Wikipedia through Altmetric results 
# https://apps.webofknowledge.com/summary.do?product=WOS&search_mode=GeneralSearch&qid=32&SID=7EIZCGZqv946vAWRQCe
# 8,036 total articles from Web of Science for L&O, L&O Methods, and L&O Letters; 

# bind all exports together 
dir = 'data'
journals = c('LO_pubs', 'LOM_pubs', 'LOL_pubs')
for(journal in journals){
  files = list.files(file.path(dir, journal))
  for(i in files){
    if(i == files[1] & journal == journals[1]){
      lo_pubs = ReadBib(file = file.path(dir, journal, i))
    }else{
      lo_pubs = merge(lo_pubs, ReadBib(file = file.path(dir, journal, i)))
    }
  }
}

# get all the Altmetrics of each publication
lo_metrics = list() 
for(i in 1:length(lo_pubs)){
  cur = tryCatch(altmetrics(doi = stringr::str_sub(lo_pubs[[i]]$doi, 2,-2), ),
                 error = function(e) e )
  lo_metrics[[i]] = cur
}

saveRDS(lo_metrics, file = 'results/lo_metrics.rds')

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

fig_out = ggplot(lo_wiki_year, 
                 aes(x= as.Date(year, '%Y'), 
                     y = as.integer(articles),
                     fill = journal)) + 
  # geom_line()+
  # geom_point() +
  geom_area(alpha = .6, color = 'white') +
  theme_classic() +
  theme(axis.title.x = element_blank()) 

fig_out

