
library(webshot)

URL = "https://en.wikipedia.org/wiki/Lake_Champlain"

# webshot::install_phantomjs()

webshot::webshot(file = 'figures/lake_champlain_screenshot.png', 
                 url = URL, 
                 delay = 0.5, 
                 zoom = 3,
                 cliprect = 'viewport')

webshot::webshot(file = 'figures/lake_champlain_infobox.png', 
                 url = URL, 
                 delay = 0.5, 
                 zoom = 3,
                 selector = '.infobox')

lake = 'Lake_Keowee'
URL = file.path('https://en.wikipedia.org/wiki', lake)
webshot::webshot(file = file.path('figures/',paste0(lake,'_screenshot.png')), 
                 url = URL, 
                 delay = 0.5, 
                 zoom = 3,
                 cliprect = 'viewport')

webshot::webshot(file = file.path('figures/',paste0(lake,'_infobox.png')), 
                 url = URL, 
                 delay = 0.5, 
                 zoom = 3,
                 selector = '.infobox')



