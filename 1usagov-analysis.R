# rewritten and cleaned up code for "mass" consumption

library(stringr)
library(plyr)
library(ggplot2)
library(scrapeR)
library(RJSONIO)

options(stringsAsFactors=FALSE)

index <- getURL('http://bitly.measuredvoice.com/bitly_archive/?C=M;O=D')
files <- str_replace(str_sub(str_extract_all(index, 'href="(.+?)"')[[1]], start=7), '"', '')
files <- files[str_detect(files, 'bitly')]

naifnull <- function(a,b) { if (is.null(a)) NA else b }

# sample a few dozen files and merge them
n.files=50
dat.samp <- ldply(sample(files, n.files), function (ff) {
  dat.txt <- str_split(getURL(paste('http://bitly.measuredvoice.com/bitly_archive/', ff, sep='')), '\n')[[1]]
  ldply(dat.txt, function(jj) { if (str_sub(jj,1,1)=='{') {
                                  ll <- fromJSON(jj) ;
                                  if (length(ll) > 1 ) data.frame(known_user=ll$nk,
                                             geo_city_name=naifnull(ll$cy,ll$cy),
                                             lat=naifnull(ll$ll, ll$ll[[1]]),
                                             lon=naifnull(ll$ll, ll$ll[[2]]),
                                             timestamp=as.POSIXct(ll$t, origin="1970-01-01", tz="GMT"),
                                             hash_timestamp=as.POSIXct(ll$hc, origin="1970-01-01", tz="GMT"),
                                             long_url=ll$u,
                                             referring_url=ll$r) else NULL
                                } else NULL
                                })
}, .progress='text')

dat.samp$hash_age <- with(dat.samp, as.numeric(timestamp - hash_timestamp, units='days'))
dat.samp$agency <- with(dat.samp, str_extract(long_url, '[[:alpha:]]+.gov'))

save(dat.samp, file='1usagov-analysis.Rdata')

common.agencies <- names(head(sort(table(dat.samp$agency), decreasing=TRUE), 25))
dat.common.agency <- subset(dat.samp, subset=agency %in% common.agencies)

plot.link.age <- ggplot(dat.common.agency, aes(hash_age)) + 
    geom_density(fill='grey') + 
    facet_wrap(~ agency, scales='free') +
    scale_x_log10('Clickthrough Time - First Shorten Time (log 10)', breaks=c(1/(24*60),1/24,1/4,1,2,10,100), labels=c('1m', '1h', '6h', '', '2d', '10d', '100d')) +
    scale_y_continuous('', breaks=0, labels='') + 
    opts(title=sprintf('Link Age Frequency\n25 most frequent target domains\n%d random hours', n.files))
ggsave(plot.link.age, file='plot_link_age.png', width=8, height=8)

# now do the navalgazing viz

freq.urls <- names(head(sort(table(dat.samp$long_url), decreasing=TRUE), 100))
freq.urls <- freq.urls[!str_detect(freq.urls, '(pdf|png|gif|jpg|zip)$')]

txt <- list()
for(u in freq.urls) {
  try({
    html.files <- list()
  html.files <- getURL(u, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE, followlocation = TRUE)

   html = htmlTreeParse(html.files, useInternal=TRUE)
  txt[u] <- toString(xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)]", xmlValue))
  })
}

dat.freq.urls <- subset(dat.samp, subset=long_url %in% freq.urls)
dat.freq.urls <- subset(dat.freq.urls, subset=geo_city_name != '(null)')
city.in.html <- ddply(dat.freq.urls, .(long_url, geo_city_name), 
                      summarise, 
                      in_text=str_detect(txt[[long_url[[1]]]], geo_city_name[[1]]),
                      .progress='text')

navelgazers <- ddply(city.in.html, .(geo_city_name), summarise, n=length(geo_city_name), rate=sum(in_text)/length(in_text))

# special cases
navelgazers <- subset(navelgazers, subset=geo_city_name!='Z') # Zurich is weird

plot.navelgazers <- ggplot(subset(navelgazers, subset=n>5), aes(n, rate, label=geo_city_name)) + 
    geom_text(position=position_jitter(height=.02)) +
    xlab('Number of searches') + ylab('Rate Target Page has City Name') +
    opts(title='Navelgazing Cities (top 100 shared .gov URLs)')
ggsave(plot.navelgazers, file='plot_navelgazers.png', width=8, height=8)
ggsave(plot.navelgazers, file='plot_navelgazers_5.png', width=5, height=5)