library(stringr)
library(plyr)
library(ggplot2)
library(scrapeR)
library(RJSONIO)

options(stringsAsFactors=FALSE)
dat1 <- read.csv('1.usa.gov 10k sample.csv')

dat1 <- rename(dat1, c('a'='user_agent',
                       'c'='country_code',
                       'nk'='known_user',
                       'g'='global_bitly_hash',
                       'h'='encoding_user_bitly_hash',
                       'l'='encoding_user_login',
                       'hh'='short_url_cname',
                       'r'='referring_url',
                       'u'='long_url',
                       't'='timestamp',
                       'gr'='geo_region',
                       'll'='latlong',
                       'cy'='geo_city_name',
                       'tz'='timezone',
                       'hc'='hash_timestamp',
                       'al'='accept_language')
               )

dat1$known_user <- as.logical(dat1$known_user)
dat1$timestamp <- as.POSIXct(dat1$timestamp, origin="1970-01-01", tz="GMT")
dat1$hash_timestamp <- as.POSIXct(dat1$hash_timestamp, origin="1970-01-01", tz="GMT")

dat1$lat <- as.numeric(str_replace(dat1$latlong, '\\[ ([-0-9.]+).*', '\\1'))
dat1$lon <- as.numeric(str_replace(dat1$latlong, '.*, ([-0-9.]+) \\]', '\\1'))

ggplot(dat1, aes(timestamp, hash_timestamp)) + stat_bin2d(bins=50)
ggplot(dat1, aes(lon,lat)) + stat_bin2d(bins=50)

dat1$hash_age <- with(dat1, as.numeric(timestamp - hash_timestamp, units='days'))
dat1$agency <- with(dat1, str_extract(long_url, '[[:alpha:]]+.gov'))

common.agencies <- names(head(sort(table(dat1$agency), decreasing=TRUE), 25))
dat.common.agency <- subset(dat1, subset=agency %in% common.agencies)

plot.link.age <- ggplot(dat.common.agency, aes(hash_age)) + 
    geom_density(fill='grey') + 
    facet_wrap(~ agency, scales='free') +
    scale_x_log10('Link Age (log scale)', breaks=c(1/(24*60),1/24,1/2,1,2,10,100), labels=c('1m', '1h', '12h', '', '2d', '10d', '100d')) +
    scale_y_continuous('', breaks=0, labels='') + 
    opts(title='Link Age Frequency, 25 most frequent target domains')



# find top N URLs, and their geo city names. How often does a geo city name appear in the target URL text?

freq.urls <- names(head(sort(table(dat1$long_url), decreasing=TRUE), 100))
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

dat.freq.urls <- subset(dat1, subset=long_url %in% freq.urls)
dat.freq.urls <- subset(dat1, subset=geo_city_name != '(null)')
city.in.html <- ddply(dat.freq.urls, .(long_url, geo_city_name), summarise, in_text=str_detect(txt[[long_url[[1]]]], geo_city_name[[1]]))

navelgazers <- ddply(city.in.html, .(geo_city_name), summarise, n=length(geo_city_name), rate=sum(in_text)/length(in_text))

plot.navelgazers <- ggplot(subset(navelgazers, subset=n>1), aes(n, rate, label=geo_city_name)) + 
    geom_text(position=position_jitter(height=.05)) +
    xlab('Number of searches') + ylab('Rate Target Page has City Name') +
    opts(title='Navelgazing Cities (top 100 shared .gov URLs)')

index <- getURL('http://bitly.measuredvoice.com/bitly_archive/?C=M;O=D')
files <- str_replace(str_sub(str_extract_all(index, 'href="(.+?)"')[[1]], start=7), '"', '')
files <- files[str_detect(files, 'bitly')]

naifnull <- function(a,b) { if (is.null(a)) NA else b }

# sample a few dozen files and merge them
dat.samp <- ldply(sample(files, 25), function (ff) {
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
 