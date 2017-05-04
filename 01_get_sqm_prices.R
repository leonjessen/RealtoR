# Clear workspace
# ------------------------------------------------------------------------------
rm(list=ls())

# Load libraries
# ------------------------------------------------------------------------------
library('tidyverse')
library('XML')
library('stringr')
library('lubridate')

# Define functions
# ------------------------------------------------------------------------------
get_sqm_prices = function(from_year,area_code,res_type="Villa",type_of_sale="Alm. Salg"){
  
  # Create url from scraping
  search_string = paste0('http://www.boliga.dk/salg/',
                         'resultater?',
                         'so=1',
                         '&sort=omregnings_dato-d',
                         '&maxsaledate=today',
                         '&iPostnr=',    area_code,
                         '&gade=',
                         '&type=',       res_type,
                         '&minsaledate=',from_year)
  i = 1
  reached_end = FALSE
  all_html_tables = list()
  header = c()
  while( !reached_end ){
    current_page = paste0(search_string,'&p=',i)
    tmp = readHTMLTable(doc = current_page,
                        header = FALSE,
                        stringsAsFactors = FALSE)$searchresult
    if( i == 1 ){ header = tmp[1,]}
    if( nrow(tmp) == 1 ){
      reached_end = TRUE
    } else {
      all_html_tables[[i]] = tmp[-1,]
      cat("Read page",i,"\n")
    }
    i = i + 1
  }
  
  price_data = do.call("rbind",all_html_tables)
  colnames(price_data) = header
  price_data = price_data[,-10] %>% as_tibble
  price_data = price_data %>%
    filter(`Dato / Type` %>% str_detect(type_of_sale)) %>% 
    transmute(city   = `Adresse / Postnr` %>% str_extract("\\d{4} \\w+$"),
              street = `Adresse / Postnr` %>% str_replace(city,""),
              sqms   = `m²` %>%  str_replace_all("\\.","") %>% as.numeric(),
              price  = `Købesum` %>% str_replace_all("\\.","") %>% as.numeric(),
              yoc    = Bygget %>% paste0("-01-01"),
              dos    = `Dato / Type` %>% str_extract('\\d{2}-\\d{2}-\\d{4}'),
              rooms  = Rum %>% as.integer,
              sqmp   = round(price / sqms)) %>% 
    mutate(dos = dmy(dos), qos = quarter(dos,with_year = TRUE), yoc = ymd(yoc)) %>%
    select(street,city,price,sqms,sqmp,rooms,dos,qos,yoc)
  return(price_data)
}

# Get square meter prices
# ------------------------------------------------------------------------------
proj_dir  = '/Users/leon/Projects/GitHub/address_to_map/'
from_year = 2014
area_code = 3460
res_type  = c('Villa','Ejerlejlighed')[1]
price_data = get_sqm_prices(from_year = from_year, area_code = area_code)

# Write results to file
# ------------------------------------------------------------------------------
output_file = paste0(proj_dir,'price_data_',from_year,'_',area_code,'.txt')
write_delim(x = price_data, path = output_file, delim = "\t")

