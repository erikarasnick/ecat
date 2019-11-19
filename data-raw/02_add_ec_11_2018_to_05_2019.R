library(tidyverse)
library(CB)

# define study bounding box in lat/lon + 4000 meters
x.min <- -84.93191
x.max <- -83.93180
y.min <- 38.71931
y.max <- 39.63506

#### daily particulate matter data -----

get_daily_ECAT <- function(fl.name) {
  # download, extract, clean up, and read in
  download.file(paste0('https://aqs.epa.gov/aqsweb/airdata/',fl.name),
                destfile=fl.name)
  unzip(fl.name)
  unlink(fl.name)
  fl.name.csv <- gsub(pattern='.zip','.csv',fl.name)
  d.tmp <- read.csv(fl.name.csv,colClasses='character')
  unlink(fl.name.csv)
  # subset to study bb + 4000 meters
  d.tmp <- d.tmp %>%
    mutate_at(c("Longitude","Latitude"),funs(as.numeric)) %>%
    filter(Latitude >= y.min & Latitude <= y.max & Longitude >= x.min & Longitude <= x.max) %>%
    mutate(date = as.Date(Date.Local))
  return(d.tmp)
}

aqs_ecat_spec <- CB::mappp(paste0('daily_SPEC_',2018:2019,'.zip'),get_daily_ECAT,parallel=FALSE)

d.aqs.EC <- aqs_ecat_spec %>%
  bind_rows() %>%
  as.tbl %>%
  filter(grepl("EC",Parameter.Name))%>%
  mutate(id = paste(State.Code,County.Code,Site.Num,sep='-')) %>%
  dplyr::select(id,
         site = Local.Site.Name,
         lat = Latitude,
         lon = Longitude,
         name = Parameter.Name,
         code = Parameter.Code,
         EC = Arithmetic.Mean,
         date) %>%
  mutate(EC=as.numeric(EC))

d.aqs.EC <- d.aqs.EC %>%
  filter(code == "88307" | code == "88380",
         id == "21-117-0007" | id == "39-061-0040") %>%
  dplyr::select(id, site, date, code, EC) %>%
  spread(key=code, value=EC) %>%
  mutate(new.88307 = 0.1041 + 0.9246*`88380`)


EC.taft.new <- d.aqs.EC %>% 
  dplyr::select(id, site, date, EC=new.88307) %>% 
  filter(date > '2018-11-28')


EC.2sites <- bind_rows(ec_2_sites, EC.taft.new) %>%
  arrange(date)

saveRDS(EC.2sites, "ec_ts.rds")

