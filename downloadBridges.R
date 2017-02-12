#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("choroplethr")
#install.packages("dplyr")
#install.packages("plotly")

library(plotly)
library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)

# I like downloading straight from url's.  
# If I was planning to share this code in a long time, I would worry about stability of the accessibility (url/data/format) and maybe backup locally.
# For class, downloading is straightforward.

# I like fread (from data.table) and read_csv (from readr).  
# in my experience, fread is faster and deals with data entry errors a little more elegantly.
# downside of fread: it saves to format data.table.  
#   if you know how to use it, this can be super-duper fast.
# we will be using dplyr instead, which is merely super fast. 
# So, I always convert to a tibble (as.tbl) after an fread.  tibbles are basically data.frames that print nicer.

dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
#tmp1 = read_csv(dest)
#tmp2 = read_csv(dest, col_types = "c")  # could make them all characters...
classes = sapply(tmp, class)


states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()


# this is one possibility...
# for(i in 2:51){
#   dest=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
#   dat[[i]] = fread(dest, colClasses = classes ) %>% as.tbl  # for loop to define data could be really bad.  Does list make it ok?  idk.
# }
# lapply(dat, dim)
# colnames(dat[[1]])


# # gosh, it would be nice if these worked!
# x = fread("https://www.fhwa.dot.gov/bridge/nbi/2016allstatesallrecsdel.zip")
# x = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2016allstatesallrecsdel.zip")


# Hadley typically has good stuff! 
# his code chunk from ftp://cran.r-project.org/pub/R/web/packages/tidyr/vignettes/tidy-data.html
# library(plyr)
# paths <- dir("data", pattern = "\\.csv$", full.names = TRUE)
# names(paths) <- basename(paths)
# ldply(paths, read.csv, stringsAsFactors = FALSE)




dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  ###########very important


# let's dig into the values and see if there are any crazy things going on...
bridgeFull = x16
#bridgeFull = bridgeFull[,-14]
#is.na(bridgeFull) %>% rowSums %>% hist
#is.na(bridgeFull) %>% colSums %>% hist(breaks = 100)
fun = function(x){ 
  return(which(x>20)) 
  }
(bad =  is.na(bridgeFull) %>% colSums %>% fun)
bridgeFull = bridgeFull[,-bad]
#jold =1
#for(j in jold:ncol(bridgeFull)){
#  nc = nchar(bridgeFull[,j], keepNA = T)
#  print(j)
#  print(summary(nc))
#  print(sum(is.na(nc)))
#  print("")
#}
#colnames(bridgeFull)[j]
#bridgeFull = bridgeFull[,-j]
#jold = j

colnames(bridgeFull)
#[1] "STATE_CODE_001"          "STRUCTURE_NUMBER_008"    "RECORD_TYPE_005A"        "ROUTE_PREFIX_005B"      
#[5] "SERVICE_LEVEL_005C"      "ROUTE_NUMBER_005D"       "DIRECTION_005E"          "HIGHWAY_DISTRICT_002"   
#[9] "COUNTY_CODE_003"         "FEATURES_DESC_006A"      "FACILITY_CARRIED_007"    "LOCATION_009"           
#[13] "MIN_VERT_CLR_010"        "KILOPOINT_011"           "LAT_016"                 "LONG_017"               
#[17] "DETOUR_KILOS_019"        "TOLL_020"                "MAINTENANCE_021"         "OWNER_022"              
#[21] "FUNCTIONAL_CLASS_026"    "YEAR_BUILT_027"          "TRAFFIC_LANES_ON_028A"   "TRAFFIC_LANES_UND_028B" 
#[25] "ADT_029"                 "YEAR_ADT_030"            "DESIGN_LOAD_031"         "APPR_WIDTH_MT_032"      
#[29] "MEDIAN_CODE_033"         "DEGREES_SKEW_034"        "STRUCTURE_FLARED_035"    "RAILINGS_036A"          
#[33] "TRANSITIONS_036B"        "APPR_RAIL_036C"          "APPR_RAIL_END_036D"      "HISTORY_037"            
#[37] "NAVIGATION_038"          "NAV_VERT_CLR_MT_039"     "NAV_HORR_CLR_MT_040"     "OPEN_CLOSED_POSTED_041" 
#[41] "SERVICE_ON_042A"         "SERVICE_UND_042B"        "STRUCTURE_KIND_043A"     "STRUCTURE_TYPE_043B"    
#[45] "APPR_KIND_044A"          "APPR_TYPE_044B"          "MAIN_UNIT_SPANS_045"     "APPR_SPANS_046"         
#[49] "HORR_CLR_MT_047"         "MAX_SPAN_LEN_MT_048"     "STRUCTURE_LEN_MT_049"    "LEFT_CURB_MT_050A"      
#[53] "RIGHT_CURB_MT_050B"      "ROADWAY_WIDTH_MT_051"    "DECK_WIDTH_MT_052"       "VERT_CLR_OVER_MT_053"   
#[57] "VERT_CLR_UND_REF_054A"   "VERT_CLR_UND_054B"       "LAT_UND_REF_055A"        "LAT_UND_MT_055B"        
#[61] "LEFT_LAT_UND_MT_056"     "DECK_COND_058"           "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060"  
#[65] "CHANNEL_COND_061"        "CULVERT_COND_062"        "OPR_RATING_METH_063"     "INV_RATING_METH_065"    
#[69] "STRUCTURAL_EVAL_067"     "DECK_GEOMETRY_EVAL_068"  "UNDCLRENCE_EVAL_069"     "POSTING_EVAL_070"       
#[73] "WATERWAY_EVAL_071"       "APPR_ROAD_EVAL_072"      "DATE_OF_INSPECT_090"     "FRACTURE_092A"          
#[77] "UNDWATER_LOOK_SEE_092B"  "SPEC_INSPECT_092C"       "STRAHNET_HIGHWAY_100"    "PARALLEL_STRUCTURE_101" 
#[81] "TRAFFIC_DIRECTION_102"   "HIGHWAY_SYSTEM_104"      "FEDERAL_LANDS_105"       "DECK_STRUCTURE_TYPE_107"
#[85] "SURFACE_TYPE_108A"       "MEMBRANE_TYPE_108B"      "DECK_PROTECTION_108C"    "NATIONAL_NETWORK_110"   
#[89] "BRIDGE_LEN_IND_112"      "SCOUR_CRITICAL_113"      "FUTURE_ADT_114"          "YEAR_OF_FUTURE_ADT_115" 
#[93] "FED_AGENCY"              "DATE_LAST_UPDATE"        "TYPE_LAST_UPDATE"        "DEDUCT_CODE"            
#[97] "PROGRAM_CODE"            "PROJ_NO"                 "STATUS_WITH_10YR_RULE"   "SUFFICIENCY_ASTERC"     
#[101] "SUFFICIENCY_RATING"      "STATUS_NO_10YR_RULE"    


keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" ,
         "ADT_029","YEAR_ADT_030" ,"YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", 
         "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090"   ,
         "FRACTURE_092A"     ,      "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C", "ROADWAY_WIDTH_MT_051"   )

# x = M[,match(keep, colnames(M))]
bridgeFull = as.tbl(bridgeFull)
x = select(bridgeFull, one_of(keep))  # see chapter 5 (section 4) in r4ds.

wi = filter(x, STATE_CODE_001 == 55)
#wi
library(ggplot2)
#ggplot(data = wi) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))
wi = filter(wi,LONG_017 > 0)
ggplot(data = wi) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))

plot_ly(y = wi$LAT_016, x = wi$LONG_017,name="bridges in wi")

min2dec = function(x){
  substr(x,3,8) %>% return
}
hist(wi$LAT_016 %>% min2dec %>% as.numeric)

min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
min2dec(wi$LAT_016[1])
hist(wi$LAT_016 %>% min2dec %>% as.numeric)

wi = mutate(wi,lat = min2dec(LAT_016), lon = min2dec(LONG_017))
#ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = lon))

wi = filter(wi,lon<100)
ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = lon))

plot_ly(y = wi$lat, x = wi$lon,name="bridges in wi")



#ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = lon,col =TOLL_020))
p = plot_ly(y = wi$lat, x = wi$lon,name='bridges in wi')
subplot( add_markers(p, symbol = (wi$TOLL_020),alpha=0.4))

#ggplot(data = wi) +geom_point(mapping = aes(y = lat, x = lon,col =YEAR_BUILT_027))
#p = plot_ly(y = wi$lat, x = wi$lon,name='year of bridges in wi')
subplot( add_markers(p,color = (wi$YEAR_BUILT_027),alpha=1))

#p = plot_ly(y = wi$lat, x = wi$lon,name='conditions of bridges in wi')
subplot( add_markers(p,color = (wi$DECK_COND_058),alpha=1))


subplot( add_markers(p,color = ~factor(wi$ROADWAY_WIDTH_MT_051),alpha=1))


#ggplot(data = wi) +geom_point(mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027))
plot_ly(y = log(wi$ADT_029), x = wi$YEAR_BUILT_027,name="bridges in wi")

ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = TOLL_020)) +geom_point() + geom_smooth()

ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = SUPERSTRUCTURE_COND_059)) +geom_point() + geom_smooth(method = "loess", span = .7)


# make function to rate bridge as NA, good, bad, fail, using 
# colnames(wi)[10:13]
# "SUPERSTRUCTURE_COND_059" "SUBSTRUCTURE_COND_060"   "CHANNEL_COND_061"        "CULVERT_COND_062"  
# good = 5:9
# bad = 2:4
# fail = 0:1

# cond "condition" is the minimum of the given ratings. 
wi = mutate(wi, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                            na.rm = T))

rateIt = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

wi$rate = rateIt(wi$cond)
table(wi$cond)
table(wi$rate)
wi = filter(wi, cond>1)
ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = rate)) +geom_point() + geom_smooth()

map = ggplot(data = wi, mapping = aes(y = lat, x = lon))
map + geom_point(aes(col=rate))+ scale_colour_brewer(palette = "Spectral")  

# where are these bad roads?!!??
ggplot(data = wi, mapping = aes(x = rate, y = log(ADT_029))) + geom_boxplot()
#colnames(wi)

# use a data playground!

wi = mutate(wi, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
wi = wi %>% mutate(fips = STATE_CODE_001*1000+COUNTY_CODE_003)
wi$fips %>% head

wi = wi %>% mutate(good = (rate == "good"))
table(wi$good)
dat = wi %>% group_by(fips) %>% summarize(propGoodRoads = mean(good))

dat = wi %>% group_by(good) %>% summarize(tmp = mean(lat))

dat %>% transmute(region=fips, value = propGoodRoads) %>% county_choropleth(state_zoom = "wisconsin")





