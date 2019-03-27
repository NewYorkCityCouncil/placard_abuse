library(sf)
library(leaflet)
library(tidyverse)
library(stringr)
library(ggplot2)
library(DescTools)
library(chron)
library(data.table)


#Read in Illegal Parking dataset

parking <- read.csv("~/Documents/parking_permit_violations/illegal_parking.csv", stringsAsFactors = FALSE)
parking <- parking[,-1]

as.data.frame(table(parking$Descriptor))
as.data.frame(table(parking[- grep("bike", parking$Descriptor),][Descriptor]))



#create geometry for parking data

parking <- parking[!is.na(parking$Latitude),] %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")

#subset data to only include permit parking and placard complaints
permits <- parking[grep("Permit", parking$Descriptor),]
  #parking[parking$Descriptor  "Permit", ]
permits$Created.Date <- as.chron(permits$Created.Date, format = "%m/%d/%Y %I:%M:%S %p")
permits$Closed.Date <- as.chron(permits$Closed.Date, format = "%m/%d/%Y %I:%M:%S %p")

permits_subset <- with(permits, permits[Created.Date < "08/06/2018", ])


placards <- read.csv('~/Documents/parking_permit_violations/placards.csv', stringsAsFactors = FALSE)
placards <- placards[!is.na(placards$Latitude),] %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")
placards$Created.Date <- as.chron(placards$Created.Date, format = "%m/%d/%Y %I:%M:%S %p")
placards$res_desc <- rep('Ambiguous',nrow(placards))

permits$res_desc <- gsub('string', 'Other', permits$res_desc)
permits$res_desc <- gsub('other', 'Other', permits$res_desc)
permits$res_desc <- gsub('no_action_taken', 'No Action Taken', permits$res_desc)
permits$res_desc <- gsub('fixed', 'Fixed', permits$res_desc)
permits$res_desc <- gsub('ambiguous', 'Ambiguous', permits$res_desc)
permits$res_desc <- gsub('wrong_agency', 'Wrong Agency', permits$res_desc)
permits$res_desc <- gsub('did_not_observe', 'Did Not Observe', permits$res_desc)
permits$res_desc <- gsub('ongoing', 'Ongoing', permits$res_desc)






binded <- rbind(permits, placards)
binded$complaint <- ifelse(binded$Complaint.Type == 'City Vehicle Placard Complaint', 'Placard Complaint', 'Permit Parking Complaint')
binded$opened <- as.Date(binded$Created.Date)
aggbind <- aggregate(x = binded$complaint,
                     FUN = count,
                     by = list(Group.date = binded$opened))
setDT(binded)
agg <- binded[,.N,by=c('complaint', 'opened')]




tmp <- subset(permits, res_desc == "ambiguous")

pal <- colorFactor(c('red','orange','yellow','blue', 'green','violet','maroon','black', 'brown'),
                   domain = c("Did Not Observe", "No Action Taken", 'Ongoing', 'violations_issued', 'Fixed', 'Ambiguous', 'Wrong Agency', 'duplicate', 'Other'))
groups = as.character(unique(permits$res_desc))


map = leaflet(permits) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addLegend("topleft", values = ~res_desc, pal = pal,
            #labels = c("Did Not Observe", "No Action Taken", 'Ongoing', 'Violations Issued', 'Fixed', 'Ambiguous', 'Wrong Agency', 'Duplicate', 'Other'),
            title = "Resolution Categorization",
            opacity = .7)
for(g in groups){
  d = permits[permits$res_desc == g, ]
  map = map %>% addCircleMarkers(data = d,color = ~pal(res_desc),group = g,
                                 fill = TRUE, fillOpacity = .45, stroke = FALSE, radius = 1.5)
}
map %>% addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE))


ggplot(agg, aes(x = opened, y = N)) + 
  geom_line(aes(color = complaint), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()



# determining average close time

permits$res_time_hours <- difftime(permits$Closed.Date,permits$Created.Date , units = c("days"))
  
  permits$Closed.Date-permits$Closed.Date


  
difftime(permits$Created.Date ,permits$Closed.Date , units = c("days"))


#see number of complaints in 2018 by first creating a year column in the dataset
permits$year <- as.numeric(format(permits$Created.Date,'%Y'))
nrow(permits[permits$year==2018])
tmp <- permits %>% filter(year==2018)
nrow(tmp)

# see top 10 resolution descriptions
sort(table(permits$Resolution.Description), decreasing = TRUE)[1:10]
     
     
tmp2 <- as.data.frame(table(tmp$Resolution.Description))
     
write.csv(tmp2, file = 'placards_2018_resolutions.csv')    
     
     
     \