library(httr)
library(curl)
library(jsonlite)
library(dplyr)
library(lubridate)
library(glue)


# Pull cookies and headers from browser
# In network tab, copy paste as curl (bash)
# Convert to R to get vectors of cookies and headers needed
# https://curlconverter.com/r
cookies = c()

headers = c()

scrape_daily <- function(date, cookies, headers, type=c('homicide', 'robbery', 'motor')) {

    date <- format(date, "%m/%d/%Y")

    dataParams = switch(type,
      homicide = paste0('{"buffer":{"enabled":false,"restrictArea":false,"value":[]},"date":{"start":"', date, '","end":"', date, '"},"agencies":[],"layers":{"selection":[null,null,{"selected":true},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,{"selected":false},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false}]},"location":{"bounds":{"east":-67.3092772618703,"north":50.457771266534564,"south":24.39348808598452,"west":-125.2638671056203},"lat":37.02273775050917,"lng":-79.36319033493295,"zoom":8},"analyticLayers":{"density":{"selected":false,"transparency":60}},"widgetConfig":{"parent":"events","type":"analyticschart","chartType":"line","layer":"EVE","name":"Crime Class Timeline - Day","description":"Displays the frequency of events by crime class by day.","selection":"EVENTS_CRIME_CLASS_TIMELINE","chartTypes":[{"type":"area","title":"Area","axis_titles":false},{"type":"bar","title":"Bar","axis_titles":true,"axis_pos":2},{"type":"line","title":"Line","axis_titles":true,"axis_pos":1},{"type":"pie","title":"Pie","axis_titles":true},{"type":"scatter","title":"Scatter","axis_titles":true,"axis_pos":1},{"type":"step","title":"Step","axis_titles":true,"axis_pos":1},{"type":"halfpie","title":"Half Pie","axis_titles":false}],"chartProperties":{"orientation":"horizontal","pointSize":5,"xAxisGuideLines":false,"xAxisOverlapMode":"rotate","xAxisLabelRotation":45,"xAxisHidden":true,"yAxisHidden":true,"xAxisTitle":"Date","legendVisible":true,"legendPosition":"right","tooltipValueFormat":",.0f","showValueFormat":",.0f"},"multiple":false,"comparisonEnable":true,"drillDownEnable":true,"grouping":"first_date,agency","drillDown":{"key":"Day","allOther":"","filter":"FIRST_DATE_TIME"}}}'),
      robbery  = paste0('{"buffer":{"enabled":false,"restrictArea":false,"value":[]},"date":{"start":"', date, '","end":"', date, '"},"agencies":[],"layers":{"selection":[null,null,{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":true},{"selected":true},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,{"selected":false},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false}]},"location":{"bounds":{"east":-67.3092772618703,"north":50.457771266534564,"south":24.39348808598452,"west":-125.2638671056203},"lat":37.02273775050917,"lng":-79.36319033493295,"zoom":8},"analyticLayers":{"density":{"selected":false,"transparency":60}},"widgetConfig":{"parent":"events","type":"analyticschart","chartType":"line","layer":"EVE","name":"Crime Class Timeline - Day","description":"Displays the frequency of events by crime class by day.","selection":"EVENTS_CRIME_CLASS_TIMELINE","chartTypes":[{"type":"area","title":"Area","axis_titles":false},{"type":"bar","title":"Bar","axis_titles":true,"axis_pos":2},{"type":"line","title":"Line","axis_titles":true,"axis_pos":1},{"type":"pie","title":"Pie","axis_titles":true},{"type":"scatter","title":"Scatter","axis_titles":true,"axis_pos":1},{"type":"step","title":"Step","axis_titles":true,"axis_pos":1},{"type":"halfpie","title":"Half Pie","axis_titles":false}],"chartProperties":{"orientation":"horizontal","pointSize":5,"xAxisGuideLines":false,"xAxisOverlapMode":"rotate","xAxisLabelRotation":45,"xAxisHidden":true,"yAxisHidden":true,"xAxisTitle":"Date","legendVisible":true,"legendPosition":"right","tooltipValueFormat":",.0f","showValueFormat":",.0f"},"multiple":false,"comparisonEnable":true,"drillDownEnable":true,"grouping":"first_date,agency","drillDown":{"key":"Day","allOther":"","filter":"FIRST_DATE_TIME"}}}'),
      motor    = paste0('{"buffer":{"enabled":false,"restrictArea":false,"value":[]},"date":{"start":"', date, '","end":"', date, '"},"agencies":[],"layers":{"selection":[null,null,{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":true},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,{"selected":false},null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false},{"selected":false}]},"location":{"bounds":{"east":-67.3092772618703,"north":50.457771266534564,"south":24.39348808598452,"west":-125.2638671056203},"lat":37.02273775050917,"lng":-79.36319033493295,"zoom":8},"analyticLayers":{"density":{"selected":false,"transparency":60}},"widgetConfig":{"parent":"events","type":"analyticschart","chartType":"line","layer":"EVE","name":"Crime Class Timeline - Day","description":"Displays the frequency of events by crime class by day.","selection":"EVENTS_CRIME_CLASS_TIMELINE","chartTypes":[{"type":"area","title":"Area","axis_titles":false},{"type":"bar","title":"Bar","axis_titles":true,"axis_pos":2},{"type":"line","title":"Line","axis_titles":true,"axis_pos":1},{"type":"pie","title":"Pie","axis_titles":true},{"type":"scatter","title":"Scatter","axis_titles":true,"axis_pos":1},{"type":"step","title":"Step","axis_titles":true,"axis_pos":1},{"type":"halfpie","title":"Half Pie","axis_titles":false}],"chartProperties":{"orientation":"horizontal","pointSize":5,"xAxisGuideLines":false,"xAxisOverlapMode":"rotate","xAxisLabelRotation":45,"xAxisHidden":true,"yAxisHidden":true,"xAxisTitle":"Date","legendVisible":true,"legendPosition":"right","tooltipValueFormat":",.0f","showValueFormat":",.0f"},"multiple":false,"comparisonEnable":true,"drillDownEnable":true,"grouping":"first_date,agency","drillDown":{"key":"Day","allOther":"","filter":"FIRST_DATE_TIME"}}}')
    )
    
    res = httr::POST(url = 'https://communitycrimemap.com/api/v1/search/widget-data', httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies), body = dataParams)

    parsed = jsonlite::fromJSON(content(res, 'text'), simplifyDataFrame = TRUE)

    parsed_df = as.data.frame(parsed$data$data)

    if(nrow(parsed_df) > 0) {
      parsed_df = parsed_df %>%  
        `colnames<-`(c(parsed$data$columns))
    }

    return(parsed_df)

}

scrape_daily(as.Date('2022/08/06'), cookies, headers, 'homicide')


alldays = seq(from = as.Date("2021-01-01"), to = as.Date("2022-12-31"), by = 'day')
homicides = lapply(alldays, function(x) scrape_daily(x, cookies, headers, 'homicide'))
homicidesdf <- do.call(dplyr::bind_rows, homicides)
write.csv(homicidesdf, 'homicides.csv')


robberies = lapply(alldays, function(x) scrape_daily(x, cookies, headers, 'robbery'))
robberiesdf <- do.call(dplyr::bind_rows, robberies)
write.csv(robberiesdf, 'robberies.csv')

motor = lapply(alldays, function(x) scrape_daily(x, cookies, headers, 'motor'))
motordf <- do.call(dplyr::bind_rows, motor)
write.csv(motordf, 'motor.csv')
