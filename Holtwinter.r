
library( tseries )
library(forecast)

data  <- paste( "C:\\Users\\daasca\\Documents\\temps.txt", sep = "" )
temp_data   <- read.table( data, stringsAsFactors = FALSE, header = TRUE )
summary( temp_data )

temp_series <- as.vector( unlist( temp_data[ , 2:21 ] ) )
temp_series <- ts( temp_series, start = 1996, frequency = 123 )
plot.ts( temp_series )

pperron@teststat


pperron@cval


pperron_diff = ur.pp( diff( temp_series ), type = "Z-tau", model = "constant" )
pperron_diff@teststat


pperron_diff@cval


pperron_diff_of_diff = ur.pp( diff( diff( temp_series ) ), type = "Z-tau",
                              model = "constant" )

pperron_diff_of_diff@teststat

pperron_diff_of_diff@cval

plot( pperron )

temp_HW_model <- HoltWinters( temp_series, beta = F )
HoltWinters(x = temp_series, beta = F5)

plot( temp_HW_model )

temp_HW_model$SSE


seasonal_temp <- temp_HW_model$fitted
plot( seasonal_temp )


temp_forecast <- predict( temp_HW_model, n.ahead = 90, prediction.interval = TRUE )



plot(forecast( temp_HW_model, h = 180 ),ylim=c(30,150), xlim = c(1995, 2020), c=5) 

