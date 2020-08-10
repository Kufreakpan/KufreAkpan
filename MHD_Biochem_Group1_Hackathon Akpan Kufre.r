library(tidyverse)
library(readxl)
library(plyr)

#Hackathon pizza v subway fare data
#Conversion Library
#install.packages(c("ISwR","languageR"),dependencies = T)
#library(ISwR)
#library(languageR)
#Add data to library

#Summary statistics of pizza sold by station
cost_data <- read_excel("MHD_BIOC492_HackathonData_072020NYCMTAJuly2014.xlsx", sheet = "Sheet2")

#After observing the data ensure that Median, LL, and UL can be read as numeric columns
cost_data[9,4] <- NA
cost_data[9,5] <- NA
cost_data[9,6] <- NA
cost_data[10,4] <- NA
cost_data[10,5] <- NA
cost_data[10,6] <- NA

cost_data$Median <- as.numeric(cost_data$Median )
cost_data$LL <- as.numeric(cost_data$LL)
cost_data$UL <- as.numeric(cost_data$UL)

str(cost_data)

#Number of riders in each cell (select first 30 days)
rider_data <- read_excel("MHD_BIOC492_HackathonData_072020NYCMTAJuly2014.xlsx", sheet = "Sheet1")


#QUESTION 1
    #Where does the pizza principle hold true?
    ##Check all occasions when the media cost in an area is equal to $2.50
    
    #Keep only costs == $2.50
    question1 <- cost_data %>% 
      filter(cost_data$Median == 2.5)
    
    #Far Rockaway and New York are where the Pizza Principle holds true
  
#QUESTION 2 
    #Where does the pizza principle not hold true?
    ##Check all occasions when the median cost in an area is not equal to 2.50
    
    question2 <- cost_data %>% 
      filter(cost_data$Median != 2.5)
    
    
    
    #The principle does not hold true for Astoria, Brooklyn, Elmhurst, Flushing, Howard Beach,
    #Jackson Heights, Jamaica, Kew Gardens, Little Neck, Woodhaven 
    
    
    
    
    #Visualize the data based on the mean difference away from mean $2.50
    
    #Reorder the Station by ascending mean value
    sorted_labels <-   c("Woodhaven",
                         "Elmhurst",
                         "Jackson Heights",
                         "Jamaica",
                         "Kew Gardens",
                         "Astoria",
                         "Brooklyn",
                         "Flushing",
                         "New York",
                         "Far Rockaway",
                         "Little Neck",
                         "Howard Beach")
    
    cost_data$`MTA Station` <- factor(cost_data$`MTA Station`, levels = sorted_labels)
    
    
#Visualize Q1 and Q2
    ggplot(data=cost_data, aes(x = `MTA Station`, y = Mean)) +
      geom_point(aes(col= `MTA Station`), size = 4) +
      #geom_pointrange(aes(col= `MTA Station`)) +
      xlab('MTA Station') + 
      ylab("Mean")+
      ggtitle("Forest Plot of Mean Pizza Cost by Mass Transit Authority (MTA) Station") +
      scale_y_continuous(breaks = seq(0, 5, by = .25)) +
      geom_hline(aes(fill = `MTA Station`), yintercept = 2.5, linetype = 3, size = 2)+
      theme(plot.title = element_text(size=16, face="bold"),
            axis.ticks.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(face="bold"),
            axis.title = element_text(size=12,face="bold"),
            strip.text.y = element_text(hjust=0, vjust = 1, angle=180, face="bold")) +
      coord_flip()
    
    #Visualize the data based on the median difference away from median $2.50
    #Reorder the Station by ascending value
    sorted_labels2 <-   c("Woodhaven",
                         "Elmhurst",
                         "Jackson Heights",
                         "Jamaica",
                         "Astoria",
                         "Brooklyn",
                         "Flushing",
                         "New York",
                         "Far Rockaway",
                         "Howard Beach",
                         "Kew Gardens",
                         "Little Neck")
    
    cost_data$`MTA Station` <- factor(cost_data$`MTA Station`, levels = sorted_labels2)
  
    ggplot(data=cost_data, aes(x = fct_rev(`MTA Station`), y = Median,  ymin = LL, ymax = UL)) +
      geom_pointrange(aes(col= `MTA Station`)) +
      xlab('MTA Station') + 
      ylab("Median (95% Confidence Interval)")+
      ggtitle("Forest Plot of Median Pizza Cost by Mass Transit Authority (MTA) Station") +
      scale_y_continuous(breaks = seq(0, 5, by = .25)) +
      geom_errorbar(aes(ymin=LL, ymax= UL,col= `MTA Station`),width=0.5,cex=1)+ 
      geom_hline(aes(fill = `MTA Station`), yintercept = 2.5, linetype = 2)+
      theme(plot.title = element_text(size=16, face="bold"),
            axis.ticks.y = element_blank(),
            legend.title = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(face="bold"),
            axis.title = element_text(size=12,face="bold"),
            strip.text.y = element_text(hjust=0, vjust = 1, angle=180, face="bold")) +
      coord_flip()
    
    
    
#Question 3 Visualization
    #Create a scatterplot to assess a relationship between Pizza Cost and Transit user count
    
    rider_datac <- rider_data %>%
      sapply(., function(col) as.numeric(col)) %>%
      colMeans(na.rm = TRUE) %>%
      as.data.frame() %>%
      rowid_to_column(., "MTA Station")
    
    
    
    rider_datac$`MTA Station` <- c("Howard Beach","Little Neck","Far Rockaway","New York",
                           "Flushing","N/A","Brooklyn","Astoria",
                           "Kew Gardens","Jamaica","Jackson Heights","Elmhurst","Woodhaven")
    
    data_merged <- inner_join(as.data.frame(rider_datac), as.data.frame(cost_data), by = "MTA Station")
    data_merged$`MTA Station` <- as.factor(data_merged$`MTA Station`)
    
    ggplot(data = data_merged, aes(x = ., y = Mean)) +
      geom_point(aes(col = `MTA Station`), size = 4) +
      #geom_text(label = `MTA Station`) +
      xlab('Mean Rider July 2014') +
      geom_smooth(method=lm) +
      #stat_cor(method = "pearson", label.x = 7500, label.y = 2.6) +
      ylab("Mean Pizza Cost") +
      ggtitle("Scatter Plot of Mean Rider and Pizza Cost for Mass Transit Authority (MTA) Stations") +
      theme(plot.title = element_text(size=16, face="bold"),
            axis.text.x = element_text(face="bold"),
            axis.title = element_text(size=12,face="bold"),
            strip.text.y = element_text(hjust=0, vjust = 1, angle=180, face="bold")) +
      coord_flip() 
   
    
    summary (rider_data)
    summary (cost_data) 