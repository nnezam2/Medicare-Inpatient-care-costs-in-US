# Loading Required Libraries 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(tidyr)
library(magrittr)
#install.packages("varhandle")
library(varhandle)
#### Loading the data
income <- read.csv(file="median income data.csv", header=TRUE)
mortality <- read.csv(file="Mortality rates.csv", header=TRUE)
main_data <- read.csv(file="main.csv", header=TRUE)

#### checking data set
#str(main_data )
#str(mortality)
#str(income)

#### cleaning type and names of variables:

## main data 
main_data$DRG.Definition<- as.character(main_data$DRG.Definition)
main_data$State<-(main_data$Provider.State)

main_data $Average.Medicare.Payments<- as.character(main_data $Average.Medicare.Payments)
main_data $Average.Medicare.Payments<- as.numeric(gsub(",", "",main_data $Average.Medicare.Payments,fixed = TRUE))

main_data $Average.Covered.Charges<- as.character(main_data $Average.Covered.Charges)
main_data $Average.Covered.Charges<- as.numeric(gsub(",", "",main_data $Average.Covered.Charges,fixed = TRUE))

main_data $Average.Total.Payments<- as.character(main_data $Average.Total.Payments)
main_data $Average.Total.Payments<- as.numeric(gsub(",", "",main_data $Average.Total.Payments,fixed = TRUE))


## Mortality 
mortality$State_names<- mortality$Title..Number.of.Deaths.per.100.000.Population...The.Henry.J..Kaiser.Family.Foundation
mortality<- mortality[c(-1),]
mortality<- mortality[c(-1),]
mortality<- mortality[c(-1),]
mortality $X.1<- as.character(mortality $X.1)
mortality$X.1<- as.numeric(gsub(",", "",mortality $X.1,fixed = TRUE))
mortality$State<- mortality$X

## Income 
income$State <- as.character(income$Acronym)
income$Income<- as.character(gsub("$"," ",income$Income,fixed = TRUE))
income$Income<- as.numeric(gsub(",", "",income$Income,fixed = TRUE))

#### checking data set details 

#head(main_data)
#head(mortality)
#head(income)



#### Table 1: Top 10 Satets based on the Avgerage of Medicare payments costs

Tabel1<- main_data%>%
    group_by(State)%>%
      summarize(Medicare_Payment_Mean=mean(Average.Total.Payments)%>% round(1),
        Medicare_Payment_Range=paste0("(",min(Average.Total.Payments),",",max(Average.Total.Payments),")")) %>%
        arrange(desc(Medicare_Payment_Mean)) %>%
        top_n(10,Medicare_Payment_Mean)
        

(Tabel1)


#### Table 2: Top 10 DRG catagories in US based on the frequency 

Tabel2<- main_data %>%
  group_by(DRG.Definition)%>%
  summarize(Count_DRG=n(),Coverage=mean(Average.Total.Payments))%>%
  arrange(desc(Count_DRG))%>%
  top_n(10,Count_DRG) %>%
    mutate(Percent_DRG=((Count_DRG)/sum(Count_DRG))*100)

Tabel2


#### Tabel 3: Top 10 DRG catagories in Minnesota based on the frequency
Tabel3<- main_data %>%
   filter (State=="MN") %>%
    group_by(DRG.Definition) %>%
    summarize(Count_MN_DRG=n(),Coverage=mean(Average.Total.Payments)) %>%
    arrange(desc(Count_MN_DRG)) %>%
    top_n(10,Count_MN_DRG) %>%
    mutate(Percent_DRG=((Count_MN_DRG)/sum(Count_MN_DRG))*100)

Tabel3


#### First graph: common DRG groups in US vs Minnesota

## US graph 
G1<- ggplot(Tabel2, aes(x = reorder(DRG.Definition,Count_DRG), y = Count_DRG, 
                        label = Count_DRG)) + 
  geom_bar( stat='identity',aes(fill=Count_DRG), width=.5) +
  labs(subtitle="Top 10 DRG Values", 
       title= "Bar Plot of DRG group count in the U.S", x = "DRG Groups", y = "count_DRG") + 
  coord_flip()+
     theme(text = element_text(size = 5), element_line(size = 1))
#G1

## Minnesota graph 
G2 <-ggplot(Tabel3, aes(x = reorder(DRG.Definition,Count_MN_DRG), y = Count_MN_DRG, 
                    label = Count_MN_DRG)) + 
  geom_bar( stat='identity',aes(fill=Count_MN_DRG), width=.5) +
  labs(subtitle="Top 10 DRG Values", 
       title= "Bar Plot of DRG group count in Minnesota", x = "DRG Groups", y = "count_MN_DRG") + 
  coord_flip()+
     theme(text = element_text(size = 5), element_line(size = 1))
#G2

## Combined Graph: Comparing top 10 DRG groups in US v.s Minnesota  

ggarrange(G1 ,G2,
          labels = c("US", "Minnesota"),
          ncol = 1)


#### second graph : Average Coverage for common DRG groups in US vs Minnesota

## US graph 
G3<- ggplot(Tabel2, aes(x = reorder(DRG.Definition,Coverage), y =Coverage , 
                        label = Coverage)) + 
  geom_bar( stat='identity',aes(fill=Coverage), width=.5) +
  labs(subtitle="Top 10 DRG Coverage amounts", 
       title= "Bar Plot of DRG Coverage amounts in the U.S", x = "DRG Groups", y = "Coverage") + 
  coord_flip()+
     theme(text = element_text(size = 5), element_line(size = 1))


## Minnesota graph 
G4 <-ggplot(Tabel3, aes(x = reorder(DRG.Definition,Coverage), y = Coverage, 
                    label =Coverage)) + 
  geom_bar( stat='identity',aes(fill=Coverage), width=.5) +
  labs(subtitle="TTop 10 DRG Coverage amounts", 
       title= "Bar Plot of DRG Coverage amounts in Minnesota", x = "DRG Groups", y = "Coverage") + 
  coord_flip()+
     theme(text = element_text(size = 5), element_line(size = 1))


## Combined Graph: Comparing top 10 DRG groups in US v.s Minnesota  

ggarrange(G3 ,G4,
          labels = c("US", "Minnesota"),
          ncol = 1)



#### Cleaning and combining datasets together 

clean_data<- main_data %>%
  group_by(State)%>%
  summarize(
            Total_payments_Mean=mean(Average.Total.Payments), 
            Covered_payments_Mean=mean(Average.Covered.Charges)) %>%
            left_join(mortality,by="State")%>%
            left_join(income, by="State") %>%
            rename(Death_Rate_per_1000000=X.1) %>%
            select(State,State_names,Total_payments_Mean,Covered_payments_Mean, Death_Rate_per_1000000, Income)


head(clean_data)



#### Plotting the medicare payments vs income and death rate in the US 


G5<- ggplot(clean_data, aes(x=Total_payments_Mean, y=Income) )+
  geom_point(alpha=0.8)+
      geom_smooth(method = 'loess')+
  labs(title="Medicare payments v.s median income in US")+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14))+
  theme_bw()+
  scale_color_brewer(palette="Dark2")



G6 <- ggplot(clean_data, aes(x=Total_payments_Mean, y=Death_Rate_per_1000000) )+
  geom_point(alpha=0.8)+
  geom_smooth(method = 'loess')+
  labs(title="Medicare payments v.s Mortality Rate in US")+
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14))+
  theme_bw()+
  scale_color_brewer(palette="Dark2")
  


## combined graphs
ggarrange(G5 ,G6,
          ncol = 1)




#### K-Means Clustering based on total Medicare Payment, death rate and income 

clustering_data<- clean_data %>%
  select(Total_payments_Mean, Death_Rate_per_1000000, Income)

result<-kmeans(clustering_data,3)
result

#### Plotting K-means Clustering results 

G7<-ggplot(clustering_data,aes(x=Death_Rate_per_1000000,y=Total_payments_Mean))+
  geom_point(aes(col=as.factor(result$cluster)))+
    labs(title="Total Medicare Payment vs Death Rate in US",col = "Clusters")

G8<-ggplot(clustering_data,aes(x=Income,y=Total_payments_Mean))+
  geom_point(aes(col=as.factor(result$cluster)))+
    labs(title="Total Medicare Payment vs Income in US",col = "Clusters")
## combined graphs
ggarrange(G7 ,G8,
          ncol = 1)


#### Tabel4: Top 10 States which are in a bad condition based on Death Rates and Income level 
# These Sates need higher medicare payments in the future !  

Table4<- clean_data %>%
        mutate(result.clustering=result$cluster)%>%
        filter(result.clustering==2)%>%
        select(State,State_names,Total_payments_Mean,Death_Rate_per_1000000,Income)%>%
        arrange(desc(Death_Rate_per_1000000))%>%
        top_n(10,Death_Rate_per_1000000)

Table4

#### Creating a function to help finding the most common DRGs for states which are in a bad condition 
Top10_DRG<-function(State){
    S=as.character(State)
    
    if ( S != "WV" & S != "MS" & S != "KY" & S !="AL" &S != "OH" || 
         S !="AR"  & S !="TN" & S !="LA" & S != "OH" & S != "IN"  )
    {
    print("Based on our data, this States is Ok! you may not need to change policies")
    }
    
   else
        {
        print(" This State has low income and high death rate ")
        paste0 (" The top 10 DRG groups for ",S,"are:")
        
        ## State 
        Table5<- main_data %>%
        filter(State== S) %>%
        group_by(State,DRG.Definition) %>%
        summarize(Count_DRG=n(),Coverage=mean(Average.Total.Payments)) %>%
        arrange(desc(Count_DRG)) %>%
        top_n(10,Count_DRG) %>%
        mutate(Percent_DRG=((Count_DRG)/sum(Count_DRG))*100)


       print(Table5)
       
        ##US
       Table6<- main_data %>%
        filter(DRG.Definition==Table5$DRG.Definition)%>%
        group_by(DRG.Definition)%>%
          summarize(Count_DRG=n(),Coverage=mean(Average.Total.Payments))%>%
          arrange(desc(Count_DRG))%>%
            mutate(Percent_DRG=((Count_DRG)/sum(Count_DRG))*100)


       
## US graph 
G10<- ggplot(Table6, aes(x = reorder(DRG.Definition,Coverage), y =Coverage , 
                        label = Coverage)) + 
  geom_bar( stat='identity',aes(fill=Coverage), width=.5) +
  labs(subtitle="Top 10 DRG Coverage amount", 
       title= "Bar Plot of DRG Coverage amount in the U.S", x = "DRG Groups", y = "Coverage") + 
  coord_flip()+
     theme(text = element_text(size = 5), element_line(size = 1))


## State graph 
G11 <-ggplot(Table5, aes(x = reorder(DRG.Definition,Coverage), y = Coverage, 
                    label =Coverage)) + 
  geom_bar( stat='identity',aes(fill=Coverage), width=.5) +
  labs(subtitle="Top 10 DRG Coverage amount", 
       title= "Bar Plot of DRG Coverage amount in State", x = "DRG Groups", y = "Coverage") + 
  coord_flip()+
     theme(text = element_text(size = 5), element_line(size = 1))


## Combined Graph: Comparing top 10 DRG groups in US v.s other State  

ggarrange(G10 ,G11,
          labels = c("US", S),
          ncol = 1)


       }
}



## Test1
Top10_DRG("OH")

## Test2
Top10_DRG("CA")
