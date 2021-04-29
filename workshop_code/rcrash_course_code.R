# Install Packages and Load Libraries

install.packages("tidyverse")
install.packages("stargazer")
install.packages("ggeffect")
install.packages("psych")
install.packages("janitor")
install.packages("fastDummies")

library(tidyverse)
library(stargazer)
library(ggeffect)
library(psych)
library(janitor)
library(fastDummies)

# Read in Data (make sure you're in the correct working directory)
setwd("~/Documents/git_repositories/rcrashcourse/workshop_data")
persson_tabellini_original<-read_csv("persson_tabellini_workshop.csv")

# Make a copy of the dataset so we don't alter the original dataset; then, view
# the copied dataset 
pt_copy<-persson_tabellini_original
View(pt_copy)

# computing basic summary statistics

pt_copy_summarystats1<-describe(pt_copy)
View(pt_copy_summarystats1)

setwd("~/Documents/git_repositories/rcrashcourse/outputs")
write.csv(pt_copy_summarystats1, "summarystats.csv")

## making a publishable summary statistics table 
pt_copy_df<-as.data.frame(pt_copy)

summary_stats_txt<-stargazer(pt_copy_df, type="text", title="Descriptive Statistics", 
                         digits=1, out="summary_stats.txt")

summary_stats_html<-stargazer(pt_copy_df, type="html", title="Descriptive Statistics", 
                             digits=1, out="summary_stats.html")

## summary statistics by group: Option 1

summary_stats_by_continent<-describeBy(pt_copy, pt_copy$continent)
View(summary_stats_by_continent)

## summary statistics by group: Option 2 (aggregated group statistics)

trade_age_by_continent<-pt_copy %>% group_by(continent) %>% 
                                          summarise(meanTrade=mean(trade),sdTrade=sd(trade),
                                                    meanAge=mean(age), sdAge=sd(age),
                                                    n=n())
trade_age_by_continent

## crosstabs

crosstab_federal_continent<-pt_copy %>% tabyl(federal, continent)

crosstab_federal_continent

# Basic Tasks

## Renaming a variable (renames "list" to "party_list")

pt_copy<-pt_copy %>% rename(party_list=list)

## changing the order of columns 

pt_copy<-pt_copy %>% relocate(party_list, trade)
View(pt_copy)

## sorting in ascending or descending order
pt_copy<-pt_copy %>% arrange(trade)
View(pt_copy)

pt_copy<-pt_copy %>% arrange(desc(trade))
View(pt_copy)

##creating new variable based on existing variable (ratio of revenue to expenditures)

pt_copy<-pt_copy %>% mutate(rev_exp=(cgrev/cgexp)) %>% 
                     relocate(rev_exp)
View(pt_copy)

## Recoding Variables: Making Dummy Variables from Continuous Numeric Variables

pt_copy<-pt_copy %>% mutate(trade_open=ifelse(trade>=77, 1, 0)) %>% 
                     relocate(trade_open, trade)
View(pt_copy)

## Recoding Variables: Making categorical variables from Continuous Numeric Variables

pt_copy<-pt_copy %>% mutate(trade_level=case_when(trade>15 & trade<50~"Low Trade",
                                                  trade>=50 & trade<100~"Intermediate Trade",
                                                  trade>=100~"High Trade")) %>% 
                    relocate(trade_level, trade)

View(pt_copy)

## Recoding Variables: Making Dummmy Variables from Categorical Variables 

pt_copy<-pt_copy %>% dummy_cols("trade_level")

View(pt_copy)

## Selecting columns and making new dataset from selection

pt_copy_selected_columns<-pt_copy %>% select(country, cgexp, cgrev, trade, col_uk)
View(pt_copy_selected_columns)

## Subsetting rows based on specified criteria

pt_copy_trade<-pt_copy %>% filter(trade_level=="High Trade"|trade_level=="Low Trade")
View(pt_copy_trade)

pt_copy_uk_colonies<-pt_copy %>% filter(col_uk==1)
View(pt_copy_uk_colonies)

## Deleting columns 

pt_copy_selected_columns<-pt_copy_selected_columns %>% select(-(trade))
View(pt_copy_selected_columns)

pt_copy_selected_columns<-pt_copy_selected_columns %>% select(-c(cgrev,col_uk))
View(pt_copy_selected_columns)

# Exploratory Visualizations using ggplot

## Visualization (bar chart) of cgexp (central government expenditure variable)

cgexp_viz1<-pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_col(aes(x=reorder(country, cgexp), y=cgexp))+
  labs(title="Central Govt Expenditure as Pct of GDP (1990-1998 Average)", x="Country Name", 
       y="CGEXP")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90))

cgexp_viz1

##Visualization (bar chart) of cgexp (central government expenditure variable); coordinate
## axes flipped

cgexp_viz2<-pt_copy %>% 
           drop_na(cgexp) %>% 
           ggplot()+
           geom_col(aes(x=reorder(country, cgexp), y=cgexp))+
           coord_flip()+
           labs(title="Central Govt Expenditure as Pct of GDP (1990-1998 Average) ", x="Country Name", 
                y="CGEXP")+
           theme(plot.title=element_text(hjust=0.5)) 

cgexp_viz2

## Scatterplot of government revenue (cgrev) against expenditure (cgexp)

scatter1_cgexp_cgrev<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgrev, y=cgexp))+
  labs(title="Central Govt Expenditure \nvs\n Central Govt Revenue (1990-1998 Average) ", 
       x="Central Gov't Revenue (Pct of GDP)", y="Central Government Expenditure (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 

scatter1_cgexp_cgrev

## Scatterplot of government revenue (cgrev) against expenditure (cgexp); grouped
## by continent category 

scatter2_cgexp_cgrev<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgrev, y=cgexp, color=continent))+
  labs(title="Central Govt Expenditure \nvs\n Central Govt Revenue (1990-1998 Average) ", 
       x="Central Gov't Revenue (Pct of GDP)", y="Central Government Expenditure (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 

scatter2_cgexp_cgrev

##Scatterplot of government revenue (cgrev) against expenditure (cgexp); with line of
## best fit
cgexp_cgrev_line<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgrev, y=cgexp))+
  geom_smooth(aes(x=cgrev, y=cgexp), method="lm")+
  labs(title="Central Govt Expenditure \nvs\n Central Govt Revenue (1990-1998 Average) ", 
       x="Central Gov't Revenue (Pct of GDP)", y="Central Government Expenditure (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 

cgexp_cgrev_line

# Basic Regression Analysis

##Regression 1

regression1<-lm(cgexp~gastil+lyp+trade+prop1564+prop65+federal+oecd, data=pt_copy)
summary(regression1)

## Regression 2 (Note treatment of "continent" variable)

pt_copy$continent<-as.factor(pt_copy$continent)
levels(pt_copy$continent)
pt_copy$continent<-relevel(pt_copy$continent, ref="other")

regression2<-lm(cgexp~gastil+lyp+trade+prop1564+prop65+federal+continent+col_espa+col_uka+col_otha+oecd, data=pt_copy)
summary(regression2)


##Regression 2 Alternative (Alternative treatment of continent variable by explicitly
## making dummy variables)

pt_copy<-pt_copy %>% dummy_cols("continent")

regression2_alt<-lm(cgexp~gastil+lyp+trade+prop1564+prop65+federal+continent_africa+
                      continent_asiae+continent_laam+col_espa+col_uka+col_otha+oecd, data=pt_copy)

summary(regression2_alt)

## Generating regression tables

setwd("~/Documents/git_repositories/rcrashcourse/outputs")
model_list<-list(regression1,regression2)
regression_pt_html<-stargazer(model_list, type="html", out="cgexp_regressions.html")
regression_pt_txt<-stargazer(model_list, type="text", out="cgexp_regressions.txt")

## Interaction effects in regression models

trade_federal_interaction<-lm(cgexp~trade*federal, data=pt_copy)
summary(trade_federal_interaction)

## Interpreting interaction term by computing marginal effects of "federal" variable,
## while holding "trade" at its mean

mean(pt_copy$trade)

marginal_effect_federalism<-ggpredict(trade_federal_interaction, terms="federal", condition=c(trade=78.76))
marginal_effect_federalism

## Graphing the marginal effects of federalism

ggpredict(trade_federal_interaction, terms="federal") %>% 
  ggplot(aes(x, predicted))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),width=0.05)+
  scale_x_continuous(breaks=(seq(0,1, by=1)))

# Merging Data 

setwd("~/Documents/git_repositories/rcrashcourse/workshop_data")
capital_openness<-read_csv("chinn_eto_capitalopenness_summary.csv")
View(capital_openness)

?join
pt_capitalmobility<-left_join(pt_copy, capital_openness, by="ctrycd")

View(pt_capitalmobility)



