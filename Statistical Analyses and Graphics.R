#Frogs Tubes VA Data Analysis
#Sarah McGrath-Blaser
#October, 2017

#Load libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)

################### Explore Data ######################
#Saved excel file as CSV
#This code inputs the data file as object data
data<-read.csv("C:/Users/Sarah/Documents/Frog Tubes/VA Frog Tubes/Data_and_Analysis/VA Frog Tubes Data Spreadsheet_CSV.csv",header=T)
data
str(data) #Check the structure of the data

#Get a dataframe with only columns for tube type and number of frogs.
Frog_data <- data %>%
  select(Tube.Type..A.orB., Number.of.Frogs) %>%
  filter(!is.na(Tube.Type..A.orB.), !is.na(Number.of.Frogs)) #Filter out any NA's
Frog_data

#Make a table to see the number of total frogs counted by tube type.
counts <- table(Frog_data$Number.of.Frogs, Frog_data$Tube.Type..A.orB.)
counts

#Calculate the average amount of time spent per survey event
avg_time <- data %>%
  select(End.Time, Start.Time) %>%
  filter(!is.na(End.Time), !is.na(Start.Time)) %>%
  mutate(Elapsed = End.Time - Start.Time) %>%
  summarise(mean(Elapsed))
avg_time
#Search time averaged to one hour and 5 minutes.

#Calculate the percentage of times a frogs was found in a tube
Frog_data %>%
  summarise(sum(Number.of.Frogs))
#This tells us that there were 23 frogs found over the survey period
#1740 = the number tube checks total. 870 = number of tube checks per device type.
(23*100)/1740
(14*100)/870
(9*100)/870
#1.32% chance of finding a frog in any given tube
#1.61% chance of finding a frog in any given novel tube
#1.03% chance of finding a frog in any given traditional tube

################## Barplots of frogs by tube type ##############################################
#Barplot of counts of frogs by tube type
barplot(counts,main="Frogs in Novel vs. Traditional Style Device",xlab = "Device Type",ylab = "Number of Frogs",names.arg = c("Novel", "Traditional"), col = "darkgreen")

#Table of percentages and barplot of percentage of frogs found per style of tube
percents<-prop.table(counts)*100
percents
barplot(percents,main="Frogs in Novel vs. Traditional Style Device",xlab = "Device Type",ylab = "Percentage",names.arg = c("Novel", "Traditional"), col = "darkgreen")

#Did the same thing to create a second barplot of frogs in tube type by species
plot_x_species <- data %>%
   {table(.$Frog.Species, .$Tube.Type..A.orB.)} %>%
  as.data.frame()
plot_x_species

#Table of percentages of frogs by species and barplot of percentage of the species found per style of tube
percents1<-prop.table(plot_x_species)*100
percents1
barplot(percents1,main = "Frog Species in Novel vs. Traditional Device",xlab = "Device Type",ylab = "Percentage",names.arg = c("Novel","Traditional"),col = c("gray46","salmon2"))

#This is best! Barplot based on count data by frog species and tube type
ggplot(data=plot_x_species, aes(fill=Var1, x=Var2, y=Freq)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  theme_classic() +
  scale_fill_manual(values=c("gray55", "salmon2")) +
  scale_x_discrete(name = "Device type", labels=c("Novel","Traditional")) +
  scale_y_continuous(name = "Number of frogs") +
  guides(fill=guide_legend(title="Species")) +
  theme(text = element_text(size = 16)) +
  theme(legend.text = element_text(face = "italic"))

######################### SVL Boxplot ######################
#Select the columns from the data that we want, remove NA's, and mutate column SVL..cm. into new column SVL_mm, adding milimeter measurements to the data.
svl <- data %>%
  select(Tube.Type..A.orB., Number.of.Frogs, SVL..cm., Frog.Species) %>%
  filter(!is.na(Tube.Type..A.orB.), !is.na(Number.of.Frogs), !is.na(SVL..cm.), !is.na(Frog.Species)) %>%
  mutate(SVL_mm = SVL..cm. * 10)
svl

#Boxplot of frog species by SVL range and the tube type they were found in
ggplot(svl, aes(x=Tube.Type..A.orB., y=SVL_mm, fill=Frog.Species)) + 
  geom_boxplot() +
  theme_bw() +
  theme_classic() +
  scale_fill_manual(values=c("gray55", "salmon2")) +
  scale_x_discrete(name = "Device type", labels=c("Novel","Traditional")) +
  scale_y_continuous(name = "Snout Vent Length (mm)") +
  guides(fill=guide_legend(title="Species")) +
  theme(text = element_text(size = 16)) +
  theme(legend.text = element_text(face = "italic"))

###################### Statistical Analysis ############################################

#In order to accurrately compare frogs amongst tube types, needed to include 0's for sampling events where no frogs were found for a given tube type. In a separate sheet in excel, I kept the columns Date, Tube type, and Number of frogs and summed frogs by sampling event. For a given sampling date I had the total number of frogs found in all type A (novel) tubes and the total number of frogs found in all type B (traditional) tubes. Tube type was converted to binary values (1 and 0, respectively) so that the test could recognize it. This sheet was saved as a csv file and imported below.
comp_data <- read.csv("C:/Users/Sarah/Documents/Frog Tubes/VA Frog Tubes/Data_and_Analysis/Comparison_data_CSV.csv", header = T)
comp_data

#Look at the distrubution of frog counts by tube type
par(mfrow = c(1,1))
hist(comp_data$No_frogs)
comp_data_A <- filter(comp_data, Tube_type == "1")
par(mfrow = c(1,2))
hist(comp_data_A$No_frogs)
comp_data_B <- filter(comp_data, Tube_type == "0")
hist(comp_data_B$No_frogs)
#Both are, predictably, strongly right skewed.

#Conducted a Mann-Whitney-Wilcoxon test because its a non-parametric test (non-normality, no assumptions of coming from a normal distribution and because two 'populations' are independent of one another [tube types are independent]). Does assume that that the two populations being compared have the same shape.
#Ho: no difference
#Ha: difference

#Mann Whitney U test for determining if any difference exists for number of frogs using each specific tube type.
wilcox.test(No_frogs~Tube_type, data=comp_data)
#Greater than 0.05 so we reject the alternative for the null that there is no difference in number of frogs by tube type.

#When comparing the SVLs of each frog, tubes 13 and 14 each had two captures of frogs that looked as though they were close enough to maybe be the same frog. Therefore, the dataset below excludes these two points to see if anything changes.
comp_data_no_dup <- comp_data[-7, ] #Removed one of the tube 13 data points from row 7 in the dataframe.
comp_data_no_dup #Check to make sure that row was actually removed.
comp_data_no_dup[20,3] = 2 #This changes the number of captures for novel tube types from October 7, 2017 from 3 to 2, excluding one of the data points for tube 14.

#Also greater than 0.05; without (what we consider) duplicates still no difference between tube types.
wilcox.test(No_frogs~Tube_type, data=comp_data_no_dup)


#Stats on if there are significant differences between SVL's of frogs between tube types.
#Explore data to see what the distribution and looks like
hist(svl$SVL_mm)
shapiro.test(svl$SVL_mm)
#Output from shapiro test is p > 0.05 so we can assume normality but we have a very small sample size so assumption of normal distribution is not robust

#Check out distribution of SVL by species (this might be interesting since we can assume that there will be different size constraints between gray treefrogs and spring peepers)
svl_Hvers <- filter(svl, Frog.Species == "Hyla versicolor")
svl_Pcruc <- filter(svl, Frog.Species == "Pseudacris crucifer")
par(mfrow=c(1,2))
hist(svl_Hvers$SVL_mm)
hist(svl_Pcruc$SVL_mm)
#Not normal distribution and SUPER small sample size. Can't run any statistical tests based on normality.
#Therefore, chose to do a Mann Whitney U test.
#https://www.sheffield.ac.uk/polopoly_fs/1.885207!/file/99_Mann_Whitney_U_Test.pdf
#https://stats.idre.ucla.edu/other/mult-pkg/whatstat/
#https://www.statisticshowto.com/mann-whitney-u-test/
wilcox.test(SVL_mm ~ Tube.Type..A.orB., data = svl)
#p < 0.05
#There is a significant difference between the summed ranks of each group. But, we have species confounding this. Need to split up by the different species and re-analyze.

#Hyla versicolor
wilcox.test(SVL_mm ~ Tube.Type..A.orB., data = svl_Hvers)
svl_Hvers
#p < 0.05
#Yes, significant difference between the summed ranks of H. versicolor SVL's by tube type.
#Check again with the two potentially repeat measurements removed..
svl_Hvers_no_dup <- svl_Hvers[-c(2,11), ] #Rows 2 & 11 contain the potential repeats from tubes 13 and 14.
svl_Hvers_no_dup
wilcox.test(SVL_mm ~ Tube.Type..A.orB., data = svl_Hvers_no_dup)
#p < 0.05
#Yes, significant difference between the summed ranks of H. versicolor SVL's by tube type even without these measurements.

#Pseudacris crucifer
wilcox.test(SVL_mm ~ Tube.Type..A.orB., data = svl_Pcruc)
#p > 0.05
#No, no significant difference between means of P. crucifer SVL's by tube type.



############################# Bar plot of frog SVLs by tube type and species ##############################3
svl_plot <- data %>%
  select(Tube.Type..A.orB., Number.of.Frogs, SVL..cm., Frog.Species, Tree.Number) %>%
  filter(!is.na(Tube.Type..A.orB.), !is.na(Number.of.Frogs), !is.na(SVL..cm.), !is.na(Frog.Species), !is.na(Tree.Number)) %>%
  mutate(SVL_mm = SVL..cm. * 10)
svl_plot
str(svl_plot)
#In order to change the facet labels and input breaks on the x axis in the plot need to first coerce tube type and tree number to factors and then relabel the tube type levels as Novel and Traditional
svl_plot$Tube.Type..A.orB. <-as.factor(svl_plot$Tube.Type..A.orB.) 
levels(svl_plot$Tube.Type..A.orB.)
levels(svl_plot$Tube.Type..A.orB.) <- c("Novel","Traditional")
svl_plot$Tree.Number <- as.factor(svl_plot$Tree.Number)

#Plot
ggplot(svl_plot, aes(x=Tree.Number, y=SVL_mm, fill=Frog.Species)) +
  geom_bar(stat="identity", position = 'dodge2') +
  facet_grid(~Tube.Type..A.orB.) +
  theme_classic() +
  theme_bw() + 
  scale_fill_manual(values=c("gray55", "salmon2")) +
  scale_x_discrete(name = "Tube Number", breaks=levels(factor(svl_plot$Tree.Number)),
                   labels = c("3", "4", "7","13","14","15","18","19","20","21","24","28","29","30")) +
  scale_y_continuous(name = "Snout Vent Length (mm)") +
  guides(fill=guide_legend(title="Species")) +
  theme(text = element_text(size = 16)) +
  theme(legend.text = element_text(face = "italic"))

#Run same stats without presumable double counts for novel tubes 13 and 14
svl_plot
svl_plot_sub_repeats <- svl_plot[-c(2, 16), ] #Look up rows for these values in the dataframe and put row labels here
svl_plot_sub_repeats

#Mann Whitney U tests for all data points and then by species. Have to run code all together.
wilcox.test(SVL_mm ~ Tube.Type..A.orB., data = svl_plot_sub_repeats)
#p < 0.05 -- significance
svl_plot_sub_repeats <- filter(svl_plot_sub_repeats, Frog.Species == "Hyla versicolor")
wilcox.test(SVL_mm ~ Tube.Type..A.orB., data = svl_plot_sub_repeats)
#p < 0.05 -- significance
svl_plot
svl_plot_sub_repeats <- svl_plot[-c(2, 16), ]
svl_plot_sub_repeats
svl_plot_sub_repeats <- filter(svl_plot_sub_repeats, Frog.Species == "Pseudacris crucifer")
wilcox.test(SVL_mm ~ Tube.Type..A.orB., data = svl_plot_sub_repeats)
#p > 0.05 -- non-significance

############## Plot of frogs in tubes by date ##############################################
#Create new columns with different characteristics for easier manipulation.
data$numeric <- as.numeric(as.character(data$Number.of.Frogs))
data$newdate <- as.character.POSIXt(data$Date)
#Change date column from character string to date format
#Need to consolidate number of frogs found on a given date. This works but I think there is a quicker way to do this.
df_bar <- data.frame(data, stringsAsFactors = TRUE)
df_bar
ndf <- df_bar[, c('numeric', 'newdate','Tube.Type..A.orB.')]
ndf
str(ndf)

#Created a new column with the header 'newdate' for the new date format (not exactly sure why this works)
ndf$newdate <- strptime(as.character(ndf$newdate), "%m/%d/%Y")
str(ndf$newdate)
#changed to desired format
#ndf$newdate <- format(ndf$newdate, "%B-%d-%Y")
ndf$newdate = as.Date(ndf$newdate)
ndf

month <- format(as.Date(ndf$newdate, format="%d/%b/%Y"),"%b")
month

#Aggregate so it sums the number of frogs by tube type for a given survey event
agg_x_date <- aggregate(.~newdate+Tube.Type..A.orB., ndf, FUN=sum, na.rm=FALSE, na.action=NULL)
agg_x_date

#Bar graph of number of frogs found per tube type per date (date displayed by month)
ggplot(agg_x_date, aes(fill=Tube.Type..A.orB., y=numeric, x=newdate)) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme_bw() +
  theme_classic() +
  guides(fill=guide_legend(title="Tube Type")) +
  theme(text = element_text(size = 23)) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("gray33","gray77"), labels = c("Novel", "Traditional")) +
  xlab("Survey date") +
  ylab("Number of frogs")

#No. of frogs found all together in Sept/Oct = 19. Jun/Jul/Aug = 4. 
19/4
#Approximately 5-fold difference of encounters by season.

################## Other stats performed but probably not the correct tests ###################
#Hypothesis is that frogs will use the novel style tube at least as often as the traditional style
#Ho: presence of frogs between tubes is random. Ha: presence of frogs between tubes is not random.
binom.test(14,22,conf.level = 0.9)
#the p-value is greater than 0.05 so we cannot statistically say that the frogs have a preference for one tube type over the
#other.  This means that our novel method is an equally viable survey method.
#We are assuming a random sampling of the frog population of Maple Flats.

#Followed this tutorial to look at data and run a one-way ANOVA http://www.sthda.com/english/wiki/one-way-anova-test-in-r
#Compute summary statistics by tube type using dplyr
group_by(data, Tube.Type..A.orB.) %>%
  summarise(
    count = n(),
    mean = mean(SVL..cm., na.rm = TRUE),
    sd = sd(SVL..cm., na.rm = TRUE)
  ) %>%
  slice_head(n=2)

ggboxplot(data, x = "Tube.Type..A.orB.", y = "SVL..cm.", 
          color = "Tube.Type..A.orB.", palette = c("#00AFBB", "#E7B800"),
          order = c("A", "B"),
          ylab = "SVL", xlab = "Tube Type")
ggline(data, x = "Tube.Type..A.orB.", y = "SVL..cm.", 
       add = c("mean_se", "jitter"), 
       order = c("A", "B"),
       ylab = "SVL", xlab = "Tube Type")

# Compute the analysis of variance
tube.type.aov <- aov(SVL..cm. ~ Tube.Type..A.orB., data = data)
# Summary of the analysis
summary(tube.type.aov)

#Levene test of homogeneity shows that variance is equal between groups...
leveneTest(SVL..cm. ~ Tube.Type..A.orB., data = data)



##########################Power Test #################################
#install.packages("wmwpow")
library(wmwpow)

# We want to calculate the statistical power to compare the distance between mutations on a DNA
# strand in two groups of people. Each group (X and Y) has 10 individuals. We assume that the
# distance between mutations in the first group is exponentially distributed with rate 3. We assume
# that the probability that the distance in the first group is less than the distance in the second
# group (i.e., P(X<Y)) is 0.8. The desired type I error is 0.05.
shiehpow(n = 10, m = 10, p = 0.80, alpha = 0.05, dist = "exp", sides = "two.sided")

#From https://arxiv.org/pdf/1901.04597.pdf "Exact Power of the Rank-Sum Test for a Continuous Variable"
#an 80% or larger true probability that the HIV replication index for any given individual in the placebo group is higher than for any given individual in the treatment group. 
#Assuming p = 0.8 (or equivalently, a true odds of 4 or larger) and using an empirical power approach, thirty individuals (m = n = 15 per group) will provide 85% power to detect a difference between two independent groups (placebo versus treatment).

#For our data, we want to calculate the statistical power to compare the number of frogs using either a novel or traditional tube type as refugia. Novel tubes (n) had 14 total frogs counted total and traditional tubes (m) had 9 frogs counted total. Used exponential distribution because there are a lot of 0's with the tail of the distribution going out towards the higher numbers (extreme right skew). Chose one.sided because we are only intersted in one direction, not both.
shiehpow(n = 14, m = 9, p = 0.8, alpha = 0.05, dist = "exp", sides = "one.sided")

#p = 0.8 an 80% or larger true probability that the number of frogs for any given point in the distribution of the novel group is higher than for any given point in the distribution in the traditional group. 
#23 individuals will provide 80% power to detect a difference between two independent groups (novel v. traditional)




##This was the power test used and put into the IACUC proposal....
#Paired t test power calculation 
power.t.test(n=15,sig.level = 0.05,delta=1,sd=1,type="paired",alternative="two.sided")

#This power calculation demonstrates that if at least 15 tree frogs are found in each type of frog tube then we would be able to say with a power of 95% that tree frogs will use the new style of tube at least the same amount as the traditional style.  If more frogs are found, then the power will increase.  To facilitate this, I plan to place at least 30 tubes of each design type in the same general location (around a common water source).


