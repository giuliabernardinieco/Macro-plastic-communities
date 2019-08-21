
# House keeping
rm(list=ls())

# Working directory
getwd()
setwd("C:/Users/Giulia/Desktop/Imperial MSc/projects/plastic/data/Git")

# Required packages
if (!require(dplyr)) {install.packages('dplyr'); library(dplyr)}
if (!require(lattice)) {install.packages('lattice'); library(lattice)}
if (!require(tibble)) {install.packages('tibble'); library(tibble)}
if (!require(ggplot2)) {install.packages('ggplot2'); library(ggplot2)}
if (!require(gridExtra)) {install.packages('gridExtra'); library(gridExtra)}
if (!require(RColorBrewer)) {install.packages('RColorBrewer'); library(RColorBrewer)}
if (!require(viridis)) {install.packages('viridis'); library(viridis)}
if (!require(stringr)) {install.packages('stringr'); library(stringr)}
if (!require(indicspecies)) {install.packages('indicspecies'); library(indicspecies)}
if (!require(MASS)) {install.packages('MASS'); library(MASS)}
if (!require(lmtest)) {install.packages('lmtest'); library(lmtest)}
if (!require(lme4)) {install.packages('lme4'); library(lme4)}
if (!require(cooccur)) {install.packages('cooccur'); library(cooccur)}
if (!require(vegan)) {install.packages('vegan'); library(vegan)}
if (!require(pvclust)) {install.packages('pvclust'); library(pvclust)}
if (!require(boot)) {install.packages('boot'); library(boot)}
if (!require(EcoSimR)) {install.packages('EcoSimR'); library(EcoSimR)}
if (!require(tidyr)) {install.packages('tidyr'); library(tidyr)}
if (!require(lubridate)) {install.packages('lubridate'); library(lubridate)}
if (!require(FSA)) {install.packages('FSA'); library(FSA)}
if (!require(reshape2)) {install.packages('reshape2'); library(reshape2)}
if (!require(ggrepel)) {install.packages('ggrepel'); library(ggrepel)}
if (!require(Kendall)) {install.packages('Kendall'); library(Kendall)}


#to view citation: citation(package = "package name")


# The data for this project was collected by citizen scientists from 2015 to 2018. 
# The data was provided to me by the organisation, I was working with, as an MS Excel file and I am responsible 
# for the data wrangling, cleaning, and the analysis performed by R. 
# The R script used was composed by me.

#BUT

# because the data is owned by the organisation the data used with this R script on github (dummy-data.csv) 
# is a mock dataset generated through R and the confidential information are chnaged or removed. 
# Hence, the results obtained by running this R script with dummy-data.csv are not representative of the reality.
# the comments about results or statistical analyses are related to the original data and results and they might 
# not be meaninful with the dummy-data


### Cleaning the dataset================================================================================

# Importing the dataset
#data<-read.csv("cleaned-data.csv")
data<-read.csv("dummy-data.csv") # The original dataset was edited via excel from the raw data obtained by the organisation.
str(data)                       # I removed by Excel all the object counted that where not made of plastic, such as tins or wood object
head(data)
tail(data)
# View(data)  # To open the dataset in a new tab

# Removing some reduntant coloumn 
d1<-data[,-c(2:9)]
d<-d1[,-c(4:15)]

# Trying to keep the R environment cleaner as possible removing dataframes I do not need anymore
rm(d1, data)  # Be aware that this command will give you an error "Error in names(frame) <- `*vtmp*` : names() applied to a non-vector" 
              # if and only if the View(data) tab is still open in Rstudio.


# Deleting only-wet wipes observation and quadrats not surveyed
d<-d%>%filter(Other.litter!="No wet wipes; other litter not included in survey")%>%filter(Other.litter!="Only wet wipes surveyed")%>%filter(Other.litter!="No wet wipes present")%>%filter(Surveyed.!="Not surveyed", Surveyed.!="No litter")

# Originally the organisation instead of using 0 used empty cells so now I am changing NA to 0
d[is.na(d)]<-0

# Creating a new variable Plastic Items per m2 that will sum all the plastic items present in a quadrat (1 m2 of surface)
d$PlasticItems_m2<-apply(d[,8:79], 1, sum)


# Adding a variable with only the year to each observation
d$Year<-d$Recorded..Date
d$Year<-substring(d$Year,7,10)

# Plastictems per m2 per years
boxplot(d$PlasticItems_m2~d$Year)

# We decided we don't want to use data collected in 2015 because it was a pilot study and of low quality data collection 
d<-filter(d, Year!=2015)

# Top items in the river
topitems<-as.data.frame(apply(d[8:79], 2, FUN=sum))

# I am going to remove items not present (in the original dataset):
  # Other.polystyrene.food.packaging
  # Commercial.fishing.gear
  # Shopping.trolley
  # Tyre
  # Large.appliance..fridge..washing.machine....
  # Small.appliance..toaster..microwave....
  # Mattress
d$Other.polystyrene.food.packaging<-NULL
d$Commercial.fishing.gear<-NULL
d$Shopping.trolley<-NULL
d$Tyre<-NULL
d$Large.appliance..fridge..washing.machine....<-NULL
d$Small.appliance..toaster..microwave....<-NULL
d$Mattress<-NULL

# Changin colnames, because very long and not practical
colnames(d)[colnames(d)=="Cutlery..plastic."] <- "Cutlery"
colnames(d)[colnames(d)=="Lollipop.stick"] <- "Lol.Stick"
colnames(d)[colnames(d)=="Food.wrapper"] <- "F.wrap"
colnames(d)[colnames(d)=="Take.away.container..plastic."] <- "T.A.cont.PL"
colnames(d)[colnames(d)=="Drink.bottle..with.or.without.lid."] <- "Drink.bottle"
colnames(d)[colnames(d)=="Lid..plastic."] <- "Lid"
colnames(d)[colnames(d)=="Cup..plastic."] <- "Cup.PL"
colnames(d)[colnames(d)=="Plate..plastic."] <- "Plate.PL"
colnames(d)[colnames(d)=="X6.4.pack.rings"] <- "Beer.rings"
colnames(d)[colnames(d)=="Wine.cork..plastic."] <- "Wine.top"
colnames(d)[colnames(d)=="Food.container..plastic."] <- "F.cont.PL"
colnames(d)[colnames(d)=="Ice.pop.container"] <- "Ice.pop.cont"
colnames(d)[colnames(d)=="Other.plastic.food.packaging"] <- "O.Food"
colnames(d)[colnames(d)=="Tetra.pak.coffee.cup"] <- "Cup.TP"
colnames(d)[colnames(d)=="Take.away.container.or.plate..polystyrene."] <- "T.A.cont.PO"
colnames(d)[colnames(d)=="Cup..polystyrene."] <- "Cup.PO"
colnames(d)[colnames(d)=="Thick..durable..shopping.bag"] <- "Shop.bag.Thick"
colnames(d)[colnames(d)=="Thin..single.use..shopping.bag"] <- "Shop.bag.Thin"
colnames(d)[colnames(d)=="Plastic.bottle.container..non.food.related."] <- "Bottle.cont"
colnames(d)[colnames(d)=="Foam.roll.packaging"] <- "Foam.R.pack"
colnames(d)[colnames(d)=="Tampon.applicator"] <- "Tampon.appl"
colnames(d)[colnames(d)=="Sanitary.towel"] <- "San.towel"
colnames(d)[colnames(d)=="Cotton.bud.stick"] <- "C.bud.stick"
colnames(d)[colnames(d)=="Syringe.needle"] <- "S.needle"
colnames(d)[colnames(d)=="Medicine.packaging"] <- "Med.pack"
colnames(d)[colnames(d)=="Gloves..plastic."] <- "Gloves.PL"
colnames(d)[colnames(d)=="Eye.drop.dispenser"] <- "Eye.drop.d"
colnames(d)[colnames(d)=="Other.sewage.related.items"] <- "O.Sewage"
colnames(d)[colnames(d)=="Cigarette.butt"] <- "Cig.butt"
colnames(d)[colnames(d)=="Cigarette.tobacco.packaging"] <- "Cig.smok.pack"
colnames(d)[colnames(d)=="Other.smoking.related"] <- "O.Smoking"
colnames(d)[colnames(d)=="Shoe.or.flip.flop"] <- "Shoes"
colnames(d)[colnames(d)=="Item.of.clothing"] <- "Item.clothing"
colnames(d)[colnames(d)=="Recreational.fishing.gear"] <- "R.fish.gear"
colnames(d)[colnames(d)=="Other.clothing"] <- "O.clothing"
colnames(d)[colnames(d)=="Traffic.cone.sign.barrier"] <- "T.cone"
colnames(d)[colnames(d)=="Carpet...20.cm."] <- "Carpet"
colnames(d)[colnames(d)=="Other.large.item"] <- "O.large.item"
colnames(d)[colnames(d)=="Building.insulation.pieces"] <- "Build.ins"
colnames(d)[colnames(d)=="Window.spacer.tiles"] <- "Window.spacer"
colnames(d)[colnames(d)=="Scaffold.bungee"] <- "S.bungee"
colnames(d)[colnames(d)=="Pipe.lagging"] <- "Pipe.lag"
colnames(d)[colnames(d)=="Tile.cross.spacers"] <- "Tile.spacer"
colnames(d)[colnames(d)=="Balloon...stick"] <- "Balloon.stick"
colnames(d)[colnames(d)=="Gardening..flower.pot..gloves..plant.label."] <- "Garden.relat"
colnames(d)[colnames(d)=="Packaging.peanut"] <- "Peanut.pack"


###First glance at the data =====================================================================================================

# How many quasrats per year?
# How may plastic Items per year?
# Mean of plastic Items per m2 per each year?

Yearsummary<-d%>%group_by(Year)%>%summarise(meanPlasticItems_m2=mean(PlasticItems_m2), TotPlasticItems=sum(PlasticItems_m2))
Yearsummary$totNQuadrats<-table(d$Year)

# Mean, SD and Range of items/m2
mean(d$PlasticItems_m2)

sd(d$PlasticItems_m2)

range(d$PlasticItems_m2)

# Number of overall quadrats in each site 
(table(d$Site.Name)) 

# There must be a typo Millennium drawdock and Millennium Drawdock
# I will correct it now
d$Site.Name [d$Site.Name=="Millennium drawdock"]<- "Millennium Drawdock"

# Dataframe of how many quadrats for each site
quadsite<-as.data.frame(table(droplevels(d$Site.Name)))
quadsite 
#I have a total of 12 sites, some of these places have very few quadrats, 
        #like Millwall and  Bermondsey, both sampled just one day, we will keep this in mind

# Now I am creating two dataframe of summary per year and sites, 
# one with TOT items/m2
Summarysum<-aggregate(d[c(75)], by=list(Category=d$Site.Name,d$Year), FUN=sum)%>% rename(Site.Name=Category, Year=Group.2)
# one with MEAN items/m2 
Summarymean<-aggregate(d[c(75)], by=list(Category=d$Site.Name,d$Year), FUN=mean)%>% rename(Site.Name=Category, Year=Group.2)
# Calculating the SD 
Summarysd<-aggregate(d[c(75)], by=list(Category=d$Site.Name,d$Year), FUN=sd)%>% rename(Site.Name=Category, Year=Group.2, SD=PlasticItems_m2)
Summarymean<-cbind.data.frame(Summarymean,Summarysd[3])

#Visualizing these information

# First I am arranging the sites from the one more upstream to the one more downstrem for both tables, Summerysum and Summarymean
Summarysum$Site.Name.dist <- factor(Summarysum$Site.Name, 
                                    levels=c("","Hammersmith Bridge", "Queen Caroline", 
                                             "Crabtree Wharf", "Church Battersea", "Battersea Bridge", 
                                             "Vauxhall Bridge", "Queenhithe", "Bermondsey", "Millwall Drawdock", 
                                             "Cutty Sark", "Isle of Dogs","Newcastle Drawdock", "Millennium Drawdock", 
                                             "Mast Quay Dock 1", "Erith Marshes")) 

Summarymean$Site.Name.dist <- factor(Summarymean$Site.Name, 
                                     levels=c("","Hammersmith Bridge", "Queen Caroline", 
                                              "Crabtree Wharf", "Church Battersea", "Battersea Bridge", 
                                              "Vauxhall Bridge", "Queenhithe", "Bermondsey", "Millwall Drawdock", 
                                              "Cutty Sark", "Isle of Dogs","Newcastle Drawdock", "Millennium Drawdock", 
                                              "Mast Quay Dock 1", "Erith Marshes")) 


# Now the barplots

# TOT plastic in each location
bar1<-ggplot(data=Summarysum, aes(x=Site.Name.dist, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+theme_minimal()+scale_fill_viridis(discrete=TRUE)+
  ggtitle("")+xlab("Site") + ylab("TOT Plastic Items") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

bar1

# Mean plastic items/m2 in each location
bar2<-ggplot(data=Summarymean, aes(x=Site.Name.dist, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+theme_minimal()+scale_fill_viridis(discrete=TRUE)+
  ggtitle("")+xlab("Site") + ylab("Mean Plastic Items per quadrat")+ scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))

bar2

# in these graphs the sites are ordered from the more distant to the river's mouth to the one closer.

# COMPOSITION by year
# Now I am looking at the composition per each year (in terms of relative aboundance (item i/tot items *100))

# Dataframe of the composition with relative abundances
yearcomp<-aggregate(d[c(8:72, 75)], by=list(Category=d$Year), FUN=sum)%>% rename(Year=Category)
rownames(yearcomp) <- yearcomp[,1]
RELAByearcomp<-(yearcomp[2:66]/yearcomp$PlasticItems_m2)*100
RELAByearcomp<-t(RELAByearcomp)
RELAByearcomp<-as.data.frame(RELAByearcomp)
RELAByearcomp<-rownames_to_column(RELAByearcomp, var= "Item")

# Table for the graph with relative abundances 
ggcompyear <- RELAByearcomp %>% 
  tidyr::gather(key = "Year", value = "Abudance", -Item)

# I am goign to exclude from the graph the items that have a relative abundance less than 0.1 
sub_ggcompyear<- subset(ggcompyear, Abudance> 0.1)

# Graphs
byear<-ggplot(data=sub_ggcompyear, aes(x=reorder(Item,-Abudance), y=Abudance, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+theme_minimal()+scale_fill_brewer(palette="Dark2")+
  ggtitle("")+xlab("Item") + ylab("Relative abundance")+theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))+
  facet_grid(Year ~ .)

byear

# The big difference in wetwipes in 2018 might be due to the different sampling effort in sinking sites. 
# I am going to exclude them and see again

dfloating<-d%>%filter(Survey.Type=="Floating hotspot")

# now I am doing the same I did before to see the composition, creating the table and plotting it
yearcompfloat<-aggregate(dfloating[c(8:72, 75)], by=list(Category=dfloating$Year), FUN=sum)%>% rename(Year=Category)
rownames(yearcompfloat) <- yearcompfloat[,1]
RELAByearcompfloat<-(yearcompfloat[2:66]/yearcompfloat$PlasticItems_m2)*100
RELAByearcompfloat<-t(RELAByearcompfloat)
RELAByearcompfloat<-as.data.frame(RELAByearcompfloat)
RELAByearcompfloat<-rownames_to_column(RELAByearcompfloat, var= "Item")

# Table for the graph with relative abundances 
ggcompyearfloat <- RELAByearcompfloat %>% 
  tidyr::gather(key = "Year", value = "Abudance", -Item)

# I am goign to exclude from the graph the items that have a relative abundance less than 0.1
sub_ggcompyearfloat<- subset(ggcompyearfloat, Abudance> 0.1)

# Graphs
byearfloat<-ggplot(data=sub_ggcompyearfloat, aes(x=reorder(Item,-Abudance), y=Abudance, fill=Year)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+theme_minimal()+scale_fill_brewer(palette="Dark2")+
  ggtitle("Composition floating sites")+xlab("Item") + ylab("Relative abundance")+theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))+
  facet_grid(Year ~ .)

byearfloat


###A different data frame for each site========================================================================

# Now we look in detail at each site
# They are ordered in the script from the most upstream to the one most downstream

#Hammersmith Bridge
Hammersmith<-d%>%filter(Site.Name=="Hammersmith Bridge") 
#this site has a total of 45 quadrats, 
table(droplevels(Hammersmith$Recorded..Date))
#3 days, but one day in 2017 has only one quadrat, this is an error, it was actually a quadrat where only wet wipes were sampled.
#I am going to remove this
Hammersmith<-Hammersmith%>%filter( Recorded..Date !="01/04/2017")
#eliminating this also from the full data d
d<-d%>%filter( Recorded..Date !="01/04/2017")
#plot
ggplot(data=Hammersmith, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Hammersmith Bridge")

#mean, range and sd
mean(Hammersmith$PlasticItems_m2[Hammersmith$Year=="2018"])
sd(Hammersmith$PlasticItems_m2[Hammersmith$Year=="2018"])
range(Hammersmith$PlasticItems_m2[Hammersmith$Year=="2018"])

#Queen Caroline
QueenCaroline<-d%>%filter(Site.Name=="Queen Caroline") 
#this site has 320 quadrats 
table(droplevels(QueenCaroline$Recorded..Date))
#30 diferent days of sampling across three years
#10 in the 2016, 11 in the 2017, 12 in the 2018.

ggplot(data=QueenCaroline, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Queen Caroline")

#mean, range and sd
mean(QueenCaroline$PlasticItems_m2[QueenCaroline$Year=="2018"])
sd(QueenCaroline$PlasticItems_m2[QueenCaroline$Year=="2018"])
range(QueenCaroline$PlasticItems_m2[QueenCaroline$Year=="2018"])


#Crabtree Wharf
CrabtreeWharf<-d%>%filter(Site.Name=="Crabtree Wharf")
#this site has 114 quadrats
table(droplevels(CrabtreeWharf$Recorded..Date))
#14 different days of sampling across three years

ggplot(data=CrabtreeWharf, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Crabtree Wharf")

#mean, range and sd
mean(CrabtreeWharf$PlasticItems_m2[CrabtreeWharf$Year=="2016"])
sd(CrabtreeWharf$PlasticItems_m2[CrabtreeWharf$Year=="2016"])
range(CrabtreeWharf$PlasticItems_m2[CrabtreeWharf$Year=="2016"])


#Church Battersea
ChurchBattersea<-d%>%filter(Site.Name=="Church Battersea") 
#this site has 46 quadrats
table(droplevels(ChurchBattersea$Recorded..Date))
#8 days but no data from 2016

ggplot(data=ChurchBattersea, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Church Battersea")

#Battersea Bridge
Battersea<-d%>%filter(Site.Name=="Battersea Bridge")
#this site has 40 quadrats
table(droplevels(Battersea$Recorded..Date))
#only one day in 2016

ggplot(data=Battersea, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Battersea Bridge")


#Vauxhall Bridge
Vauxhall<-d%>%filter(Site.Name=="Vauxhall Bridge") 
#this site has 26 quadrats
table(droplevels(Vauxhall$Recorded..Date))
#only one day in 2016

ggplot(data=Vauxhall, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Vauxhall Bridge")

#Queenhithe
Queenhithe<-d%>%filter(Site.Name=="Queenhithe")  
#this site has 39 quadrats
table(droplevels(Queenhithe$Recorded..Date))
#6 days no data from 2018

ggplot(data=Queenhithe, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Queenhithe")

#Bermondsey
Bermondsey<-d%>%filter(Site.Name=="Bermondsey")
#this site has 6 quadrats
table(droplevels(Bermondsey$Recorded..Date))
#only one day in 2016

ggplot(data=Bermondsey, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Bermondsey")

#Millwall Drawdock
Millwall<-d%>%filter(Site.Name=="Millwall Drawdock")
#this site has 4 quadrats
table(droplevels(Millwall$Recorded..Date))
#only one day in 2017

ggplot(data=Millwall, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Millwall Drawdock")

#Cutty Sark
CuttySark<-d%>%filter(Site.Name=="Cutty Sark")
#this site has 169 quadrats
table(droplevels(CuttySark$Recorded..Date))
#25 days of data
table(CuttySark$Year)

ggplot(data=CuttySark, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Cutty Sark")

#Newcastle Drawdock
Newcastle<-d%>%filter(Site.Name=="Newcastle Drawdock")
#this site has 18 quadrats
table(droplevels(Newcastle$Recorded..Date))
# 3 day of data from 2016

ggplot(data=Newcastle, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Newcastle")

#Millennium Drawdock
Millennium<-d%>%filter(Site.Name=="Millennium Drawdock") 
#this site has 92 quadrats
table(droplevels(Millennium$Recorded..Date))
#only one day of data from 2016

ggplot(data=Millennium, aes(x=Year, y=PlasticItems_m2, fill=Year)) +
  geom_bar(stat="identity")+theme_minimal()+scale_fill_brewer(palette="Dark2")+ggtitle("Millennium")

#Millwall Drawdock and Bermondsey has respectevely 6 and 4 quadrats and where sampled just once each, I can not consider them rapresentatiive of the site, so I want to try to exclude them

d<-d%>%filter(Site.Name!="Bermondsey")%>%filter(Site.Name!="Millwall Drawdock")

#trying to keep the encionment tidy
rm(Millennium, Newcastle, CuttySark, Millwall, Bermondsey, Queenhithe, Vauxhall, Battersea, ChurchBattersea, CrabtreeWharf, QueenCaroline, Hammersmith)

###Co-occurance of items==========================================================================================

## community data matrix 
community<-aggregate(d[8:72], by=list(Category=d$Site.Name), FUN=sum)
sitestable<-as.data.frame(table(droplevels(community$Category)))
rownames(community) <- community[,1]
community$Category<-NULL
community

# Co-occurance with the pacakge coocur
# I am going to use the package cooccur to look for co-occurance of species
# I need to use a dataframe with site as coloumn and species as rows and presence absence data
TransposeCom<- data.frame(t(community))
TransposeCom<-ifelse (TransposeCom==0, print(0) , print(1) )

# now I am testing the co-occurrence
cooccur.items <- cooccur(mat = TransposeCom, type = "spp_site", thresh = TRUE, spp_names = TRUE)
summary(cooccur.items)
print(cooccur.items)
prob.table(cooccur.items)

#now a plot to visualize
dev.off() 
plot(cooccur.items)

#I want to chnage colors of the graphs, but it is a funcion of the package so I have found the 
# code of the function on https://github.com/cran/cooccur/blob/master/R/plot.cooccur.R and changed 
# the colors using my palette and hiding the title

plot.Items<-
  function(x, ...){
    
    ##
    allargs <- match.call(expand.dots = TRUE)
    plotrand <- allargs$plotrand
    plotrand <- ifelse(test = is.null(plotrand),yes = FALSE,no = plotrand)
    randsummary<- allargs$randsummary
    randsummary <- ifelse(test = is.null(randsummary),yes = FALSE,no = randsummary)
    
    ##
    
    dim <- x$species
    comat_pos <- comat_neg <- matrix(nrow=dim,ncol=dim)
    
    co_tab <- x$result
    for (i in 1:nrow(co_tab)){
      comat_pos[co_tab[i,"sp1"],co_tab[i,"sp2"]] <- co_tab[i,"p_gt"]
      comat_pos[co_tab[i,"sp2"],co_tab[i,"sp1"]] <- co_tab[i,"p_gt"]
      
      row.names(comat_pos[co_tab[i,"sp2"],co_tab[i,"sp1"]])
      
    }
    for (i in 1:nrow(co_tab)){
      comat_neg[co_tab[i,"sp1"],co_tab[i,"sp2"]] <- co_tab[i,"p_lt"]
      comat_neg[co_tab[i,"sp2"],co_tab[i,"sp1"]] <- co_tab[i,"p_lt"]
    }
    comat <- ifelse(comat_pos>=0.05,0,1) + ifelse(comat_neg>=0.05,0,-1)
    colnames(comat) <- 1:dim
    row.names(comat) <- 1:dim
    
    if ("spp_key" %in% names(x)){
      
      sp1_name <- merge(x=data.frame(order=1:length(colnames(comat)),sp1=colnames(comat)),y=x$spp_key,by.x="sp1",by.y="num",all.x=T)
      sp2_name <- merge(x=data.frame(order=1:length(row.names(comat)),sp2=row.names(comat)),y=x$spp_key,by.x="sp2",by.y="num",all.x=T)
      
      colnames(comat) <- sp1_name[with(sp1_name,order(order)),"spp"]  
      row.names(comat) <- sp2_name[with(sp2_name,order(order)),"spp"]
      
    }  
    
    #ind <- apply(comat, 1, function(x) all(is.na(x)))
    #comat <- comat[!ind,]
    #ind <- apply(comat, 2, function(x) all(is.na(x)))
    #comat <- comat[,!ind]
    
    comat[is.na(comat)] <- 0
    
    origN <- nrow(comat)
    
    # SECTION TO REMOVE SPECIES INTERACTION WITH NO OTHERS
    
    #rmrandomspp <- function(orimat,plotrand = FALSE,randsummary = FALSE){
    if(plotrand == FALSE){
      ind <- apply(comat, 1, function(x) all(x==0))
      comat <- comat[!ind,]    
      ind <- apply(comat, 2, function(x) all(x==0))
      comat <- comat[,!ind]
      #ind <- apply(orimat, 1, function(x) all(x==0))
      #orimat <- orimat[!ind,]    
      #ind <- apply(orimat, 2, function(x) all(x==0))
      #orimat <- orimat[,!ind]
    }
    #return(orimat)
    #}
    
    #comat <- rmrandomspp(orimat = comat, dots)
    ####################################################### 
    
    postN <- nrow(comat)
    
    
    comat <- comat[order(rowSums(comat)),]
    comat <- comat[,order(colSums(comat))]
    
    #comat <- rmrandomspp(orimat = comat, ...)
    
    #ind <- apply(comat, 1, function(x) all(x==0))
    #comat <- comat[!ind,]
    #ind <- apply(comat, 2, function(x) all(x==0))
    #comat <- comat[,!ind]
    
    ind <- apply(comat, 1, function(x) all(x==0))
    comat <- comat[names(sort(ind)),]
    ind <- apply(comat, 2, function(x) all(x==0))
    comat <- comat[,names(sort(ind))]
    
    #comat
    data.m = melt(comat)
    colnames(data.m) <- c("X1","X2","value")
    data.m$X1 <- as.character(data.m$X1)
    data.m$X2 <- as.character(data.m$X2)
    
    meas <- as.character(unique(data.m$X2))
    
    dfids <- subset(data.m, X1 == X2)
    
    X1 <- data.m$X1
    X2 <- data.m$X2
    
    df.lower = subset(data.m[lower.tri(comat),],X1 != X2)
    
    ##### testing the rand summary
    if(randsummary == FALSE){  
    }else{
      dim <- nrow(comat)
      ext.dim <- round(dim*0.2,digits = 0)
      if(ext.dim<0){ext.dim<-1}
      placehold <- paste("ext_", rep(c(1:ext.dim),each = dim), sep="")
      
      randcol.df <- data.frame(
        X1 = placehold,
        X2 = rep(meas,times = ext.dim),
        value = rep(x = c(-2), times = dim*ext.dim))
      
      df.lower <- rbind(df.lower,randcol.df)
      meas <- c(meas,unique(placehold))
    }
    
    
    
    
    #####
    
    X1 <- df.lower$X1
    X2 <- df.lower$X2
    value <- df.lower$value
    
    
    
    ####
    if(randsummary == FALSE){  
      p <- ggplot(df.lower, aes(X1, X2)) + geom_tile(aes(fill = factor(value,levels=c(-1,0,1))), colour ="white") 
      p <- p + scale_fill_manual(values = c("#FDE725FF","light gray","#440154FF"), name = "", labels = c("negative","random","positive"),drop=FALSE) + 
        theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),plot.title = element_text(vjust=-4,size=20, face="bold"),panel.background = element_rect(fill='white', colour='white'),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = c(0.9, 0.5),legend.text=element_text(size=18)) + 
        #ggtitle("Items Co-occurrence Matrix") + 
        xlab("") + ylab("") + 
        scale_x_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) + 
        scale_y_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) 
      p <- p + geom_text(data=dfids,aes(label=X1),hjust=1,vjust=0,angle = -22.5)#, color="dark gray")
        
        
        
    
      
    }else{
      
      p <- ggplot(df.lower, aes(X1, X2)) + geom_tile(aes(fill = factor(value,levels=c(-1,0,1,-2))), colour ="white") 
      p <- p + scale_fill_manual(values = c("#440154FF","light gray","#FDE725FF","#666666"), name = "", labels = c("negative","random","positive","random"),drop=FALSE) + 
        theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),plot.title = element_text(vjust=-4,size=20, face="bold"),panel.background = element_rect(fill='white', colour='white'),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = c(0.9, 0.5),legend.text=element_text(size=18)) + 
        #ggtitle("Items Co-occurrence Matrix") + 
        xlab("") + ylab("") + 
        scale_x_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) + 
        scale_y_discrete(limits=meas, expand = c(0.3, 0),drop=FALSE) 
      p <- p + geom_text(data=dfids,aes(label=X1),hjust=1,vjust=0,angle = -22.5)#, color="dark gray")
      
      dim <- nrow(comat)
      ext_x <- dim + 0.5 #(ext.dim/2)
      ext_y <- dim + 1
      nrem <- origN - postN
      randtext <- paste(nrem, " completely\nrandom species")
      ext_dat <- data.frame(ext_x=ext_x,ext_y=ext_y,randtext=randtext)
      
      p <- p + geom_text(data=ext_dat,aes(x = ext_x,y = ext_y,label=randtext),hjust=0,vjust=0, color="dark gray")
    }
    ####
    
    p
    
  }

dev.off()

plot.Items(cooccur.items) 

# with the dummy dataset this is not working because I have neither positive nor negative interactions.

# visualize the interaction of one species in detail
pair(mod = cooccur.items, " C.bud.stick")
# now we looked at the percentage of each species total pairings that were classified as positive, negative, and random (columns with prefix "num" are counts)
pair.attributes(cooccur.items)
pair.profile(cooccur.items) #Boxplot showing the percent of total pairings for each species that are positive,
                            #negative, or random. Species are ordered by increasing number of total associations

# now I want to compare expected vs observed
cooccur(mat = TransposeCom, type = "spp_site", thresh = FALSE,
        spp_names = TRUE, only_effects = TRUE, eff_standard = TRUE,
        eff_matrix = TRUE)

obs.v.exp(cooccur.items)# there are more positive relationship that expected.


#In general from this section about co-occurance of items I would say that there are a lots of items cooccuring together. 
#This is important because in this ways we know that there are certain Items alsways present with others. 
#But, this might be also due to the fact that our sites are very similars in terms of species composition.

##looking at the co-occurance of some items
pair(mod = cooccur.items, "Stirrer") 
pair(mod = cooccur.items, "Lol.Stick")
pair(mod = cooccur.items, "T.A.cont.PO")
pair(mod = cooccur.items, "Cup.PO")
pair(mod = cooccur.items, "Bottle.cont")
pair(mod = cooccur.items, "Lighter")
pair(mod = cooccur.items, "Build.ins")
pair(mod = cooccur.items, "C.bud.stick")
pair(mod = cooccur.items, "Drink.bottle")
pair(mod = cooccur.items, "Bottle.top")
pair(mod = cooccur.items, "Straw")
pair(mod = cooccur.items, "Cup.PL")
pair(mod = cooccur.items, "Cig.smok.pack") 
pair(mod = cooccur.items, "T.A.cont.PL")
pair(mod = cooccur.items, "Pen")
pair(mod = cooccur.items, "Tile.spacer")
pair(mod = cooccur.items, "Cable.tie")
pair(mod = cooccur.items, "Cutlery")
pair(mod = cooccur.items, "Shoes") 
pair(mod = cooccur.items, "Lid")
pair(mod = cooccur.items, "S.needle")
pair(mod = cooccur.items, "String")
pair(mod = cooccur.items, "Wine.top")
pair(mod = cooccur.items, "F.cont.PL")
pair(mod = cooccur.items, "O.Food") 
pair(mod = cooccur.items, "Drink.carton")
pair(mod = cooccur.items, "Eye.drop.d")
pair(mod = cooccur.items, "Window.spacer")
pair(mod = cooccur.items, "Ball")
pair(mod = cooccur.items, "Nerf.bullet")
pair(mod = cooccur.items, "Peanut.pack")

###Clusters: Test the differences in items composition across sites and years =================================================================================================

#non-hierarchical cluster analysis:
comkm <- kmeans(community, centers=3)
groupskm <- comkm$cluster
groupskm

# I want to see if they grouped for similarities in comunity following the diferent nature of sinking hotspot and floating hotspot
# I am going to group them following the type of site. Floating (2), Sinking (1) 
group<-c(1, 2, 2, 2, 1, 2, 1, 2, 2, 1)
sitestable$type<-group
sitestable$Freq<-NULL
sitestable$Site.type<- ifelse(sitestable$type==1, print("Sinking"), print("Floating"))
sitestable$type<-NULL


# Similarity and significant clusters through a Bray-Curtis matrix for sites

# The first thing to do is to create a dissimilarity matrix with Bray-Curtis index (if 1, they don't share any species)
# First I am going to use the matrix community that has aboundaces for three years and each rows is a site
community # community matrix with aboundance and every row is a location
sitestable
# For community compute the distances among the rows (sites):
bc<-vegdist(community, method="bray", binary=FALSE) #binary=FALSE means you look at the number of individuals.  TRUE would give the result for presence-absence (Sorenson's index)
bc

# Many analyses are sensitive to absolute abundance in a sample and can skew results, one solution for this is 
# to take absolute abundance data and convert it to relative abundance estimates

# Moreover, the prblem about using Community  is that locations has heterogeneous number of quadrats, some where sampled much more than others, 
# to overcome this I am going to use relative percentage instead.
RELABcommunity<-aggregate(d[c(8:72,75)], by=list(Category=d$Site.Name), FUN=sum)
rownames(RELABcommunity) <- RELABcommunity[,1]
RELABcommunity<-(RELABcommunity[2:66]/RELABcommunity$PlasticItems_m2)*100

RELABcommunity
# Bray-Curtis dissimilarity matrix
bc2<-vegdist(RELABcommunity, method="bray", binary=FALSE) 
bc2 #Bray-Curtis dissimilarity matrix

bc3<-as.data.frame(as.matrix(bc2))
# now I want to create a hierarchical clustering where the most similar are close together 
clu<- hclust(bc2, method = "average")
plot(clu, main="Dendrogram of sites similarities")

# now I want to test the significance of the clusters, using the pvclust function I create the cluster direcly from the community matrix 
# instead of the BC dissimilarity matrix. It use bootstraping and the result might differ
clu2<-pvclust(t(RELABcommunity), method.hclust ="average" )
plot(clu2)
pvrect(clu2, alpha = 0.95)
# the following command is to view significant clusters
pvpick(clu2)


###NMDS and PERMANOVA

#with vegan NMDS
bc2.nmds<-metaMDS(RELABcommunity, distance = "bray", k=2) 

#checking the stress
bc2.nmds$stress #very good value with the real dataset

# creating dataframe to be used to see the different groups and test with bootstraping
NMDSdata<-rownames_to_column(RELABcommunity, var = "Var1")
NMDSdata<-merge(NMDSdata,sitestable, by="Var1")

# Bootstrapping and testing for an actual difference between the groups with adonis (vegan package)
# adonis allows to perform permutational multivariate analysis of variance using distance matrices 
fit <- adonis(RELABcommunity ~ Site.type, data=NMDSdata, permutations=1000, method="bray")
fit #our groups in the real dataset are very different from eachother, it is significant

## Adonis works by first finding the centroids for each group and then calculates the squared deviations 
# of each of site to that centroid. Then significance tests are performed using F-tests based on sequential 
# sums of squares from permutations of the raw data.

### ggplot of the NDMS (https://chrischizinski.github.io/rstats/vegan-ggplot2/)

data.scores <- as.data.frame(scores(bc2.nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- sitestable$Site.type  #  add the grp variable created earlier
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(bc2.nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

# plot hull around each group
grp.a <- data.scores[data.scores$grp == "Sinking", ][chull(data.scores[data.scores$grp == 
                                                                         "Sinking", c("NMDS1", "NMDS2")]), ]  # hull values for grp Sinking
grp.b <- data.scores[data.scores$grp == "Floating", ][chull(data.scores[data.scores$grp == 
                                                                          "Floating", c("NMDS1", "NMDS2")]), ]  # hull values for grp Floating

hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b
hull.data

# plotting
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp), alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, check_overlap = T) + # add the species labels, this remove all the species that overlap
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=4) + # add the point markers
  scale_colour_manual(values=c("Sinking" = "#440154FF", "Floating" = "#FDE725FF" )) +
  scale_fill_manual(values=c("Sinking" = "#440154FF", "Floating" = "#FDE725FF"))+
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  ggtitle("Plastic Items community ordination")
 
#two different ways of dealing with labels overlapping:

#geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, check_overlap = T) +  
#geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), alpha=0.5) + 


# Sites: Shannon Diversity Index and anova for sites====


# I am starting creating a community dataframe to make comparison using replicated samples. 
# I am following instructions as illustrated in the book "Community Ecology:Analytical Methods Using R and Excel" by Mark Gardener , Pelagic Publishing 2014, pp(252-259)
comm<-aggregate(d[8:72], by=list(Category=d$ID), FUN=sum)
names(comm)[1]<-paste("ID")
rownames(comm)=comm$ID
comm$ID<-NULL


# I am subsetting and creating a dataframe to not lose variables as date and site location in relation to ID
sub<-d[1:5]
sub$occurrences <- 1
sub<-aggregate(occurrences ~ Recorded..Date + Site.Name+ID, sub, sum)
sub$occurrences <- NULL
rownames(sub)=sub$ID
sub$ID<-NULL

# Now I am creating a matrix with the shannon index for every sample.
H=diversity(comm, index="shannon")
dat=data.frame(H=H, Site.Name=sub$Site.Name, Recorded..Date=sub$Recorded..Date)
dat

#I have to escludes sites with less than 2 repeated measure because I cannot compare them with ANOVA
#these are:
#Battersea Bridge
#Bermondsey
#Millwall Drawdock
#Newcastle Drawdock
#Vauxhall Bridge
datred<-dat%>%filter(Site.Name!="Battersea Bridge")%>%filter(Site.Name!="Vauxhall Bridge")%>%
  filter(Site.Name!="Millwall Drawdock")


# now I want to test each of them if they are normally distributed
tapply(datred$H[-c(11,12)], INDEX = datred$Site.Name[-c(11,12)], FUN=shapiro.test)
shapiro.test(datred$H)
#they are not, for this reason I am using Kruskal-Wallis instead of ANOVA

# Kruskal-Wallis test
datred$Site.Name<-as.factor(datred$Site.Name)
KWH=kruskal.test(datred$H,datred$Site.Name)
KWH 

# post-hoc test using Dunn's test to see significant pairs
dunnTest(H~Site.Name, data=datred, method="bh") 
# the only site that differ with significance (p<0.05) from all the other is Hammersmith, 
# the pair Queen Caroline-Cutty Sark is also different


# boxplot to visualise the situation, take into accout that heigher values of H means heigher diversity

# creating  a new variable to hightlight differences
dat$type=ifelse(dat$Site.Name==c("Newcastle Drawdock"),"Sinking","Floating")
dat$type[dat$Site.Name == "Hammersmith Bridge"] <- "Sinking"
dat$type[dat$Site.Name == "Battersea Bridge"] <- "Sinking"
dat$type[dat$Site.Name == "Vauxhall Bridge"] <- "Sinking"


Hplot<-ggplot(dat, aes(x=Site.Name, y=H, fill=type)) + 
  geom_boxplot()+theme_minimal()+ labs(y="H", x = "Site")+ scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_manual(breaks = c("Sinking","Floating"), 
                    values=c("#FDE725FF","#440154FF"))+theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))+theme(legend.position = "none")+
  ggtitle("(a) Items Diversity")

# mean, SD and range
tH1<-na.omit(as.data.frame(tapply(dat$H, dat$Site.Name, mean)))
tH2<-na.omit(as.data.frame(tapply(dat$H, dat$Site.Name, sd)))
tH3<-as.data.frame(tapply(dat$H, dat$Site.Name, range))
tH3<-rownames_to_column(tH3)
tH3<-tH3%>%rename(Row.names=rowname)

#merging the tables
tH<-merge(tH1, tH2, by="row.names", all=F)
tH<-merge(tH, tH3, by="Row.names", all=F)


# Sites: Richness and anova per sites=====
# Shannon diversity index can be very criticise, for this reason I will do a similar analysis using the richness.
# calculating richness for each transects

Richness<-as.data.frame(apply(comm>0,1,sum))
colnames(Richness)[colnames(Richness)=="apply(comm > 0, 1, sum)"] <- "Richness"

#now I adding the impormation needed to group and perform the test as the location
dat2=data.frame(Richness=Richness, Site.Name=sub$Site.Name, Recorded..Date=sub$Recorded..Date)
dat2

# I have to escludes sites with less than 2 repeated measure because I cannot compare them with an anova,
# they don't have variance
#these are:
#Battersea Bridge
#Bermondsey
#Millwall Drawdock
#Newcastle Drawdock
#Vauxhall Bridge
dat2red<-dat2%>%subset(Site.Name!="Battersea Bridge")%>%
  subset(Site.Name!="Millwall Drawdock")%>%
  subset(Site.Name!="Vauxhall Bridge")

#shapiro test if the richness is normal distributed in each site
tapply(dat2red$Richness[-c(11,12)], INDEX = dat2red$Site.Name[-c(11,12)], FUN=shapiro.test) 
shapiro.test(dat2red$Richness)# it is better to use KW, that is non parametric

#Kruskal-Wallis test
dat2red$Site.Name<-as.factor(dat2red$Site.Name)
KWRich=kruskal.test(dat2red$Richness,dat2red$Site.Name)
KWRich

#post-hoc test using Dunn's test
dunnTest(Richness~Site.Name, data=dat2red, method="bh") 
#the only site that differ with significance is Hammersmith, Queen Caroline-Cutty Sark differ also

#mean and Sd and range
tS1<-na.omit(as.data.frame(tapply(dat2$Richness, dat2$Site.Name, mean)))
tS2<-na.omit(as.data.frame(tapply(dat2$Richness, dat2$Site.Name, sd)))
tS3<-as.data.frame(tapply(dat2$Richness, dat2$Site.Name, range))
tS3<-rownames_to_column(tS3)
tS3<-tS3%>%rename(Row.names=rowname)

#merging the tables
tS<-merge(tS1, tS2, by="row.names", all=F)
tS<-merge(tS, tS3, by="Row.names", all=F)

#trying to keep the environment tidy
rm(tS1, tS2, tS3, tH1, tH2, tH3)

#boxplot to visualise the situation
#creating a new variable to store the information abot sinking and floating
dat2$type=ifelse(dat2$Site.Name==c("Newcastle Drawdock"),"Sinking","Floating")
dat2$type[dat2$Site.Name == "Hammersmith Bridge"] <- "Sinking"
dat2$type[dat2$Site.Name == "Battersea Bridge"] <- "Sinking"
dat2$type[dat2$Site.Name == "Vauxhall Bridge"] <- "Sinking"

Splot<-ggplot(dat2, aes(x=Site.Name, y=Richness, fill=type)) + 
  geom_boxplot()+theme_minimal()+ labs(y="S", x = "Site")+ scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_manual(breaks = c("Sinking","Floating"), 
                    values=c("#FDE725FF","#440154FF"))+theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))+
  ggtitle("(b) Items Richness")

#saving the legend of this graph to be used in the combined one
get_legend<-function(Splot){
  tmp <- ggplot_gtable(ggplot_build(Splot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  }
legend <- get_legend(Splot)
# removing the legend from Splot
Splot <- Splot + theme(legend.position="none")

# creating the multiple graph
grid.arrange(Hplot,Splot,legend,widths=c(2.3, 2.3, 1), layout_matrix=rbind(c(1,1,3), c(2,2,3)))

# boxplot for sinking and floating in general
Stype<-ggplot(dat2, aes(x=type, y=Richness, fill=type)) + 
  geom_boxplot()+theme_minimal()+ labs(y="S", x = "Site")+ scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_manual(breaks = c("Sinking","Floating"), 
                    values=c("#FDE725FF","#440154FF"))+ggtitle("Items' Richness")+theme(legend.position="none")

Htype<-ggplot(dat, aes(x=type, y=H, fill=type)) + 
  geom_boxplot()+theme_minimal()+ labs(y="S", x = "Site")+ scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_fill_manual(breaks = c("Sinking","Floating"), 
                    values=c("#FDE725FF","#440154FF"))+ggtitle("Items' Diversity")+theme(legend.position="none")

grid.arrange(Htype,Stype,legend,widths=c(2.3, 2.3, 1))


#I am going to look at difference in richness by plotting the composition in a barchar

bsitedf<-as.data.frame(t(RELABcommunity))
bsitedf<-rownames_to_column(bsitedf, var= "Item")
bsitedf <- bsitedf %>%tidyr::gather(key = "Site.Name", value = "Abudance", -Item)
bsitedf$type=ifelse(bsitedf$Site.Name==c("Newcastle Drawdock"),"Sinking","Floating")
bsitedf$type[bsitedf$Site.Name == "Hammersmith Bridge"] <- "Sinking"
bsitedf$type[bsitedf$Site.Name == "Battersea Bridge"] <- "Sinking"
bsitedf$type[bsitedf$Site.Name == "Vauxhall Bridge"] <- "Sinking"


#barchart
bsitedf$Site.Name.dist<-factor(bsitedf$Site.Name, 
       levels=c("Queen Caroline", "Hammersmith Bridge", 
                "Crabtree Wharf", "Battersea Bridge", 
                "Church Battersea", "Vauxhall Bridge",
                "Queenhithe", "Newcastle Drawdock", 
                "Cutty Sark",  
                "Millennium Drawdock"))


bsite<-  ggplot(data=bsitedf, aes(x=reorder(Item,-Abudance), y=Abudance, fill=type)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single"))+theme_minimal()+
  ggtitle("Plastic Items Composition")+xlab("Item") + ylab("Abundance")+theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))+
  facet_wrap(~Site.Name.dist, ncol=2)+ 
  scale_fill_manual(breaks = c("Sinking","Floating"), values=c("#21908CFF","#440154FF"))
  
bsite


# Years: Similarities in diversity and richness between years ========

# creating a table with relative abbundaces per years
Summaryyears<-aggregate(d[c(8:72,75)], by=list(Category=d$Year), FUN=sum)%>% rename(Year=Category)
rownames(Summaryyears) <- Summaryyears[,1]
SummaryyearsRELAB<-(Summaryyears[2:66]/Summaryyears$PlasticItems_m2)*100

# Bray-Curtis dissimilarity matrix
bcyears<-vegdist(SummaryyearsRELAB, method="bray", binary=FALSE) 
bcyears #they are very similar

# now I want to see if they are indeed equal using Anova with replicated samples
# I am using the same dat table that I have used for comparing the sites,but now I am grouping for time instead of sites
dat
# I am going to add a coloumn with the year
dat$Year<-dat$Recorded..Date
dat$Year<-substring(dat$Year,7,10)

# test the normality
tapply(dat$H, INDEX = dat$Year, FUN=shapiro.test)#different from normal distribution
shapiro.test(dat$H)

# Kruskal-Wallis test
dat$Year <- as.factor(dat$Year)#otherwise the k-w test is not working
KW=kruskal.test(dat$H,dat$Year)
KW #difference in H in the two years, mainly because the wetwipes and the different sampling effort in Hammersmith

#performing Dunn's test to see the pairs
dunnTest(H~ Year, data=dat, method="bh")
#2018 is different from the others years in H

#boxplot to visualise the situation, take into accout that heigher values of H means heigher diversity
ggplot(dat, aes(x=Year, y=H)) + 
  geom_boxplot()+theme_minimal()+ labs(y="Shannon diversity index, H", x = "year")



###Only floating
# now I want to see if for the floating sites only there is a differences between years
datf<-dat%>%filter(type=="Floating")

# test the normality
tapply(datf$H, INDEX = datf$Year, FUN=shapiro.test)#different from normal distribution

# Kruskal-Wallis test
datf$Year <- as.factor(datf$Year)#otherwise the k-w test is not working
KWf=kruskal.test(datf$H,datf$Year)
KWf #no difference ij H in the two years

#boxplot to visualise the situation, take into accout that heigher values of H means heigher diversity
ggplot(datf, aes(x=Year, y=H)) + 
  geom_boxplot()+theme_minimal()+ labs(y="Shannon diversity index, H", x = "year")



#Let's see how the richness changed during these years

# I want to see if they are indeed equal using Anova with replicated samples
# I am using the same dat table that I have used for comparing the sites,but now I am grouping for time instead of sites
dat2

# I am going to add a coloumn with the year
dat2$Year<-dat2$Recorded..Date
dat2$Year<-substring(dat2$Year,7,10)

# test the normality
tapply(dat2$Richness, INDEX = dat2$Year, FUN=shapiro.test)
shapiro.test(dat2$Richness)

# Kruskal-Wallis test
dat2$Year <- as.factor(dat2$Year)#otherwise the k-w test is not working
KW2=kruskal.test(dat2$Richness,dat2$Year)
KW2 #difference in R in the two years, mainly because the wetwipes and the different sampling effort in Hammersmith

#performing Dunn's test to see the pairs
bipbip=dunnTest(Richness~ Year, data=dat2, method="bh")
#only 2018-2017 are significantly different

#boxplot to visualise the situation, take into accout that heigher values of H means heigher diversity
ggplot(dat2, aes(x=Year, y=Richness)) + 
  geom_boxplot()+theme_minimal()+ labs(y="Richness", x = "year")

###Only floating
# I want to see if for the floating sites only there is a differences between years
dat2f<-dat2%>%filter(type=="Floating")
#test the normality
tapply(dat2f$Richness, INDEX = dat2f$Year, FUN=shapiro.test)#normal distribution

# anova test
HaovRich2f=aov(dat2f$Richness~dat2f$Year)
summary(HaovRich2f)
# checking assuption
plot(HaovRich2f)

# post-hoc test using TukeyHSD
TukeyHSD(HaovRich2f)
plot(TukeyHSD(HaovRich2f)) #2018 is slighly different in richness in 2018 than 2017

# boxplot to visualise the situation, take into accout that heigher values of H means heigher diversity
ggplot(dat2f, aes(x=Year, y=Richness)) + 
  geom_boxplot()+theme_minimal()+ labs(y="Richness", x = "year")


###Indicator items using the package indispecies==============================================================

#This Package consider the the differences between group of sites and gives you a list of indicator species
#following the instruction from https://cran.r-project.org/web/packages/indicspecies/vignettes/indicspeciesTutorial.pdf

# community data matrix from previous section
community

# Defining the classification of sites from the previous section and from the type of survey
# the list of the location is 
sitestable

##I am going to groop them following the type of site. Floating (2), Sinking (1) 
group

#Indicator species analysis using the type of site as a grouping method for sites
indval<- multipatt(community, group, control = how(nperm = 999), func = "IndVal.g")
summary(indval)
indval$sign

summary(indval, alpha=1)
# to visualize to component A and B for each item

summary(indval, indvalcomp=TRUE, alpha=1)

# there are several items with NAs, this are items with high indval for the all sites group, for this reason the p value cannot be tested 
# their indicator values are
s<-c(1.0000000,0.5477226,0.8944272,0.6324555,1.0000000, 0.4472136, 0.7071068,1.0000000,0.8366600,0.9486833)
sqrt(s)

# Indicator species analysis using the non-hierarchical cluster analysis groupkm from the previous section for sites
indvalR<- multipatt(community, groupskm, control = how(nperm = 999))
summary(indvalR)
indvalR$sign

# The original Indicator Value method of Dufr^ene and Legendre [1997] did not consider combinations of site groups. 
# In other words, the only site group combinations permitted in the original method were singletons. 
# When using multipatt it is possible to avoid considering site group combinations, as in the original method, 
# by using duleg = TRUE
indvalori<-multipatt(community, group, duleg=T, control = how(nperm=999))

summary(indvalori)
summary(indvalori, indvalcomp=TRUE)
indvalori$sign



###Linear mixed model to see changes in tot aboundance thought time=====================================================

# I want to test changes in total abbundance of plastic items throught time. 
# because I have a lot of heterogeneity on where the data were sampled across time and space I am going to overcome
# this problem considering site as random effects 

# moreover, beacuse each years they have different numbers of quadrats to overcome this problem I am going
# to use abundance data, this means I am using the tot num items/ tot num quadrat
# so I am testing if there was a reduction in the abundance of plastic per m2

# preparing the data for the lmm. I am using data from each transect, not quadrat, and I am going to give a consecutive number a ecah date
LMMdata<-aggregate(d[c(8:72,75)], by=list(Category=d$Site.Name,d$Recorded..Date), FUN=mean)%>% rename(Site.Name=Category, Recorded..Date=Group.2)
LMMdata$Recorded..Date<-as.Date(LMMdata$Recorded..Date, "%d/%m/%Y")

# the ref date is the 26-02-2016, in this way the 27th is #1
ref_date <- ymd('2016-02-26')
LMMdata$day <- as.numeric(difftime(LMMdata$Recorded..Date, ref_date, units = 'days'))

# normal data?
hist(LMMdata$PlasticItems_m2, breaks = 50)

#visualizing if there was a decrease
ggplot(LMMdata, aes(x=day, y=(PlasticItems_m2)))+ geom_point()+theme_minimal()+ labs(y="Plastic Items/m2", x = "day") # + geom_smooth(method=lm, se=FALSE,color="#D95F02")

#Linear Mixed model with day
mm<-lmer(PlasticItems_m2~day+(1|Site.Name), data=LMMdata)
summary(mm) 

#checking assumption of normality of residual
shapiro.test(residuals(mm))#not notmal,I am going to see at the plots
plot(mm)
qqplot(LMMdata$day,LMMdata$PlasticItems_m2)#not so bad, accetable

#mm reduced
mm.red<-lmer(PlasticItems_m2~1+(1|Site.Name), data=LMMdata)
summary(mm.red)

#test the models
anova(mm.red,mm)

#from this anova we can see that the two models are not significant diferent, this means that the effectd day does not explaing much of the 
#variation and we cannot conclude that there in no significative changese in the plastic items amount 


#many might argue that it is better to use Mann-Kendall test
#mann kendall test

MKpp1<-SeasonalMannKendall(as.ts(LMMdata$PlasticItems_m2))
summary(MKpp1)

#this test has as assumption that there is not bias data. Our data are most likely biased and pseudoreplicated, 
#for this reason I do not think that this test is the best to use. The LMM allows to correct the pseudoreplication of the sites. 
#anyway the result of the M-K is not significant and the tau is positive, all of this is coherent with the results of the LMM
#Moreover, M-K is a non-parametic test, and our data are sliglty skewed. 
#The LMM diagnostic plots are accetable and the LMM is considered more powerful because it take into consideration the pseudoreplication.