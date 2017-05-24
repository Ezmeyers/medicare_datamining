
source("fin_proj_functions.R")

# Initialization of libraries and packages via custom function ----
installPackages()

# loading medicare data set
medi.dat = read.csv("medi.csv", header = FALSE, stringsAsFactors = FALSE)
# This data set has HPCS codes relating to mental health only, so we can
# pull them out of medi.dat
mh.dat = read.csv("MH_codes.csv", header = TRUE, stringsAsFactors = FALSE)

# Data Preparation -----------
# Check the dataset
#View(medi.dat)

# Our headers don't look how we want, they're sitting on row 2
# so lets fix that
medi.dat[1:2, ] = na.omit(medi.dat[1:2,])

# we want to grab the row 2 and then set it as the column headers instead
medi.names = unlist(medi.dat[2,])

# remove name attributes we don't want/need in order to make data look nicer
names(medi.names)  = NULL
row.names(medi.names) = NULL
names(medi.dat) = NULL

# finally, put our proper headers
colnames(medi.dat) = medi.names

# remove first 2 rows now that we set our headers from them, they're useless
medi.dat = medi.dat[3:nrow(medi.dat), ]

# var removal from global space, no longer needed
rm(medi.names)

# Data Cleaning -----------

# Grab unique state names
states = unique(medi.dat$`NPPES Provider State Description`)

# we are only looking at US 50 states
# rm all territories which start with "Armed" of which there are multiple
rm.armed = c(startsWith(unique(medi.dat$`NPPES Provider State Description`), "Armed"))

# hardcode, remove all other territories we don't want
rm.states = c(states[rm.armed], "American Samoa", "District of Columbia", "Foreign Country", "Guam",
            "North Mariana Islands", "Puerto Rico", "Virgin Islands","Unknown")

# get indexes of rows we don't want 
# (if the state is in our remove list, then we will grab the indexes)
rm.idx = ifelse(medi.dat$`NPPES Provider State Description` %in% rm.states, 1, 0) == 1

# these are rows in the data set that match the territories we don't want
rm.rows = medi.dat[rm.idx, ]

# unjoin will extract original data - territories
# so now we have only 50 states we want.
medi.dat = anti_join(medi.dat, rm.rows)

# Now we need to remove dollar signs from numeric columns
# All our columns beginning with "Average" have $ so we remove them
# by calling our custom function with apply
no.dols = apply(medi.dat[startsWith(names(medi.dat), "Average")], 2, FUN =
                  removeDollars)

# Replace columns with dollar signs to ones without dollar signs
medi.dat[,10:13] = as.data.frame(no.dols)

# we need numeric columns, but we have chars and factors
# so we change them with custom functions
medi.dat[,10:13] = factors2Numeric(medi.dat)
medi.dat[,6:9] = chars2Numeric(medi.dat[,6:9])
medi.dat$`Place of Service`= ifelse(medi.dat$`Place of Service`=="F",1,0)
# we don't need these anymore
rm(no.dols, rm.rows,rm.armed,rm.idx,rm.states,states)

# Data Transforming --------

# We want to make sure that for each state we have equivalent HCPCS codes
# Why: We don't want to compare across states with very different procedures
# Additionally, we want to remove duplicated codes

# We start with Indiana arbitrarily
medi.state = medi.dat[medi.dat$`NPPES Provider State Description` == "Indiana", ]

# get the non duplicated codes from Indiana to be represented
hcpcs.codes = as.data.frame(medi.state[!duplicated(medi.state$`HCPCS Code`), 2:2])
names(hcpcs.codes) = c("hcpcs codes")
# gather the codes in each state
medi.state.hcpcs = aggregate(medi.dat$`HCPCS Code`,by = list(medi.dat$`NPPES Provider State Description`),
                   FUN = function(x) unlist(x))

# We are going to reduce our unique codes in each state so that all of the
# codes per state are the same set of unique codes
# using indiana codes as arbitrary beginning value
# to do this, we need to turn these into a list, because that's what reduce accepts
medi.intersect = toList(medi.state.hcpcs)

# Now we can reduce all the states to the same set of HCPCS codes
medi.states.common.codes = Reduce(f = intersect, x = medi.intersect, accumulate =
                                    TRUE)

# now we have the accumulated intersection of all states by code
# which is the very last list that reduce turns, as it accumuluates down
# so we grab the set we want to keep
medi.intersect = medi.states.common.codes[[50]]
# get all the HCPCS codes in the reduced set
medi.dat = medi.dat[medi.dat$`HCPCS Code` %in% medi.intersect, ]
# do not need these vars anymore
rm(medi.intersect, medi.states.common.codes,medi.state,medi.state.hcpcs,hcpcs.codes)

# Data Processing -----------

# We need to calculate the difference between the average allowed medicare 
# amount and the average charged amount
medi.pay.gap = medi.dat$`Average Medicare Payment Amount`-medi.dat$`Average Submitted Charge Amount`
medi.dat = cbind(medi.dat, medi.pay.gap)

# We want the difference as a proportion
# first get the average allowed over the submitted charges
medi.pay.gap.perc = medi.dat$`Average Medicare Payment Amount`/medi.dat$`Average Submitted Charge Amount`

# med.pay.gap.perc is the amount covered, 
# so subtract from 1 to get the difference
medi.pay.gap.perc = 1 - medi.pay.gap.perc
medi.dat = cbind(medi.dat, medi.pay.gap.perc)

# Now we would like to partition our set into two sets
# The sets we will partition into are Physical Health Procedures
# and Mental Health Procedures

# first we use the HCPCS codes excel sheet to pull out mental health procedures
medi.mh = medi.dat[medi.dat$`HCPCS Code` %in% mh.dat$MH.Codes, ]
# We do a setdiff, because what is left is the physical set after pulling out
# mental health subset from the whole
medi.phys = setdiff(medi.dat, medi.mh)

# Removal of Surgery Data
# further subset physical group: for instance, we don't want to look at:
# drug charges, surgeries, long study procedures, and so on...
# They are not very comparable to 30-60 minute mental health procedures

# first we remove all drug charges, we don't want to look at medications
medi.phys = medi.phys[medi.phys$`HCPCS Drug Indicator` == "N", ]

# customly removing rows involving these situations
medi.rm = c("surgery", "drugs", "bypass", "home visit", "MRI", "biopsy", "ultrasound", "X-ray",
            "stents", "surgical", "CT", "studies", "inpatient", "replacement","incision","dialysis","repair")

# get cases which pertain to the wordset we just defined
medi.rm = medi.phys[grep(paste(medi.rm, collapse = '|'),
                         medi.phys$`HCPCS Description`,
                         ignore.case = TRUE), ]

# remove the cases we don't want
medi.phys = setdiff(medi.phys, medi.rm)

# Pull out the payment gaps per set for analysis
medi.phys.perc = as.data.frame(cbind(medi.phys$medi.pay.gap.perc))
medi.mh.perc = as.data.frame(cbind(medi.mh$medi.pay.gap.perc))



# remove unwanted vars
rm(medi.rm,medi.pay.gap,medi.pay.gap.perc)

# Statistical Analysis & Visualizations---------

# checking the data summaries
summary(medi.mh)
summary(medi.phys)

# checking correlation coefficients
# mental health set num providers/payment gap
cor(medi.mh$`Number of Providers`, medi.mh$medi.pay.gap.perc)
# physical health set num providers/payment gap
cor(medi.phys$`Number of Providers`, medi.phys$medi.pay.gap.perc)

# Checking the distributions
# Normalizing data for side-by-side boxplot scaling
medi.phys.perc = data.frame(group = "Physical Health", value = scale(medi.phys.perc))
medi.mh.perc = data.frame(group = "Mental Health", value = scale(medi.mh.perc))

# We need to do a rowbind by group for side by side box plot
medi.phys.mh.perc = rbind(medi.phys.perc, medi.mh.perc)
write.csv("compare2.csv", x=compare2)
# Checking the side by side box plots
ggplot(medi.phys.mh.perc, aes(x = group, y = V1)) + ylab("Payment Gaps") +
  xlab("Medicare sets") + geom_boxplot(outlier.color = c("dodgerblue1"))

# clearing
rm(medi.phys.mh.perc,medi.mh.perc,mh.dat, medi.phys.perc)

# Data Transform part 2 -------------
# Based on the above data, we cannot say there is a correlation between
# the number of providers and payment gaps in either set.
# Additionally, we could not find a suitable model for linear or multilinear
# regression. Therefore, we have moved into another direction with the project
# We will look at the state level in each set.


# Pulling out mh average payment gaps by state for statewide analysis
medi.names = c("States","Place of Service","Total Num Providers","Total Num Services","Total Unique Interactions","Avg Daily Interactions", "Avg Payment Gap","Avg Submitted Charge","Avg Allowed Amount","Avg Medicare Payment","Avg Standardized Payment")
medi.means = aggregate(cbind(`Number of Distinct Medicare Beneficiary/Per Day Services`,medi.pay.gap.perc, `Average Submitted Charge Amount`,`Average Medicare Allowed Amount`,`Average Medicare Payment Amount`,`Average Medicare Standardized Payment Amount`)~`NPPES Provider State Description`, data=medi.mh, FUN=mean)
medi.sums = aggregate(cbind(`Place of Service`,`Number of Providers`,`Number of Services`, `Number of Unique Beneficiary/Provider Interactions`)~`NPPES Provider State Description`, data=medi.mh, FUN=sum)
medi.mh.states = cbind(medi.sums,medi.means[-c(1)])
names(medi.mh.states) = medi.names

# same for physical set
medi.means1 = aggregate(cbind(`Number of Distinct Medicare Beneficiary/Per Day Services`,medi.pay.gap.perc, `Average Submitted Charge Amount`,`Average Medicare Allowed Amount`,`Average Medicare Payment Amount`,`Average Medicare Standardized Payment Amount`)~`NPPES Provider State Description`, data=medi.phys, FUN=mean)
medi.sums1 = aggregate(cbind(`Place of Service`,`Number of Providers`,`Number of Services`, `Number of Unique Beneficiary/Provider Interactions`)~`NPPES Provider State Description`, data=medi.phys, FUN=sum)
medi.phys.states = cbind(medi.sums1,medi.means1[-c(1)])
names(medi.phys.states) = medi.names

# Statistical Analysis Part 2 -----------
# Checking possible correlations between and within data sets
medi.test = cor(medi.mh.states[,2:11],medi.phys.states[,2:11])

rownames(medi.test) = as.list(paste(dimnames(medi.test)[[1]],"mh",sep="."))
colnames(medi.test) = as.list(paste(dimnames(medi.test)[[1]],"phys",sep="."))
ggcorrplot::ggcorrplot(medi.test, lab=TRUE, insig="blank")

medi.mh.test = cor(medi.mh.states[,2:11],medi.mh.states[,2:11])
#rownames(medi.test) = as.list(paste(dimnames(medi.test)[[1]],"mh",sep="."))
#colnames(medi.test) = as.list(paste(dimnames(medi.test)[[1]],"phys",sep="."))
medi.phys.test = cor(medi.phys.states[,2:11],medi.phys.states[,2:11])
rownames(medi.phys.test) = as.list(paste(dimnames(medi.test)[[1]],"phys",sep="."))
#rownames(medi.test) = as.list(paste(dimnames(medi.test)[[1]],"mh.phys",sep="."))

# checking high correlations
#corr.mh = ifelse((medi.mh.test>.70 | medi.mh.test<(-.70)),medi.mh.test,0)
dimnames(corr.mh)[[1]] = as.list(paste(dimnames(corr.mh)[[1]],"m",sep="."))
dimnames(corr.mh)[[2]] = as.list(paste(dimnames(corr.mh)[[2]],"m",sep="."))
#corr.phys = ifelse((medi.phys.test>.70 | medi.phys.test<(-.70)),medi.phys.test,0)
dimnames(corr.phys)[[1]] = as.list(paste(dimnames(corr.phys)[[1]],"p",sep="."))
dimnames(corr.phys)[[2]] = as.list(paste(dimnames(corr.phys)[[2]],"p",sep="."))

#corr = ifelse((medi.test>.70 | medi.test<(-.70)),medi.test,0)
compare2=as.data.frame(rbind(medi.mh.test,medi.phys.test,medi.test))
#compare = as.data.frame(rbind(corr.mh,corr.phys,corr))
names(compare2) = dimnames(medi.test)[[1]]
#compare[compare == 0] = NA
compare2=compare2[6:ncol(compare2)]
View(compare2)


# Testing linear regression models
# NOTE ALOT OF THE BELOW ARE MADE PURELY FOR POWERPOINT VISUALIZING
mhSub.physCharge.lm = lm(medi.mh.states$`Avg Submitted Charge`~medi.phys.states$`Avg Submitted Charge`)
mhGap.physGap.lm = lm(medi.mh.states$`Avg Payment Gap`~medi.phys.states$`Avg Payment Gap`)
physGap.mhGap.lm = lm(medi.phys.states$`Avg Payment Gap`~ medi.mh.states$`Avg Payment Gap`)
physGap.physCharge.lm = lm(medi.phys.states$`Avg Payment Gap`~ medi.phys.states$`Avg Submitted Charge`)
mhGap.mhCharge.lm = lm(medi.mh.states$`Avg Payment Gap`~ medi.mh.states$`Avg Submitted Charge`)
mhGap.PhysGap.PhysCharge.MhCharge.lm = lm(medi.mh.states$`Avg Payment Gap`~ medi.phys.states$`Avg Payment Gap`+medi.phys.states$`Avg Submitted Charge`+medi.mh.states$`Avg Submitted Charge`)
mhCharge.PhysGap.PhysCharge.MhGap.lm = lm(medi.mh.states$`Avg Submitted Charge`~ medi.mh.states$`Avg Payment Gap`+ medi.phys.states$`Avg Payment Gap`+ medi.phys.states$`Avg Submitted Charge`)


# check out summaries and get p values
lm1 = summary(mhSub.physCharge.lm)
lm1.1 = summary(mhGap.physGap.lm)
lm2 = summary(physGap.mhGap.lm)
lm3 = summary(physGap.physCharge.lm)
lm4 = summary(mhGap.mhCharge.lm)
lm5 = summary(mhGap.PhysGap.PhysCharge.MhCharge.lm)
lm6 = summary(mhCharge.PhysGap.PhysCharge.MhGap.lm)
# get p values
p1 = glance(lm1)$p.value
p1.1= glance(lm1.1)$p.value
p2  = glance(lm2)$p.value
p3 = glance(lm3)$p.value
p4 = glance(lm4)$p.value
p5 = glance(lm5)$p.value
p6 = glance(lm6)$p.value

# just make a data frame for our own viewing
p.df = as.data.frame((c(p1,p1.1,p2,p3,p4,p5,p6)))
response = c("MH Avg Submitted Charge", "MH Avg Payment Gap", "Phys Avg Payment Gap", "States Avg Payment Gap", "MH Avg Payment Gap", "MH Avg Payment Gap","MH Avg Submitted Charge")
predictors = c("Phys Avg Submitted Charge","Phys Avg Payment Gap", "Mh Avg Payment Gap","Phys Avg Submitted Charge", "Mh Avg Submitted Charge","Phys Avg Payment Gap + Phys Avg Submitted Charge + MH Avg Submitted Charge","Phys Avg Payment Gap + Phys Avg Submitted Charge + MH Avg Payment Gap")

model = c("lm1","lm1.1","lm2","lm3","lm4","lm5","lm6")
p.df = cbind(model, response, predictors, p.df)
colnames(p.df) = c("model", "dependent", "independent", "P-Value")
p.df = p.df[order(p.df$`P-Value`),]

# don't need
rm(medi.means,medi.means1,medi.sums,medi.sums1)

# Plotting Linear Regressions

# Plot the model
names(mhGap.physGap.lm$model) = c("mh","phys")
mh.phys.gaps = ggplot(mhGap.physGap.lm$model, aes(x=mhGap.physGap.lm$model$phys, y=mhGap.physGap.lm$model$mh, colour = phys)) + 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) + geom_point() + 
  stat_smooth(method = "lm", col = "#654321", linetype = "solid",size=0.5)+
  scale_colour_gradient(low = "#88bdbc", high = "654321")+
  geom_text(aes(label=medi.phys.states$States),hjust=.5, vjust=.7,size=3,nudge_y=-.005,color="#4f4a41",fontface=15)

print(mh.phys.gaps + ggtitle("Physical Procedure payment Gaps vs Mental Procedure Payment Gaps") + labs(y = "MH", x = "PHYS"))

# Plot the models
names(mhGap.mhCharge.lm$model) = c("gap","charge")
plotMH = ggplot(mhGap.mhCharge.lm$model, aes(mhGap.mhCharge.lm$model$charge ,mhGap.mhCharge.lm$model$gap, colour = charge)) + 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.2),legend.position="none") + geom_point() + 
  stat_smooth(method = "lm", col = "black", linetype = "solid", size=0.7) +
  labs(y = "Mental Health Procedures Avg Payment Gap", x = "Mental Health Procedures Avg Submitted Charge")+
  scale_colour_gradient(low = "gold", high = "darkgoldenrod4")+
  ggtitle(bquote(atop(.("Linearity across mental and physical procedure sets"), atop(italic(.("Avg Payment gap~ Avg Submitted charge")), "")))) 

names(physGap.physCharge.lm$model) = c("gap","charge")
plotPH = ggplot(physGap.physCharge.lm$model, aes(physGap.physCharge.lm$model$charge,physGap.physCharge.lm$model$gap, colour = charge)) + 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 10.10)) + geom_point() + 
  stat_smooth(method = "lm", col = "black", linetype = "solid", size=0.7)+
  scale_colour_gradient(low = "gold", high = "darkgoldenrod4")+
  labs(y = "Phys Procedures Avg Payment Gaps", x = "Phys Procedures Avg Submitted Charge")+
  ggtitle(expression(atop(" . ")))

grid.arrange(plotMH, plotPH, ncol = 2)





# Testbed ------------------
t = as.data.frame(medi.mh.states$`Avg Payment Gap` - medi.phys.states$`Avg Payment Gap`)
t = cbind(medi.mh.states$States,t)
names(t) = c("states","diff")
t = as.data.frame(t[order(t$diff),])
tz = lm(medi.mh.states$`Avg Payment Gap`~ t$diff)
names(tz$model) = c("y","x")
plot(tz$model$x,tz$model$y)
abline(lm(medi.mh.states$`Avg Payment Gap`~t$diff))

