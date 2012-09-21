# TraMineR course assignment 3

library(TraMineR)
install.packages("combinat")
install.packages("TraMineRextras", repos="http://R-forge.R-project.org")
library(TraMineRextras)


#Load biofam data

data(biofam)

names(biofam)                   # Print the variable names
str(biofam)                     # Look at the data structure, variable types
?biofam

# Create new variables

age <- 2002 - biofam$birthyr

cohort <- cut(biofam$birthyr, breaks=c(1900,1930,1940,1950,1960), 
              labels=c("1900-1929","1930-1939","1940-1949","1950-1959"), right=FALSE)
table(cohort)

mwc <- as.numeric(biofam$a25==6)   # Married with children @ 25 y, yes/no for logistic regressions


biofam2 <- cbind(biofam, age, cohort, mwc)     # Merge the new variables with the dataframe



# Look at the sequence data

head(biofam2[, 10:25])


# Check the order of the states so that labels can be specified in the correct order
seqstatl(biofam2[, 10:25])

# Create state names and labels

biofam2.states <- c("P", "L", "M", "LM", "C", "LC", "LMC", "D")
biofam2.labels <- c("Parent","Left","Married","Left/Married","Child","Left/Child","Left/Married/Child","Divorced")

## Create a fully defined and formatted sequence object

biofam2.seq <- seqdef(biofam2, 10:25, states=biofam2.states, labels=biofam2.labels, weights=biofam2$wp00tbgs, xtstep = 2)


####### End of data steps




# Create a full sequence index plot, sorted, by cohort

seqIplot(biofam2.seq, group=biofam2$cohort, xtstep=2,
         sortv="from.end", title="Sequences of Family Life States by Cohort")


# Print the frequencies of the first 20 sequences

seqtab(biofam2.seq, tlim=1:20)


# Create a sequence index frequency plot, sorted, by cohort saved as .jpg

jpeg(file="C://Documents and Settings//sham//Desktop//TraMiner//assignment 3 seqfplot1.jpg")

seqfplot(biofam2.seq, group=biofam2$cohort, tlim=1:20, xtstep=2, border=NA, title="Frequent Sequences of Family Life States by Cohort")
dev.off()


# Compute the transition rate matrix

round(seqtrate(biofam2.seq), digits=4)

  #### Transition rate between Left/Married and Left/Married/Child is 0.1843


# Display the sequence of transversal state distributions by cohort

seqdplot(biofam2.seq, group=biofam2$cohort, border=NA, title="Transversal State Distributions by Cohort")


# Compute entropy by cohort

sd <- seqstatd(biofam2.seq)
sd$Entropy[1:16]

seqHtplot(biofam2.seq, xtstep=1, title="Transversal entropies")

seqplot.tentrop(biofam2.seq, group=biofam2$cohort, xtstep=1, title="Transversal entropies by Cohort")
  #### Diversity (entropy) is highest for each cohort at the following ages:
  #### 1900-1929: 28-30
  #### 1930-1939: 26-27
  #### 1940-1949: 24-26
  #### 1950-1959: 27-30


# Plot the mean time spent in each state and modal state

par(mfrow=c(1,3))
seqmtplot(biofam2.seq, ylim=c(0,10), withlegend=FALSE, title="Mean Time per State")
seqmsplot(biofam2.seq, border=NA, withlegend=FALSE, title="Modal State")
seqlegend(biofam2.seq, fontsize=1.0)