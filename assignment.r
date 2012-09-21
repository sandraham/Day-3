> # TraMineR course assignment 2
> 
> #Load biofam data
> 
> data(biofam)
> 
> names(biofam)                   # Print the variable names
 [1] "idhous"   "sex"      "birthyr"  "nat_1_02" "plingu02" "p02r01"   "p02r04"   "cspfaj"   "cspmoj"  
[10] "a15"      "a16"      "a17"      "a18"      "a19"      "a20"      "a21"      "a22"      "a23"     
[19] "a24"      "a25"      "a26"      "a27"      "a28"      "a29"      "a30"      "wp00tbgp" "wp00tbgs"
> str(biofam)                     # Look at the data structure, variable types
'data.frame':  2000 obs. of  27 variables:
 $ idhous  : num  66891 28621 57711 17501 147701 ...
 $ sex     : Factor w/ 2 levels "man","woman": 1 1 2 1 1 1 1 1 1 2 ...
 $ birthyr : num  1943 1935 1946 1918 1946 ...
 $ nat_1_02: Factor w/ 200 levels "other error",..: 6 6 6 6 6 6 6 6 6 6 ...
 $ plingu02: Factor w/ 3 levels "french","german",..: 2 2 1 2 2 3 2 1 1 2 ...
 $ p02r01  : Factor w/ 13 levels "other error",..: 6 7 13 7 7 7 6 9 6 7 ...
 $ p02r04  : Factor w/ 14 levels "other error",..: 9 13 7 13 7 6 7 14 9 13 ...
 $ cspfaj  : Factor w/ 12 levels "active occupied but not classified",..: 7 7 7 5 NA 12 NA 11 7 7 ...
 $ cspmoj  : Factor w/ 12 levels "active occupied but not classified",..: 7 NA 9 NA NA NA NA NA 7 NA ...
 $ a15     : num  0 0 0 0 0 0 0 0 0 1 ...
 $ a16     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a17     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a18     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a19     : num  0 1 0 0 0 0 0 0 0 1 ...
 $ a20     : num  0 1 0 1 1 0 0 0 0 1 ...
 $ a21     : num  0 1 0 1 1 0 0 1 0 1 ...
 $ a22     : num  0 1 1 1 1 0 0 1 0 1 ...
 $ a23     : num  0 1 1 1 1 0 0 1 0 1 ...
 $ a24     : num  3 1 1 1 1 0 2 1 0 6 ...
 $ a25     : num  6 1 1 1 1 0 2 1 0 6 ...
 $ a26     : num  6 3 1 1 1 0 2 3 6 6 ...
 $ a27     : num  6 6 3 1 1 0 2 3 6 6 ...
 $ a28     : num  6 6 6 1 6 0 2 3 6 6 ...
 $ a29     : num  6 6 6 1 6 0 2 6 6 6 ...
 $ a30     : num  6 6 6 1 6 0 2 6 6 6 ...
 $ wp00tbgp: num  1053 855 575 1527 796 ...
 $ wp00tbgs: num  0.935 0.759 0.51 1.356 0.707 ...
> ?biofam
> 
> # Create new variables
> 
> age <- 2002 - biofam$birthyr
> 
> cohort <- cut(biofam$birthyr, breaks=c(1900,1930,1940,1950,1960), 
+               labels=c("1900-1929","1930-1939","1940-1949","1950-1959"), right=FALSE)
> table(cohort)
cohort
1900-1929 1930-1939 1940-1949 1950-1959 
      260       466       632       642 
> 
> mwc <- as.numeric(biofam$a25==6)   # Married with children @ 25 y, yes/no for logistic regressions
> 
> 
> biofam2 <- cbind(biofam, age, cohort, mwc)     # Merge the new variables with the dataframe
> 
> 
> 
> # Look at the sequence data
> 
> head(biofam2[, 10:25])
     a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
1167   0   0   0   0   0   0   0   0   0   3   6   6   6   6   6   6
514    0   1   1   1   1   1   1   1   1   1   1   3   6   6   6   6
1013   0   0   0   0   0   0   0   1   1   1   1   1   3   6   6   6
275    0   0   0   0   0   1   1   1   1   1   1   1   1   1   1   1
2580   0   0   0   0   0   1   1   1   1   1   1   1   1   6   6   6
773    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
> 
> ## Create formats and labels for the states to use in output in the order of the states
> ## Default state order is alphabetical, but may vary across Windows and MacOS installations
> 
> # Check the order of the states so that labels can be specified in the correct order
> seqstatl(biofam2[, 10:25])
[1] 0 1 2 3 4 5 6 7
> 
> # Create state names and labels
> 
> biofam2.states <- c("P", "L", "M", "LM", "C", "LC", "LMC", "D")
> biofam2.labels <- c("Parent","Left","Married","Left/Married","Child","Left/Child","Left/Married/Child","Divorced")
> 
> ## Create a fully defined and formatted sequence object
> 
> biofam2.seq <- seqdef(biofam2, 10:25, states=biofam2.states, labels=biofam2.labels, weights=biofam2$wp00tbgs, xtstep = 2)
 [>] state coding:
       [alphabet]  [label]  [long label] 
     1  0           P        Parent
     2  1           L        Left
     3  2           M        Married
     4  3           LM       Left/Married
     5  4           C        Child
     6  5           LC       Left/Child
     7  6           LMC      Left/Married/Child
     8  7           D        Divorced
 [>] sum of weights: 2119.09 - min/max: 0/6.02881860733032
 [>] 2000 sequences in the data set
 [>] min/max sequence length: 16/16
> 
> 
> ####### End of data steps
> 
> 
> 
> 
> # Create a full sequence index plot, sorted, by cohort
> 
> seqIplot(biofam2.seq, group=biofam2$cohort, xtstep=2,
+          sortv="from.end", title="Sequences of Family Life States by Cohort")
> 
> 
> # Print the frequencies of the first 20 sequences
> 
> seqtab(biofam2.seq, tlim=1:20)
                Freq Percent
P/16             196     9.3
P/6-L/10          40     1.9
P/10-M/6          38     1.8
P/5-L/11          38     1.8
P/9-M/7           32     1.5
P/7-LMC/9         31     1.5
P/8-M/8           29     1.4
P/6-M/10          28     1.3
P/10-LM/6         28     1.3
P/7-M/9           27     1.3
P/7-L/9           26     1.2
P/6-LMC/10        25     1.2
P/10-L/6          25     1.2
P/15-LM/1         24     1.1
P/10-LM/1-LMC/5   23     1.1
P/14-LM/2         22     1.0
P/4-L/12          22     1.0
P/10-LMC/6        21     1.0
P/5-M/11          21     1.0
P/15-M/1          21     1.0
> 
> 
> # Create a sequence index frequency plot, sorted, by cohort saved as .jpg
> 
> jpeg(file="C://Documents and Settings//sham//Desktop//TraMiner//assignment 3 seqfplot1.jpg")
> 
> seqfplot(biofam2.seq, group=biofam2$cohort, tlim=1:20, xtstep=2, border=NA, title="Frequent Sequences of Family Life States by Cohort")
> dev.off()
RStudioGD 
        2 
> 
> 
> # Compute the transition rate matrix
> 
> round(seqtrate(biofam2.seq), digits=4)
 [>] computing transition rates for states P/L/M/LM/C/LC/LMC/D ...
         [-> P] [-> L] [-> M] [-> LM] [-> C] [-> LC] [-> LMC] [-> D]
[P ->]   0.8906 0.0478 0.0182  0.0304 0.0005  0.0011   0.0114 0.0000
[L ->]   0.0000 0.8916 0.0000  0.0800 0.0000  0.0036   0.0247 0.0002
[M ->]   0.0000 0.0000 0.9711  0.0130 0.0000  0.0000   0.0092 0.0067
[LM ->]  0.0000 0.0000 0.0000  0.8013 0.0000  0.0000   0.1843 0.0144
[C ->]   0.0000 0.0000 0.3383  0.0000 0.6094  0.0523   0.0000 0.0000
[LC ->]  0.0000 0.0000 0.0000  0.0000 0.0000  0.8662   0.1338 0.0000
[LMC ->] 0.0000 0.0000 0.0000  0.0000 0.0000  0.0000   0.9941 0.0059
[D ->]   0.0000 0.0000 0.0000  0.0000 0.0000  0.0000   0.0000 1.0000
> 
>   #### Transition rate between Left/Married and Left/Married/Child is 0.1843
> 
> 
> # Display the sequence of transversal state distributions by cohort
> 
> seqdplot(biofam2.seq, group=biofam2$cohort, border=NA, title="Transversal State Distributions by Cohort")
> 
> 
> # Compute entropy by cohort
> 
> sd <- seqstatd(biofam2.seq)
> sd$Entropy[1:16]
       a15        a16        a17        a18        a19        a20        a21        a22        a23 
0.03498258 0.09185595 0.12657360 0.19902582 0.28333609 0.40721725 0.51946867 0.61697058 0.68836488 
       a24        a25        a26        a27        a28        a29        a30 
0.74367664 0.78882753 0.79913743 0.80348974 0.79819988 0.78573329 0.76952579 
> 
> seqHtplot(biofam2.seq, xtstep=1, title="Transversal entropies")
> 
> seqplot.tentrop(biofam2.seq, group=biofam2$cohort, xtstep=1, title="Transversal entropies by Cohort")
[1] 4
>   #### Diversity (entropy) is highest for each cohort at the following ages:
>   #### 1900-1929: 28-30
>   #### 1930-1939: 26-27
>   #### 1940-1949: 24-26
>   #### 1950-1959: 27-30
> 
> 
> # Plot the mean time spent in each state and modal state
> 
> par(mfrow=c(1,3))
> seqmtplot(biofam2.seq, ylim=c(0,10), withlegend=FALSE, title="Mean Time per State")
> seqmsplot(biofam2.seq, border=NA, withlegend=FALSE, title="Modal State")
> seqlegend(biofam2.seq, fontsize=1.0)