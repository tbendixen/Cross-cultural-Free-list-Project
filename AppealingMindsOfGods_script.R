#####################################################
### R script for "Appealing to the minds of gods" ###
#####################################################

### Script by: Theiss Bendixen & Benjamin Grant Purzycki
### Contact: tb@cas.au.dk
### Last update: October 26th 2021

### The script follows the order in which each table and figure appear in the main paper, 
### followed by supplementary tables and plots.

##################
### Setting up ###
##################

### Set working directory
setwd("")

### Load packages
library(AnthroTools)  # for salience calculations
library(xtable)       # for printing LaTex tables
library(dplyr)        # exclusively used for the "add_row()" command

### Load data
data <- read.csv("FreeList_CERC_V0.1_FIN.csv", sep = ";") # free-list data
cerc <- read.csv("CERC Dataset (Wave 1) Version 6.0.csv", sep = ";") # demographic data

#######################################################
### Table 1: Demographic and free-list descriptives ###
#######################################################

### Calculate item salience grouped by culture

BGL.FL <- CalculateSalience(data, Order = "Order", Subj = "CERCID",
                            CODE = "BGL", GROUPING = "Culture", Rescale = FALSE, Salience = "BGL.S")
BGD.FL <- CalculateSalience(BGL.FL, Order = "Order", Subj = "CERCID",
                            CODE = "BGD", GROUPING = "Culture", Rescale = FALSE, Salience = "BGD.S")
LGL.FL <- CalculateSalience(BGD.FL, Order = "Order", Subj = "CERCID",
                            CODE = "LGL", GROUPING = "Culture", Rescale = FALSE, Salience = "LGL.S")
LGD.FL <- CalculateSalience(LGL.FL, Order = "Order", Subj = "CERCID",
                            CODE = "LGD", GROUPING = "Culture", Rescale = FALSE, Salience = "LGD.S")
POL.FL <- CalculateSalience(LGD.FL, Order = "Order", Subj = "CERCID",
                            CODE = "POL", GROUPING = "Culture", Rescale = FALSE, Salience = "POL.S")
POD.FL <- CalculateSalience(POL.FL, Order = "Order", Subj = "CERCID",
                            CODE = "POD", GROUPING = "Culture", Rescale = FALSE, Salience = "POD.S")

FL <- POD.FL

### Free-list descriptives - globally per domain

# just a small convencience function for extracting reported free-list descriptives
fl_tab_fun <- function(x){
  x$Subject <- NULL
  x$SumList <- rowSums(x) # number of listed items per domain and participant
  print(length(x$SumList)) # culture's n of free-list domain
  print(round(mean(x$SumList), digits = 1)) # mean number of listed items per domain and culture
}

# MG pleases
bgllabs <- c("CERCID", "BGL", "Culture")
bgl <- FL[bgllabs]
bgl <- bgl[complete.cases(bgl),]
bgltab <- FreeListTable(bgl, CODE = "BGL", Subj = "CERCID", tableType = "FREQUENCY")
fl_tab_fun(bgltab)

# MG angers
bgdlabs <- c("CERCID", "BGD", "Culture")
bgd <- FL[bgdlabs]
bgd <- bgd[complete.cases(bgd),]
bgdtab <- FreeListTable(bgd, CODE = "BGD", Subj = "CERCID", tableType = "FREQUENCY")
fl_tab_fun(bgdtab)

# LG pleases
lgllabs <- c("CERCID", "LGL", "Culture")
lgl <- FL[lgllabs]
lgl <- lgl[complete.cases(lgl),]
lgltab <- FreeListTable(lgl, CODE = "LGL", Subj = "CERCID", tableType = "FREQUENCY")
fl_tab_fun(lgltab)

# LG angers
lgdlabs <- c("CERCID", "LGD", "Culture")
lgd <- FL[lgdlabs]
lgd <- lgd[complete.cases(lgd),]
lgdtab <- FreeListTable(lgd, CODE = "LGD", Subj = "CERCID", tableType = "FREQUENCY")
fl_tab_fun(lgdtab)

# PO pleases
pollabs <- c("CERCID", "POL", "Culture")
pol <- FL[pollabs]
pol <- pol[complete.cases(pol),]
poltab <- FreeListTable(pol, CODE = "POL", Subj = "CERCID", tableType = "FREQUENCY")
fl_tab_fun(poltab)

# PO angers
podlabs <- c("CERCID", "POD", "Culture")
pod <- FL[podlabs]
pod <- pod[complete.cases(pod),]
podtab <- FreeListTable(pod, CODE = "POD", Subj = "CERCID", tableType = "FREQUENCY")
fl_tab_fun(podtab)

#### Free-list descriptives - per site and domain

# MG pleases
for (culture in unique(bgl$Culture)){  # for each category that is unique in the culture variable
  sub <- subset(bgl, culture == bgl$Culture) # subset the data based on culture
  subtab <- FreeListTable(sub, CODE = "BGL", Subj = "CERCID", tableType = "FREQUENCY")
  print(unique(sub$Culture)) # print culture name
  fl_tab_fun(subtab)
}

# MG angers
for (culture in unique(bgd$Culture)){  # for each category that is unique in the culture variable
  sub <- subset(bgd, culture == bgd$Culture) # subset the data based on culture
  subtab <- FreeListTable(sub, CODE = "BGD", Subj = "CERCID", tableType = "FREQUENCY")
  print(unique(sub$Culture)) # print culture name
  fl_tab_fun(subtab)
}

# LG pleases
for (culture in unique(lgl$Culture)){  # for each category that is unique in the culture variable
  sub <- subset(lgl, culture == lgl$Culture) # subset the data based on culture
  subtab <- FreeListTable(sub, CODE = "LGL", Subj = "CERCID", tableType = "FREQUENCY")
  print(unique(sub$Culture)) # print culture name
  fl_tab_fun(subtab)
}

# LG angers
for (culture in unique(lgd$Culture)){  # for each category that is unique in the culture variable
  sub <- subset(lgd, culture == lgd$Culture) # subset the data based on culture
  subtab <- FreeListTable(sub, CODE = "LGD", Subj = "CERCID", tableType = "FREQUENCY")
  print(unique(sub$Culture)) # print culture name
  fl_tab_fun(subtab)
}

# Police pleases
for (culture in unique(pol$Culture)){  # for each category that is unique in the culture variable
  sub <- subset(pol, culture == pol$Culture) # subset the data based on culture
  subtab <- FreeListTable(sub, CODE = "POL", Subj = "CERCID", tableType = "FREQUENCY")
  print(unique(sub$Culture)) # print culture name
  fl_tab_fun(subtab)
}

# Police angers
for (culture in unique(pod$Culture)){  # for each category that is unique in the culture variable
  sub <- subset(pod, culture == pod$Culture) # subset the data based on culture
  subtab <- FreeListTable(sub, CODE = "POD", Subj = "CERCID", tableType = "FREQUENCY")
  print(unique(sub$Culture)) # print culture name
  fl_tab_fun(subtab)
}

### Demography

demo.labels <- c("CERCID", "SITE", "AGE", "SEX")
demo1 <- cerc[demo.labels]
demo <- setNames(demo1, c("CERCID", "SITE", "Age","SEX"))

fl.labels <- c("Culture", "CERCID", "Order", "BGL", "BGD", "LGL", "LGD", "POL", "POD")
FLsub <- FL[fl.labels]
FLsub <- FLsub[rowSums(is.na(FLsub[,4:9]))!=6,] # remove participants who did not complete at least one free-list task
FLsub.d <- FLsub[!duplicated(FLsub[, c("CERCID")]),] # remove duplicates

fl.demo <- merge(demo, FLsub.d, by = "CERCID")
fl.demo$SITE <- NULL

# Convencience function for extracting relevant demographics
demfun <- function(x) {
  N <- sum(table(x))
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  return(data.frame(N = N, Mean = mean, SD = sd, Min. = min, Max. = max))
}

# Demopgrahy loop - per culture 
for (site in unique(fl.demo$Culture)){
  demo.sub <- subset(fl.demo, site == fl.demo$Culture)
  Sextab <- table(demo.sub$SEX, exclude = NULL)
  Sex <- setNames(Sextab, c("women", "men"))
  results <- xtable(t(sapply(demo.sub[2], demfun)), digits = c(0,0,1,1,1,1), caption = site)
  print(results)
  print(Sex)
}

# Demography globally 
sapply(fl.demo[2], demfun) # total N and age
table(fl.demo$SEX, exclude = NULL) # number of women = 0s

#########################################################
### Tables 3 and 4:  Smith's S globally and per sites ###
#########################################################

### Item salience - general codes - globally

G.BGL.FL <- CalculateSalience(data, Order = "Order", Subj = "CERCID",
                              CODE = "BGL", GROUPING = NA, Rescale = FALSE, Salience = "G.BGL.S")
G.BGD.FL <- CalculateSalience(G.BGL.FL, Order = "Order", Subj = "CERCID",
                              CODE = "BGD", GROUPING = NA, Rescale = FALSE, Salience = "G.BGD.S")
G.LGL.FL <- CalculateSalience(G.BGD.FL, Order = "Order", Subj = "CERCID",
                              CODE = "LGL", GROUPING = NA, Rescale = FALSE, Salience = "G.LGL.S")
G.LGD.FL <- CalculateSalience(G.LGL.FL, Order = "Order", Subj = "CERCID",
                              CODE = "LGD", GROUPING = NA, Rescale = FALSE, Salience = "G.LGD.S")
G.POL.FL <- CalculateSalience(G.LGD.FL, Order = "Order", Subj = "CERCID",
                              CODE = "POL", GROUPING = NA, Rescale = FALSE, Salience = "G.POL.S")
G.POD.FL <- CalculateSalience(G.POL.FL, Order = "Order", Subj = "CERCID",
                              CODE = "POD", GROUPING = NA, Rescale = FALSE, Salience = "G.POD.S")

G.FL <- G.POD.FL

### Salience by code - general codes - globally

G.BGL.FL.S <- SalienceByCode(G.FL, Subj = "CERCID", CODE = "BGL", GROUPING = NA,
                             Salience = "G.BGL.S", dealWithDoubles = "MAX")

G.BGD.FL.S <- SalienceByCode(G.FL, Subj = "CERCID", CODE = "BGD", GROUPING = NA,
                             Salience = "G.BGD.S", dealWithDoubles = "MAX")

G.LGL.FL.S <- SalienceByCode(G.FL, Subj = "CERCID", CODE = "LGL", GROUPING = NA,
                             Salience = "G.LGL.S", dealWithDoubles = "MAX")

G.LGD.FL.S <- SalienceByCode(G.FL, Subj = "CERCID", CODE = "LGD", GROUPING = NA,
                             Salience = "G.LGD.S", dealWithDoubles = "MAX")

G.POL.FL.S <- SalienceByCode(G.FL, Subj = "CERCID", CODE = "POL", GROUPING = NA,
                             Salience = "G.POL.S", dealWithDoubles = "MAX")

G.POD.FL.S <- SalienceByCode(G.FL, Subj = "CERCID", CODE = "POD", GROUPING = NA,
                             Salience = "G.POD.S", dealWithDoubles = "MAX")

### Salience by code - general codes - per site
BGL.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "BGL", GROUPING = "Culture",
                           Salience = "BGL.S", dealWithDoubles = "MAX")

BGD.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "BGD", GROUPING = "Culture",
                           Salience = "BGD.S", dealWithDoubles = "MAX")

LGL.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "LGL", GROUPING = "Culture",
                           Salience = "LGL.S", dealWithDoubles = "MAX")

LGD.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "LGD", GROUPING = "Culture",
                           Salience = "LGD.S", dealWithDoubles = "MAX")

POL.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "POL", GROUPING = "Culture",
                           Salience = "POL.S", dealWithDoubles = "MAX")

POD.FL.S <- SalienceByCode(FL, Subj = "CERCID", CODE = "POD", GROUPING = "Culture",
                           Salience = "POD.S", dealWithDoubles = "MAX")

### Table 3: LaTeX salience tables globally, general codes

# Sort and cut free-list outputs for global outputs
G.sortnaxe.g <- function(x, y){ # x = FL object, y = threshold
  step1 <- x[order(-x$SmithsS),]
  step2 <- step1[(step1$SmithsS >= y),]
  return(step2)
}

G.cutoff <- .095 # change Smith's S cutoff value

G.columns <- c("Code", "M Salience", "S Salience", "Smith's S", "n")

G.BGL.FL.S$n <- G.BGL.FL.S$SumSalience/G.BGL.FL.S$MeanSalience # number of times each code is listed per site
G.BGL.SORT <- G.sortnaxe.g(G.BGL.FL.S, G.cutoff) # sorts and cuts Smith's S below cutoff
G.BGL.S.tab <- G.BGL.SORT[!grepl("D/K", G.BGL.SORT$CODE),] # removes "D/K"s
colnames(G.BGL.S.tab) <- G.columns # column names for the table
View(G.BGL.S.tab)
print(xtable(G.BGL.S.tab, digits=c(0,0,2,2,2,0), caption = "Moralistic gods -- General -- Likes -- Global"), include.rownames = FALSE) # The digits command removes decimals in the sixth row (as well as the first and the second). It specifies the number of digits for each column.

G.BGD.FL.S$n <- G.BGD.FL.S$SumSalience/G.BGD.FL.S$MeanSalience
G.BGD.SORT <- G.sortnaxe.g(G.BGD.FL.S, G.cutoff)
G.BGD.S.tab <- G.BGD.SORT[!grepl("D/K", G.BGD.SORT$CODE),]
colnames(G.BGD.S.tab) <- G.columns
View(G.BGD.S.tab)
print(xtable(G.BGD.S.tab, digits=c(0,0,2,2,2,0), caption = "Moralistic gods -- General -- Dislikes -- Global"), include.rownames = FALSE)

G.LGL.FL.S$n <- G.LGL.FL.S$SumSalience/G.LGL.FL.S$MeanSalience
G.LGL.SORT <- G.sortnaxe.g(G.LGL.FL.S, G.cutoff)
G.LGL.S.tab <- G.LGL.SORT[!grepl("D/K", G.LGL.SORT$CODE),]
colnames(G.LGL.S.tab) <- G.columns
View(G.LGL.S.tab)
print(xtable(G.LGL.S.tab, digits=c(0,0,2,2,2,0), caption = "Local gods -- General -- Likes -- Global"), include.rownames = FALSE)

G.LGD.FL.S$n <- G.LGD.FL.S$SumSalience/G.LGD.FL.S$MeanSalience
G.LGD.SORT <- G.sortnaxe.g(G.LGD.FL.S, G.cutoff)
G.LGD.S.tab <- G.LGD.SORT[!grepl("D/K", G.LGD.SORT$CODE),]
colnames(G.LGD.S.tab) <- G.columns
View(G.LGD.S.tab)
print(xtable(G.LGD.S.tab, digits=c(0,0,2,2,2,0), caption = "Local gods -- General -- Dislikes -- Global"), include.rownames = FALSE)

G.POL.FL.S$n <- G.POL.FL.S$SumSalience/G.POL.FL.S$MeanSalience
G.POL.SORT <- G.sortnaxe.g(G.POL.FL.S, G.cutoff)
G.POL.S.tab <- G.POL.SORT[!grepl("D/K", G.POL.SORT$CODE),]
colnames(G.POL.S.tab) <- G.columns
View(G.POL.S.tab)
print(xtable(G.POL.S.tab, digits=c(0,0,2,2,2,0), caption = "Police -- General -- Likes -- Global"), include.rownames = FALSE)

G.POD.FL.S$n <- G.POD.FL.S$SumSalience/G.POD.FL.S$MeanSalience
G.POD.SORT <- G.sortnaxe.g(G.POD.FL.S, G.cutoff)
G.POD.S.tab <- G.POD.SORT[!grepl("D/K", G.POD.SORT$CODE),]
colnames(G.POD.S.tab) <- G.columns
View(G.POD.S.tab)
print(xtable(G.POD.S.tab, digits=c(0,0,2,2,2,0), caption = "Police -- General -- Dislikes -- Global"), include.rownames = FALSE)

### Table 4: LaTeX salience tables for each site, general codes ###

# Table 4 in the main text reports the top-most salient code (Smith's) and 
# number of individuals mentioning that code (n) for each site.

# Sort and cut free-list outputs with grouping vars
sortnaxe.g <- function(x, y){ # x = FL object, y = threshold
  step1 <- x[order(as.character(x$GROUPING), -x$SmithsS),]
  step2 <- step1[(step1$SmithsS >= y),]
  return(step2)
}

cutoff <- .049 # change Smith's S cutoff value

columns <- c("Culture", "Code", "M Salience", "S Salience", "Smith's S", "n", "N")

BGL.FL.S$n <- BGL.FL.S$SumSalience/BGL.FL.S$MeanSalience # number of times each code is listed per site
BGL.FL.S$N <- BGL.FL.S$SumSalience/BGL.FL.S$SmithsS # total number of free-list participants per site and free-list domain
BGL.SORT <- sortnaxe.g(BGL.FL.S, cutoff) # sorts and cuts Smith's S below cutoff value
BGL.S.tab <- BGL.SORT[!grepl("D/K", BGL.SORT$CODE),] # removes "D/K"s
colnames(BGL.S.tab) <- columns # column names for the table
View(BGL.S.tab)
print(xtable(BGL.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Moralistic Gods -- General -- Likes"), include.rownames = FALSE) # The digits command controls the number of digits for each column.

BGD.FL.S$n <- BGD.FL.S$SumSalience/BGD.FL.S$MeanSalience
BGD.FL.S$N <- BGD.FL.S$SumSalience/BGD.FL.S$SmithsS
BGD.SORT <- sortnaxe.g(BGD.FL.S, cutoff)
BGD.S.tab <- BGD.SORT[!grepl("D/K", BGD.SORT$CODE),]
colnames(BGD.S.tab) <- columns
View(BGD.S.tab)
print(xtable(BGD.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Moralistic Gods -- General -- Dislikes"), include.rownames = FALSE)

LGL.FL.S$n <- LGL.FL.S$SumSalience/LGL.FL.S$MeanSalience
LGL.FL.S$N <- LGL.FL.S$SumSalience/LGL.FL.S$SmithsS
LGL.SORT <- sortnaxe.g(LGL.FL.S, cutoff)
LGL.S.tab <- LGL.SORT[!grepl("D/K", LGL.SORT$CODE),]
colnames(LGL.S.tab) <- columns
View(LGL.S.tab)
print(xtable(LGL.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Local Gods -- General -- Likes"), include.rownames = FALSE)

LGD.FL.S$n <- LGD.FL.S$SumSalience/LGD.FL.S$MeanSalience
LGD.FL.S$N <- LGD.FL.S$SumSalience/LGD.FL.S$SmithsS
LGD.SORT <- sortnaxe.g(LGD.FL.S, cutoff)
LGD.S.tab <- LGD.SORT[!grepl("D/K", LGD.SORT$CODE),]
colnames(LGD.S.tab) <- columns
View(LGD.S.tab)
print(xtable(LGD.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Local Gods -- General -- Dislikes"), include.rownames = FALSE)

POL.FL.S$n <- POL.FL.S$SumSalience/POL.FL.S$MeanSalience
POL.FL.S$N <- POL.FL.S$SumSalience/POL.FL.S$SmithsS
POL.SORT <- sortnaxe.g(POL.FL.S, cutoff)
POL.S.tab <- POL.SORT[!grepl("D/K", POL.SORT$CODE),]
colnames(POL.S.tab) <- columns
View(POL.S.tab)
print(xtable(POL.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Police -- General -- Likes"), include.rownames = FALSE)

POD.FL.S$n <- POD.FL.S$SumSalience/POD.FL.S$MeanSalience
POD.FL.S$N <- POD.FL.S$SumSalience/POD.FL.S$SmithsS
POD.SORT <- sortnaxe.g(POD.FL.S, cutoff)
POD.S.tab <- POD.SORT[!grepl("D/K", POD.SORT$CODE),]
colnames(POD.S.tab) <- columns
View(POD.S.tab)
print(xtable(POD.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Police -- General -- Dislikes"), include.rownames = FALSE)

#############################################################################################################
### Table 5: LaTex Tables for most salient specific codes globally for the moralistic gods and the police ###
#############################################################################################################

### Store the free-list data in new objects, so that it is easy to shift between coders.
### For the present study, we take NC's specific codes that were subsequently cleaned by TB ("..._TB_cl") 
### and stored in separate columns for full transparency (see Supplements for details).

FL.SPEC.L <- data
FL.SPEC.L$BGL_SPEC.L <- FL.SPEC.L$BGL_SPEC_TB_cl
FL.SPEC.L$BGD_SPEC.L <- FL.SPEC.L$BGD_SPEC_TB_cl
FL.SPEC.L$LGL_SPEC.L <- FL.SPEC.L$LGL_SPEC_TB_cl
FL.SPEC.L$LGD_SPEC.L <- FL.SPEC.L$LGD_SPEC_TB_cl
FL.SPEC.L$POL_SPEC.L <- FL.SPEC.L$POL_SPEC_TB_cl
FL.SPEC.L$POD_SPEC.L <- FL.SPEC.L$POD_SPEC_TB_cl

### Item salience - specifics - globally

G.BGL.SPEC.FL <- CalculateSalience(FL.SPEC.L, Order = "Order", Subj = "CERCID",
                                   CODE = "BGL_SPEC.L", Rescale = FALSE, Salience = "BGL.SPEC.S")
G.BGD.SPEC.FL <- CalculateSalience(G.BGL.SPEC.FL, Order = "Order", Subj = "CERCID",
                                   CODE = "BGD_SPEC.L", Rescale = FALSE, Salience = "BGD.SPEC.S")
G.LGL.SPEC.FL <- CalculateSalience(G.BGD.SPEC.FL, Order = "Order", Subj = "CERCID",
                                   CODE = "LGL_SPEC.L", Rescale = FALSE, Salience = "LGL.SPEC.S")
G.LGD.SPEC.FL <- CalculateSalience(G.LGL.SPEC.FL, Order = "Order", Subj = "CERCID",
                                   CODE = "LGD_SPEC.L", Rescale = FALSE, Salience = "LGD.SPEC.S")
G.POL.SPEC.FL <- CalculateSalience(G.LGD.SPEC.FL, Order = "Order", Subj = "CERCID",
                                   CODE = "POL_SPEC.L", Rescale = FALSE, Salience = "POL.SPEC.S")
G.POD.SPEC.FL <- CalculateSalience(G.POL.SPEC.FL, Order = "Order", Subj = "CERCID",
                                   CODE = "POD_SPEC.L", Rescale = FALSE, Salience = "POD.SPEC.S")

G.FL.SPEC.S <- G.POD.SPEC.FL

### Salience by code - specifics - globally

G.BGL.SPEC.FL.S <- SalienceByCode(G.FL.SPEC.S, Subj = "CERCID", CODE = "BGL_SPEC.L",
                                  Salience = "BGL.SPEC.S", dealWithDoubles = "MAX")

G.BGD.SPEC.FL.S <- SalienceByCode(G.FL.SPEC.S, Subj = "CERCID", CODE = "BGD_SPEC.L",
                                  Salience = "BGD.SPEC.S", dealWithDoubles = "MAX")

G.LGL.SPEC.FL.S <- SalienceByCode(G.FL.SPEC.S, Subj = "CERCID", CODE = "LGL_SPEC.L",
                                  Salience = "LGL.SPEC.S", dealWithDoubles = "MAX")

G.LGD.SPEC.FL.S <- SalienceByCode(G.FL.SPEC.S, Subj = "CERCID", CODE = "LGD_SPEC.L",
                                  Salience = "LGD.SPEC.S", dealWithDoubles = "MAX")

G.POL.SPEC.FL.S <- SalienceByCode(G.FL.SPEC.S, Subj = "CERCID", CODE = "POL_SPEC.L",
                                  Salience = "POL.SPEC.S", dealWithDoubles = "MAX")

G.POD.SPEC.FL.S <- SalienceByCode(G.FL.SPEC.S, Subj = "CERCID", CODE = "POD_SPEC.L",
                                  Salience = "POD.SPEC.S", dealWithDoubles = "MAX")

### Latex Tables

# Sort and cut free-list outputs for global outputs
G.sortnaxe.g <- function(x, y){ # x = FL object, y = threshold
  step1 <- x[order(-x$SmithsS),]
  step2 <- step1[(step1$SmithsS >= y),]
  return(step2)
}

G.SPEC.cutoff <- .049 # change Smith's S cutoff value

G.columns <- c("Code", "M Salience", "S Salience", "Smith's S", "n")

G.BGL.SPEC.FL.S$n <- G.BGL.SPEC.FL.S$SumSalience/G.BGL.SPEC.FL.S$MeanSalience # number of times each code is listed per site
G.BGL.SPEC.SORT <- G.sortnaxe.g(G.BGL.SPEC.FL.S, G.SPEC.cutoff) # sorts and cuts Smith's S below cutoff
G.BGL.SPEC.S.tab <- G.BGL.SPEC.SORT[!grepl("D/K", G.BGL.SPEC.SORT$CODE),] # removes "D/K"s
colnames(G.BGL.SPEC.S.tab) <- G.columns # column names for the table
View(G.BGL.SPEC.S.tab)
print(xtable(G.BGL.SPEC.S.tab, digits=c(0,0,2,2,2,0), caption = "Moralistic gods -- Specific -- Likes -- Global"), include.rownames = FALSE) # The digits command removes decimals in the sixth row (as well as the first and the second). It specifies the number of digits for each column.

G.BGD.SPEC.FL.S$n <- G.BGD.SPEC.FL.S$SumSalience/G.BGD.SPEC.FL.S$MeanSalience
G.BGD.SPEC.SORT <- G.sortnaxe.g(G.BGD.SPEC.FL.S, G.SPEC.cutoff)
G.BGD.SPEC.S.tab <- G.BGD.SPEC.SORT[!grepl("D/K", G.BGD.SPEC.SORT$CODE),]
colnames(G.BGD.SPEC.S.tab) <- G.columns
View(G.BGD.SPEC.S.tab)
print(xtable(G.BGD.SPEC.S.tab, digits=c(0,0,2,2,2,0), caption = "Moralistic gods -- Specific -- Dislikes -- Global"), include.rownames = FALSE)

G.LGL.SPEC.FL.S$n <- G.LGL.SPEC.FL.S$SumSalience/G.LGL.SPEC.FL.S$MeanSalience
G.LGL.SPEC.SORT <- G.sortnaxe.g(G.LGL.SPEC.FL.S, G.SPEC.cutoff)
G.LGL.SPEC.S.tab <- G.LGL.SPEC.SORT[!grepl("D/K", G.LGL.SPEC.SORT$CODE),]
colnames(G.LGL.SPEC.S.tab) <- G.columns
View(G.LGL.SPEC.S.tab)
print(xtable(G.LGL.SPEC.S.tab, digits=c(0,0,2,2,2,0), caption = "Local gods -- Specific -- Likes -- Global"), include.rownames = FALSE)

G.LGD.SPEC.FL.S$n <- G.LGD.SPEC.FL.S$SumSalience/G.LGD.SPEC.FL.S$MeanSalience
G.LGD.SPEC.SORT <- G.sortnaxe.g(G.LGD.SPEC.FL.S, G.SPEC.cutoff)
G.LGD.SPEC.S.tab <- G.LGD.SPEC.SORT[!grepl("D/K", G.LGD.SPEC.SORT$CODE),]
colnames(G.LGD.SPEC.S.tab) <- G.columns
View(G.LGD.SPEC.S.tab)
print(xtable(G.LGD.SPEC.S.tab, digits=c(0,0,2,2,2,0), caption = "Local gods -- Specific -- Dislikes -- Global"), include.rownames = FALSE)

G.POL.SPEC.FL.S$n <- G.POL.SPEC.FL.S$SumSalience/G.POL.SPEC.FL.S$MeanSalience
G.POL.SPEC.SORT <- G.sortnaxe.g(G.POL.SPEC.FL.S, G.SPEC.cutoff)
G.POL.SPEC.S.tab <- G.POL.SPEC.SORT[!grepl("D/K", G.POL.SPEC.SORT$CODE),]
colnames(G.POL.SPEC.S.tab) <- G.columns
View(G.POL.SPEC.S.tab)
print(xtable(G.POL.SPEC.S.tab, digits=c(0,0,2,2,2,0), caption = "Police -- Specific -- Likes -- Global"), include.rownames = FALSE)

G.POD.SPEC.FL.S$n <- G.POD.SPEC.FL.S$SumSalience/G.POD.SPEC.FL.S$MeanSalience
G.POD.SPEC.SORT <- G.sortnaxe.g(G.POD.SPEC.FL.S, G.SPEC.cutoff)
G.POD.SPEC.S.tab <- G.POD.SPEC.SORT[!grepl("D/K", G.POD.SPEC.SORT$CODE),]
colnames(G.POD.SPEC.S.tab) <- G.columns
View(G.POD.SPEC.S.tab)
print(xtable(G.POD.SPEC.S.tab, digits=c(0,0,2,2,2,0), caption = "Police -- Specific -- Dislikes -- Global"), include.rownames = FALSE)

#####################################################################################################
### Figure 1: Barplots of Smith's S for all general codes across sites for what angers the agents ###
#####################################################################################################

### Prepare data frame with Smith' S for general codings for each site for moralistic gods - dislikes (BGD)
Tab.BGD <- as.data.frame(matrix(ncol = 12, nrow = 8))

colnames(Tab.BGD) <- c("GROUPING", "D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
                       "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

Tab.BGD$GROUPING <- c("Coastal Tanna", "Hadza",
                      "Inland Tanna","Lovu Fiji","Marajó", "Mauritius", 
                      "Tyva Republic", "Yasawa Fiji")

### Extract Smith's S for general codings of Moralistic Gods, Dislikes
DK.BGD <- subset(BGD.FL.S, CODE == "D/K") ; DK.BGD <- DK.BGD[order(DK.BGD$GROUPING),]
ECO.BGD <- subset(BGD.FL.S, CODE == "Ecology") ; ECO.BGD <- ECO.BGD[order(ECO.BGD$GROUPING),]
ETI.BGD <- subset(BGD.FL.S, CODE == "Etiquette") ; ETI.BGD <- ETI.BGD[order(ETI.BGD$GROUPING),]
FOOD.BGD <- subset(BGD.FL.S, CODE == "Food") ; FOOD.BGD <- FOOD.BGD[order(FOOD.BGD$GROUPING),]
MISC.BGD <- subset(BGD.FL.S, CODE == "Miscellaneous") ; MISC.BGD <- MISC.BGD[order(MISC.BGD$GROUPING),]
MOR.BGD <- subset(BGD.FL.S, CODE == "Morality") ; MOR.BGD <- MOR.BGD[order(MOR.BGD$GROUPING),]
PEOP.BGD <- subset(BGD.FL.S, CODE == "People") ; PEOP.BGD <- PEOP.BGD[order(PEOP.BGD$GROUPING),]
REL.BGD <- subset(BGD.FL.S, CODE == "Religion") ; REL.BGD <- REL.BGD[order(REL.BGD$GROUPING),]
RIT.BGD <- subset(BGD.FL.S, CODE == "Ritual") ; RIT.BGD <- RIT.BGD[order(RIT.BGD$GROUPING),]
SUBST.BGD <- subset(BGD.FL.S, CODE == "Substance Use/Abuse") ; SUBST.BGD <- SUBST.BGD[order(SUBST.BGD$GROUPING),]
VIRT.BGD <- subset(BGD.FL.S, CODE == "Virtue") ; VIRT.BGD <- VIRT.BGD[order(VIRT.BGD$GROUPING),]

### Insert extracted variables into Tab.BGD dataframe
Tab.BGD$"D/K" <- DK.BGD$SmithsS
Tab.BGD$Ecology <- ECO.BGD$SmithsS
Tab.BGD$Etiquette <- ETI.BGD$SmithsS
Tab.BGD$Food <- FOOD.BGD$SmithsS # Does not feature in BGD
Tab.BGD$Miscellaneous <- MISC.BGD$SmithsS
Tab.BGD$Morality <- MOR.BGD$SmithsS
Tab.BGD$People <- PEOP.BGD$SmithsS
Tab.BGD$Religion <- REL.BGD$SmithsS
Tab.BGD$Ritual <- RIT.BGD$SmithsS
Tab.BGD$"Substance Use/Abuse" <- SUBST.BGD$SmithsS
Tab.BGD$Virtue <- VIRT.BGD$SmithsS

### Prepare dataframe with salience for general codings for each site for local gods - dislikes (LGD)
Tab.LGD <- as.data.frame(matrix(ncol = 12, nrow = 7))

colnames(Tab.LGD) <- c("GROUPING", "D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
                       "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

Tab.LGD$GROUPING <- c("Coastal Tanna", "Hadza",
                      "Inland Tanna","Marajo", "Mauritius", 
                      "Tyva Republic", "Yasawa Fiji") # No Local Gods were identified for Lovu

### Extract Smith's S for general codings of Local Gods, Dislikes
DK.LGD <- subset(LGD.FL.S, CODE == "D/K") ; DK.LGD <- DK.LGD[order(DK.LGD$GROUPING),]
ECO.LGD <- subset(LGD.FL.S, CODE == "Ecology") ; ECO.LGD <- ECO.LGD[order(ECO.LGD$GROUPING),]
ETI.LGD <- subset(LGD.FL.S, CODE == "Etiquette") ; ETI.LGD <- ETI.LGD[order(ETI.LGD$GROUPING),]
FOOD.LGD <- subset(LGD.FL.S, CODE == "Food") ; FOOD.LGD <- FOOD.LGD[order(FOOD.LGD$GROUPING),]
MISC.LGD <- subset(LGD.FL.S, CODE == "Miscellaneous") ; MISC.LGD <- MISC.LGD[order(MISC.LGD$GROUPING),]
MOR.LGD <- subset(LGD.FL.S, CODE == "Morality") ; MOR.LGD <- MOR.LGD[order(MOR.LGD$GROUPING),]
PEOP.LGD <- subset(LGD.FL.S, CODE == "People") ; PEOP.LGD <- PEOP.LGD[order(PEOP.LGD$GROUPING),]
REL.LGD <- subset(LGD.FL.S, CODE == "Religion") ; REL.LGD <- REL.LGD[order(REL.LGD$GROUPING),]
RIT.LGD <- subset(LGD.FL.S, CODE == "Ritual") ; RIT.LGD <- RIT.LGD[order(RIT.LGD$GROUPING),]
SUBST.LGD <- subset(LGD.FL.S, CODE == "Substance Use/Abuse") ; SUBST.LGD <- SUBST.LGD[order(SUBST.LGD$GROUPING),]
VIRT.LGD <- subset(LGD.FL.S, CODE == "Virtue") ; VIRT.LGD <- VIRT.LGD[order(VIRT.LGD$GROUPING),]

### Insert extracted variables into Tab.LGD dataframe
Tab.LGD$"D/K" <- DK.LGD$SmithsS
Tab.LGD$Ecology <- ECO.LGD$SmithsS
Tab.LGD$Etiquette <- ETI.LGD$SmithsS
Tab.LGD$Food <- FOOD.LGD$SmithsS
Tab.LGD$Miscellaneous <- MISC.LGD$SmithsS
Tab.LGD$Morality <- MOR.LGD$SmithsS
Tab.LGD$People <- PEOP.LGD$SmithsS
Tab.LGD$Religion <- REL.LGD$SmithsS
Tab.LGD$Ritual <- RIT.LGD$SmithsS
Tab.LGD$"Substance Use/Abuse" <- SUBST.LGD$SmithsS
Tab.LGD$Virtue <- VIRT.LGD$SmithsS

# Add dummy variables for Lovu Fiji
Tab.LGD <- add_row(Tab.LGD, .before = 4)

Tab.LGD[is.na(Tab.LGD)] <- 0

### Prepare dataframe with salience for general codings for each site for police - dislikes (POD)
Tab.POD <- as.data.frame(matrix(ncol = 12, nrow = 8))

colnames(Tab.POD) <- c("GROUPING", "D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
                       "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

Tab.POD$GROUPING <- c("Coastal Tanna", "Hadza",
                      "Inland Tanna","Lovu Fiji","Marajó", "Mauritius", 
                      "Tyva Republic", "Yasawa Fiji")

### Extract Smith's S for general codings of Police, Dislikes
DK.POD <- subset(POD.FL.S, CODE == "D/K") ; DK.POD <- DK.POD[order(DK.POD$GROUPING),]
ECO.POD <- subset(POD.FL.S, CODE == "Ecology") ; ECO.POD <- ECO.POD[order(ECO.POD$GROUPING),]
ETI.POD <- subset(POD.FL.S, CODE == "Etiquette") ; ETI.POD <- ETI.POD[order(ETI.POD$GROUPING),]
FOOD.POD <- subset(POD.FL.S, CODE == "Food") ; FOOD.POD <- FOOD.POD[order(FOOD.POD$GROUPING),]
MISC.POD <- subset(POD.FL.S, CODE == "Miscellaneous") ; MISC.POD <- MISC.POD[order(MISC.POD$GROUPING),]
MOR.POD <- subset(POD.FL.S, CODE == "Morality") ; MOR.POD <- MOR.POD[order(MOR.POD$GROUPING),]
PEOP.POD <- subset(POD.FL.S, CODE == "People") ; PEOP.POD <- PEOP.POD[order(PEOP.POD$GROUPING),]
REL.POD <- subset(POD.FL.S, CODE == "Religion") ; REL.POD <- REL.POD[order(REL.POD$GROUPING),]
RIT.POD <- subset(POD.FL.S, CODE == "Ritual") ; RIT.POD <- RIT.POD[order(RIT.POD$GROUPING),]
SUBST.POD <- subset(POD.FL.S, CODE == "Substance Use/Abuse") ; SUBST.POD <- SUBST.POD[order(SUBST.POD$GROUPING),]
VIRT.POD <- subset(POD.FL.S, CODE == "Virtue") ; VIRT.POD <- VIRT.POD[order(VIRT.POD$GROUPING),]

### Insert extracted variables into Tab.POD dataframe
Tab.POD$"D/K" <- DK.POD$SmithsS
Tab.POD$Ecology <- ECO.POD$SmithsS
Tab.POD$Etiquette <- ETI.POD$SmithsS
Tab.POD$Food <- FOOD.POD$SmithsS # Does not feature in POD
Tab.POD$Miscellaneous <- MISC.POD$SmithsS
Tab.POD$Morality <- MOR.POD$SmithsS
Tab.POD$People <- PEOP.POD$SmithsS
Tab.POD$Religion <- REL.POD$SmithsS # Does not feature in POD
Tab.POD$Ritual <- RIT.POD$SmithsS
Tab.POD$"Substance Use/Abuse" <- SUBST.POD$SmithsS
Tab.POD$Virtue <- VIRT.POD$SmithsS

### Print to pdf - start. Ends with dev.off() 
pdf("HistogramPanel.pdf", width=10, height=7) 

### set up panel, mar = c(b, l, t, r) 
par(mfrow = c(3,1), mar = c(3, 4, 2, .05))

### X axis labels (all general codes, with abbreviations)
xaxis <- c("D/K", "Ecology", "Etiquette", "Food", "Misc.", "Morality", 
           "People", "Religion", "Ritual", "Drugs", "Virtue")

### Plotting BGD with Codings on X-axis
codelabs <- c("D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
               "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

TabBGD <- t(Tab.BGD[codelabs])

TabBGD1 <- t(TabBGD)

TabBGD1[is.na(TabBGD1)] <- 0 # Food is not listed by any participants in this domain. These NAs are turned into 0s for visualization purposes.

rownames(TabBGD1) <- c("Coastal Tanna", "Hadza",
                       "Inland Tanna","Lovu Fiji", "Marajó", "Mauritius", 
                       "Tyva Republic", "Yasawa Fiji")

barplot(TabBGD1, beside = TRUE,
        names.arg = NULL, ylim = c(0,1), axisnames = F, col = grey((1:8)/8)) 

axis(side = 1, padj = 0.01, labels = xaxis, 
     at = c(5, 14, 23, 32, 41, 50, 59, 68, 77, 86, 95), gap.axis = NA, tick = F, cex.axis = 1.4) # tick marks
axis(side = 2, labels = "Smith's S", at = 0.5, tick = F, cex.axis = 1.3, line = 1.5)
text(20, .90, "What angers the moralistic gods?", font = 2, cex = 1.5)

### Plotting LGD with Codings on X-axis
TabLGD <- t(Tab.LGD[codelabs])

TabLGD1 <- t(TabLGD)

rownames(TabLGD1) <- c("Coastal Tanna", "Hadza",
                       "Inland Tanna","Lovu Fiji","Marajo", "Mauritius", 
                       "Tyva Republic", "Yasawa Fiji")

barplot(TabLGD1, beside = TRUE,
        names.arg = NULL, ylim = c(0,1), axisnames = F, col = grey((1:8)/8)) 

axis(side = 1, padj = 0.01, labels = xaxis, 
     at = c(5, 14, 23, 32, 41, 50, 59, 68, 77, 86, 95), gap.axis = NA, tick = F, cex.axis = 1.4) # tick marks
text(18, .90, "What angers the local gods?", font = 2, cex = 1.5)
axis(side = 2, labels = "Smith's S", at = 0.5, tick = F, cex.axis = 1.3, line = 1.5)

ax <- 1.75 # size of asterisks
text(4.5, .02, "*", cex = ax)
text(13.5, .02, "*", cex = ax)
text(22.5, .02, "*", cex = ax)
text(31.5, .02, "*", cex = ax)
text(40.5, .02, "*", cex = ax)
text(49.5, .02, "*", cex = ax)
text(58.5, .02, "*", cex = ax)
text(67.5, .02, "*", cex = ax)
text(76.5, .02, "*", cex = ax)
text(85.5, .02, "*", cex = ax)
text(94.5, .02, "*", cex = ax)
text(103.5, .02, "*", cex = ax)

### Plotting POD with Codings on X-axis
TabPOD <- t(Tab.POD[codelabs])

TabPOD1 <- t(TabPOD)

rownames(TabPOD1) <- c("Coastal Tanna", "Hadza",
                       "Inland Tanna","Lovu Fiji","Marajó", "Mauritius", 
                       "Tyva Republic", "Yasawa Fiji")

TabPOD1[is.na(TabPOD1)] <- 0 # Food and Religion are not listed by any participants in this domain. These NAs are turned into 0s for visualization purposes.

barplot(TabPOD1, beside = TRUE,
        names.arg = NULL, ylim = c(0,1), axisnames = F, col = grey((1:8)/8)) 

legend(66, 1, rownames(TabBGD1), fill = grey((1:8)/8),
       cex = 1.1, pt.cex = 4, bty = "n", y.intersp = 1.1)

axis(side = 1, padj = 0.01, labels = xaxis, 
     at = c(5, 14, 23, 32, 41, 50, 59, 68, 77, 86, 95), gap.axis = NA, tick = F, cex.axis = 1.4) # tick marks
axis(side = 2, labels = "Smith's S", at = 0.5, tick = F, cex.axis = 1.3, line = 1.5)
text(16, .90, "What angers the police?", font = 2, cex = 1.5)

# Print
dev.off()

###############################
### Supplementary Materials ###
###############################

#########################################
### Table S1: Inter-rater Reliability ###
#########################################

DIS_BGL <- as.data.frame(
  table(data$Culture, data$DIS_BGL_AB)[,1]/(table(data$Culture, data$DIS_BGL_AB)[,1]+table(data$Culture, data$DIS_BGL_AB)[,2]))
DIS_BGD <- as.data.frame(
  table(data$Culture, data$DIS_BGD_AB)[,1]/(table(data$Culture, data$DIS_BGD_AB)[,1]+table(data$Culture, data$DIS_BGD_AB)[,2]))
DIS_LGL <- as.data.frame(
  table(data$Culture, data$DIS_LGL)[,1]/(table(data$Culture, data$DIS_LGL)[,1]+table(data$Culture, data$DIS_LGL)[,2]))
DIS_LGD <- as.data.frame(
  table(data$Culture, data$DIS_LGD)[,1]/(table(data$Culture, data$DIS_LGD)[,1]+table(data$Culture, data$DIS_LGD)[,2]))
DIS_POL <- as.data.frame(
  table(data$Culture, data$DIS_POL)[,1]/(table(data$Culture, data$DIS_POL)[,1]+table(data$Culture, data$DIS_POL)[,2]))
DIS_POD <- as.data.frame(
  table(data$Culture, data$DIS_POD)[,1]/(table(data$Culture, data$DIS_POD)[,1]+table(data$Culture, data$DIS_POD)[,2]))

RelTab <- data.frame(Culture = row.names(DIS_BGL), BGL = DIS_BGL[,1], BGD = DIS_BGD[,1], LGL = DIS_LGL[,1],
                     LGD = DIS_LGD[,1], POL = DIS_POL[,1], POD = DIS_POD[,1])

print(xtable(RelTab), include.rownames=FALSE)

#################################################
### Table S2: BGP's disagreements with coders ###
#################################################

# tabulates disagreements, with NAs in code excluded
DIS_BGL_BGP_d <- with(data[!is.na(data$BGL),], table(Culture, DIS_BGL_BGP))
DIS_BGD_BGP_d <- with(data[!is.na(data$BGD),], table(Culture, DIS_BGD_BGP))
DIS_LGL_BGP_d <- with(data[!is.na(data$LGL),], table(Culture, DIS_LGL_BGP))
DIS_LGD_BGP_d <- with(data[!is.na(data$LGD),], table(Culture, DIS_LGD_BGP))
DIS_POL_BGP_d <- with(data[!is.na(data$POL),], table(Culture, DIS_POL_BGP))
DIS_POD_BGP_d <- with(data[!is.na(data$POD),], table(Culture, DIS_POD_BGP))

RelTab_BGP_list <- list(
                   Culture = row.names(DIS_BGL_BGP_d),
                   BGL = paste0(DIS_BGL_BGP_d[,2], "/", DIS_BGL_BGP_d[,1]),
                   
                   BGD = paste0(DIS_BGD_BGP_d[,2], "/", DIS_BGD_BGP_d[,2]+DIS_BGD_BGP_d[,1]),
                   
                   LGL = paste0(DIS_LGL_BGP_d[,2], "/", DIS_LGL_BGP_d[,2]+DIS_LGL_BGP_d[,1]),
                   
                   LGD = paste0(DIS_LGD_BGP_d[,2], "/", DIS_LGD_BGP_d[,2]+DIS_LGD_BGP_d[,1]),
                   
                   POL = paste0(DIS_POL_BGP_d[,2], "/", DIS_POL_BGP_d[,2]+DIS_POL_BGP_d[,1]),
                   
                   POD = paste0(rep(0,8), "/", DIS_POD_BGP_d[,1])) # no disagreements in POD
                   

RelTab_BGP_list[[4]] <- append(RelTab_BGP_list[[4]], "--", after = 3)
RelTab_BGP_list[[5]] <- append(RelTab_BGP_list[[5]], "--", after = 3)

RelTab_BGP_df <- as.data.frame(RelTab_BGP_list)

RelTab_BGP_totals <- data.frame(Culture = "Total", 
                                BGL = paste0(sum(data[!is.na(data$BGL),]$DIS_BGL_BGP), "/", length(data[!is.na(data$BGL),]$DIS_BGL_BGP)),
                                BGD = paste0(sum(data[!is.na(data$BGD),]$DIS_BGD_BGP), "/", length(data[!is.na(data$BGD),]$DIS_BGD_BGP)),
                                LGL = paste0(sum(data[!is.na(data$LGL),]$DIS_LGL_BGP), "/", length(data[!is.na(data$LGL),]$DIS_LGL_BGP)),
                                LGD = paste0(sum(data[!is.na(data$LGD),]$DIS_LGD_BGP), "/", length(data[!is.na(data$LGD),]$DIS_LGD_BGP)), 
                                POL = paste0(sum(data[!is.na(data$POL),]$DIS_POL_BGP), "/", length(data[!is.na(data$POL),]$DIS_POL_BGP)), 
                                POD = paste0(sum(data[!is.na(data$POD),]$DIS_POD_BGP), "/", length(data[!is.na(data$POD),]$DIS_POD_BGP)))

RelTab_BGP <- rbind(RelTab_BGP_df, RelTab_BGP_totals)

print(xtable(as.data.frame(RelTab_BGP)), include.rownames=FALSE)

#######################################################################################################
### Figure S2: Barplots of Smith's S for all general codes across sites for what pleases the agents ###
####################################################################################################### x[!is.na(x)]

### Prepare dataframe with salience for general codings across cultures for moralistic gods - likes (BGL)
Tab.BGL <- as.data.frame(matrix(ncol = 12, nrow = 8))

colnames(Tab.BGL) <- c("GROUPING", "D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
                       "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

Tab.BGL$GROUPING <- c("Coastal Tanna", "Hadza",
                      "Inland Tanna","Lovu Fiji","Marajó", "Mauritius", 
                      "Tyva Republic", "Yasawa Fiji")

### Extract Smith's S for general codings of Moralistic Gods, Likes
DK.BGL <- subset(BGL.FL.S, CODE == "D/K") ; DK.BGL <- DK.BGL[order(DK.BGL$GROUPING),]
ECO.BGL <- subset(BGL.FL.S, CODE == "Ecology") ; ECO.BGL <- ECO.BGL[order(ECO.BGL$GROUPING),]
ETI.BGL <- subset(BGL.FL.S, CODE == "Etiquette") ; ETI.BGL <- ETI.BGL[order(ETI.BGL$GROUPING),]
FOOD.BGL <- subset(BGL.FL.S, CODE == "Food") ; FOOD.BGL <- FOOD.BGL[order(FOOD.BGL$GROUPING),]
MISC.BGL <- subset(BGL.FL.S, CODE == "Miscellaneous") ; MISC.BGL <- MISC.BGL[order(MISC.BGL$GROUPING),]
MOR.BGL <- subset(BGL.FL.S, CODE == "Morality") ; MOR.BGL <- MOR.BGL[order(MOR.BGL$GROUPING),]
PEOP.BGL <- subset(BGL.FL.S, CODE == "People") ; PEOP.BGL <- PEOP.BGL[order(PEOP.BGL$GROUPING),]
REL.BGL <- subset(BGL.FL.S, CODE == "Religion") ; REL.BGL <- REL.BGL[order(REL.BGL$GROUPING),]
RIT.BGL <- subset(BGL.FL.S, CODE == "Ritual") ; RIT.BGL <- RIT.BGL[order(RIT.BGL$GROUPING),]
SUBST.BGL <- subset(BGL.FL.S, CODE == "Substance Use/Abuse") ; SUBST.BGL <- SUBST.BGL[order(SUBST.BGL$GROUPING),]
VIRT.BGL <- subset(BGL.FL.S, CODE == "Virtue") ; VIRT.BGL <- VIRT.BGL[order(VIRT.BGL$GROUPING),]

### Insert extracted variables into Tab.BGL dataframe
Tab.BGL$"D/K" <- DK.BGL$SmithsS
Tab.BGL$Ecology <- ECO.BGL$SmithsS
Tab.BGL$Etiquette <- ETI.BGL$SmithsS
Tab.BGL$Food <- FOOD.BGL$SmithsS
Tab.BGL$Miscellaneous <- MISC.BGL$SmithsS
Tab.BGL$Morality <- MOR.BGL$SmithsS
Tab.BGL$People <- PEOP.BGL$SmithsS
Tab.BGL$Religion <- REL.BGL$SmithsS
Tab.BGL$Ritual <- RIT.BGL$SmithsS
Tab.BGL$"Substance Use/Abuse" <- SUBST.BGL$SmithsS
Tab.BGL$Virtue <- VIRT.BGL$SmithsS

### Prepare dataframe with salience for general codings across cultures for Local Gods - Likes (LGL)
Tab.LGL <- as.data.frame(matrix(ncol = 12, nrow = 7))

colnames(Tab.LGL) <- c("GROUPING", "D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
                       "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

Tab.LGL$GROUPING <- c("Coastal Tanna", "Hadza",
                      "Inland Tanna","Marajó", "Mauritius", 
                      "Tyva Republic", "Yasawa Fiji") # No Local Gods were identified for Lovu

### Extract Smith's S for general codings of Local Gods, Likes
DK.LGL <- subset(LGL.FL.S, CODE == "D/K") ; DK.LGL <- DK.LGL[order(DK.LGL$GROUPING),]
ECO.LGL <- subset(LGL.FL.S, CODE == "Ecology") ; ECO.LGL <- ECO.LGL[order(ECO.LGL$GROUPING),]
ETI.LGL <- subset(LGL.FL.S, CODE == "Etiquette") ; ETI.LGL <- ETI.LGL[order(ETI.LGL$GROUPING),]
FOOD.LGL <- subset(LGL.FL.S, CODE == "Food") ; FOOD.LGL <- FOOD.LGL[order(FOOD.LGL$GROUPING),]
MISC.LGL <- subset(LGL.FL.S, CODE == "Miscellaneous") ; MISC.LGL <- MISC.LGL[order(MISC.LGL$GROUPING),]
MOR.LGL <- subset(LGL.FL.S, CODE == "Morality") ; MOR.LGL <- MOR.LGL[order(MOR.LGL$GROUPING),]
PEOP.LGL <- subset(LGL.FL.S, CODE == "People") ; PEOP.LGL <- PEOP.LGL[order(PEOP.LGL$GROUPING),]
REL.LGL <- subset(LGL.FL.S, CODE == "Religion") ; REL.LGL <- REL.LGL[order(REL.LGL$GROUPING),]
RIT.LGL <- subset(LGL.FL.S, CODE == "Ritual") ; RIT.LGL <- RIT.LGL[order(RIT.LGL$GROUPING),]
SUBST.LGL <- subset(LGL.FL.S, CODE == "Substance Use/Abuse") ; SUBST.LGL <- SUBST.LGL[order(SUBST.LGL$GROUPING),]
VIRT.LGL <- subset(LGL.FL.S, CODE == "Virtue") ; VIRT.LGL <- VIRT.LGL[order(VIRT.LGL$GROUPING),]

### Insert extracted variables into Tab.LGL dataframe
Tab.LGL$"D/K" <- DK.LGL$SmithsS
Tab.LGL$Ecology <- ECO.LGL$SmithsS
Tab.LGL$Etiquette <- ETI.LGL$SmithsS
Tab.LGL$Food <- FOOD.LGL$SmithsS
Tab.LGL$Miscellaneous <- MISC.LGL$SmithsS
Tab.LGL$Morality <- MOR.LGL$SmithsS
Tab.LGL$People <- PEOP.LGL$SmithsS
Tab.LGL$Religion <- REL.LGL$SmithsS
Tab.LGL$Ritual <- RIT.LGL$SmithsS
Tab.LGL$"Substance Use/Abuse" <- SUBST.LGL$SmithsS
Tab.LGL$Virtue <- VIRT.LGL$SmithsS

# Add dummy variables for Lovu Fiji
Tab.LGL <- add_row(Tab.LGL, .before = 4)

Tab.LGL[is.na(Tab.LGL)] <- 0 # NAs are turned into 0s for visualization purposes.

### Prepare dataframe with salience for general codings across cultures for Police - Likes (POL)
Tab.POL <- as.data.frame(matrix(ncol = 12, nrow = 8))

colnames(Tab.POL) <- c("GROUPING", "D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
                       "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

Tab.POL$GROUPING <- c("Coastal Tanna", "Hadza",
                      "Inland Tanna","Lovu Fiji","Marajó", "Mauritius", 
                      "Tyva Republic", "Yasawa Fiji")

### Extract Smith's S for general codings of Police, Likes
DK.POL <- subset(POL.FL.S, CODE == "D/K") ; DK.POL <- DK.POL[order(DK.POL$GROUPING),]
ECO.POL <- subset(POL.FL.S, CODE == "Ecology") ; ECO.POL <- ECO.POL[order(ECO.POL$GROUPING),]
ETI.POL <- subset(POL.FL.S, CODE == "Etiquette") ; ETI.POL <- ETI.POL[order(ETI.POL$GROUPING),]
FOOD.POL <- subset(POL.FL.S, CODE == "Food") ; FOOD.POL <- FOOD.POL[order(FOOD.POL$GROUPING),]
MISC.POL <- subset(POL.FL.S, CODE == "Miscellaneous") ; MISC.POL <- MISC.POL[order(MISC.POL$GROUPING),]
MOR.POL <- subset(POL.FL.S, CODE == "Morality") ; MOR.POL <- MOR.POL[order(MOR.POL$GROUPING),]
PEOP.POL <- subset(POL.FL.S, CODE == "People") ; PEOP.POL <- PEOP.POL[order(PEOP.POL$GROUPING),]
REL.POL <- subset(POL.FL.S, CODE == "Religion") ; REL.POL <- REL.POL[order(REL.POL$GROUPING),]
RIT.POL <- subset(POL.FL.S, CODE == "Ritual") ; RIT.POL <- RIT.POL[order(RIT.POL$GROUPING),]
SUBST.POL <- subset(POL.FL.S, CODE == "Substance Use/Abuse") ; SUBST.POL <- SUBST.POL[order(SUBST.POL$GROUPING),]
VIRT.POL <- subset(POL.FL.S, CODE == "Virtue") ; VIRT.POL <- VIRT.POL[order(VIRT.POL$GROUPING),]

### Insert extracted variables into Tab.POL dataframe
Tab.POL$"D/K" <- DK.POL$SmithsS
Tab.POL$Ecology <- ECO.POL$SmithsS
Tab.POL$Etiquette <- ETI.POL$SmithsS
Tab.POL$Food <- FOOD.POL$SmithsS
Tab.POL$Miscellaneous <- MISC.POL$SmithsS
Tab.POL$Morality <- MOR.POL$SmithsS
Tab.POL$People <- PEOP.POL$SmithsS
Tab.POL$Religion <- REL.POL$SmithsS
Tab.POL$Ritual <- RIT.POL$SmithsS # Does not feature in POL
Tab.POL$"Substance Use/Abuse" <- SUBST.POL$SmithsS
Tab.POL$Virtue <- VIRT.POL$SmithsS

### Print to pdf - start. Ends with dev.off() 
pdf("S_HistogramPanel_Pleases.pdf", width=10, height=7) 

### set up panel, mar = c(b, l, t, r) 
par(mfrow = c(3,1), mar = c(3, 4, 2, .05))

### X axis labels (all general codes, with abbreviations)
xaxis <- c("D/K", "Ecology", "Etiquette", "Food", "Misc.", "Morality", 
           "People", "Religion", "Ritual", "Drugs", "Virtue")

### Plotting BGL with Codings on X-axis
codelabs <- c("D/K", "Ecology", "Etiquette", "Food", "Miscellaneous", "Morality", 
              "People", "Religion", "Ritual", "Substance Use/Abuse", "Virtue")

TabBGL <- t(Tab.BGL[codelabs])

TabBGL1 <- t(TabBGL)

rownames(TabBGL1) <- c("Coastal Tanna", "Hadza",
                       "Inland Tanna","Lovu Fiji","Marajó", "Mauritius", 
                       "Tyva Republic", "Yasawa Fiji")

barplot(TabBGL1, beside = TRUE,
        names.arg = NULL, ylim = c(0,1), axisnames = F, col = grey((1:8)/8)) #, 

axis(side = 1, padj = 0.01, labels = xaxis, 
     at = c(5, 14, 23, 32, 41, 50, 59, 68, 77, 86, 95), gap.axis = NA, tick = F, cex.axis = 1.4) # tick marks
axis(side = 2, labels = "Smith's S", at = 0.5, tick = F, cex.axis = 1.3, line = 1.5)
text(20, .90, "What pleases the moralistic gods?", font = 2, cex = 1.5)

### Plotting LGD with Codings on X-axis
TabLGL <- t(Tab.LGL[codelabs])

TabLGL1 <- t(TabLGL)

rownames(TabLGL1) <- c("Coastal Tanna", "Hadza",
                       "Inland Tanna", "Lovu Fiji", "Marajo", "Mauritius", 
                       "Tyva Republic", "Yasawa Fiji")

barplot(TabLGL1, beside = TRUE,
        names.arg = NULL, ylim = c(0,1), axisnames = F, col = grey((1:8)/8)) 

axis(side = 1, padj = 0.01, labels = xaxis, 
     at = c(5, 14, 23, 32, 41, 50, 59, 68, 77, 86, 95), gap.axis = NA, tick = F, cex.axis = 1.4) # tick marks
text(18, .90, "What pleases the local gods?", font = 2, cex = 1.5)
axis(side = 2, labels = "Smith's S", at = 0.5, tick = F, cex.axis = 1.3, line = 1.5)
ax <- 1.75 # size of asterisk
text(4.5, .02, "*", cex = ax)
text(13.5, .02, "*", cex = ax)
text(22.5, .02, "*", cex = ax)
text(31.5, .02, "*", cex = ax)
text(40.5, .02, "*", cex = ax)
text(49.5, .02, "*", cex = ax)
text(58.5, .02, "*", cex = ax)
text(67.5, .02, "*", cex = ax)
text(76.5, .02, "*", cex = ax)
text(85.5, .02, "*", cex = ax)
text(94.5, .02, "*", cex = ax)
text(103.5, .02, "*", cex = ax)

### Plotting POD with Codings on X-axis
TabPOL <- t(Tab.POL[codelabs])

TabPOL1 <- t(TabPOL)

rownames(TabPOL1) <- c("Coastal Tanna", "Hadza",
                       "Inland Tanna","Lovu Fiji","Marajó", "Mauritius", 
                       "Tyva Republic", "Yasawa Fiji")

TabPOL1[is.na(TabPOL1)] <- 0 # Ritual is not listed by any participants in this domain. NAs are turned into 0s for visualization purposes.

barplot(TabPOL1, beside = TRUE,
        names.arg = NULL, ylim = c(0,1), axisnames = F, col = grey((1:8)/8)) 

legend(66, 1, rownames(TabBGL1), fill = grey((1:8)/8),
       cex = 1.1, pt.cex = 4, bty = "n", y.intersp = 1.1)

axis(side = 1, padj = 0.01, labels = xaxis, 
     at = c(5, 14, 23, 32, 41, 50, 59, 68, 77, 86, 95), gap.axis = NA, tick = F, cex.axis = 1.4) # tick marks
axis(side = 2, labels = "Smith's S", at = 0.5, tick = F, cex.axis = 1.3, line = 1.5)
text(16, .90, "What pleases the police?", font = 2, cex = 1.5)

# Print
dev.off()

############################################################
### Figure S3 and S4: Flower plots of Hadza participants ###
############################################################

### Item salience - general codes - grouped by whether a (Hadza) participant thinks Haine and Ishoko are the "same" or "different" (GDSMDIFF)

H.BGL.FL <- CalculateSalience(data, Order = "Order", Subj = "CERCID",
                              CODE = "BGL", GROUPING = "GDSMDIFF", Rescale = FALSE, Salience = "H.BGL.S")
H.BGD.FL <- CalculateSalience(H.BGL.FL, Order = "Order", Subj = "CERCID",
                              CODE = "BGD", GROUPING = "GDSMDIFF", Rescale = FALSE, Salience = "H.BGD.S")
H.LGL.FL <- CalculateSalience(H.BGD.FL, Order = "Order", Subj = "CERCID",
                              CODE = "LGL", GROUPING = "GDSMDIFF", Rescale = FALSE, Salience = "H.LGL.S")
H.LGD.FL <- CalculateSalience(H.LGL.FL, Order = "Order", Subj = "CERCID",
                              CODE = "LGD", GROUPING = "GDSMDIFF", Rescale = FALSE, Salience = "H.LGD.S")

H.FL <- H.LGD.FL

### Salience by code - general codes - grouped by whether a (Hadza) participant thinks Haine and Ishoko are the "same" or "different" (GDSMDIFF)

H.BGL.FL.S <- SalienceByCode(H.FL, Subj = "CERCID", CODE = "BGL", GROUPING = "GDSMDIFF",
                             Salience = "H.BGL.S", dealWithDoubles = "MAX")

H.BGD.FL.S <- SalienceByCode(H.FL, Subj = "CERCID", CODE = "BGD", GROUPING = "GDSMDIFF",
                             Salience = "H.BGD.S", dealWithDoubles = "MAX")

H.LGL.FL.S <- SalienceByCode(H.FL, Subj = "CERCID", CODE = "LGL", GROUPING = "GDSMDIFF",
                             Salience = "H.LGL.S", dealWithDoubles = "MAX")

H.LGD.FL.S <- SalienceByCode(H.FL, Subj = "CERCID", CODE = "LGD", GROUPING = "GDSMDIFF",
                             Salience = "H.LGD.S", dealWithDoubles = "MAX")

### Compute number of times each code is listed per domain.
### Not used directly in the plots but useful to get a sense of proportionality.
H.BGL.FL.S$n <- H.BGL.FL.S$SumSalience/H.BGL.FL.S$MeanSalience
H.BGD.FL.S$n <- H.BGD.FL.S$SumSalience/H.BGD.FL.S$MeanSalience
H.LGL.FL.S$n <- H.LGL.FL.S$SumSalience/H.LGL.FL.S$MeanSalience
H.LGD.FL.S$n <- H.LGD.FL.S$SumSalience/H.LGD.FL.S$MeanSalience

### Abbreviate long code names
H.BGL.FL.S$CODE <- gsub("Miscellaneous", "Misc.", H.BGL.FL.S$CODE)
H.BGL.FL.S$CODE <- gsub("Substance Use/Abuse", "Drugs", H.BGL.FL.S$CODE)
H.BGD.FL.S$CODE <- gsub("Miscellaneous", "Misc.", H.BGD.FL.S$CODE)
H.BGD.FL.S$CODE <- gsub("Substance Use/Abuse", "Drugs", H.BGD.FL.S$CODE)

H.LGL.FL.S$CODE <- gsub("Miscellaneous", "Misc.", H.LGL.FL.S$CODE)
H.LGL.FL.S$CODE <- gsub("Substance Use/Abuse", "Drugs", H.LGL.FL.S$CODE)
H.LGD.FL.S$CODE <- gsub("Miscellaneous", "Misc.", H.LGD.FL.S$CODE)
H.LGD.FL.S$CODE <- gsub("Substance Use/Abuse", "Drugs", H.LGD.FL.S$CODE)

### Remove the "Don't Know" group, since they all responded "Don't know" in their free-list responses too
H.BGL.FL.S <- H.BGL.FL.S[!H.BGL.FL.S$GROUPING == "Don't Know",]
H.BGD.FL.S <- H.BGD.FL.S[!H.BGD.FL.S$GROUPING == "Don't Know",]
H.LGL.FL.S <- H.LGL.FL.S[!H.LGL.FL.S$GROUPING == "Don't Know",]
H.LGD.FL.S <- H.LGD.FL.S[!H.LGD.FL.S$GROUPING == "Don't Know",]

### Plot BG

# sort for consistency
H.BGL.FL.S <- H.BGL.FL.S[order(as.character(H.BGL.FL.S$GROUPING)),]
H.BGD.FL.S <- H.BGD.FL.S[order(as.character(H.BGD.FL.S$GROUPING)),]

pdf("Hadza_BG_SameDiff.pdf", width=8, height=8) 

par(mfrow = c(2, 2), mar=c(0,0,1,0)) # margin: b, l, t, r

for (group in unique(H.BGL.FL.S$GROUPING)){  # for each category that is unique in the grouping variable
  sub <- subset(H.BGL.FL.S, group == H.BGL.FL.S$GROUPING) # subset the data based on group
  sub$GROUPING <- NULL
  AnthroTools:::FlowerPlot(sub, "BGL")
  title(main = group, outer = FALSE, line = 0)
}

for (group in unique(H.BGD.FL.S$GROUPING)){  # for each category that is unique in the grouping variable
  sub <- subset(H.BGD.FL.S, group == H.BGD.FL.S$GROUPING) # subset the data based on group
  sub$GROUPING <- NULL
  AnthroTools:::FlowerPlot(sub, "BGD")
  title(main = group, outer = FALSE, line = 0)
}

# Print
dev.off()

### Plot LG

# sort for consistency
H.LGL.FL.S <- H.LGL.FL.S[order(as.character(H.LGL.FL.S$GROUPING)),]
H.LGD.FL.S <- H.LGD.FL.S[order(as.character(H.LGD.FL.S$GROUPING)),]

pdf("Hadza_LG_SameDiff.pdf", width=8, height=8) 

par(mfrow = c(2, 2), mar=c(0,0,1,0)) # margin: b, l, t, r

for (group in unique(H.LGL.FL.S$GROUPING)){  # for each category that is unique in the grouping variable
  sub <- subset(H.LGL.FL.S, group == H.LGL.FL.S$GROUPING) # subset the data based on group
  sub$GROUPING <- NULL
  AnthroTools:::FlowerPlot(sub, "LGL")
  title(main = group, outer = FALSE, line = 0)
}

for (group in unique(H.LGD.FL.S$GROUPING)){  # for each category that is unique in the grouping variable
  sub <- subset(H.LGD.FL.S, group == H.LGD.FL.S$GROUPING) # subset the data based on group
  sub$GROUPING <- NULL
  AnthroTools:::FlowerPlot(sub, "LGD")
  title(main = group, outer = FALSE, line = 0)
}

# Print
dev.off()

#########################################################################################
### Tables S4-S9: LaTex Tables for most salient general codes for each site and agent ###
#########################################################################################

### Toggle cutoff under script for Table 4. In the Supplements, we report salience tables with cutoff of > .049.

###########################################################################################
### Tables S10-S15: LaTex Tables for most salient specific codes for each site and agent ###
###########################################################################################

### Item salience - specifics - per culture

BGL.SPEC.FL <- CalculateSalience(FL.SPEC.L, Order = "Order", Subj = "CERCID",
                                 CODE = "BGL_SPEC.L", GROUPING = "Culture", Rescale = FALSE, Salience = "BGL.SPEC.S")
BGD.SPEC.FL <- CalculateSalience(BGL.SPEC.FL, Order = "Order", Subj = "CERCID",
                                 CODE = "BGD_SPEC.L", GROUPING = "Culture", Rescale = FALSE, Salience = "BGD.SPEC.S")
LGL.SPEC.FL <- CalculateSalience(BGD.SPEC.FL, Order = "Order", Subj = "CERCID",
                                 CODE = "LGL_SPEC.L", GROUPING = "Culture", Rescale = FALSE, Salience = "LGL.SPEC.S")
LGD.SPEC.FL <- CalculateSalience(LGL.SPEC.FL, Order = "Order", Subj = "CERCID",
                                 CODE = "LGD_SPEC.L", GROUPING = "Culture", Rescale = FALSE, Salience = "LGD.SPEC.S")
POL.SPEC.FL <- CalculateSalience(LGD.SPEC.FL, Order = "Order", Subj = "CERCID",
                                 CODE = "POL_SPEC.L", GROUPING = "Culture", Rescale = FALSE, Salience = "POL.SPEC.S")
POD.SPEC.FL <- CalculateSalience(POL.SPEC.FL, Order = "Order", Subj = "CERCID",
                                 CODE = "POD_SPEC.L", GROUPING = "Culture", Rescale = FALSE, Salience = "POD.SPEC.S")

FL.SPEC.S <- POD.SPEC.FL

### Salience by code - specifics - per culture

BGL.SPEC.FL.S <- SalienceByCode(FL.SPEC.S, Subj = "CERCID", CODE = "BGL_SPEC.L", GROUPING = "Culture",
                                Salience = "BGL.SPEC.S", dealWithDoubles = "MAX")

BGD.SPEC.FL.S <- SalienceByCode(FL.SPEC.S, Subj = "CERCID", CODE = "BGD_SPEC.L", GROUPING = "Culture",
                                Salience = "BGD.SPEC.S", dealWithDoubles = "MAX")

LGL.SPEC.FL.S <- SalienceByCode(FL.SPEC.S, Subj = "CERCID", CODE = "LGL_SPEC.L", GROUPING = "Culture",
                                Salience = "LGL.SPEC.S", dealWithDoubles = "MAX")

LGD.SPEC.FL.S <- SalienceByCode(FL.SPEC.S, Subj = "CERCID", CODE = "LGD_SPEC.L", GROUPING = "Culture",
                                Salience = "LGD.SPEC.S", dealWithDoubles = "MAX")

POL.SPEC.FL.S <- SalienceByCode(FL.SPEC.S, Subj = "CERCID", CODE = "POL_SPEC.L", GROUPING = "Culture",
                                Salience = "POL.SPEC.S", dealWithDoubles = "MAX")

POD.SPEC.FL.S <- SalienceByCode(FL.SPEC.S, Subj = "CERCID", CODE = "POD_SPEC.L", GROUPING = "Culture",
                                Salience = "POD.SPEC.S", dealWithDoubles = "MAX")

### LaTex Tables

cutoff <- .049 # change Smith's S cutoff value

columns <- c("Culture", "Code", "M Salience", "S Salience", "Smith's S", "n", "N")

BGL.SPEC.FL.S$n <- BGL.SPEC.FL.S$SumSalience/BGL.SPEC.FL.S$MeanSalience # number of times each code is listed per site
BGL.SPEC.FL.S$N <- BGL.SPEC.FL.S$SumSalience/BGL.SPEC.FL.S$SmithsS # total number of free-list participants per site and free-list domain
BGL.SPEC.SORT <- sortnaxe.g(BGL.SPEC.FL.S, cutoff) # sorts and cuts Smith's S below cutoff value
BGL.SPEC.S.tab <- BGL.SPEC.SORT[!grepl("D/K", BGL.SPEC.SORT$CODE),] # removes "D/K"s
colnames(BGL.SPEC.S.tab) <- columns # column names for the table
View(BGL.SPEC.S.tab)
print(xtable(BGL.SPEC.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Moralistic Gods -- Specific -- Likes"), include.rownames = FALSE) # The digits command controls the number of digits for each column.

BGD.SPEC.FL.S$n <- BGD.SPEC.FL.S$SumSalience/BGD.SPEC.FL.S$MeanSalience
BGD.SPEC.FL.S$N <- BGD.SPEC.FL.S$SumSalience/BGD.SPEC.FL.S$SmithsS
BGD.SPEC.SORT <- sortnaxe.g(BGD.SPEC.FL.S, cutoff)
BGD.SPEC.S.tab <- BGD.SPEC.SORT[!grepl("D/K", BGD.SPEC.SORT$CODE),]
colnames(BGD.SPEC.S.tab) <- columns
View(BGD.SPEC.S.tab)
print(xtable(BGD.SPEC.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Moralistic Gods -- Specific -- Dislikes"), include.rownames = FALSE)

LGL.SPEC.FL.S$n <- LGL.SPEC.FL.S$SumSalience/LGL.SPEC.FL.S$MeanSalience
LGL.SPEC.FL.S$N <- LGL.SPEC.FL.S$SumSalience/LGL.SPEC.FL.S$SmithsS
LGL.SPEC.SORT <- sortnaxe.g(LGL.SPEC.FL.S, cutoff)
LGL.SPEC.S.tab <- LGL.SPEC.SORT[!grepl("D/K", LGL.SPEC.SORT$CODE),]
colnames(LGL.SPEC.S.tab) <- columns
View(LGL.SPEC.S.tab)
print(xtable(LGL.SPEC.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Local Gods -- Specific -- Likes"), include.rownames = FALSE)

LGD.SPEC.FL.S$n <- LGD.SPEC.FL.S$SumSalience/LGD.SPEC.FL.S$MeanSalience
LGD.SPEC.FL.S$N <- LGD.SPEC.FL.S$SumSalience/LGD.SPEC.FL.S$SmithsS
LGD.SPEC.SORT <- sortnaxe.g(LGD.SPEC.FL.S, cutoff)
LGD.SPEC.S.tab <- LGD.SPEC.SORT[!grepl("D/K", LGD.SPEC.SORT$CODE),]
colnames(LGD.SPEC.S.tab) <- columns
View(LGD.SPEC.S.tab)
print(xtable(LGD.SPEC.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Local Gods -- Specific -- Dislikes"), include.rownames = FALSE)

POL.SPEC.FL.S$n <- POL.SPEC.FL.S$SumSalience/POL.SPEC.FL.S$MeanSalience
POL.SPEC.FL.S$N <- POL.SPEC.FL.S$SumSalience/POL.SPEC.FL.S$SmithsS
POL.SPEC.SORT <- sortnaxe.g(POL.SPEC.FL.S, cutoff)
POL.SPEC.S.tab <- POL.SPEC.SORT[!grepl("D/K", POL.SPEC.SORT$CODE),]
colnames(POL.SPEC.S.tab) <- columns
View(POL.SPEC.S.tab)
print(xtable(POL.SPEC.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Police -- Specific -- Likes"), include.rownames = FALSE)

POD.SPEC.FL.S$n <- POD.SPEC.FL.S$SumSalience/POD.SPEC.FL.S$MeanSalience
POD.SPEC.FL.S$N <- POD.SPEC.FL.S$SumSalience/POD.SPEC.FL.S$SmithsS
POD.SPEC.SORT <- sortnaxe.g(POD.SPEC.FL.S, cutoff)
POD.SPEC.S.tab <- POD.SPEC.SORT[!grepl("D/K", POD.SPEC.SORT$CODE),]
colnames(POD.SPEC.S.tab) <- columns
View(POD.SPEC.S.tab)
print(xtable(POD.SPEC.S.tab, digits=c(0,0,2,2,2,2,0,0), 
             caption = "Police -- Specific -- Dislikes"), include.rownames = FALSE)


###########
### END ###
###########
