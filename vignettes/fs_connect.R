## ---- FSLobeFunctions ----

fslobes <- function(aparcregions,
                    sep = "_",
                    computeSuff = TRUE)
{
  # https://surfer.nmr.mgh.harvard.edu/fswiki/CorticalParcellation
  frontal <- c(
    "caudalmiddlefrontal",
    "lateralorbitofrontal",
    "medialorbitofrontal",
    "rostralmiddlefrontal",
    "superiorfrontal",
    "frontalpole",
    "parsopercularis",
    "parsorbitalis",
    "parstriangularis",
    "precentral",
    "paracentral"
  )
  parietal <-
    c(
      "inferiorparietal",
      "superiorparietal",
      "supramarginal",
      "postcentral",
      "precuneus"
    )
  temporal <-
    c(
      "inferiortemporal",
      "middletemporal",
      "superiortemporal",
      "temporalpole",
      "transversetemporal",
      "fusiform",
      "bankssts",
      "entorhinal",
      "parahippocampal"
    )
  occipital <- c("lateraloccipital", "lingual", "cuneus",
                 "pericalcarine")
  ## can be included in frontal and parietal
  cingulate <-
    c(
      "rostralanteriorcingulate",
      "caudalanteriorcingulate",
      "isthmuscingulate",
      "posteriorcingulate"
    )
  
  insula <- c("insula")
  
  ## may want to add other categories.
  
  ## Build a map of regions to lobes
  onemap <- function(lobe, nn)
  {
    a <- rep(lobe, length(nn))
    names(a) <- nn
    return(a)
  }
  frontal.m <- onemap("frontal", frontal)
  parietal.m <- onemap("parietal", parietal)
  temporal.m <- onemap("temporal", temporal)
  occipital.m <- onemap("occipital", occipital)
  cingulate.m <- onemap("cingulate", cingulate)
  insula.m <- onemap("insula", insula)
  
  all.m <- c(frontal.m,
             parietal.m,
             temporal.m,
             occipital.m,
             cingulate.m,
             insula.m)
  
  all.m <-
    factor(
      all.m,
      levels = c(
        "frontal",
        "insula",
        "cingulate",
        "temporal",
        "parietal",
        "occipital"
      )
    )
  if (computeSuff) {
    ## Figure out the suffix : thickness/volume etc
    pattern <- paste0("^.+", sep, "([[:alpha:]]+)$")
    suff <- gsub(pattern, "\\1", aparcregions)
    suff <- unique(suff)
    if (length(suff) > 1) {
      stop("Something wrong with procedure figuring out the suffix")
    }
    
    ## modify the names so we can look up easily.
    pattern <- paste0("^[lr]h", sep, "(.+)", sep, suff, "$")
    regions <- gsub(pattern, "\\1", aparcregions)
  } else {
    pattern <- paste0("^[lr]h", sep, "(.+)", "$")
    regions <- gsub(pattern, "\\1", aparcregions)
  }
  return(all.m[regions])
  
}

freeSurfCortexData <-
  function(this.group,
           aparc.thickness.names,
           aparc.area.names,
           aparc.volume.names,
           tidycols = c("ID"))
  {
    dfT <- this.group[, c(tidycols, aparc.thickness.names)]
    dfT <-
      tidyr::gather_(dfT,
                     key = "region",
                     value = "thickness",
                     aparc.thickness.names)
    dfA <- this.group[, c(tidycols, aparc.area.names)]
    dfA <-
      tidyr::gather_(dfA, key = "region", value = "area", aparc.area.names)
    dfV <- this.group[, c(tidycols, aparc.volume.names)]
    dfV <-
      tidyr::gather_(dfV, key = "region", value = "volume", aparc.volume.names)
    
    ## rename the regions
    dfA$region <- gsub("_area$", "", dfA$region)
    dfT$region <- gsub("_thickness$", "", dfT$region)
    dfV$region <- gsub("_volume$", "", dfV$region)
    
    df <- merge(dfT, dfA, by = c(tidycols, "region"))
    df <- merge(df, dfV, by = c(tidycols, "region"))
    
    ## summarise this for plotting purposes
    dfS <- dplyr::group_by(df, region)
    dfAve <-
      summarise(
        dfS,
        thickness = mean(thickness),
        area = mean(area),
        volume = mean(volume)
      )
    # create separate hemisphere and new region columns
    dfAve <- within(dfAve, {
      origregion <- region
      hemisphere <- factor(gsub("([lr]h)_.+", "\\1", region))
      region <- gsub("([lr]h)_(.+)", "\\2", region)
    })
    # reorder the region factor so that it is by lobe
    lobes <- fslobes(as.character(dfAve$region), computeSuff = FALSE)
    dfAve$lobe <- lobes[dfAve$region]
    ## This is messy - need to order each hemisphere separately
    ## because we want to have them going in opposite directions in
    ## the circle
    lhs <- subset(dfAve, hemisphere == "lh")
    rhs <- subset(dfAve, hemisphere == "rh")
    ol <- order(lhs$lobe, lhs$region)
    or <- rev(order(rhs$lobe, rhs$region))
    regorder <- c(lhs$origregion[ol], rhs$origregion[or])
    
    
    dfAve$regionfactors <- factor(dfAve$origregion, levels = regorder)
    return(dfAve)
  }



##---- LoadFSData ----
library(dplyr)
library(tidyr)
loadFSTab <- function(fn)
{
  t1 <- read.csv(fn, stringsAsFactors = FALSE)
  colnames(t1)[1] <- "ID"
  return(t1)
}

mergeLots <- function(tbls, by = "ID")
{
  result <- tbls[[1]]
  for (x in 2:length(tbls)) {
    result <- merge(result, tbls[[x]], by = by)
  }
  return(result)
}

## We'll only use aparc this time
fsfiles <-
  list.files(pattern = "bert_.+_aparc_.+\\.csv", full.names = TRUE)

allfsdata <- lapply(fsfiles, loadFSTab)
allfsdata <- mergeLots(allfsdata)

## Not interested in the MeanThickness etc

allfsdata <- within(allfsdata, {
  rh_MeanThickness_thickness <- lh_MeanThickness_thickness <- NULL
  rh_MeanThickness_thickness.y <-
    rh_MeanThickness_thickness.x <-
    lh_MeanThickness_thickness.y <-
    lh_MeanThickness_thickness.x <- NULL
  rh_WhiteSurfArea_area <- lh_WhiteSurfArea_area <- NULL
  rh_WhiteSurfArea_area.y <-
    rh_WhiteSurfArea_area.x <-
    lh_WhiteSurfArea_area.y <- lh_WhiteSurfArea_area.x <- NULL
})

## collect the area and thickness names

region.thickness.names <-
  grep("_thickness", colnames(allfsdata), value = TRUE)
region.area.names <- grep("_area", colnames(allfsdata), value = TRUE)
region.volume.names <-
  grep("_volume", colnames(allfsdata), value = TRUE)

## ---- CreateFSFrame ----
fstable <-
  freeSurfCortexData(
    allfsdata,
    aparc.thickness.names = region.thickness.names,
    aparc.area.name = region.area.names,
    aparc.volume.names = region.volume.names
  )

fstable

## ---- CircOne ----
library(ggforce)
fstable <- fstable[order(fstable$regionfactors),]
ggplot(fstable) + geom_arc_bar(aes(x0=0, y0=0, r0=0.8, r=1, amount=area,
                                   fill=region), stat='pie') +
  coord_fixed() +
  theme_no_axes() 

## ---- CircTwo ----
ggplot(fstable) + geom_arc_bar(aes(x0=0, y0=0, r0=0.8, r=1, amount=volume,
                                   fill=region), stat='pie') +
  geom_arc_bar(aes(x0=0, y0=0, r0=1.1, r=1.2, amount=volume,
                   fill=lobe), stat='pie') +
  coord_fixed() +
  theme_no_axes() 

## ---- CircThree ----
ggplot(fstable) + geom_arc_bar(aes(x0=0, y0=0, r0=thickness, r=2, amount=area,
                                   fill=region), stat='pie') +
  coord_fixed() +
  theme_no_axes() 

## ---- CircFour ----
ggplot(fstable) + geom_arc_bar(aes(x0=0, y0=0, r0=1.15, r=1.2, amount=area,
                                   fill=region), stat='pie') +
  geom_text(stat="text_pie", aes(x0=0, y0=0, r0=1, r=1.25, amount=area, label=region,
                                          sourcenode=regionfactors, hjust=0, vjust=0)) +
  coord_fixed() +
  theme_no_axes() +
  theme(legend.position="none")+
  xlim(c(-2,2)) + ylim(c(-2,2))

## ---- LinkSetup ----
## Create 10 random links, with some common nodes
set.seed(21)
sourcenodes <- sample(1:nrow(fstable), 10, replace=FALSE)
sourcenodes <- c(sourcenodes, sourcenodes[1:2])
sourcenodes <- fstable$regionfactors[sourcenodes]
destnodes <- sample(fstable$regionfactors, length(sourcenodes), replace=FALSE)

linkframe <- data.frame(snode=sourcenodes, dnode=destnodes)

## This one contains just the links
fstable2 <- merge(fstable, linkframe, by.x="regionfactors", by.y="snode")
## rbind them

fstable$dnode <- NA
fslinks <- rbind(fstable, fstable2)

## ---- PlotLinks ----

ggplot(fstable) + geom_arc_bar(aes(x0=0, y0=0, r0=1.15, r=1.2, amount=area,
                                   fill=region), stat='pie') +
  geom_text(stat="text_pie", aes(x0=0, y0=0, r0=1, r=1.25, amount=area, label=region,
                                 sourcenode=regionfactors, hjust=0, vjust=0)) +
  geom_bezier(data=fslinks, aes(x0=0,y0=0, r0=1.14, r=1.14, amount=area, destinationnode=dnode,
                                sourcenode=regionfactors, size=thickness, alpha=area), stat='link_pie') +
  coord_fixed() +
  theme_no_axes() +
  theme(legend.position="none")+
  xlim(c(-2,2)) + ylim(c(-2,2))
