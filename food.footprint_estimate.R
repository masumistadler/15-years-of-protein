## -------------------------------------------------------------------------
##
## Script name: owid_food.R
##
## Purpose of script: Calculate GHG and water footprint of various dietary
##                    lifestyles over 15 years.
##
## Author: Masumi Stadler
##
## Date Finalized: March 16, 2026
##
## Copyright (c) Masumi Stadler, 2026
## Email: m.stadler.jp.at@gmail.com
##
## -------------------------------------------------------------------------
##
## Notes:
##
##
## -------------------------------------------------------------------------

## Use R project with regular scripts, all paths are relative 

# Server set-up -----------------------------------------------------------
## Working directory is set from where the job is submitted
## Load library path, if on a server
# .libPaths( c( .libPaths(), "/home/mstadler/projects/def-pauldel/R/x86_64-pc-linux-gnu-library/4.2") )

# R-setup -----------------------------------------------------------------
## Load Packages -----------------------------------------------------------
pckgs <- list('plyr', 'tidyverse','data.table', 'owidR', # wrangling & programming
              'ggpubr', 'plotly', 'patchwork',# plotting
              "khroma", # Paul Tol's colour blind friendly palettes
              "htmlwidgets" # export interactive plotly
             ) # change as needed

## Check if packages are installed, output packages not installed:
(miss.pckgs <- unlist(pckgs)[!(unlist(pckgs) %in% installed.packages()[,"Package"])])
#if(length(miss.pckgs) > 0) install.packages(miss.pckgs)

## Load
invisible(lapply(pckgs, library, character.only = T))
rm(pckgs, miss.pckgs)

# ## Load custom functions --------------------------------------------------
# funs <- list.files("./Functions", full.names = T)
# invisible(lapply(funs, source))

## Other set-up -----------------------------------------------------------
options(scipen = 6, digits = 4) # view outputs in non-scientific notation
 
## Parallel environment ---------------------------------------------------
## Server version
# cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
# registerDoMC(cores = cores)

## Personal version
# detectCores(); registerDoMC(); getDoParWorkers()
# numCores <- detectCores()
# cl <- makeCluster(numCores, type = "FORK")
 
# Get data from OWID -----------------------------------------------------------

# greenhouse gas footprint of various proteins by 100g of protein
ghg_food <- fread("https://ourworldindata.org/grapher/ghg-per-protein-poore.csv")

# water footprint  of various proteins by 100g of protein
# general water usage
water_protein <-fread("https://ourworldindata.org/grapher/water-per-protein-poore.csv")
# weighed by global average water scarcity
scar.water_protein <- fread("https://ourworldindata.org/grapher/scarcity-water-protein-poore.csv")

# Original data from
# Joseph Poore and Thomas Nemecek (2018)

# Dietary types ----------------------------------------------------------------
# Flexitarian definition
# 5+ meatless days a week (pretty strict 'expert' flexitarian)
# 2 meatless days a week (beginner)
# Omnivore (Average human)
# Lacto-ovo representative (me)

# Get consumption data ---------------------------------------------------------
# Searched for other data found on World Population Review:

# beef consumption per capita per year (2023)
# USA 37.04 kg/capita 
# Austria 13.85 kg/capita
# Japan's data not available found some data on AHDB, around 8 kg/capita

# chicken consumption per capita per year (2022)
# USA: 53.54 kg/capita 
# Austria: 19.14
# Japan: 27.90

# pork consumption per capita per year (2022)
# USA: 29.86 kg/capita
# Austria: 42.82 kg/capita
# japan: 22.77 kg/capita
# Source: Food supply balances - FAO stat - Food and Agriculture Organization of the United Nations

# Milk consumption (2022) kg/yr per capita (includes yoghurt)
# USA: 230.32 kg/capita
# Austria: 212.21 kg/capita
# japan: 62.15 kg/capita
# Source: Food Balances - FAOSTAT 

# Austria cheese consumption is that of EU, not for single country
# seafood consumption has been declining in Japan from ~28 kg to 22 kg 2011 to 2022. Taking average at around 25 kg.

# These are supply data, do a meat adjustment of 0.7, and dairy adjustment of 0.9

# get beans data, units in kg/capita/yr
# legumes <- read.csv("./Data/FAOSTAT_data_en_3-11-2026.csv", sep = ",") %>% 
#   setDT()
# 
# # calculate averages by year, country and type
# legumes <- legumes[, .(avg = mean(Value, na.rm = T)), by = .(Area, Item)]
# # sum by country
# legumes <- legumes[, .(sum = sum(avg)), by = .(Area)]

# add to protein table
protein.consumption <- data.frame(country = c("Austria","Canada","Japan","USA"),
                               beef = c(13.85,25.64,8,37.04), 
                               chicken = c(19.14, 41.17, 27.9, 53.54),
                               pork = c(42.82, 22.41, 22.77, 29.86),
                               lamb = c(1.00,1.23,0.15,0.69),
                               milk = c(212.21, 165.08,62.15, 230.32),
                               cheese = c(20.9,13.70, 2.60,17.40),
                               egg = c(14.97,15.73,19.92,15.90),
                               fish = c(14.12,20.82,25,21.94),
                               pulses = c(0.96, 6.78, 1.43, 3.2),
                               tofu = c(1,1,10,1)) %>% setDT() # tofu values are approximate from web
# currently all units are kg / capita / year

# need to use a correction factor, since these are currently mere supply data
# also need to convert kg into g of protein

# convert to protein units (searched for protein per 100g)
# values retrieved from Google, their source is USDA
protein.consumption[, beef := beef * 0.65 * 0.26]
protein.consumption[, chicken := chicken * 0.6 * 0.27]
protein.consumption[, pork := pork * 0.7 * 0.27]
protein.consumption[, lamb := lamb * 0.6 * 0.27]
protein.consumption[, egg := egg * 0.9 * 0.13]
protein.consumption[, milk := milk * 0.95 * 0.032]
protein.consumption[, cheese := cheese * 0.95 * 0.28]
# checked different cheese, ranges 20-35 took the middle
protein.consumption[, fish := fish * 0.6 * 0.21]
# checked different fish, most of them are between 18-25 took middle
protein.consumption[, pulses := pulses * 0.9 * 0.16] 
protein.consumption[, tofu := tofu * 0.95 * 0.08]
protein.consumption[, alt.meat := 0]

# kg/capita/yr * waste factor * protein proportion
# unit is still kg

# Beans are other pulses
# amount of protein per day from each source

# melt data table
protein.consumption <- 
  melt(protein.consumption, id.vars = c("country"), variable.name = "protein", value.name = "per.year")

# how much protein do people eat pet country?
sum.by.country <- protein.consumption[, .(sum.protein = sum(per.year * 1000)), by = .(country)]

# this is the omnivore diet.

# Brainstorm my consumption ---------------------------------------------------
# I usually eat 6 eggs per week, 52 weeks in a year
# that's 312 eggs per year
# 1 egg has ~6g of protein
(312*6)

# Cheese: we usually go through a 226g (8oz) cheese package in 2 weeks
56.5 # per person per week, I will round it up because we do also eat other cheese
(70 * 0.28)*52

# I usually eat yogurt every morning (use 1 500g yoghurt per week)
# that's 36 g of yogurt per day (3.8 g of protein per 100g)
(36 * 0.038) *365
# I don't drink milk, so we don't account for that.

# tofu: we eat 2 packages of tofu per week (340g), that makes 340 per person per week
340 * 52 * 0.08

# I make approximately 2 dishes with legumes per week
chick <- (250/2) * 0.19 #g of chickpea per week
len <- (200/2) * 0.09 #g of lentils per week
# this is for two people
(chick+len) * 52

# meat-substitute: beyond meat (from Beyond Burger Life Cycle assessment 2025)
# 4 oz patty has 21 g protein
21 *1 / 113.4 # around 18.5% protein
# we have meat-substitutes about once a week (340 g of ground vegan "beef")
# one ground beef packet for two people
(340/2) * 0.185 * 52

# it seems that I am not eating enough protein by these calculations
# we will match the remaining protein to CDC/RDA's recommended protein consumption
# to not falsely lower my footpring, just because I don't eat enough

# 46g for women, 56g for men (https://blogs.cdc.gov/nchs/2010/03/03/953/)
# will unify the calculations for recommended men's protein consumption
# just so we can account for the upper end of recommended consumption
56*365 # 20440 g of protein per year

# let's calculate my consumption
my.consumption <- data.frame(country = "Me",
                             beef = 0, 
                             chicken = 0,
                             pork = 0,
                             lamb = 0,
                             milk = 499.3, # grams of protein from yoghurt per year
                             cheese = 1019,
                             egg = 1872, # grams of protein from eggs per year
                             fish = 0,
                             pulses = 1703,
                             tofu = 1414,
                             alt.meat = 1635) %>% setDT()

missing <- (56*365) - rowSums(my.consumption[,-1]) # 6507
# in g

# we will distribute them by proportion of perceived consumption
my.consumption <- melt(my.consumption, id.vars = "country", variable.name = "protein", value.name = "per.year")
# I actually do not eat more alt.meat than I estimated above, so remove from percent

my.consumption[, total := sum(per.year, na.rm = T)]
my.consumption[, percent := per.year * 1 / total]
# add the 20% of alt meat to beans and tofu
my.consumption[protein %in% c("tofu","pulses"), percent := percent + 0.1]
my.consumption[protein == "alt.meat", percent := 0]
my.consumption[, per.year := per.year + (missing * percent)]
# sanity check
sum(my.consumption$per.year)

# flexitarian diet ------------------------------------------------------------
flex.consumption <- copy(protein.consumption)

# CDC-recommended protein intake (RDA)
# 0.8 g/kg x avg adult weight = 56 g/d
# Annual = 56 g x 365 d = 20.44 kg protein / yr

# now adjust
# reduction by 70% = Expert Flexi, that is equivalent to approx. meatfree 5 days per week
# reduction by 25% = Beginner Flexi, that is equivalent to appox. meatfree 2 days per week

flex.consumption[protein %in% c("beef","chicken","lamb",
                                "pork","fish"), expert := per.year * 0.3]
flex.consumption[protein %in% c("beef","chicken","lamb",
                                "pork","fish"), beginner := per.year * 0.75]

# calculate protein deficit
flex.consumption[, total := 20.44] # unify with recommended protein intake of CDC

flex.consumption[, begin.deficit := total - sum(beginner, na.rm = T), by = .(country)]
flex.consumption[, expert.deficit := total - sum(expert, na.rm = T), by = .(country)]

# proportion of non-meat protein consumption by country
flex.consumption[protein %in% c("egg","milk","cheese",
                                "pulses","tofu"), 
                 total.veg := sum(per.year), by = .(country)]
flex.consumption[protein %in% c("egg","milk","cheese",
                                "pulses","tofu"), 
                 prop.veg := per.year * 1 / total.veg, by = .(country)]

# actually these proportions are likely not reality
# I doubt that flexitarians would increase their milk and cheese consumption
# proportionally to their protein deficit
# more likely that they increase:
# 25% legumes, 25% tofu, 20% eggs, 10% alt-meat, 10 % milk, 10% cheese
# 50% from animal protein, 50% from plant sources

flex.consumption[protein %in% c("pulses","tofu"), prop := 0.25]
flex.consumption[protein %in% c("egg"), prop := 0.20]
flex.consumption[protein %in% c("milk","cheese","alt.meat"), prop := 0.1]

# let's get rid of proportion by protein that we derived before
flex.consumption[, c("total.veg","prop.veg") := list(NULL, NULL)]

# let's calculate the protein consumption per vegetarian protein group
# from the protein deficit we calculated
flex.consumption[is.na(expert), expert := expert.deficit * prop]
flex.consumption[is.na(beginner), beginner := begin.deficit * prop]

# sanity check: make sure total protein is the same as omnivores
merge(flex.consumption[, .(sum.expert = sum(expert) * 1000,
                     sum.begin = sum(beginner) * 1000), by = .(country)],
      sum.by.country, by = "country")
# all good.

# melt data frame for subsequent analyses
flex.consumption <- melt(flex.consumption, id.vars = c("country","protein"),
     measure.vars = c("beginner","expert"), variable.name = "lifestyle.type",
     value.name = "per.year")


# Merge all consumption data --------------------------------------------------
my.consumption # Masumi's consumption
flex.consumption # flexitarian
protein.consumption # omnivores by country

# unify columns
protein.consumption[, lifestyle.type := "omnivore"]
my.consumption[, lifestyle.type := "lacto-ovo vegetarian"]
# put on same scale
protein.consumption[,per.year := per.year * 1000] # from kg to g
flex.consumption[, per.year := per.year * 1000]
# remove some now obsolete columns from my data
my.consumption[, c("total","percent") := list(NULL, NULL)]

# add alt-meat to flex and omni
flex.consumption 

# merge
consum <- bind_rows(flex.consumption, protein.consumption)
consum <- bind_rows(consum, my.consumption)


# Merge GHG/water impact data --------------------------------------------------
owid.protein <- c("Beef (beef herd)",
                  "Poultry Meat",
                  "Pig Meat",
                  "Lamb & Mutton",
                  "Fish (farmed)",
                  "Milk",
                  "Cheese",
                  "Eggs","Other Pulses",
                  "Tofu",
                  "Peas") # adding peas for Beyond meat

# get matching categories
cat.leg <- data.frame(protein = c("beef","chicken",
                      "pork","lamb","fish","milk","cheese",
                      "egg","pulses","tofu","alt.meat"),
           owid.cat = owid.protein)

# take subset of proteins we use in this analysis
ghg_food <- ghg_food[Entity %in% owid.protein,] # kg of CO2eq
water_protein <- water_protein[Entity %in% owid.protein,] # in Litres
scar.water_protein <- scar.water_protein[Entity %in% owid.protein,] # in Litres

# for dairy, we will apply loca AWARE water stress index (WRI) taken from Poore data
dairy.water <- water_protein[Entity %in% c("Milk","Cheese"),]
country.aware <- data.frame(country = c("Austria","Canada","Japan","USA"),
                            aware = c(1.1923,2.61836,0.94786,9.51396))

# number of countries
country <- country.aware$country
# use lapply to create a list of data frames
ls.df <- lapply(country, function(y){
  dairy.water$country <- y
  return(dairy.water)
})

# merge together
dairy.water <- bind_rows(ls.df)

# merge with aware data
dairy.water <- merge(dairy.water, country.aware, by = "country")
dairy.water[, `Scarcity-weighted water use per 100g protein` := `Freshwater withdrawals per 100g protein` * aware]

# calculate average and assign to country "Me"
dairy.water <- bind_rows(dairy.water,
                         dairy.water[,.(`Scarcity-weighted water use per 100g protein` = 
                                          mean(`Scarcity-weighted water use per 100g protein`)), by = "Entity"][, country := "Me"])

# merge to scarcity data
# use lapply to create a list of data frames
ls.df <- lapply(c(country,"Me"), function(y){
  scar.water_protein$country <- y
  return(scar.water_protein)
})

# merge together
scar.water_protein <- bind_rows(ls.df)

# overwrite dairy and milk with country AWARE corrected water use
scar.water_protein <- merge(scar.water_protein, 
      dairy.water %>% dplyr::select(country, Entity,
                                    `Scarcity-weighted water use per 100g protein`),
      by = c("country","Entity"), all.x = T)

# clean up
scar.water_protein[Entity %in% c("Cheese","Milk"), 
                   `Scarcity-weighted water use per 100g protein.x` := `Scarcity-weighted water use per 100g protein.y`]
scar.water_protein[, `Scarcity-weighted water use per 100g protein.y` := NULL]
colnames(scar.water_protein)[4] <- "Scarcity-weighted water use per 100g protein"

# add our protein categories for merging
ghg_food <- merge(ghg_food, cat.leg, by.x = "Entity", by.y = "owid.cat")
scar.water_protein <- merge(scar.water_protein, cat.leg, by.x = "Entity", by.y = "owid.cat")

# merge with consumption profiles
consum <- merge(consum, ghg_food %>% dplyr::select(-Year,-Entity), by = "protein")
consum <-merge(consum, scar.water_protein %>% dplyr::select(-Year,-Entity), by = c("protein","country"))

# Beyond Meat foot print ------------------------------------------------------
# Found GHG and water usage data in Beyond Meat LCA Report 2025
# will be assuming that all the protein comes from peas
# 100 g of peas have 2.8 g of protein
# to get 100 g of proteins from peas you need
100/2.8
# 35.71
35.71* 100 # 3.57 kg of peas to get 100 g of pea protein

# actually we have Peas GHG and water footprint in the OWID data
# Just add the footprint from the LCA of Beyond manufacturing

# 4 oz = 0.25 lb = 113.4 g
0.51 # kg Co2eq
(0.51/21) * 100 # 2.429 kg CO2eq produced for 100 g of protein
# water consumption
16.9 # litres for one patty
(16.9/21) * 100 # 80.48 L used per 100 g of protein
# calculate scarcity-weighted water
80.48 * 0.4

# get a water scarcity index estimate for Beyond
# searched where US factories are and got their water scarcity coefficient
# AWARE 2.0 characterization factors (m3 equiv/m3)
# AWARE 2.0 Columbia Missouri: CF_annual_nonagri	1.75
# AWARE 2.0 Devault Pennsylvania CF_annual_nonagri	0.852
aware <- (1.75 + 0.852) /2
((16.9/21) * 100) * aware

# ref:
# Boulay A-M, Bare J, Benini L, et al (2018)
# The WULCA consensus characterization model for water
# scarcity footprints: assessing impacts of water consumption
# based on available water remaining (AWARE). Int J Life Cycle Assess 23:368–378.
# https://wulca-waterlca.org/aware/how-to/

# Add agricultural footprint and manufacturing footprint:
# water scarcity weighted water usage
consum[protein == "alt.meat",`Scarcity-weighted water use per 100g protein` := 
         `Scarcity-weighted water use per 100g protein` + (((16.9/21) * 100)* aware)]
# GHG
consum[protein == "alt.meat", `Greenhouse gas emissions per 100g protein` :=
         `Greenhouse gas emissions per 100g protein` + ((0.51/21) * 100)] # add manufacturing

# Plot footprints of evaluated proteins
# get averages per protein (avg only for country adjusted water use
# rest is global average
# take one lifestyle to not artificially change averages
sum.wat <- consum[lifestyle.type == "omnivore",.(avg = mean(`Scarcity-weighted water use per 100g protein`),
                                 sd = sd(`Scarcity-weighted water use per 100g protein`)),
                              by = .(protein)]
sum.ghg <- consum[lifestyle.type == "omnivore",.(avg = mean(`Greenhouse gas emissions per 100g protein`),
                       sd = sd(`Greenhouse gas emissions per 100g protein`)),
                    by = .(protein)]
# combine
food.foot <- bind_rows(sum.wat[,data := "water"], sum.ghg[, data := "ghg"])

food.foot[, protein := factor(protein,
                              levels = c("beef","lamb","cheese","milk","pork","fish","chicken",
                                         "egg",
                                         "alt.meat","tofu","pulses"),
                              labels = c("Beef","Lamb","Cheese","Milk","Pork","Fish","Chicken",
                                         "Egg",
                                         "Alt-Meat","Tofu","Beans"))]
# get col palette
incandescent <- color("incandescent")

# plot
(gp <- ggplot(food.foot %>% filter(data == "ghg"), aes(x = protein, y = avg)) +
  theme_bw() +
  geom_col(fill = "#663333") +
  labs(y = "",
       x = "",
       title = "Greenhouse gas emissions per 100g protein",
       subtitle = "Greenhouse gas emissions measured in kilograms of carbon\ndioxoide-equivalents.") +
  scale_y_continuous(limits = c(0,55)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks.x = element_blank(),
          #axis.text.x = element_blank()
          plot.title = element_text(colour = "grey20"),
          plot.subtitle = element_text(colour = "grey40", face = "italic", size = 9)
    ))

# save to keep label reference
ggsave("./Figure/ghg.protein_rank.png", gp, width = 15, height = 10, unit = "cm")

# re-level
food.foot[, w.protein := factor(protein,
                              levels = c("Lamb","Pork","Fish","Beef","Egg","Alt-Meat",
                                         "Beans","Cheese","Chicken","Milk","Tofu"))]

(wp <- ggplot() +
  theme_bw() +
  geom_col(data = food.foot %>% filter(data == "water"), 
           aes(x = w.protein, y = avg),
           fill = "#225555") +
  geom_linerange(data = food.foot %>% filter(data == "water" & protein %in% c("Cheese",
                                                                             "Milk")), 
                aes(x = protein, 
                    ymin = avg, ymax = avg + sd), 
                size = 0.5, lineend = "round",
                colour = "#225555") +
  labs(y = "",
       x = "",
       title = "Scarcity-weighted water use per 100g protein",
       subtitle = "Scarcity-weighted water use represents freshwater use weighted\nby local water scarcity (in litres). Dairy was weighted by local water\nscarcity, while other proteins where weighted by their global average.") +
  scale_y_continuous(limits = c(0,78000),
                     breaks = c(0,20000,40000,60000),
                     labels = c(0, "20K","40K","60K")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.x = element_blank()
              plot.title = element_text(colour = "grey20"),
              plot.subtitle = element_text(colour = "grey40", face = "italic", size = 9)
        ))
# save to keep label reference
ggsave("./Figure/water.protein_rank.png", wp, width = 15, height = 10, unit = "cm")

(p <- (gp + theme(axis.text.x = element_blank()) |
         wp + theme(axis.text.x = element_blank())
       ) + 
    plot_layout(ncol = 2, guides = "collect"))
    # plot_annotation(title = "Cumulative 15-year impact of protein intake",
    #                 theme = theme(plot.title = element_text(colour = "grey20", hjust = 0.03))))
ggsave('./Figure/all.ranking.png', p, width = 27, height = 12, units = "cm")


# Extrapolate ------------------------------------------------------------------
# get total impact
consum[, GHG.impact := (per.year / 100) * `Greenhouse gas emissions per 100g protein`]
# kg CO2eq per year
consum[, water.impact := (per.year / 100) * `Scarcity-weighted water use per 100g protein`]
# L of scarcity-weighted water per year

# define factors for plotting
consum[, protein := factor(protein,
                           levels = c("beef","lamb","cheese","milk","pork","fish","chicken",
                                      "egg",
                                      "alt.meat","tofu","pulses"),
                           labels = c("Beef","Lamb","Cheese","Milk","Pork","Fish","Chicken",
                                      "Egg",
                                      "Alt-Meat","Tofu","Beans"))] # ordered by GHG impact

consum[, lifestyle.type := factor(lifestyle.type,
                                  levels = c("omnivore","beginner","expert",
                                             "lacto-ovo vegetarian"),
                                  labels = c("Omnivore", "2d meat-free", "5d meat-free",
                                             "Lacto-Ovo\nVegetarian"))]
consum[, country := factor(country,
                           levels = c("Me","USA","Canada","Austria","Japan"))]
consum[, nested.label := paste(country, lifestyle.type, sep = "-")]
consum[, nested.label := factor(nested.label, 
                                levels = c("Japan-Expert Flexi","Japan-Beginner Flexi", "Japan-Omnivore",
                                           "Austria-Expert Flexi","Austria-Beginner Flexi", "Austria-Omnivore",
                                           "Canada-Expert Flexi","Canada-Beginner Flexi", "Canada-Omnivore",
                                           "USA-Expert Flexi","USA-Beginner Flexi", "USA-Omnivore",
                                           "Me-Vegetarian"))]

# make colour palettes
PRGn <- color("PRGn")
PRGn(10)
incandescent <- color("incandescent")
col.pal <- as.character(rev(incandescent(11)))
color_map <- setNames(col.pal,
                      levels(consum$protein))

consum <- consum %>% arrange(protein, country, lifestyle.type)
#setorder(consum, country, lifestyle.type,)

consum <- consum[,total := sum(per.year/1000), by = .(country,lifestyle.type)]
consum <- consum[,nested.label := paste(country, lifestyle.type, sep = "::")]

(
  p <- plot_ly(type = "bar",
        orientation = 'h',
        data = consum, 
        x = ~per.year/1000, y = list(~country, ~lifestyle.type),
        color = ~protein,
        colors = color_map,
        customdata= ~protein,
        marker = list(line = list(color = "grey90",width = 0.5)),
        hovertemplate = paste('%{customdata}:',
                              '%{x:.2f}',
                              '<extra></extra>')) %>%
  # add_annotations(data = consum %>% 
  #                   filter(lifestyle.type %in% c("2d meat-free","Lacto-Ovo\nVegetarian")) %>%
  #                   dplyr::select(total, nested.label) %>% unique(),
  #                 x = ~total, y = ~nested.label, 
  #                 text = ~total, showarrow = F) %>%
  layout(barmode = "stack",
         margin = list(b = 85, l = 85, r= 85, t=85),
         xaxis = list(title = list(text = "Annual Protein Intake (kg yr<sup>-1</sup>)",
                                   font = list(size = 18))),
         yaxis = list(categoryorder = "array"),
         title = "Proportion of protein sources by country and diet type",
         legend = list(title=list(text='<b> Protein source </b><br><span style="font-size: 10px; font-weight: normal;"> Sorted by GHG impact</span>')))
)

htmlwidgets::saveWidget(p, "./Figure/protein.sources.html")
save_image(p, "./Figure/protein.sources.png", width = 800, height = 550, scale = 2)
# ggplot(consum, aes(y = lifestyle.type, x = per.year)) +
#   theme_bw() +
#   facet_wrap(~ country, strip.position = "left", scale = "free_y") +
#   geom_col(aes(fill = protein), colour = "grey70") +
#   scale_fill_manual(values = rev(incandescent(10))) +
#   theme(strip.placement = "outside",
#         panel.spacing = unit(0,"cm"))

# Time-series ------------------------------------------------------------------

# sum by country-lifestyle category
annual <- consum[, .(annual.GHG = sum(GHG.impact),
           annual.scar.water = sum(water.impact)),
       by = .(lifestyle.type, country)]

# number of years
years <- 2011:2026
# use lapply to create a list of data frames
ls.yrs <- lapply(years, function(y){
  annual$Year <- y
  return(annual)
})

# merge together
all.annual <- bind_rows(ls.yrs)

# calculate cumulative impact over years
cum.annual <- ddply(all.annual, .(lifestyle.type, country), function(x){
  x <- x %>% arrange(Year) %>% setDT()
  x[, c("GHG.cum","water.cum") := list(cumsum(annual.GHG),
                                       cumsum(annual.scar.water))]
  return(x)
})
# sanity check
setDT(cum.annual)
cum.annual[, .(min = min(GHG.cum),
               max = max(GHG.cum)), by = .(lifestyle.type, country)]
cum.annual[, nested.label := paste(country, lifestyle.type, sep = " | ")]

cum.annual[, nested.label := factor(nested.label, 
                                levels = c("USA | Omnivore", "USA | 2d meat-free","USA | 5d meat-free",
                                           "Canada | Omnivore", "Canada | 2d meat-free","Canada | 5d meat-free", 
                                           "Austria | Omnivore", "Austria | 2d meat-free","Austria | 5d meat-free", 
                                           "Japan | Omnivore", "Japan | 2d meat-free","Japan | 5d meat-free",
                                           "Me | Lacto-Ovo\nVegetarian"))]
cum.annual[, lifestyle.type := factor(lifestyle.type,
                                      levels = c("Omnivore", "2d meat-free","5d meat-free",
                                                 "Lacto-Ovo\nVegetarian"))]
my.cols <- c('#72190E','#A5170E','#DC050C', # USA
             '#E65518', '#EE8026', '#F6C141', # Canada
             '#1965B0', '#5289C7', '#7BAFDE', # Austria
             '#882E72','#AA6F9E','#BA8DB4', #Japan
             '#90C987')# Japan
diet.cols <- c('#72190E','#EE8026','#F6C141',
               #'#7BAFDE',
               #'#90C987',
               'grey70')

# calculate a rank by year
cum.annual[, ghg.rank := rank(GHG.cum, ties.method = "first"), by = .(Year)]
cum.annual[, water.rank := rank(water.cum, ties.method = "first"), by = .(Year)]

# get deltas
delta.ghg <- cum.annual[country == "USA" & lifestyle.type == "Omnivore" &
                          Year == 2026,]$GHG.cum -
  cum.annual[country == "Me" & Year == 2026,]$GHG.cum
delta.water <- cum.annual[country == "USA" & lifestyle.type == "Omnivore" &
                          Year == 2026,]$water.cum -
  cum.annual[country == "Me" & Year == 2026,]$water.cum

(ghg <- ggplot(cum.annual %>% filter(Year == 2026), aes(xmin = 0,
                       xmax = GHG.cum,
                       y = ghg.rank,
                       ymin = ghg.rank - 0.45,
                       ymax = ghg.rank + 0.45,
                       label = GHG.cum,
                       fill =lifestyle.type)) +
  theme_bw() +
  annotate(geom = "rect", xmin =  cum.annual[country == "Me" & Year == 2026,]$GHG.cum,
           xmax = cum.annual[country == "USA" & lifestyle.type == "Omnivore" &
                               Year == 2026,]$GHG.cum,
           ymin = 1 - 0.45, ymax = 1 + 0.45, y = 1,
           fill = '#90C987', alpha = 0.5) +
    annotate(geom = "text", label = "- 76K", y = 1, x = cum.annual[country == "USA" & lifestyle.type == "Omnivore" &
                                                                     Year == 2026,]$GHG.cum - 7000,
             colour = "grey30") +
  geom_rect(aes(fill = lifestyle.type)) +
  #geom_text(aes(x = 500, label = country), hjust = "left", colour = "white") +
  scale_fill_manual(values = diet.cols, name = "Diet Type") +
  theme(strip.placement = "outside",
        panel.spacing = unit(0,"cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, colour = "grey40"),
        plot.title = element_text(colour = "grey20"),
        plot.subtitle = element_text(colour = "grey40", face = "italic", size = 9)) +
  scale_x_continuous(breaks = c(0, 25000, 50000, 75000), labels = c(0,"25K","50K","75K")) +
  labs(title = "Total GHG emissions",
       subtitle = "Greenhouse gas emissions measured in kilograms of carbon\ndioxoide-equivalents.", 
                                   x = "", y = ""))

(wat <- ggplot(cum.annual %>% filter(Year == 2026), aes(xmin = 0,
                                                xmax = water.cum,
                                                y = water.rank,
                                                ymin = water.rank - 0.45,
                                                ymax = water.rank + 0.45,
                                                label = water.cum,
                                                fill =lifestyle.type)) +
  theme_bw() +
  geom_rect(aes(fill = lifestyle.type)) +
    annotate(geom = "rect", xmin =  cum.annual[country == "Me" & Year == 2026,]$water.cum,
             xmax = cum.annual[country == "USA" & lifestyle.type == "Omnivore" &
                                 Year == 2026,]$water.cum,
             ymin = 1 - 0.45, ymax = 1 + 0.45, y = 1,
             fill = '#90C987', alpha = 0.5) +
    annotate(geom = "text", label = "- 87M", y = 1, x = cum.annual[country == "USA" & lifestyle.type == "Omnivore" &
                                                                     Year == 2026,]$water.cum - 10000000,
             colour = "grey30") +
  # geom_text(aes(x = GHG.cum, label = as.character(round(GHG.cum / 1000,0))),
  #               hjust = "left") +
  #geom_text(aes(x = 1000000, label = country), hjust = "left", colour = "white") +
  scale_fill_manual(values = diet.cols, name = "Diet Type") +
  theme(strip.placement = "outside",
        panel.spacing = unit(0,"cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, colour = "grey40"),
        plot.title = element_text(colour = "grey20"),
        plot.subtitle = element_text(colour = "grey40", face = "italic", size = 9)) +
    
  scale_x_continuous(breaks = c(0, 50000000, 100000000, 150000000, 200000000), labels = c(0,"50M","100M","150M","200M")) +
  labs(title = "Total Scarcity-Weighted Water Use",
       subtitle = "Scarcity-weighted water use represents freshwater use weighted\nby local water scarcity (in litres). Dairy was weighted by local water\nscarcity, while other proteins where weighted by their global average.", x = "", y = ""))

(p <- (ghg | wat) +
    plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Cumulative 15-year impact of protein intake of a single person",
                  theme = theme(plot.title = element_text(colour = "grey20", hjust = 0.03))))
ggsave('./Figure/total.impact.png', p, width = 27, height = 12, units = "cm")


# make another version without country names
# comment out geom_text above an re-save
#ggsave('./Figure/no-country.total.impact.png', p, width = 27, height = 12, units = "cm")


(p <- (ghg | wat) +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(title = "Cumulative 15-year impact of protein intake",
                    theme = theme(plot.title = element_text(colour = "grey20", hjust = 0.03))))
ggsave('./Figure/total.impact.png', p, width = 27, height = 12, units = "cm")


(cum.annual[country == "USA" & lifestyle.type == "Omnivore" & Year == 2026,"GHG.cum"] -
  cum.annual[country == "USA" & lifestyle.type == "2d meat-free" & Year == 2026,"GHG.cum"]) / 1000
# saving 35 kg

(cum.annual[country == "USA" & lifestyle.type == "Omnivore" & Year == 2026,"GHG.cum"] -
    cum.annual[country == "USA" & lifestyle.type == "5d meat-free" & Year == 2026,"GHG.cum"]) / 1000
# saving 59 kg

# What do these numbers mean?

# olympic size pool is 2500000 litres
delta.water / 2500000
# Saved 34.8 olympic sized pools
# water consumption per day
3.7 # litres per day
3.7*365 # 1350 litres per year
delta.water / (3.7*365)
# water for 64,422 people

# Mature tree absorbtion 22 kg of CO2 per year
75000/(22*15)
# 3409 trees
# Tokyo-NYK (round-trip): 1 tonne per passenger
75000/1000 # 75 round-trips saved
# Total emission per km: cost is about 0.09 kg CO2 per km (Boeing 777 or 787)
# to emit 1000kg one has to fly around 11,111km
# Earth circumference 40,000km, one full trip around Earth 3.6 tonnes or 3,600 kg
75000/3600 # 20.83 passenger-trips complete circumnavigations of the Earth


means <- cum.annual[,.(avg.ghg = mean(GHG.cum),avg.water = mean(water.cum)), by = .(lifestyle.type)]

# per year GHG savings
(means[lifestyle.type == "5d meat-free",]$avg.ghg - 
  means[lifestyle.type == "Lacto-Ovo\nVegetarian",]$avg.ghg)/15
(means[lifestyle.type == "2d meat-free",]$avg.ghg - 
    means[lifestyle.type == "Lacto-Ovo\nVegetarian",]$avg.ghg)/15

(means[lifestyle.type == "Omnivore",]$avg.ghg -
    means[lifestyle.type == "2d meat-free",]$avg.ghg)/15
(means[lifestyle.type == "Omnivore",]$avg.ghg-
    means[lifestyle.type == "5d meat-free",]$avg.ghg)/15

# 15 year savings
(means[lifestyle.type == "Omnivore",]$avg.ghg -
    means[lifestyle.type == "2d meat-free",]$avg.ghg)
(means[lifestyle.type == "Omnivore",]$avg.ghg-
    means[lifestyle.type == "5d meat-free",]$avg.ghg)

# per year water savings
(means[lifestyle.type == "5d meat-free",]$avg.water - 
    means[lifestyle.type == "Lacto-Ovo\nVegetarian",]$avg.water)/15
(means[lifestyle.type == "2d meat-free",]$avg.water - 
    means[lifestyle.type == "Lacto-Ovo\nVegetarian",]$avg.water)/15

(means[lifestyle.type == "Omnivore",]$avg.water -
    means[lifestyle.type == "2d meat-free",]$avg.water)/15
(means[lifestyle.type == "Omnivore",]$avg.water-
    means[lifestyle.type == "5d meat-free",]$avg.water)/15

# 15 year savings
(means[lifestyle.type == "Omnivore",]$avg.water -
  means[lifestyle.type == "2d meat-free",]$avg.water)
(means[lifestyle.type == "Omnivore",]$avg.water-
    means[lifestyle.type == "5d meat-free",]$avg.water)
