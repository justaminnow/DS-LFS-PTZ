############################
# DS + PTZ DM graph panel
# Elle Patullo
# File 1 of 1
# 2022 annotated file, for public reference
############################

# NOTE: the color palettes used in these graphs are the ones that will likely be used in my manuscript
  # I HIGHLY recommend you explore paletteer to find a color palette that fits your own style

# packages
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(matrixStats)
library(forcats)
library(paletteer)
library(gridExtra)
library(ggpubr)

# set working directory
setwd("~/Thesis Project/Data/Graph files/DS")

  # resources
# http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# https://www.statology.org/ggplot-line-thickness/
# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
# https://stackoverflow.com/questions/33529116/ggplot2-stagger-axis-labels

################################################

##### Graph 1: DS 48hph bar #####

#### Step 1: Load data ####

DS_PTZ_48hph_bar <- read_csv("30secbins_EDITED_022822_DS_PTZ_48hph_RETRACK.csv")
View(DS_PTZ_48hph_bar)

# head(DS_PTZ_48hph_bar)

# A tibble: 6 x 22
# treatment trial       replicate stamp   distance_se total_distance_~ mean_velocity max_velocity velocity_se
# <chr>     <chr>       <chr>     <chr>         <dbl>            <dbl>         <dbl>        <dbl>       <dbl>
# 1 2mM       Trial     2 A3        0:00:0~     0.00835             265.          8.85         40.9       0.209
# 2 2mM       Trial     2 A3        0:00:3~     0.00882             268.          8.93         43.8       0.220
# 3 2mM       Trial     2 A3        0:01:0~     0.0154              303.         10.1          74.2       0.384
# 4 2mM       Trial     2 A3        0:01:3~     0.0141              297.          9.91         65.7       0.353
# 5 2mM       Trial     2 A3        0:02:0~     0.0119              201.          6.71         46.3       0.296
# 6 2mM       Trial     2 A3        0:02:3~     0.0124              161.          5.36        112.        0.311
# ... with 13 more variables: cruise_se <dbl>, cruise_freq <dbl>, total_cruise <dbl>, burst_se <dbl>,
#   burst_freq <dbl>, total_burst <dbl>, freeze_se <dbl>, freeze_freq <dbl>, total_freeze <dbl>,
#   center_se <dbl>, center_freq <dbl>, total_center <dbl>, photoperiod <chr>


#### Step 2: Prep and reorganize the data ####

# remove NA values
DS_PTZ_48hph_bar.omit <-DS_PTZ_48hph_bar[!(is.na(DS_PTZ_48hph_bar$total_distance_moved)), ]
View(DS_PTZ_48hph_bar.omit)

# calculate mean and sd
DM_48h_bar_means <- DS_PTZ_48hph_bar.omit %>% # creating a pipe
  mutate(treatment = fct_relevel(treatment, "Control", "2mM", "4mM", "8mM", "12mM", "16mM"),
         photoperiod = fct_relevel(photoperiod, "dark1", "dark2", "light1", "dark3", "light2", "dark4", "dark5")) %>%
  group_by(treatment, photoperiod) %>% 
  summarize(MeanDM = mean(total_distance_moved),
            MeanDM.sd = sd(total_distance_moved))
View(DM_48h_bar_means)

# head(DM_48h_bar_means)

# A tibble: 6 x 4
# Groups:   treatment [1]
# treatment photoperiod MeanDM MeanDM.sd
# <fct>     <fct>        <dbl>     <dbl>
# 1 Control   dark1         43.2      34.4
# 2 Control   dark2         43.1      35.1
# 3 Control   light1        46.0      36.1
# 4 Control   dark3         39.7      28.0
# 5 Control   light2        47.3      39.3
# 6 Control   dark4         45.8      39.4

# summary(DM_48h_bar_means)
# str(DM_48h_bar_means)

# save file (to calculate se in excel)
  # I calculate SE in excel because I already know how, but you can use whichever method you like
    # first, I add n values from my Dunn's test data file
    # Then, I calculate SE
    # The formula I used was standard deviation (SD) divided by the square root of the sample size (n)
      # SD/sqrt(n)

write.csv(DM_48h_bar_means, "DM_48h_bar_means.csv")


#### Step 3: Merge Ethovision data with stats ####

# upload data w/ se information
DM_48h_bar_means <- read_csv("DM_48h_bar_means.csv") 
View(DM_48h_bar_means)

# head(DM_48h_bar_means)

# A tibble: 6 x 7
# ...1 treatment photoperiod MeanDM MeanDM.sd     n    se
# <dbl> <chr>     <chr>        <dbl>     <dbl> <dbl> <dbl>
# 1     1 Control   dark1         43.2      34.4    16  8.59
# 2     2 Control   dark2         43.1      35.1    16  8.78
# 3     3 Control   light1        46.0      36.1    16  9.02
# 4     4 Control   dark3         39.7      28.0    16  7.01
# 5     5 Control   light2        47.3      39.3    16  9.83
# 6     6 Control   dark4         45.8      39.4    16  9.84


# merge temperature and photoperiod columns
DM_48h_bar_means <- DM_48h_bar_means %>%
  unite(treatment_temp, treatment, photoperiod, sep = "_")
  # I am creating an extra column that merges the treatment value and photoperiod, since
    # I want to merge the files by these variables

# upload Dunn's test data 
DS_PTZ_48hph_DUNNS_simp <- read_csv("DS_PTZ_48h_DUNNS_simp.csv")
View(DS_PTZ_48hph_DUNNS_simp)

  # NOTE: This file was made at the end of my file named "Total Distance Moved Stats"
    # this file is publically available on Github

# head(DS_PTZ_48hph_DUNNS_simp)

# A tibble: 6 x 9
# photoperiod treatment .y.    group1 group2 statistic        p  p.adj p.adj.signif
# <chr>       <chr>     <chr>   <dbl>  <dbl>     <dbl>    <dbl>  <dbl> <chr>       
#   1 dark1       2mM       MeanDM      0      2      2.42 0.0156   0.233  ns          
#   2 dark1       4mM       MeanDM      0      4      2.99 0.00277  0.0415 *           
#   3 dark1       8mM       MeanDM      0      8      3.21 0.00133  0.0199 *           
#   4 dark1       12mM      MeanDM      0     12      3.36 0.000776 0.0116 *           
#   5 dark1       16mM      MeanDM      0     16      2.17 0.0301   0.451  ns          
#   6 dark2       2mM       MeanDM      0      2      2.52 0.0119   0.178  ns 

# merge the two data files
DM_48h_bar_merge <- DS_PTZ_48hph_DUNNS_simp %>%
  unite(treatment_temp, treatment, photoperiod, sep = "_") %>% # create merged temp & photoperiod column in Dunn's data file
  full_join(DM_48h_bar_means, by = "treatment_temp") %>% # join Dunn's and mean value data 
  mutate(p.adj.signif = ifelse(is.na(p.adj.signif), "ns", p.adj.signif)) %>%  # notates NA values as "ns" 
  separate(treatment_temp, into = c("treatment", "photoperiod"), sep = "_") %>% # separate the merged column back into two separate columns for treatment and photoperiod 
  mutate(photoperiod = as.factor(photoperiod), 
         treatment = as.factor(treatment)) # classify data type as factor instead of character
View(DM_48h_bar_merge)

# head(DM_48h_bar_merge)

# A tibble: 6 x 13
# treatment photoperiod .y.    group1 group2 statistic        p  p.adj p.adj.signif MeanDM MeanDM.sd     n
# <fct>     <fct>       <chr>   <dbl>  <dbl>     <dbl>    <dbl>  <dbl> <chr>         <dbl>     <dbl> <dbl>
# 1 2mM       dark1       MeanDM      0      2      2.42 0.0156   0.233  ns             77.6      57.9    18
# 2 4mM       dark1       MeanDM      0      4      2.99 0.00277  0.0415 *              88.6      61.5    18
# 3 8mM       dark1       MeanDM      0      8      3.21 0.00133  0.0199 *             102.       71.9    17
# 4 12mM      dark1       MeanDM      0     12      3.36 0.000776 0.0116 *             107.       84.1    20
# 5 16mM      dark1       MeanDM      0     16      2.17 0.0301   0.451  ns             76.7      59.9    19
# 6 2mM       dark2       MeanDM      0      2      2.52 0.0119   0.178  ns             76.6      51.3    18
# ... with 1 more variable: se <dbl>

# transform: relevel and recode
DM_48h_bar.final <- DM_48h_bar_merge %>% # creating a pipe
  mutate(treatment = fct_relevel(treatment, "Control", "2mM", "4mM", "8mM", "12mM", "16mM"),
         photoperiod = fct_relevel(photoperiod, "dark1", "dark2", "light1", "dark3", "light2", "dark4", "dark5"),
         photoperiod = fct_recode(photoperiod,
                                  "Dark 1" = "dark1",
                                  "Dark 2" = "dark2",
                                  "Light 1" = "light1",
                                  "Dark 3" = "dark3",
                                  "Light 2" = "light2",
                                  "Dark 4" = "dark4",
                                  "Dark 5" = "dark5"),
         p.adj.signif = fct_recode(p.adj.signif,
                                   " " = "ns",
                                   "*" = "*", # not including any higher significance values since there are none
                                   )) %>%
  
  group_by(treatment, photoperiod, .y., group1, group2, MeanDM, se, p.adj.signif) %>% 
  summarize()
View(DM_48h_bar.final)

# head(DM_48h_bar.final)

# A tibble: 6 x 8
# Groups:   treatment, photoperiod, .y., group1, group2, MeanDM, se [6]
# treatment photoperiod .y.   group1 group2 MeanDM    se p.adj.signif
# <fct>     <fct>       <chr>  <dbl>  <dbl>  <dbl> <dbl> <fct>       
# 1 Control   Dark 1      NA        NA     NA   43.2  8.59 " "         
# 2 Control   Dark 2      NA        NA     NA   43.1  8.78 " "         
# 3 Control   Light 1     NA        NA     NA   46.0  9.02 " "         
# 4 Control   Dark 3      NA        NA     NA   39.7  7.01 " "         
# 5 Control   Light 2     NA        NA     NA   47.3  9.83 " "         
# 6 Control   Dark 4      NA        NA     NA   45.8  9.84 " " 

# summary(DM_48h_bar.final)


#### Step 4: Make bar graph ####

DM_bar <- ggplot(DM_48h_bar.final, aes(x=photoperiod, y=MeanDM, 
                                       fill = treatment))+
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = p.adj.signif, group = treatment), 
            position = position_dodge(width = 0.9),
            vjust = -4) +
  theme_minimal()+
  theme(legend.position = "none") +
  scale_fill_paletteer_d("PNWColors::Sailboat") +
  labs(x = "Photoperiod",
       y = "Mean Distance Moved (mm)",
       tag = "A") +
  ylim(0,150) +
  geom_errorbar(aes(ymin=MeanDM-se, ymax=MeanDM+se), width=.2,
                position=position_dodge(.9))

DM_bar


################################################################################


##### Graph 2: DS 48hph Line #####

# NOTE: we are NOT using error bars in this graph since it will obscure the lines and make the graph more difficult to read
  # for similar reasons, we are also not including significance data in this figure
  # HOWEVER, it is not a bad idea to include a chart with se and significance data in supplementary figures

#### Step 1: Load data ####
DS_PTZ_48hph_line <- read_csv("1minbins_EDITED_022822_DS_PTZ_48hph_RETRACK.csv")
View(DS_PTZ_48hph_line)

head(DS_PTZ_48hph_line)

# A tibble: 6 x 22
# treatment trial       replicate stamp     time distance_moved_~ total_distance_~ mean_velocity max_velocity
# <chr>     <chr>       <chr>     <chr>    <dbl>            <dbl>            <dbl>         <dbl>        <dbl>
# 1 2mM       Trial     2 A3        0:00:00~     1          0.00607            533.           8.89         43.8
# 2 2mM       Trial     2 A3        0:01:00~     2          0.0104             600.          10.0          74.2
# 3 2mM       Trial     2 A3        0:02:00~     3          0.00862            362.           6.03        112. 
# 4 2mM       Trial     2 A3        0:03:00~     4          0.00762            226.           3.76         57.2
# 5 2mM       Trial     2 A3        0:04:00~     5          0.00342             87.4          1.46         40.5
# 6 2mM       Trial     2 A3        0:05:00~     6          0.00716            233.           3.89         62.9
# ... with 13 more variables: velocity_se <dbl>, cruise_se <dbl>, cruise_freq <dbl>, total_cruise <dbl>,
#   burst_se <dbl>, burst_freq <dbl>, total_burst <dbl>, freeze_se <dbl>, freeze_freq <dbl>,
#   total_freeze <dbl>, center_se <dbl>, center_freq <dbl>, total_center <dbl>

#### Step 2: Prep and reorganize the data ####

# remove NA values
DS_PTZ_48hph_line.omit <-DS_PTZ_48hph_line[!(is.na(DS_PTZ_48hph_line$total_distance_moved)), ]
View(DS_PTZ_48hph_line.omit)

# calculate means
DM_48h_line_means <- DS_PTZ_48hph_line.omit %>% # creating a pipe
  mutate(treatment = fct_relevel(treatment, "Control", "2mM", "4mM", "8mM", "12mM", "16mM")) %>%
  group_by(treatment, time) %>% 
  summarize(MeanDM = mean(total_distance_moved))
View(DM_48h_line_means)
# summary(DM_48h_line_means)

# head(DM_48h_line_means)

# A tibble: 6 x 3
# Groups:   treatment [1]
# treatment  time MeanDM
# <fct>     <dbl>  <dbl>
# 1 Control       1   82.3
# 2 Control       2   91.1
# 3 Control       3   88.5
# 4 Control       4   98.2
# 5 Control       5   71.8
# 6 Control       6   79.5


#### Step 3: make graph ####

DM_line <- ggplot(DM_48h_line_means, aes(x = time, 
                              y = MeanDM, 
                              group = treatment, color = treatment)) +
  geom_line(size = 0.75) + 
  geom_point() +
  scale_color_paletteer_d("PNWColors::Sailboat") + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Time (min)",
       y = "Mean Distance Moved (mm)",
       tag = "C") +
  ylim(0,400) +
  scale_x_continuous(breaks = seq(0, 50, 5))


DM_line


################################################################################


##### Graph 3: DS + PTZ 72hph bar ##### 

#### Step 1: load data
DS_PTZ_72hph_bar <- read_csv("30secbins_EDITED_030122_RETRACK_DS_PTZ_72hph.csv")
View(DS_PTZ_72hph_bar)

# head(DS_PTZ_72hph_bar)

# A tibble: 6 x 21
# treatment trial     replicate stamp total_distance_~ total_distance_~ mean_velocity velocity_se mean_cruise
# <chr>     <chr>     <chr>     <chr>            <dbl>            <dbl>         <dbl>       <dbl>       <dbl>
# 1 2mM       Trial   ~ A6        0:00~         0.0124              138.           4.59      0.309       0.311 
# 2 2mM       Trial   ~ A6        0:00~         0.000847             31.5          1.05      0.0212     NA     
# 3 2mM       Trial   ~ A6        0:01~         0.000907             31.3          1.04      0.0227     NA     
# 4 2mM       Trial   ~ A6        0:01~         0.000941             30.9          1.03      0.0235     NA     
# 5 2mM       Trial   ~ A6        0:02~         0.00424              49.7          1.66      0.106       0.0600
# 6 2mM       Trial   ~ A6        0:02~         0.0160              322.          10.8       0.401       0.235 
# ... with 12 more variables: cruise_se <dbl>, total_cruise <dbl>, burst_se <dbl>, burst_freq <dbl>,
#   total_burst <dbl>, freeze_se <dbl>, freeze_freq <dbl>, total_freeze <dbl>, center_se <dbl>,
#   center_freq <dbl>, total_center <dbl>, photoperiod <chr>


#### Step 2: Prep and reorganize data ####

# remove NA values
DS_PTZ_72hph_bar.omit <-DS_PTZ_72hph_bar[!(is.na(DS_PTZ_72hph_bar$total_distance_moved)), ]
View(DS_PTZ_72hph_bar.omit)

# calculate means and SD

DM_72h_bar_means <- DS_PTZ_72hph_bar.omit %>% # creating a pipe
  mutate(treatment = fct_relevel(treatment, "Control", "2mM", "4mM", "8mM", "12mM", "16mM"),
         photoperiod = fct_relevel(photoperiod, "dark1", "dark2", "light1", "dark3", "light2", "dark4", "dark5")) %>%
  group_by(treatment, photoperiod) %>% 
  summarize(MeanDM = mean(total_distance_moved),
            MeanDM.sd = sd(total_distance_moved))
View(DM_72h_bar_means)
# summary(DM_72h_bar_means)

# head(DM_72h_bar_means)

# A tibble: 6 x 4
# Groups:   treatment [1]
# treatment photoperiod MeanDM MeanDM.sd
# <fct>     <fct>        <dbl>     <dbl>
# 1 Control   dark1         47.8      33.5
# 2 Control   dark2         40.6      32.6
# 3 Control   light1        37.6      27.0
# 4 Control   dark3         33.0      19.3
# 5 Control   light2        40.6      36.5
# 6 Control   dark4         36.1      26.3

# save file (to calculate se in excel)
write.csv(DM_72h_bar_means, "DM_72h_bar_means.csv")

# upload data w/ se information

DM_72h_bar_means <- read_csv("DM_72h_bar_means.csv")
View(DM_72h_bar_means)

# head(DM_72h_bar_means)

# A tibble: 6 x 6
# treatment photoperiod MeanDM MeanDM.sd     n    se
# <chr>     <chr>        <dbl>     <dbl> <dbl> <dbl>
# 1 Control   dark1         47.8      33.5    19  7.69
# 2 Control   dark2         40.6      32.6    19  7.49
# 3 Control   light1        37.6      27.0    19  6.19
# 4 Control   dark3         33.0      19.3    19  4.42
# 5 Control   light2        40.6      36.5    19  8.38
# 6 Control   dark4         36.1      26.3    19  6.04

# NOTE: for this data, I did not need to merge significance data and means data into one file
  # this is because there was no significant difference between TDM in any treatment group. 

# transform: relevel and recode
DM_72h_bar.final <- DM_72h_bar_means %>% # creating a pipe
  mutate(treatment = fct_relevel(treatment, "Control", "2mM", "4mM", "8mM", "12mM", "16mM"),
         photoperiod = fct_relevel(photoperiod, "dark1", "dark2", "light1", "dark3", "light2", "dark4", "dark5"),
         photoperiod = fct_recode(photoperiod,
                                  "Dark 1" = "dark1",
                                  "Dark 2" = "dark2",
                                  "Light 1" = "light1",
                                  "Dark 3" = "dark3",
                                  "Light 2" = "light2",
                                  "Dark 4" = "dark4",
                                  "Dark 5" = "dark5")) %>%
  group_by(treatment, photoperiod, MeanDM, se) %>% 
  summarize()
View(DM_72h_bar.final)
# summary(DM_72h_bar.final)


#### Step 3: Make bar graph ####

DM_72_bar <- ggplot(DM_72h_bar.final, aes(x=photoperiod, y=MeanDM, 
                                          fill = treatment))+
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()+
  scale_fill_paletteer_d("PNWColors::Sailboat") +
  labs(x = "Photoperiod",
       y = "Mean Distance Moved (mm)",
       tag = "B",
       fill = "Treatment") +
  ylim(0,150) +
  geom_errorbar(aes(ymin=MeanDM-se, ymax=MeanDM+se), width=.2,
                position=position_dodge(.9), color = "black")

DM_72_bar


################################################################################


##### Graph 4: DS + PTZ 72hph line #####

#### Step 1: Load data ####

DS_PTZ_72hph_line <- read_csv("1minbins_EDITED_030122_RETRACK_DS_PTZ_72hph.csv")
View(DS_PTZ_72hph_line)

# head(DS_PTZ_72hph_line)

# A tibble: 6 x 21
# treatment trial       replicate stamp      time total_distance_~ total_distance_~ mean_velocity velocity_se
# <chr>     <chr>       <chr>     <chr>     <dbl>            <dbl>            <dbl>         <dbl>       <dbl>
# 1 2mM       Trial     1 A6        0:00:00-~     1         0.00645             169.           2.82      0.161 
# 2 2mM       Trial     1 A6        0:01:00-~     2         0.000653             62.2          1.04      0.0163
# 3 2mM       Trial     1 A6        0:02:00-~     3         0.00953             372.           6.20      0.238 
# 4 2mM       Trial     1 A6        0:03:00-~     4         0.00828             489.           8.15      0.207 
# 5 2mM       Trial     1 A6        0:04:00-~     5         0.00681             233.           3.89      0.170 
# 6 2mM       Trial     1 A6        0:05:00-~     6         0.00745             286.           4.77      0.186 
# ... with 12 more variables: mean_cruise <dbl>, cruise_se <dbl>, total_cruise <dbl>, burst_se <dbl>,
#   burst_freq <dbl>, total_burst <dbl>, freeze_se <dbl>, freeze_freq <dbl>, total_freeze <dbl>,
#   center_se <dbl>, center_freq <dbl>, total_center <dbl>


#### Step 2: Prep and reorganize data ####

# remove NA values
DS_PTZ_72hph.omit <-DS_PTZ_72hph[!(is.na(DS_PTZ_72hph$total_distance_moved)), ]
View(DS_PTZ_72hph.omit)

# Calculate means 
DM_72h_line_means <- DS_PTZ_72hph.omit %>% # creating a pipe
  mutate(treatment = fct_relevel(treatment, "Control", "2mM", "4mM", "8mM", "12mM", "16mM")) %>%
  group_by(treatment, time) %>% 
  summarize(MeanDM = mean(`total_distance_moved`, na.omit = T))
View(DM_72h_line_means)
# summary(DM_72h_line_means)

# head(DM_72h_line_means)

# A tibble: 6 x 3
# Groups:   treatment [1]
# treatment  time MeanDM
# <fct>     <dbl>  <dbl>
# 1 Control       1   88.8
# 2 Control       2   93.5
# 3 Control       3   97.7
# 4 Control       4  103. 
# 5 Control       5   95.2
# 6 Control       6   86.5

#### Step 3: Make graph ####

DM_72_line <- ggplot(DM_72h_line_means, aes(x = time, 
                              y = MeanDM, 
                              group = treatment, color = treatment)) +
  geom_line(size = 0.75) + 
  geom_point() +
  scale_color_paletteer_d("PNWColors::Sailboat") + 
  theme_minimal() +
  labs(x = "Time (min)",
       y = "Mean Distance Moved (mm)",
       tag = "D",
       color = "Treatment") +
  ylim(0,400) +
  scale_x_continuous(breaks = seq(0, 50, 5)) 


DM_72_line


##### Graph 5: DS Bar + line panel #####

DS_DM_panel <- grid.arrange(DM_bar, DM_72_bar, DM_line, DM_72_line,
                            widths = c(16,20)) 
