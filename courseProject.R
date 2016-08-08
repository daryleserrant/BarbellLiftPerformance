library(ggplot2)
library(caret)
library(plyr)

training <- read.csv("pml-training.csv", na.strings=c("NA",""))

# View summary information on the training set
str(training)

#'data.frame':   19622 obs. of  160 variables:
# X                       : int  1 2 3 4 5 6 7 8 9 10 ...
# user_name               : Factor w/ 6 levels "adelmo","carlitos",..: 2 2 2 2 2 2 2 2 2 2 ...
# raw_timestamp_part_1    : int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
# raw_timestamp_part_2    : int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
# cvtd_timestamp          : Factor w/ 20 levels "02/12/2011 13:32",..: 9 9 9 9 9 9 9 9 9 9 ...
# new_window              : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
# num_window              : int  11 11 11 12 12 12 12 12 12 12 ...
# roll_belt               : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
# pitch_belt              : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
# yaw_belt                : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
# total_accel_belt        : int  3 3 3 3 3 3 3 3 3 3 ...
# kurtosis_roll_belt      : Factor w/ 396 levels "-0.016850","-0.021024",..: NA NA NA NA NA NA NA NA NA NA ...
# kurtosis_picth_belt     : Factor w/ 316 levels "-0.021887","-0.060755",..: NA NA NA NA NA NA NA NA NA NA ...
# kurtosis_yaw_belt       : Factor w/ 1 level "#DIV/0!": NA NA NA NA NA NA NA NA NA NA ...
# skewness_roll_belt      : Factor w/ 394 levels "-0.003095","-0.010002",..: NA NA NA NA NA NA NA NA NA NA ...
# skewness_roll_belt.1    : Factor w/ 337 levels "-0.005928","-0.005960",..: NA NA NA NA NA NA NA NA NA NA ...
# skewness_yaw_belt       : Factor w/ 1 level "#DIV/0!": NA NA NA NA NA NA NA NA NA NA ...
# max_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
# max_picth_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
# max_yaw_belt            : Factor w/ 67 levels "-0.1","-0.2",..: NA NA NA NA NA NA NA NA NA NA ...
# min_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
# min_pitch_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
# min_yaw_belt            : Factor w/ 67 levels "-0.1","-0.2",..: NA NA NA NA NA NA NA NA NA NA ...
# amplitude_roll_belt     : num  NA NA NA NA NA NA NA NA NA NA ...
# amplitude_pitch_belt    : int  NA NA NA NA NA NA NA NA NA NA ...
# amplitude_yaw_belt      : Factor w/ 3 levels "#DIV/0!","0.00",..: NA NA NA NA NA NA NA NA NA NA ...
# var_total_accel_belt    : num  NA NA NA NA NA NA NA NA NA NA ...
# avg_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
# stddev_roll_belt        : num  NA NA NA NA NA NA NA NA NA NA ...
# var_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
# avg_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
# stddev_pitch_belt       : num  NA NA NA NA NA NA NA NA NA NA ...
# var_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
# avg_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
# stddev_yaw_belt         : num  NA NA NA NA NA NA NA NA NA NA ...
# var_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
# gyros_belt_x            : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
# gyros_belt_y            : num  0 0 0 0 0.02 0 0 0 0 0 ...
# gyros_belt_z            : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
# accel_belt_x            : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
# accel_belt_y            : int  4 4 5 3 2 4 3 4 2 4 ...
# accel_belt_z            : int  22 22 23 21 24 21 21 21 24 22 ...
# magnet_belt_x           : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
# magnet_belt_y           : int  599 608 600 604 600 603 599 603 602 609 ...
# magnet_belt_z           : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
# roll_arm                : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
# pitch_arm               : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
# yaw_arm                 : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
# total_accel_arm         : int  34 34 34 34 34 34 34 34 34 34 ...
# var_accel_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
# avg_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
# stddev_roll_arm         : num  NA NA NA NA NA NA NA NA NA NA ...
# var_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
# avg_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
# stddev_pitch_arm        : num  NA NA NA NA NA NA NA NA NA NA ...
# var_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
# avg_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
# stddev_yaw_arm          : num  NA NA NA NA NA NA NA NA NA NA ...
# var_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
# gyros_arm_x             : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
# gyros_arm_y             : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
# gyros_arm_z             : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
# accel_arm_x             : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
# accel_arm_y             : int  109 110 110 111 111 111 111 111 109 110 ...
# accel_arm_z             : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
# magnet_arm_x            : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
# magnet_arm_y            : int  337 337 344 344 337 342 336 338 341 334 ...
# magnet_arm_z            : int  516 513 513 512 506 513 509 510 518 516 ...
# kurtosis_roll_arm       : Factor w/ 329 levels "-0.02438","-0.04190",..: NA NA NA NA NA NA NA NA NA NA ...
# kurtosis_picth_arm      : Factor w/ 327 levels "-0.00484","-0.01311",..: NA NA NA NA NA NA NA NA NA NA ...
# kurtosis_yaw_arm        : Factor w/ 394 levels "-0.01548","-0.01749",..: NA NA NA NA NA NA NA NA NA NA ...
# skewness_roll_arm       : Factor w/ 330 levels "-0.00051","-0.00696",..: NA NA NA NA NA NA NA NA NA NA ...
# skewness_pitch_arm      : Factor w/ 327 levels "-0.00184","-0.01185",..: NA NA NA NA NA NA NA NA NA NA ...
# skewness_yaw_arm        : Factor w/ 394 levels "-0.00311","-0.00562",..: NA NA NA NA NA NA NA NA NA NA ...
# max_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
# max_picth_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
# max_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
# min_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
# min_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
# min_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
# amplitude_roll_arm      : num  NA NA NA NA NA NA NA NA NA NA ...
# amplitude_pitch_arm     : num  NA NA NA NA NA NA NA NA NA NA ...
# amplitude_yaw_arm       : int  NA NA NA NA NA NA NA NA NA NA ...
# roll_dumbbell           : num  13.1 13.1 12.9 13.4 13.4 ...
# pitch_dumbbell          : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
# yaw_dumbbell            : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
# kurtosis_roll_dumbbell  : Factor w/ 397 levels "-0.0035","-0.0073",..: NA NA NA NA NA NA NA NA NA NA ...
# kurtosis_picth_dumbbell : Factor w/ 400 levels "-0.0163","-0.0233",..: NA NA NA NA NA NA NA NA NA NA ...
# kurtosis_yaw_dumbbell   : Factor w/ 1 level "#DIV/0!": NA NA NA NA NA NA NA NA NA NA ...
# skewness_roll_dumbbell  : Factor w/ 400 levels "-0.0082","-0.0096",..: NA NA NA NA NA NA NA NA NA NA ...
# skewness_pitch_dumbbell : Factor w/ 401 levels "-0.0053","-0.0084",..: NA NA NA NA NA NA NA NA NA NA ...
# skewness_yaw_dumbbell   : Factor w/ 1 level "#DIV/0!": NA NA NA NA NA NA NA NA NA NA ...
# max_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
# max_picth_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
# max_yaw_dumbbell        : Factor w/ 72 levels "-0.1","-0.2",..: NA NA NA NA NA NA NA NA NA NA ...
# min_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
# min_pitch_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
# min_yaw_dumbbell        : Factor w/ 72 levels "-0.1","-0.2",..: NA NA NA NA NA NA NA NA NA NA ...
# amplitude_roll_dumbbell : num  NA NA NA NA NA NA NA NA NA NA ...

# Remove all fields that have NAs. These include:
# - amplitude_roll_dumbbel
# - min_yaw_dumbbell
# - min_roll_dumbbell
# - max_picth_dumbbell
# - max_roll_dumbbell
# - max_roll_arm
# - max_picth_arm
# - max_yaw_arm
# - min_roll_arm
# - min_pitch_arm
# - min_yaw_arm
# - amplitude_roll_arm
# - amplitude_pitch_arm
# - amplitude_yaw_arm
# - var_accel_arm
# - avg_roll_arm
# - stddev_roll_arm
# - var_roll_arm
# - avg_pitch_arm
# - stddev_pitch_arm
# - var_pitch_arm
# - avg_yaw_arm
# - stddev_yaw_arm
# - var_yaw_arm
# - var_total_accel_belt
# - avg_roll_belt
# - stddev_roll_belt
# - var_roll_belt
# - avg_pitch_belt
# - stddev_pitch_belt
# - var_pitch_belt
# - avg_yaw_belt
# - stddev_yaw_belt
# - var_yaw_belt
# - min_roll_belt
# - min_pitch_belt
# - kurtosis_roll_belt
# - kurtosis_picth_belt
# - kurtosis_yaw_belt
# - skewness_roll_belt
# - skewness_roll_belt.1
# - skewness_yaw_belt

# Also remove this these fields. They won't really be useful for what we're trying to do:
# - X
# - user_name
# - raw_timestamp_part_1
# - raw_timestamp_part_2
# - cvtd_timestamp

# Trim the above fields from the training set before doing any more processing on the data
training <- training[,!sapply(training, function(x) any(is.na(x)))]
training <- training[,c(-1,-2,-3,-4,-5,-6,-7)]

# View summary statistics on the training set
summary(training)
# new_window    num_window      roll_belt        pitch_belt          yaw_belt       total_accel_belt
# no :19216   Min.   :  1.0   Min.   :-28.90   Min.   :-55.8000   Min.   :-180.00   Min.   : 0.00   
# yes:  406   1st Qu.:222.0   1st Qu.:  1.10   1st Qu.:  1.7600   1st Qu.: -88.30   1st Qu.: 3.00   
#             Median :424.0   Median :113.00   Median :  5.2800   Median : -13.00   Median :17.00   
#             Mean   :430.6   Mean   : 64.41   Mean   :  0.3053   Mean   : -11.21   Mean   :11.31   
#             3rd Qu.:644.0   3rd Qu.:123.00   3rd Qu.: 14.9000   3rd Qu.:  12.90   3rd Qu.:18.00   
#             Max.   :864.0   Max.   :162.00   Max.   : 60.3000   Max.   : 179.00   Max.   :29.00   
# gyros_belt_x        gyros_belt_y       gyros_belt_z      accel_belt_x       accel_belt_y   
# Min.   :-1.040000   Min.   :-0.64000   Min.   :-1.4600   Min.   :-120.000   Min.   :-69.00  
# 1st Qu.:-0.030000   1st Qu.: 0.00000   1st Qu.:-0.2000   1st Qu.: -21.000   1st Qu.:  3.00  
# Median : 0.030000   Median : 0.02000   Median :-0.1000   Median : -15.000   Median : 35.00  
# Mean   :-0.005592   Mean   : 0.03959   Mean   :-0.1305   Mean   :  -5.595   Mean   : 30.15  
# 3rd Qu.: 0.110000   3rd Qu.: 0.11000   3rd Qu.:-0.0200   3rd Qu.:  -5.000   3rd Qu.: 61.00  
# Max.   : 2.220000   Max.   : 0.64000   Max.   : 1.6200   Max.   :  85.000   Max.   :164.00  
# accel_belt_z     magnet_belt_x   magnet_belt_y   magnet_belt_z       roll_arm         pitch_arm      
# Min.   :-275.00   Min.   :-52.0   Min.   :354.0   Min.   :-623.0   Min.   :-180.00   Min.   :-88.800  
# 1st Qu.:-162.00   1st Qu.:  9.0   1st Qu.:581.0   1st Qu.:-375.0   1st Qu.: -31.77   1st Qu.:-25.900  
# Median :-152.00   Median : 35.0   Median :601.0   Median :-320.0   Median :   0.00   Median :  0.000  
# Mean   : -72.59   Mean   : 55.6   Mean   :593.7   Mean   :-345.5   Mean   :  17.83   Mean   : -4.612  
# 3rd Qu.:  27.00   3rd Qu.: 59.0   3rd Qu.:610.0   3rd Qu.:-306.0   3rd Qu.:  77.30   3rd Qu.: 11.200  
# Max.   : 105.00   Max.   :485.0   Max.   :673.0   Max.   : 293.0   Max.   : 180.00   Max.   : 88.500  
# yaw_arm          total_accel_arm  gyros_arm_x        gyros_arm_y       gyros_arm_z     
# Min.   :-180.0000   Min.   : 1.00   Min.   :-6.37000   Min.   :-3.4400   Min.   :-2.3300  
# 1st Qu.: -43.1000   1st Qu.:17.00   1st Qu.:-1.33000   1st Qu.:-0.8000   1st Qu.:-0.0700  
# Median :   0.0000   Median :27.00   Median : 0.08000   Median :-0.2400   Median : 0.2300  
# Mean   :  -0.6188   Mean   :25.51   Mean   : 0.04277   Mean   :-0.2571   Mean   : 0.2695  
# 3rd Qu.:  45.8750   3rd Qu.:33.00   3rd Qu.: 1.57000   3rd Qu.: 0.1400   3rd Qu.: 0.7200  
# Max.   : 180.0000   Max.   :66.00   Max.   : 4.87000   Max.   : 2.8400   Max.   : 3.0200  
# accel_arm_x       accel_arm_y      accel_arm_z       magnet_arm_x     magnet_arm_y     magnet_arm_z   
# Min.   :-404.00   Min.   :-318.0   Min.   :-636.00   Min.   :-584.0   Min.   :-392.0   Min.   :-597.0  
# 1st Qu.:-242.00   1st Qu.: -54.0   1st Qu.:-143.00   1st Qu.:-300.0   1st Qu.:  -9.0   1st Qu.: 131.2  
# Median : -44.00   Median :  14.0   Median : -47.00   Median : 289.0   Median : 202.0   Median : 444.0  
# Mean   : -60.24   Mean   :  32.6   Mean   : -71.25   Mean   : 191.7   Mean   : 156.6   Mean   : 306.5  
# 3rd Qu.:  84.00   3rd Qu.: 139.0   3rd Qu.:  23.00   3rd Qu.: 637.0   3rd Qu.: 323.0   3rd Qu.: 545.0  
# Max.   : 437.00   Max.   : 308.0   Max.   : 292.00   Max.   : 782.0   Max.   : 583.0   Max.   : 694.0  
# roll_dumbbell     pitch_dumbbell     yaw_dumbbell      total_accel_dumbbell gyros_dumbbell_x   
# Min.   :-153.71   Min.   :-149.59   Min.   :-150.871   Min.   : 0.00        Min.   :-204.0000  
# 1st Qu.: -18.49   1st Qu.: -40.89   1st Qu.: -77.644   1st Qu.: 4.00        1st Qu.:  -0.0300  
# Median :  48.17   Median : -20.96   Median :  -3.324   Median :10.00        Median :   0.1300  
# Mean   :  23.84   Mean   : -10.78   Mean   :   1.674   Mean   :13.72        Mean   :   0.1611  
# 3rd Qu.:  67.61   3rd Qu.:  17.50   3rd Qu.:  79.643   3rd Qu.:19.00        3rd Qu.:   0.3500  
# Max.   : 153.55   Max.   : 149.40   Max.   : 154.952   Max.   :58.00        Max.   :   2.2200  
# gyros_dumbbell_y   gyros_dumbbell_z  accel_dumbbell_x  accel_dumbbell_y  accel_dumbbell_z 
# Min.   :-2.10000   Min.   : -2.380   Min.   :-419.00   Min.   :-189.00   Min.   :-334.00  
# 1st Qu.:-0.14000   1st Qu.: -0.310   1st Qu.: -50.00   1st Qu.:  -8.00   1st Qu.:-142.00  
# Median : 0.03000   Median : -0.130   Median :  -8.00   Median :  41.50   Median :  -1.00  
# Mean   : 0.04606   Mean   : -0.129   Mean   : -28.62   Mean   :  52.63   Mean   : -38.32  
# 3rd Qu.: 0.21000   3rd Qu.:  0.030   3rd Qu.:  11.00   3rd Qu.: 111.00   3rd Qu.:  38.00  
# Max.   :52.00000   Max.   :317.000   Max.   : 235.00   Max.   : 315.00   Max.   : 318.00  
# magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z  roll_forearm       pitch_forearm   
# Min.   :-643.0    Min.   :-3600     Min.   :-262.00   Min.   :-180.0000   Min.   :-72.50  
# 1st Qu.:-535.0    1st Qu.:  231     1st Qu.: -45.00   1st Qu.:  -0.7375   1st Qu.:  0.00  
# Median :-479.0    Median :  311     Median :  13.00   Median :  21.7000   Median :  9.24  
# Mean   :-328.5    Mean   :  221     Mean   :  46.05   Mean   :  33.8265   Mean   : 10.71  
# 3rd Qu.:-304.0    3rd Qu.:  390     3rd Qu.:  95.00   3rd Qu.: 140.0000   3rd Qu.: 28.40  
# Max.   : 592.0    Max.   :  633     Max.   : 452.00   Max.   : 180.0000   Max.   : 89.80  
# yaw_forearm      total_accel_forearm gyros_forearm_x   gyros_forearm_y     gyros_forearm_z   
# Min.   :-180.00   Min.   :  0.00      Min.   :-22.000   Min.   : -7.02000   Min.   : -8.0900  
# 1st Qu.: -68.60   1st Qu.: 29.00      1st Qu.: -0.220   1st Qu.: -1.46000   1st Qu.: -0.1800  
# Median :   0.00   Median : 36.00      Median :  0.050   Median :  0.03000   Median :  0.0800  
# Mean   :  19.21   Mean   : 34.72      Mean   :  0.158   Mean   :  0.07517   Mean   :  0.1512  
# 3rd Qu.: 110.00   3rd Qu.: 41.00      3rd Qu.:  0.560   3rd Qu.:  1.62000   3rd Qu.:  0.4900  
# Max.   : 180.00   Max.   :108.00      Max.   :  3.970   Max.   :311.00000   Max.   :231.0000  
# accel_forearm_x   accel_forearm_y  accel_forearm_z   magnet_forearm_x  magnet_forearm_y magnet_forearm_z
# Min.   :-498.00   Min.   :-632.0   Min.   :-446.00   Min.   :-1280.0   Min.   :-896.0   Min.   :-973.0  
# 1st Qu.:-178.00   1st Qu.:  57.0   1st Qu.:-182.00   1st Qu.: -616.0   1st Qu.:   2.0   1st Qu.: 191.0  
# Median : -57.00   Median : 201.0   Median : -39.00   Median : -378.0   Median : 591.0   Median : 511.0  
# Mean   : -61.65   Mean   : 163.7   Mean   : -55.29   Mean   : -312.6   Mean   : 380.1   Mean   : 393.6  
# 3rd Qu.:  76.00   3rd Qu.: 312.0   3rd Qu.:  26.00   3rd Qu.:  -73.0   3rd Qu.: 737.0   3rd Qu.: 653.0  
# Max.   : 477.00   Max.   : 923.0   Max.   : 291.00   Max.   :  672.0   Max.   :1480.0   Max.   :1090.0  
# classe  
# A:5580  
# B:3797  
# C:3422  
# D:3216  
# E:3607

# Compute correlation matrix of the data
M <- abs(cor(training[-53]))
diag(M) <- 0
which(M > 0.7, arr.ind=T)

#                      row col
# yaw_belt               3   1
# total_accel_belt       4   1
# accel_belt_y           9   1
# accel_belt_z          10   1
# accel_arm_y           22   1
# accel_belt_x           8   2
# magnet_belt_x         11   2
# roll_belt              1   3
# total_accel_belt       4   3
# accel_belt_x           8   3
# accel_belt_z          10   3
# magnet_belt_x         11   3
# roll_belt              1   4
# yaw_belt               3   4
# accel_belt_y           9   4
# accel_belt_z          10   4
# accel_arm_y           22   4
# magnet_dumbbell_x     37   5
# magnet_dumbbell_y     38   5
# pitch_belt             2   8
# yaw_belt               3   8
# magnet_belt_x         11   8
# roll_belt              1   9
# total_accel_belt       4   9
# accel_belt_z          10   9
# accel_arm_y           22   9
# roll_belt              1  10
# yaw_belt               3  10
# total_accel_belt       4  10
# accel_belt_y           9  10
# accel_arm_y           22  10
# pitch_belt             2  11
# yaw_belt               3  11
# accel_belt_x           8  11
# magnet_belt_z         13  12
# magnet_belt_y         12  13
# gyros_arm_y           19  18
# gyros_arm_x           18  19
# magnet_arm_x          24  21
# roll_belt              1  22
# total_accel_belt       4  22
# accel_belt_y           9  22
# accel_belt_z          10  22
# magnet_arm_z          26  23
# accel_arm_x           21  24
# magnet_arm_y          25  24
# magnet_arm_x          24  25
# magnet_arm_z          26  25
# accel_arm_z           23  26
# magnet_arm_y          25  26
# accel_dumbbell_y      35  27
# accel_dumbbell_x      34  28
# accel_dumbbell_z      36  29
# accel_dumbbell_y      35  30
# gyros_dumbbell_z      33  31
# gyros_forearm_y       45  31
# gyros_forearm_z       46  31
# gyros_dumbbell_x      31  33
# gyros_forearm_y       45  33
# gyros_forearm_z       46  33
# pitch_dumbbell        28  34
# roll_dumbbell         27  35
# total_accel_dumbbell  30  35
# yaw_dumbbell          29  36
# gyros_belt_x           5  37
# magnet_dumbbell_y     38  37
# gyros_belt_x           5  38
# magnet_dumbbell_x     37  38
# gyros_dumbbell_x      31  45
# gyros_dumbbell_z      33  45
# gyros_forearm_z       46  45
# gyros_dumbbell_x      31  46
# gyros_dumbbell_z      33  46
# gyros_forearm_y       45  46
# magnet_forearm_y      51  48
# accel_forearm_y       48  51

# The correlation matrix shows that there are multiple variables that are highly correllated.
# Let's plot each of these variables to see if they have a linear relationship.

# yaw_belt and roll_belt
ggplot(data=training, aes(x=yaw_belt, y=roll_belt)) +
   geom_point()

# No visible linear relationship between yaw_belt and roll_belt. But I am seeing 4 clusters in the
# scatter plot. I wonder what's going on there?

ggplot(data=training, aes(x=yaw_belt, y=roll_belt, color = classe)) +
   geom_point()

# total_accel_belt and roll_belt
ggplot(data=training, aes(x=total_accel_belt, y=roll_belt)) +
   geom_point()

# I'm seeing 2 clusters in the scatter plot. One that appears to slope downwards and another that
# appears to slope upwards. I wonder what's going on there?

ggplot(data=training, aes(x=total_accel_belt, y=roll_belt, color = classe)) +
   geom_point()

# accel_belt_y and roll_belt
ggplot(data=training, aes(x=accel_belt_y, y=roll_belt)) +
   geom_point()

# No linear relationship. But two noticeable clusters are present in the data. 

ggplot(data=training, aes(x=accel_belt_y, y=roll_belt, color=classe)) +
   geom_point()

# accel_belt_z and roll_belt
ggplot(data=training, aes(x=accel_belt_z, y=roll_belt)) +
   geom_point()

# Linear relationship. But two noticeable clusters in the data. What's going on here???

ggplot(data=training, aes(x=accel_belt_z, y=roll_belt, color=classe)) +
   geom_point()

# accel_belt_x and pitch_belt
ggplot(data=training, aes(x=accel_belt_x, y=pitch_belt)) +
   geom_point()

# No linear relationship. But clusters in data.

ggplot(data=training, aes(x=accel_belt_x, y=pitch_belt, color=classe)) +
   geom_point()

# magnet_belt_x and pitch_belt
ggplot(data=training, aes(x=magnet_belt_x, y=pitch_belt)) +
   geom_point()

# No linear relationship. But there's two big clusters in the data. Also see some outliers
# in the data

ggplot(data=training, aes(x=magnet_belt_x, y=pitch_belt, color=classe)) +
   geom_point()

# accel_belt_z and total_accel_belt
ggplot(data=training, aes(x=accel_belt_z, y=total_accel_belt)) +
   geom_point()

# No linear relationship. There's some outliers in the plot.

ggplot(data=training, aes(x=accel_belt_z, y=total_accel_belt, color=classe)) +
   geom_point()

# magnet_belt_x and accel_belt_x
ggplot(data=training, aes(x=magnet_belt_x, y=accel_belt_x)) +
   geom_point()

# No linear relationship. But again, there are noticable clusters in the data.

ggplot(data=training, aes(x=magnet_belt_x, y=accel_belt_x, color=classe)) +
   geom_point()

# accel_belt_y and total_accel_belt

ggplot(data=training, aes(x=accel_belt_y, y=total_accel_belt)) +
   geom_point()

ggplot(data=training, aes(x=accel_belt_y, y=total_accel_belt, color=classe)) +
   geom_point()

# accel_belt_z and accel_belt_y
ggplot(data=training, aes(x=accel_belt_z, y=accel_belt_y)) +
   geom_point()

# No linear relationship. But again, 2 major clusters in the data. Also see some outliers.

ggplot(data=training, aes(x=accel_belt_z, y=accel_belt_y, color=classe)) +
   geom_point()

# gyros_arm_y and gyros_arm_x
ggplot(data=training, aes(x=gyros_arm_y, y=gyros_arm_x)) +
   geom_point()

# Definite linear relationship between gyros_arm_y and gyros_arm_x.

ggplot(data=training, aes(x=gyros_arm_y, y=gyros_arm_x, color=classe)) +
   geom_point()

# magnet_arm_x and accel_arm_x
ggplot(data=training, aes(x=magnet_arm_x, y=accel_arm_x)) +
   geom_point()

# Linear relationship exists. Noticing 2 linear lines in the data going in the same direction.
# What's going on there?

ggplot(data=training, aes(x=magnet_arm_x, y=accel_arm_x, color=classe)) +
   geom_point()

# magnet_arm_z and magnet_arm_y
ggplot(data=training, aes(x=magnet_arm_z, y=magnet_arm_y)) +
   geom_point()

# Linear relationship. Noticing a large cluster of points on the upper right of the plot.

ggplot(data=training, aes(x=magnet_arm_z, y=magnet_arm_y, color=classe)) +
   geom_point()

# accel_dumbbell_x and pitch_dumbbell
ggplot(data=training, aes(x=accel_dumbbell_x, y=pitch_dumbbell)) +
   geom_point()

# Linear relationship?
ggplot(data=training, aes(x=accel_dumbbell_x, y=pitch_dumbbell, color=classe)) +
   geom_point()


# accel_dumbbell_z and yaw_dumbbell
ggplot(data=training, aes(x=accel_dumbbell_z, y=yaw_dumbbell)) +
   geom_point()

# No linear relationship. Noticing 4 clusters in the data.
ggplot(data=training, aes(x=accel_dumbbell_z, y=yaw_dumbbell, color=classe)) +
   geom_point()


# gyros_dumbbell_z and gyros_dumbbell_x
ggplot(data=training, aes(x=gyros_dumbbell_z, y=gyros_dumbbell_x)) +
   geom_point()

# No linear relationship. Interesting how all the points are clustered tightly in two regions
# in the plot
ggplot(data=training, aes(x=gyros_dumbbell_z, y=gyros_dumbbell_x, color=classe)) +
   geom_point()


# gyros_forearm_z and gyros_dumbbell_x
ggplot(data=training, aes(x=gyros_forearm_z, y=gyros_dumbbell_x)) +
   geom_point()

# No linear relationship. Interesting how all the points are clustered tightly in two regions
# in the plot

ggplot(data=training, aes(x=gyros_forearm_z, y=gyros_dumbbell_x, color=classe)) +
   geom_point()


# gyros_forearm_z and gyros_dumbbell_z
ggplot(data=training, aes(x=gyros_forearm_z, y=gyros_dumbbell_z)) +
   geom_point()

# No linear relationship. Interesting how all the points are clustered tightly in two regions
# in the plot
ggplot(data=training, aes(x=gyros_forearm_z, y=gyros_dumbbell_z, color=classe)) +
   geom_point()

# gyros_forearm_z and gyros_forearm_y
ggplot(data=training, aes(x=gyros_forearm_z, y=gyros_forearm_y)) +
   geom_point()

# No linear relationship. Interesting how all the points are clustered tightly in two regions
# in the plot
ggplot(data=training, aes(x=gyros_forearm_z, y=gyros_forearm_y, color=classe)) +
   geom_point()

# accel_arm_y and roll_belt
ggplot(data=training, aes(x=accel_arm_y, y=roll_belt)) +
   geom_point()

# total_accel_belt and yaw_belt
ggplot(data=training, aes(x=total_accel_belt, y=yaw_belt)) +
   geom_point()

# accel_belt_x and yaw_belt
ggplot(data=training, aes(x=accel_belt_x, y=yaw_belt)) +
   geom_point()

# accel_belt_z and yaw_belt
ggplot(data=training, aes(x=accel_belt_z, y=yaw_belt)) +
   geom_point()

# magnet_belt_x and yaw_belt
ggplot(data=training, aes(x=magnet_belt_x, y=yaw_belt)) +
   geom_point()

# accel_arm_y and total_accel_belt
ggplot(data=training, aes(x=accel_arm_y, y=total_accel_belt)) +
   geom_point()

# magnet_dumbbel_x and gyros_belt_x
ggplot(data=training, aes(x=magnet_dumbbell_x, y=gyros_belt_x)) +
   geom_point()

# magnet_dumbbel_x and gyros_belt_x
ggplot(data=training, aes(x=magnet_dumbbell_y, y=gyros_belt_x)) +
   geom_point()

# accel_arm_y and accel_belt_y
ggplot(data=training, aes(x=accel_arm_y, y=accel_belt_y)) +
   geom_point()

# accel_arm_y and accel_belt_z
ggplot(data=training, aes(x=accel_arm_y, y=accel_belt_z)) +
   geom_point()

# magnet_belt_z and magnet_belt_y
ggplot(data=training, aes(x=magnet_belt_z, y=magnet_belt_y)) +
   geom_point()

ggplot(data=training, aes(x=magnet_belt_z, y=magnet_belt_y, color=classe)) +
   geom_point()

# Linear relationship exists.

# magnet_arm_z and accel_arm_z
ggplot(data=training, aes(x=magnet_arm_z, y=accel_arm_z)) +
   geom_point()

# Linear relationship exists

# magnet_arm_y and magnet_arm_x
ggplot(data=training, aes(x=magnet_arm_y, y=magnet_arm_x)) +
   geom_point()

# accel_dumbbell_y and roll_dumbbell
ggplot(data=training, aes(x=accel_dumbbell_y, y=roll_dumbbell)) +
   geom_point()

# accel_dumbbel_y and total_accel_dumbbell
ggplot(data=training, aes(x=accel_dumbbell_y, y=total_accel_dumbbell)) +
   geom_point()

# gyros_forearm_y and gyros_dumbbell_x
ggplot(data=training, aes(x=gyros_forearm_y, y=gyros_dumbbell_x)) +
   geom_point()

# gyros_forearm_y and gyros_dumbbell_z
ggplot(data=training, aes(x=gyros_forearm_y, y=gyros_dumbbell_z)) +
   geom_point()

# magnet_dumbbell_y and magnet_dumbbell_x
ggplot(data=training, aes(x=magnet_dumbbell_x, y=magnet_dumbbell_y)) +
   geom_point()

ggplot(data=training, aes(x=magnet_dumbbell_x, y=magnet_dumbbell_y, color=classe)) +
   geom_point()

# magnet_forearm_y and accel_forearm_y

ggplot(data=training, aes(x=magnet_forearm_y, y=accel_forearm_y)) +
   geom_point()

# We've confirmed multicolinearity between several pairs of variables:
# - gyros_arm_x and gyros_arm_y
# - magnet_arm_x and accel_arm_x
# - magnet_arm_z and magnet_arm_y
# - magnet_belt_z and magnet_belt_y
# - magnet_arm_z and accel_arm_z
# - magnet_dummbell_y and magnet_dumbbell_x
# - magnet_forearm_y and accel_forearm_y

# We'll chose one variable for each pair to use in our prediction. These will be,
# gyros_arm_x, magnet_arm_x, magnet_arm_z, magnet_belt_z, magnet_dumbbell_y, and magnet_forearm_y

# Let's assemble our final dataset:

training <- training[c(-12,-23,-37,-48,-19,-21,-25)]

# Train a random forest model on the data. We'll use 10 fold cross validation to get a better estimate
# of the model's out of sample accuracy

computeAccuracy <- function(prediction, truth)
{
   sum(prediction == truth)/length(truth)
}

doCrossValidation <- function(data, k)
{
   data$id <- sample(1:k, nrow(data), replace=TRUE)
   list <- 1:k
   accuracy <- vector(mode = "numeric", length=k)
   
   #Creating a progress bar to know the status of CV
   progress.bar <- create_progress_bar("text")
   progress.bar$init(k)
   
   for (i in 1:k)
   {
      trainFold <- subset(data, id %in% list[-i])
      testFold <- subset(data, id %in% c(i))
      trainFold <- trainFold[,-ncol(data)]
      testFold <- trainFold[,-ncol(data)]
      
      modelRF <- train(classe ~.,data=trainFold, method="rf", ntree=10)
      
      predFold <- predict(modelRF, testFold)
      accuracy[i] <- computeAccuracy(predFold, testFold$classe)
      
      progress.bar$step()
   }
   avgAccuracy <- mean(accuracy)
   # output the average 
   avgAccuracy
}

# Perform 10 fold cross validation on the training set
doCrossValidation(training,10)

# We can expect the model to have a 99% accurate on new data.


modelRF <- train(classe ~., data=training, method="rf", ntree=10)

# Read in the test set and perform some predictions.

test <- read.csv("pml-testing.csv", na.strings=c("NA",""))
test <- test[,which(names(test) %in% names(training))]


pml_write_files = function(x){
   n = length(x)
   for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
   }
}
classePred <- predict(modelRF, test)
pml_write_files(classePred)
