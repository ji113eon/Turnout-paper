###################################################################################################################
# 2012 Registered Voters: Cross tabulation of voting in 2012 General Election and voting in 2016 General Election #
###################################################################################################################


# Calculate age at a given reference date
# Create an interval between the date of birth and the enrollment date; 
# intervals are specific to the two dates. Periods give the actual length
# of time between those dates, so convert to period and extract the year.
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(new_interval(birthDate, refDate), unit = "year")
  period$year
}


# People who are registered before 10/10/2012 (Deadline to participate in the 2010 General Election is 29 days before the election)
b12a <- public_records[public_records$"Registration_Date" < "2012-10-10", ]


# Voters who voted in 2012 General Election
v12 <- b12a[b12a$`2012 General Election(Nov/06/2012)` == "Y" | b12a$`2012 General Election(Nov/06/2012)` == "Z" | 
              b12a$`2012 General Election(Nov/06/2012)` == "A" | b12a$`2012 General Election(Nov/06/2012)` == "B" |
              b12a$`2012 General Election(Nov/06/2012)` == "E" | b12a$`2012 General Election(Nov/06/2012)` == "F" |
              b12a$`2012 General Election(Nov/06/2012)` == "P" ]   

# Out of v12 who did not vote in 2014 General Election
nv14 <- v12[v12$`2014 General Election(Nov/04/2014)` == "N" | v12$`2014 General Election(Nov/04/2014)` == "X" |
              v12$`2014 General Election(Nov/04/2014)` == "NA"]

# Out of v12 who voted in 2014 General Election
v14 <- v12[v12$`2014 General Election(Nov/04/2014)` == "Y" | v12$`2014 General Election(Nov/04/2014)` == "Z" |
             v12$`2014 General Election(Nov/04/2014)` == "A" | v12$`2014 General Election(Nov/04/2014)` == "B" |
             v12$`2014 General Election(Nov/04/2014)` == "E" | v12$`2014 General Election(Nov/04/2014)` == "F" |
             v12$`2014 General Election(Nov/04/2014)` == "P" ]


# Voters who did not vote in 2012 General Election
nv12 <- b12a[b12a$`2012 General Election(Nov/06/2012)` == "N" | b12a$`2012 General Election(Nov/06/2012)` == "X" | 
             b12a$`2012 General Election(Nov/06/2012)` == "NA"]

# Out of nv12 who did not vote in 2014 General Election
n_nv14 <- nv12[nv12$`2014 General Election(Nov/04/2014)` == "N" | nv12$`2014 General Election(Nov/04/2014)` == "X" |
               nv12$`2014 General Election(Nov/04/2014)` == "NA"]

# Out of nv12 who voted in 2014 General Election
n_v14 <- nv12[nv12$`2014 General Election(Nov/04/2014)` == "Y" | nv12$`2014 General Election(Nov/04/2014)` == "Z" |
                nv12$`2014 General Election(Nov/04/2014)` == "A" | nv12$`2014 General Election(Nov/04/2014)` == "B" |
                nv12$`2014 General Election(Nov/04/2014)` == "E" | nv12$`2014 General Election(Nov/04/2014)` == "F" |
                nv12$`2014 General Election(Nov/04/2014)` == "P" ]



# Age of those voters
AgeGen12v <- calc_age(v12$Birth_Date, "2014-11-04") 
v12 <- data.table(v12, AgeGen12v)
setnames(v12,"AgeGen12v","Age")

AgeGen12nv <- calc_age(nv12$Birth_Date, "2014-11-04") 
nv12 <- data.table(nv12, AgeGen12nv)
setnames(nv12,"AgeGen12nv","Age")



# Age breakdowns
AgeCat12v <- findInterval(AgeGen12v, c(18, 21, 25, 30, 40, 50, 60, 65, 70, 75, 80))
v12 <- data.table(v12, AgeCat12v)
setnames(v12, "AgeCat12v", "AgeCat")

AgeCat12nv <- findInterval(AgeGen12nv, c(18, 21, 25, 30, 40, 50, 60, 65, 70, 75, 80))
nv12 <- data.table(nv12, AgeCat12nv)
setnames(nv12, "AgeCat12nv", "AgeCat")




v12[, .N, by = "AgeCat"]
v14[, .N, by = "AgeCat"]
nv14[, .N, by = "AgeCat"]

nv12[, .N, by = "AgeCat"]
n_v14[, .N, by = "AgeCat"]
n_nv14[, .N, by = "AgeCat"]


# Create frequency tables
v12Group <- c(rep("0", 1),
              rep("1", 1),
              rep("2", 2541),
              rep("3", 9213),
              rep("4", 15689),
              rep("5", 14174),
              rep("6", 17800),
              rep("7", 9964),
              rep("8", 9518),
              rep("9", 6538),
              rep("10", 4275),
              rep("11", 5968))
groups.t1 <- table(v12Group)
groups.t1

# Proportions and percentages
prop.table(groups.t1) # Give proportions of total
round(prop.table(groups.t1), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t1), 3)*100 # Give percentages without decimal places

# Create frequency tables
v14Group <- c(rep("1", 1056),
              rep("2", 3251),
              rep("3", 4007),
              rep("4", 8664),
              rep("5", 9915),
              rep("6", 14526),
              rep("7", 8441),
              rep("8", 7324),
              rep("9", 4979),
              rep("10", 3140),
              rep("11", 3860))
groups.t2 <- table(v14Group)
groups.t2

# Proportions and percentages
prop.table(groups.t2) # Give proportions of total
round(prop.table(groups.t2), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t2), 3)*100 # Give percentages without decimal places

# Create frequency tables
nv14Group <- c(rep("1", 1670),
               rep("2", 6123),
               rep("3", 5707),
               rep("4", 7489),
               rep("5", 5307),
               rep("6", 4885),
               rep("7", 1734),
               rep("8", 1261),
               rep("9", 787),
               rep("10", 513),
               rep("11", 889))
groups.t3 <- table(nv14Group)
groups.t3

# Proportions and percentages
prop.table(groups.t3) # Give proportions of total
round(prop.table(groups.t3), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t3), 3)*100 # Give percentages without decimal places

# Create frequency tables
nv12Group <- c(rep("1", 1),
               rep("2", 616),
               rep("3", 3358),
               rep("4", 4899),
               rep("5", 2787),
               rep("6", 2465),
               rep("7", 1048),
               rep("8", 792),
               rep("9", 465),
               rep("10", 309),
               rep("11", 603))
groups.t4 <- table(nv12Group)
groups.t4

# Proportions and percentages
prop.table(groups.t4) # Give proportions of total
round(prop.table(groups.t4), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t4), 3)*100 # Give percentages without decimal places

# Create frequency tables
n_v14Group <- c(rep("1", 45),
                rep("2", 260),
                rep("3", 299),
                rep("4", 456),
                rep("5", 282),
                rep("6", 348),
                rep("7", 176),
                rep("8", 104),
                rep("9", 71),
                rep("10", 34),
                rep("11", 68))
groups.t5 <- table(n_v14Group)
groups.t5

# Proportions and percentages
prop.table(groups.t5) # Give proportions of total
round(prop.table(groups.t5), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t5), 3)*100 # Give percentages without decimal places

# Create frequency tables
n_nv14Group <- c(rep("1", 539),
                 rep("2", 2634),
                 rep("3", 3502),
                 rep("4", 4142),
                 rep("5", 2596),
                 rep("6", 2316),
                 rep("7", 814),
                 rep("8", 608),
                 rep("9", 315),
                 rep("10", 237),
                 rep("11", 480))
groups.t6 <- table(n_nv14Group)
groups.t6

# Proportions and percentages
prop.table(groups.t6) # Give proportions of total
round(prop.table(groups.t6), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t6), 3)*100 # Give percentages without decimal places




###################################################################################################################
# 2012 Registered Voters: Cross tabulation of voting in 2014 General Election and voting in 2016 General Election #
###################################################################################################################

# Voters who voted in 2014 General Election
v14 <- b12a[b12a$`2014 General Election(Nov/04/2014)` == "Y" | b12a$`2014 General Election(Nov/04/2014)` == "Z" | 
            b12a$`2014 General Election(Nov/04/2014)` == "A" | b12a$`2014 General Election(Nov/04/2014)` == "B" |
            b12a$`2014 General Election(Nov/04/2014)` == "E" | b12a$`2014 General Election(Nov/04/2014)` == "F" |
            b12a$`2014 General Election(Nov/04/2014)` == "P" ]   

# Out of v14 who did not vote in 2016 General Election
nv16 <- v14[v14$`2016 General Election(Nov/08/2016)` == "N" | v14$`2016 General Election(Nov/08/2016)` == "X" |
            v14$`2016 General Election(Nov/08/2016)` == "NA"]

# Out of v14 who voted in 2016 General Election
v16 <- v14[v14$`2016 General Election(Nov/08/2016)` == "Y" | v14$`2016 General Election(Nov/08/2016)` == "Z" |
             v14$`2016 General Election(Nov/08/2016)` == "A" | v14$`2016 General Election(Nov/08/2016)` == "B" |
             v14$`2016 General Election(Nov/08/2016)` == "E" | v14$`2016 General Election(Nov/08/2016)` == "F" |
             v14$`2016 General Election(Nov/08/2016)` == "P" ]


# Voters who did not vote in 2014 General Election
nv14 <- b12a[b12a$`2014 General Election(Nov/04/2014)` == "N" | b12a$`2014 General Election(Nov/04/2014)` == "X" | 
             b12a$`2014 General Election(Nov/04/2014)` == "NA"]

# Out of nv14 who did not vote in 2016 General Election
n_nv16 <- nv14[nv14$`2016 General Election(Nov/08/2016)` == "N" | nv14$`2016 General Election(Nov/08/2016)` == "X" |
               nv14$`2016 General Election(Nov/08/2016)` == "NA"]

# Out of nv14 who voted in 2016 General Election
n_v16 <- nv14[nv14$`2016 General Election(Nov/08/2016)` == "Y" | nv14$`2016 General Election(Nov/08/2016)` == "Z" |
              nv14$`2016 General Election(Nov/08/2016)` == "A" | nv14$`2016 General Election(Nov/08/2016)` == "B" |
              nv14$`2016 General Election(Nov/08/2016)` == "E" | nv14$`2016 General Election(Nov/08/2016)` == "F" |
              nv14$`2016 General Election(Nov/08/2016)` == "P" ]



# Age of those voters
AgeGen14v <- calc_age(v14$Birth_Date, "2014-11-04") 
v14 <- data.table(v14, AgeGen14v)
setnames(v14,"AgeGen14v","Age")

AgeGen14nv <- calc_age(nv14$Birth_Date, "2014-11-04") 
nv14 <- data.table(nv14, AgeGen14nv)
setnames(nv14,"AgeGen14nv","Age")



# Age breakdowns
AgeCat14v <- findInterval(AgeGen14v, c(18, 21, 25, 30, 40, 50, 60, 65, 70, 75, 80))
v14 <- data.table(v14, AgeCat14v)
setnames(v14, "AgeCat14v", "AgeCat")

AgeCat14nv <- findInterval(AgeGen14nv, c(18, 21, 25, 30, 40, 50, 60, 65, 70, 75, 80))
nv14 <- data.table(nv14, AgeCat14nv)
setnames(nv14, "AgeCat14nv", "AgeCat")


cut(AgeGen16nv, breaks=c(18, 21, 25, 30, 40, 50, 60, 65, 70, 75, 80), right = FALSE) # In intervals 

v14[, .N, by = "AgeCat"]
v16[, .N, by = "AgeCat"]
nv16[, .N, by = "AgeCat"]

nv14[, .N, by = "AgeCat"]
n_v16[, .N, by = "AgeCat"]
n_nv16[, .N, by = "AgeCat"]


# Create frequency tables
v14Group <- c(rep("0", 1),
              rep("1", 1),
              rep("2", 2541),
              rep("3", 9213),
              rep("4", 15689),
              rep("5", 14174),
              rep("6", 17800),
              rep("7", 9964),
              rep("8", 9518),
              rep("9", 6538),
              rep("10", 4275),
              rep("11", 5968))
groups.t1 <- table(v12Group)
groups.t1

# Proportions and percentages
prop.table(groups.t1) # Give proportions of total
round(prop.table(groups.t1), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t1), 3)*100 # Give percentages without decimal places

# Create frequency tables
v16Group <- c(rep("1", 1486),
              rep("2", 2882),
              rep("3", 3771),
              rep("4", 8441),
              rep("5", 9769),
              rep("6", 14360),
              rep("7", 8399),
              rep("8", 7223),
              rep("9", 4897),
              rep("10", 3052),
              rep("11", 3620))
groups.t2 <- table(v16Group)
groups.t2

# Proportions and percentages
prop.table(groups.t2) # Give proportions of total
round(prop.table(groups.t2), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t2), 3)*100 # Give percentages without decimal places

# Create frequency tables
nv16Group <- c(rep("1", 1543),
               rep("2", 3516),
               rep("3", 3837),
               rep("4", 5832),
               rep("5", 4759),
               rep("6", 4391),
               rep("7", 1533),
               rep("8", 1045),
               rep("9", 580),
               rep("10", 348),
               rep("11", 485))
groups.t3 <- table(nv16Group)
groups.t3

# Proportions and percentages
prop.table(groups.t3) # Give proportions of total
round(prop.table(groups.t3), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t3), 3)*100 # Give percentages without decimal places

# Create frequency tables
nv14Group <- c(rep("1", 1),
               rep("2", 616),
               rep("3", 3358),
               rep("4", 4899),
               rep("5", 2787),
               rep("6", 2465),
               rep("7", 1048),
               rep("8", 792),
               rep("9", 465),
               rep("10", 309),
               rep("11", 603))
groups.t4 <- table(nv12Group)
groups.t4

# Proportions and percentages
prop.table(groups.t4) # Give proportions of total
round(prop.table(groups.t4), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t4), 3)*100 # Give percentages without decimal places

# Create frequency tables
n_v16Group <- c(rep("1", 1),
                rep("2", 212),
                rep("3", 1050),
                rep("4", 1708),
                rep("5", 1059),
                rep("6", 988),
                rep("7", 429),
                rep("8", 309),
                rep("9", 168),
                rep("10", 91),
                rep("11", 119))
groups.t5 <- table(n_v16Group)
groups.t5

# Proportions and percentages
prop.table(groups.t5) # Give proportions of total
round(prop.table(groups.t5), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t5), 3)*100 # Give percentages without decimal places

# Create frequency tables
n_nv16Group <- c(rep("1", 1436),
                 rep("2", 5578),
                 rep("3", 5669),
                 rep("4", 6085),
                 rep("5", 3309),
                 rep("6", 2924),
                 rep("7", 1056),
                 rep("8", 847),
                 rep("9", 538),
                 rep("10", 411),
                 rep("11", 907))
groups.t6 <- table(n_nv16Group)
groups.t6

# Proportions and percentages
prop.table(groups.t6) # Give proportions of total
round(prop.table(groups.t6), 3) # Give proportions with 3 decimal places
round(prop.table(groups.t6), 3)*100 # Give percentages without decimal places

