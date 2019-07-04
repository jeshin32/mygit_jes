
mydata = fread("https://github.com/arunsrinivasan/satrdays-workshop/raw/master/flights_2014.csv")
dim(mydata)

# fwrite(mydata, "data/data_table_50example.csv")

# Selecting or Keeping Columns ----------------------------

mydata[ , origin] # returns a vector
mydata[ , .(origin)] # returns a data.table

# It can also be written like data.frame way  ------------------------
#mydata[, c("origin"), with=FALSE]
mydata[, c("origin")]


# Keeping a column based on column position  ------------------------
mydata[, 2, with=FALSE]

# Keeping Multiple Columns   ------------------------
mydata[, .(origin, year, month, hour)]
mydata[, c(2:4), with=FALSE]
mydata[, c(2:4)]

# Dropping a Column  -----------------------------
#mydata[, !c("origin"), with=FALSE]
mydata[, !c("origin")]
mydata[, -c("origin")]

# Dropping Multiple Columns  ------------------------
#mydata[, !c("origin", "year", "month"), with=FALSE]
mydata[, !c("origin", "year", "month")]
mydata[, -c("origin", "year", "month")]

# Keeping variables that contain 'dep'  ------------------------
mydata[, names(mydata) %like% "dep", with=FALSE]

# Rename Variables ------------------------
#setnames(mydata, c("dest"), c("Destination"))
#setnames(mydata, c("dest","origin"), c("Destination", "origin.of.flight"))


# Filter based on one variable  ---------------------
mydata[origin == "JFK"]
mydata[origin %in% c("JFK", "LGA")]

# Apply Logical Operator : NOT  -------------------

# Exclude Values
mydata[!origin %in% c("JFK", "LGA")]

mydata[origin == "JFK" & carrier == "AA"]


# Indexing (Set Keys)
setkey(mydata, origin)

data12 = mydata[c("JFK", "LGA")]

system.time(mydata[origin %in% c("JFK", "LGA")])
system.time(mydata[c("JFK", "LGA")])

# Indexing Multiple Columns ------------------------------

setkey(mydata, origin, dest)
mydata[.("JFK", "MIA")] #list
mydata[J("JFK", "MIA")]

mydata[origin == "JFK" & dest == "MIA"]
key(mydata)

# Sorting Data -------------------------------
mydata01 = setorder(mydata, origin)
mydata02 = setorder(mydata, -origin)

# Sorting Data based on multiple variables
mydata03 = setorder(mydata, origin, -carrier)

# Adding Columns (Calculation on rows) --------------------------
mydata[, dep_sch:=dep_time - dep_delay]

# Adding Multiple Columns
mydata[,":="(dep_sch :=dep_time - dep_delay,
             arr-sch :=arr_time - arr_delay)]

mydata[, c("dep_sch","arr_sch"):=list(dep_time - dep_delay, arr_time - arr_delay)]

mydata1 <- mydata #data table에서는 이름만 다르게 복사 하는 것, 동일하게 변동이 가해진다
mydata_C <- copy(mydata) #이렇게 해야 메모리에 별도로 테이블이 올라감

mydata_C[, c("dep_sch","arr_sch"):=list(dep_time - dep_delay, arr_time - arr_delay)]

# IF THEN ELSE
mydata[, flag:= 1*(min < 50)] #true/false*1 = 논리연산자를 숫자로 바꿔줌
mydata[, flag:= ifelse(min < 50, 1,0)]




# How to write Sub Queries (like SQL) --------------------------

# We can use this format - DT[ ] [ ] [ ] to build a chain in data.table. It is like sub-queries like SQL.
mydata[, dep_sch:=dep_time - dep_delay][,.(dep_time,dep_delay,dep_sch)]
mydata[, dep_sch:=dep_time - dep_delay] %>% 
        .[,.(dep_time,dep_delay,dep_sch)]



# First, we are computing scheduled departure time and then selecting only relevant columns.

# Summarize or Aggregate Columns
mydata[, .(mean = mean(arr_delay, na.rm = TRUE),
           median = median(arr_delay, na.rm = TRUE),
           min = min(arr_delay, na.rm = TRUE),
           max = max(arr_delay, na.rm = TRUE))]

# Summarize Multiple Columns
mydata[, .(mean(arr_delay), mean(dep_delay))]
mydata[, lapply(.SD, mean), .SDcols = c("arr_delay", "dep_delay")]

# Summarize all numeric Columns
mydata[, lapply(.SD, mean)]
mydata[, lapply(.SD, mean, na.rm=T)]

mydata[, sapply(.SD, function(x) c(mean=mean(x), median=median(x)))] #inline function
#--myfun <- function(x) c(mean=mean(x), median=median(x))
#--myfun(c(1,2,1.5,3))
#--mydata[, sapply(.SD, myfun)]




# GROUP BY (Within Group Calculation)
mydata[, .(mean_arr_delay = mean(arr_delay, na.rm = TRUE)), by = origin]
mydata[, mean_arr_delay = mean(arr_delay, na.rm = TRUE), by = origin] #error
mydata[, mean(arr_delay,na.rm=T), by=origin]

# Summary by group
# Use key column in a by operation

# Instead of 'by', you can use keyby= operator.
mydata[, .(mean_arr_delay = mean(arr_delay, na.rm = TRUE)), keyby = origin]

# Summarize multiple variables by group 'origin'
mydata[, .(mean(arr_delay, na.rm = TRUE), mean(dep_delay, na.rm = TRUE)), by = origin]
mydata[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("arr_delay", "dep_delay"), by = origin]




# Remove Duplicates
setkey(mydata, "carrier")
unique(mydata)

setkey(mydata, NULL)
unique(mydata)

# Extract values within a group

# The following command selects first and second values from a categorical variable carrier.
mydata[, .SD[1:2], by=carrier] # first, second rows(data)

# Select LAST value from a group
mydata[, .SD[.N], by=carrier] # last row(data)




# SQL's RANK OVER PARTITION

# In SQL, Window functions are very useful for solving complex data problems. 
# RANK OVER PARTITION is the most popular window function. 
# It can be easily translated in data.table with the help of frank() function. 
# frank() is similar to base R's rank() function but much faster. See the code below.
dt = mydata[, rank:=frank(-distance,ties.method = "min"), by=carrier]
print(dt)
View(dt)

# Cumulative SUM by GROUP
dat = mydata[, cum:=cumsum(distance), by=carrier]
?cumsum
cumsum(c(1,2,3)) # 순차적으로 누적값을 반환해줌, 순서대로 하려면 ordering을 먼저 해줘야


# Lag and Lead (time series **************) ---------------------------- 
DT <- data.table(A=1:5)
DT
DT[ , X := shift(A, 1, type="lag")]
DT
DT[ , Y := shift(A, 1, type="lead")]
DT

# Lag and Lead Function


# We can use %between% operator to define a range. It is inclusive of the values of both the ends.
DT = data.table(x=6:10)
DT

DT[x %between% c(7,9)]

# The %like% is mainly used to find all the values that matches a pattern.
DT = data.table(Name=c("dep_time","dep_delay","arrival"), ID=c(2,3,4))
DT
DT[Name %like% "dep"] 




# Merging / Joins --------------------------

# The merging in data.table is very similar to base R merge() function. The only difference is data.table by default takes common key variable as a primary key to merge two datasets. Whereas, data.frame takes common variable name as a primary key to merge the datasets.

# Sample Data
(dt1 <- data.table(A = letters[rep(1:3, 2)], X = 1:6, key = "A"))
(dt2 <- data.table(A = letters[rep(2:4, 2)], Y = 6:1, key = "A"))
# Inner Join

# It returns all the matching observations in both the datasets.
merge(dt1, dt2, by="A")

# Left Join

# It returns all observations from the left dataset and the matched observations from the right dataset.
merge(dt1, dt2, by="A", all.x = TRUE)

# Right Join

# It returns all observations from the right dataset and the matched observations from the left dataset.
merge(dt1, dt2, by="A", all.y = TRUE)

C
# It return all rows when there is a match in one of the datasets.
merge(dt1, dt2, all=TRUE)


# Convert a data.table to data.frame

# You can use setDF() function to accomplish this task.
setDF(mydata)
# Similarly, you can use setDT() function to convert data frame to data table.
set.seed(123)
X = data.frame(A=sample(3, 10, TRUE),
               B=sample(letters[1:3], 10, TRUE)
               setDT(X, key = "A")
               
 # Other Useful Functions
 

 # Examples for Practise
 # 
 # Q1. Calculate total number of rows by month and then sort on descending order
 mydata[, .N, by = month][order(-N)]
 # The .N operator is used to find count.

 # Q2. Find top 3 months with high mean arrival delay
 mydata[, .(mean_arr_delay = mean(arr_delay, na.rm = TRUE)), by = month][order(-mean_arr_delay)][1:3]

 # Q3. Find origin of flights having average total delay is greater than 20 minutes
 mydata[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("arr_delay", "dep_delay"), by = origin][(arr_delay + dep_delay) > 20]
 
 # Q4.  Extract average of arrival and departure delays for carrier == 'DL' by 'origin' and 'dest' variables
 mydata[carrier == "DL",
        lapply(.SD, mean, na.rm = TRUE),
        by = .(origin, dest),
        .SDcols = c("arr_delay", "dep_delay")]

 # Q5. Pull first value of 'air_time' by 'origin' and then sum the returned values when it is greater than 300
 mydata[, .SD[1], .SDcols="air_time", by=origin][air_time > 300, sum(air_time)]