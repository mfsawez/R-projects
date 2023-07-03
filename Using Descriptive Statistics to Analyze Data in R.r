#-------------------------------------------------------------------------------
# Guided project to calculate descriptive statistical metrics on a dataset
# and create a data quality report file.
#-------------------------------------------------------------------------------
# Task 1 - Load and View Real World data set in RStudio
print("Salaam!")
data <- read.csv("data.csv")

#-------------------------------------------------------------------------------
# Task 2 - Calculate Measure of Frequency metrics
length(data$Transmission.Type)
length(unique(data$Transmission.Type))
table(data$Transmission.Type)
freq <- table(data$Transmission.Type)
freq <- sort(freq, decreasing = TRUE)
freq

#-------------------------------------------------------------------------------
# Task 3 - Calculate Measure of Central Tendency metrics
mean(data$Engine.HP)
mean(data$Engine.HP,na.rm=TRUE)
median(data$Engine.HP, na.rm=TRUE)
uniqueValues <- unique(data$Engine.HP)
uniqueValues[which.max(tabulate(match(data$Engine.HP, uniqueValues)))]
#-------------------------------------------------------------------------------
# Task 4 - Calculate Measure of Dispersion metrics
min(data$Engine.HP, na.rm=TRUE)
max(data$Engine.HP, na.rm=TRUE)
range(data$Engine.HP, na.rm=TRUE)
var(data$Engine.HP, na.rm=TRUE)
sd(data$Engine.HP, na.rm=TRUE)
x <-c(4,7,22,NA,9,3,15,32,NA,27)
mean(x, na.rm=TRUE)
sd(x, na.rm=TRUE)


#-------------------------------------------------------------------------------
# Task 5 - Calculate additional data quality metrics
as.numeric(2)
as.character(2)
test <- as.character(1:3)
mean(test)
class(test)
class(data$Engine.HP)
class(data$Engine.Fuel.Type)
test2 <- c(NA,2,35,46,NA)
test2
sum(is.na(test2))
sum(is.na(data$Transmission.Type))
sum(is.na(data$Number.of.Doors))

#-------------------------------------------------------------------------------
# Task 6 - Calculate descriptive statistics on all columns
apply(data, MARGIN=2, length)
sapply(data, function(x) min(x, na.rm=TRUE))
quality_data <- function(df=NULL){
  if(is.null(df)) print("Please pass a non-empty data frame")
  summary_table <- do.call(data.frame,
                           list(
                             Min = sapply(df, function(x) min(x, na.rm=TRUE)),
                             Max = sapply(df, function(x) max(x, na.rm=TRUE)),
                             SD = sapply(df, function(x) sd(x, na.rm=TRUE)),
                             Total = apply(df, 2, length),
                             NULLS = sapply(df, function(x) sum(is.na(x))),
                             Unique = sapply(df, function(x) length(Unique(x))),
                             dataType = sapply(df, class)
                           ))
  nums <- vapply(summary_table, is.numeric, FUN.VALUE = logical(1))
  summary_table[,nums] <- round(summary_table[,nums], digits = 3)
  return(summary_table)
}
View(data)
#-------------------------------------------------------------------------------
# Task 7- Generate a data quality report file
df_quality <- quality_data(data)
View(df_quality)
df_quality <- cbind(Columns=rownames(df_quality),
                    data.frame(df_quality, row.names = NULL))
write.csv(df_quality, paste0("Data Quality Report ",
                             format(Sys.time(), "%d-%m-%Y-%H%M%S"),
                             ".csv"),
          row.names = FALSE)

#-------------------------------------------------------------------------------
# END OF PROJECT
#-------------------------------------------------------------------------------