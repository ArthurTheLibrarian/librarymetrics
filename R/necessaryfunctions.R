#'These are the four functions for the package
#'You can read the Markdown file for more information

#'The readData function takes the csv files from the indicated folder and
#'compiles them into a list before converting the elements into data.frames
#'and recompiling them into a list
#'the fread function was super useful for compiling the csv files
#' @export
readData <- function(datapath) {
  filenames <- list.files(path=datapath, full.names=TRUE)
  temp1 <- array(lapply(filenames, data.table::fread))
  finallist <- list()
  for(i in 1:length(filenames)){
    finallist[[i]] <- createddf(temp1, i)
  }
  return(finallist)
}

#'The createddf is used in the readData function to convert each element of
#'a list into a data.frame, which is necessary so that we can easily manipulate
#'the data
#' @export
createddf <- function(lst, i) {
  data.frame(lst[i])
}


#'The daily metrics function finds the mean, range, and total values and outputs
#'some of them as a string to display the results to the user regarding
#'the daily metrics of the library; the inputs are the list, and the element or
#'day that they want to get the information of
#' @export
dailymetrics <- function(r, i) {
  k <- r[[i]]
  date <- k[14, 5]
  vis <- k[-length(k[,2]),2]
  bout <- k[-length(k[,3]),3]
  bin <- k[-length(k[,4]),4]
  tvis <- k[length(k[,2]),2]
  tbout <- k[length(k[,3]),3]
  tbin <- k[length(k[,4]),4]
  mvis <- mean(vis)
  mbout <- mean(bout)
  mbin <- mean(bin)
  rvis <- range(vis)
  rbout <- range(bout)
  rbin <- range(bin)
  rcom <- range(k[-length(k[,5]),5])
  print(date)
  print("On this day, the total number of people that visited the library was:")
  print(tvis)
  print("The total number of books checked out was:")
  print(tbout)
  print("The total number of books returned was:")
  print(tbin)
  print("The largest number of visitors that we had within an hour was:")
  print(rvis[2])
  print("Out of 14 computers, the most computers in use at any point was:")
  print(rcom[2])
}

#'The barwvis function takes the list and then creates a bar graph plotting the
#'total number of visitors over the course of the collected data
#' @export
barwvis <- function(lst){
  tv <- c()
  dates <- c()
  for(i in 1:length(lst)) {
    dates[i] <- lst[[i]][14,5]
  }
  for(i in 1:length(lst)) {
    tv[i] <- lst[[i]][14,2]
  }
  barplot(tv, xlab="Date", ylab="Total Visitors", names.arg=dates)
}

#'in future versions of this package there would be more functions added to
#'display other types of graphs and show more daily metrics, but this
#'is good enough of a prototype to show how it'd work
