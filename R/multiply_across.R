#' Multiplies percents across individual columns
#'
#' This function allows you to multiply individual columns by a list of percentages.
#' @param data A data set
#' @param list A list of percentages to multiply across columns ofthe dataset
#' @return A binded object of modified tabled that had multiplied across percentages
#' @examples
#' multiply_across(df, list)
#' multiply_across(df, c(.05,.1))
#' multiply_across(data, list)


#' @export
multiply_across<-function(data=data_select_weight,
  list=percent_to_1){

  assertthat::assert_that(is.numeric(list), msg= "Your percents are not numerics")
  #get the names of all columns into a list, without the first column as that is just the disagg label. (this was placed there in the sens analysis function)
  varlist <- colnames(data[, -1])
  #create an empty list we will populate later
  output_list <- list()

  #create a for loop within a for loop to multiply the amount of the a good by the percentage, while holding everything else constant
  for (i in varlist) {
    #create a quick df of a single column that will be multiplied and combined into the base dataset later
    df1 <- data[,i]
    for (j in list) {
      #create a name of what we are multiplying to follow at a later point
      name <- paste0(i, " BY ", (as.numeric(j)-1)*100, "% OVER ", disagg)

      df <- as_tibble(data)
      #replace the original data (df) with the new df1 times the percent in the list, then round it
      df[,i] <- round(df1*j, 2)
      #creat the new total of the columns with the new multiplied disag
      df <- mutate(df, total = round(rowSums(df[,-1]),2))%>%
            mutate(type = paste0(i, " BY ", (as.numeric(j)-1)*100, "% OVER ", disagg))

      #df<-df[,c(6,1,2,3,4,5)]

      #place into an output list and assign to the new df, name.
      output_list[[paste0(i," BY ",(as.numeric(j)-1)*100)]] <- (assign(paste0(i," BY ",(as.numeric(j)-1)*100,"% OVER ",disagg),df))

    }

  }
  #bind all output lists into a data.frame that will compile the new output.
  bind_rows(lapply(output_list,
    as.data.frame.list,
    stringsAsFactors=F))

}

