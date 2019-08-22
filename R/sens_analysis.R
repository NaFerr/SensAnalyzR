#' Sensitivity Analysis
#'
#' This function creates a weighted data set that will produce an object where columns are disaggregated and multiplied by stated percentages.
#' @param data A data set
#' @param good A list of column names in the dataset that constitute the basket you want to look at
#' @param weight A list of weights that should correspond to the amount AND POSITION to the goods in the market basket
#' @param disagg A column name in the data set to disaggregate the data by
#' @param percent A list of percentages you would like to see columns multiplied by, NOTE: there is code in the script to make all values negative as well so for example 5, 10, 15 would actually be 5,10,15,-5,-10,-15 within the script
#' @param func The function that the market basket will be disaggregated with, will be used in the summarise_at funciton.
#' @return A binded object of modified tabled that had multiplied across percentages that utilizes a weighted dataset
#' @examples
#' sens_analysis(data, goods, weight, disagg, percent, func)
#' sens_analysis(df, c(a,b,c), c(1,2,3), "coiuntry_ID", c(.01,.05), median)

#' @export
sens_analysis<-function(data=data,
  goods=goods,
  weight=weight,
  disagg=disagg,
  precent=percent,
  func=func){

assertthat::assert_that(is.data.frame(data), msg = "Data selected is not a data.frame, would recommend making a tibble")
assertthat::assert_that(is.character(goods), msg = "Your goods list isn't as a character")
assertthat::assert_that(is.numeric(weight), msg= "Your weights are not numerics")
assertthat::assert_that(is.character(disagg), msg="Make your disagge variable a character")
assertthat::assert_that(is.numeric(percent), msg= "Your percents are not numerics")
assertthat::assert_that(all(percent < 1), msg="Put your percents in the 0.05 format, ie. no 5 for 5%, use 0.05")
assertthat::are_equal(length(goods),length(weight), msg="The goods and weights lists must be the same length, please change")

if(!is.function(func)){
warning("Function used is not Median, Mean, or Mode, would recommend not using this function in the summarise_at within current function")
}

  #creates a basket of the goods, with weights for associated linked
  basket <- (rbind(goods, weight))
  #create the percentages to multiply across, will create a base at 0, and then negatives associated with each listed weight
  #percent_to_1 makes sures that are able to be used in multiply across in the desired manner
  percent <- c(0, percent,(percent*-1))
  percent_to_1 <- as.numeric(percent+1)


  #pull out the data needed from the dataset relating to the market basket
  #data should be in numerics
  data_select <- data %>%
    dplyr::select(c(goods, disagg)) %>%
    group_by_at(disagg) %>%
    summarise_at(.vars = goods , .funs = func) %>%
    as_tibble()

  #create the market basket base, by multiplying the goods by the corresponding position within the weighted list
  data_select_weight <- purrr::map2(data_select[, -1], as.numeric(basket[2, ]), function(var, weight){
    var*weight
  }) %>% as_tibble %>%
    #add column before to then populate with what was this market was disaggregated by, this will allow you later to know which region or type of area you disaggregated by
    add_column(data_select[, 1], .before = 1)
  colnames(data_select_weight)[1] <- disagg

  #call the multiply across function within the package to multiply the percent_to_1 variable, created above, across the base market basket.
  multiply_across(data_select_weight, percent_to_1)

}
