#Example code


data<-mtcars
percent<-c(.05,.1,.15)
goods<-c("hp","gear","wt")
weight<-c(6,7,8)
disagg<-"cyl"
func<-median

library(dplyr)
library(plyr)
library(tidyr)
library(purrr)
library(tibble)



sens_analysis<-function(data=data,
  goods=goods,
  weight=weight,
  disagg=disagg,
  precent=percent,
  func=func){


  basket <- (rbind(goods, weight))
  percent <- c(0, percent,(percent*-1))
  percent_to_1 <- percent+1

  data_select <- data %>%
    dplyr::select(c(goods, disagg)) %>%
    group_by_at(disagg) %>%
    summarise_at(.vars = goods , .funs = func) %>%
    as_tibble()

  data_select_weight <- purrr::map2(data_select[, -1], as.numeric(basket[2, ]), function(var, weight){
    var*weight
  }) %>% as_tibble %>%
    add_column(data_select[, 1], .before = 1)
  colnames(data_select_weight)[1] <- disagg


  multiply_across(data_select_weight, percent_to_1)

}


multiply_across<-function(data=data_select_weight,
  list=percent_to_1){
  varlist <- colnames(data[, -1])
  output_list <- list()
  for (i in varlist) {
    df1 <- data[,i]
    for (j in list) {
      name <- paste0(i, " BY ", (as.numeric(j)-1)*100, "% OVER ", disagg)

      df <- as_tibble(data)
      df[,i] <- round(df1*j, 2)
      df <- mutate(df, total = round(rowSums(df[,-1]),2))%>%
        mutate(type = paste0(i, " BY ", (as.numeric(j)-1)*100, "% OVER ", disagg))
      df<-df[,c(6,1,2,3,4,5)]


      output_list[[paste0(i," BY ",(as.numeric(j)-1)*100)]] <- (assign(paste0(i," BY ",(as.numeric(j)-1)*100,"% OVER ",disagg),df))

    }

  }

  bind_rows(lapply(output_list,
    as.data.frame.list,
    stringsAsFactors=F))

}


output<-sens_analysis(data,goods,weight,disagg,percent,func)
