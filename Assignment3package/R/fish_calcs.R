#' A Fish Catch and Revenue Function
#'
#' This function tells you which fish was most frequently caught per location, the revenue generated at each location, and plots the revenue across locations
#' @param table Dataframe with Location as column names and Fish names as rows
#' @param priceTab Dataframe with columns for Fish name and Price
#' @param plot Do you want a plot of revenue across locations? Default is FALSE
#' @author Emma Siegfried
#' @keywords fish
#' fish_calcs()

fish_calcs = function(catch_table, catch_price, plot = FALSE) {

  #Organize Data
  ncol = ncol(catch_table)

  #1) Most Frequently Caught Fish per Location
  Majority_Fish = rownames(catch_table)[apply(catch_table[,1:ncol], 2, which.max)]

  Catch = apply(catch_table[,1:ncol], 2, max)

  Majority_Loc = colnames(catch_table)[apply(catch_table[,1:ncol], 2, which.max)]

  Majority_Fish_table = cbind.data.frame(Majority_Loc, Catch, Majority_Fish)
  Majority_Fish_table = Majority_Fish_table[-c(1)]

  #Reorganize catch table:
  table_reordered = catch_table %>%
    rownames_to_column() %>%
    rename(fish_name = rowname)

  table_reordered = gather(table_reordered,
                           key = "Location",
                           value = "Catch", 2:ncol, na.rm = FALSE)


  #Join tables
  table_combo = full_join(table_reordered, catch_price,
                          by = "fish_name")
  table_combo = table_combo %>%
    mutate(Ind_Revenue = Catch * Price)

  #2) Revenue per Location
  table_rev_loc = table_combo %>%
    group_by(Location) %>%
    summarize_at(vars(Ind_Revenue), sum) %>%
    rename(Revenue = Ind_Revenue)

  #3) Overall Revenue
  total_revenue = sum(table_rev_loc$Revenue)

  #4) Optional Graph
  if (plot) {
    lb = sprintf("The total revenue is $%s ", total_revenue)
    p = ggplot(table_rev_loc, aes(Location, Revenue, fill=Location))+
      geom_col(show.legend = F)+
      labs(y="Revenue ($)")+
      annotate("text", x=4, y=1000, label=lb, col="red")+
      theme_classic()
  }
  else p=NULL

#  return(Majority_Fish_table)

  return(list("Catch data provided by User:" = catch_table,
             "Fish caught the most per Location:" = Majority_Fish_table,
              "Total Revenue at each Location" = table_rev_loc,
              "Overall Catch Revenue" = total_revenue,
              "Plot: Total Revenue per Location" = p))

}
