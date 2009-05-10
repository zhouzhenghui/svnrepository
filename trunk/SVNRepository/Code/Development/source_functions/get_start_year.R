"get_start_year"=function(state){
table = read.csv("../../../Data/Supply Demand/Data/lagranges.csv")
start_year=table$start_year[which(table$State==state)]
start_year
}
