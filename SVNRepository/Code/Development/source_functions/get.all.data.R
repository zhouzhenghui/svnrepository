#states is a vector you specify

"get.all.data" = function(supply_demand_data_code_vector,state,begin_month,begin_year,end_month,end_year,smooth) {
   source("../source_functions/get.sd.data.R")
   source("../source_functions/get.sreturn.R")
   housing_data_code = c("housing_price_index")

   state_data = as.data.frame(get.sd.data(housing_data_code,state,begin_month,begin_year,end_month,end_year)$interpolated_data)
   names(state_data)=state
   sd_data_vector = NULL 
   for (i in 1:length(supply_demand_data_code_vector)){
	sd_data_vector_temp = as.numeric(as.matrix(get.sd.data(supply_demand_data_code_vector[i],state,begin_month,begin_year,end_month,end_year)$interpolated_data))
	sd_data_vector_temp = as.data.frame(sd_data_vector_temp)
      names(sd_data_vector_temp)=supply_demand_data_code_vector[i]
      if(length(sd_data_vector)==0){
		sd_data_vector = sd_data_vector_temp
	}else{
		sd_data_vector = cbind(sd_data_vector,sd_data_vector_temp)
	}
   }
  combined_data_vector = cbind(sd_data_vector,state_data)
	#tim added this
	if (smooth==TRUE){
		combined_data_vector = smoother(combined_data_vector)
	}
  state_data.sreturn = as.data.frame(sapply(state_data,"get.sreturn"))
  sd_data_vector_stdi = NULL
  sd_data_vector_special = NULL
  sd_data_vector_mean = mean(sd_data_vector)
  for (i in 1:length(supply_demand_data_code_vector)){
	name = names(sd_data_vector[i])
      flag =  (name == "thirty_year_current_coupon") || (name == "thirty_year_commitment_rate") || (name == "primary_interest_rate") || (name == "libor_2") || (name == "libor_5") || (name == "libor_2") || (name == "libor_20")
      if(flag){
		sd_data_vector_stid_temp = sd_data_vector[i] - sd_data_vector_mean[i]
            if (name == "thirty_year_commitment_rate"){
               sd_data_vector_special_temp = as.data.frame(diff(sd_data_vector_stid_temp[,1]))
               names(sd_data_vector_special_temp) = name 
            }else{
               sd_data_vector_special_temp = as.data.frame(sd_data_vector_stid_temp[,1][2:length(sd_data_vector_stid_temp[,1])])
               names(sd_data_vector_special_temp) = name
            }
            sd_data_vector_stid_temp = as.data.frame(sd_data_vector_stid_temp[,1][2:length(sd_data_vector_stid_temp[,1])])
            names(sd_data_vector_stid_temp) = supply_demand_data_code_vector[i]
	}else{
		sd_data_vector_stid_temp = as.data.frame(sapply(sd_data_vector[i],"get.sreturn"))
            sd_data_vector_special_temp = sd_data_vector_stid_temp
	}
      if(length(sd_data_vector_stdi)==0){
		sd_data_vector_stdi = sd_data_vector_stid_temp
            sd_data_vector_special = sd_data_vector_special_temp
	}else{
		sd_data_vector_stdi = cbind(sd_data_vector_stdi,sd_data_vector_stid_temp)
            sd_data_vector_special = cbind(sd_data_vector_special,sd_data_vector_special_temp)
      }
   }
   combined_data_vector_stdi = cbind(sd_data_vector_stdi,state_data.sreturn)
   combined_data_vector_special = cbind(sd_data_vector_special,state_data.sreturn)
   combined_data_vector.diff = as.data.frame(sapply(combined_data_vector,"diff"))
   combined_data_vector.doublediff = as.data.frame(sapply(combined_data_vector_stdi,"diff"))
   structure(list(combined=combined_data_vector,combined_spec=combined_data_vector_special,combined_stand=combined_data_vector_stdi,combined.diff=combined_data_vector.diff,combined_dbdiff=combined_data_vector.doublediff))
}