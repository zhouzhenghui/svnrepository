"get.tim.data" = function(combined_data_vector) {
   combined_data_vector = as.data.frame(combined_data_vector)
   supply_demand_data_code_vector = names(combined_data_vector)
   sd_data_vector_mean = mean(combined_data_vector)
   sd_data_vector_stdi = NULL

   for (i in 1:length(supply_demand_data_code_vector)){
	name = names(combined_data_vector[i])
      flag =  (name == "thirty_year_current_coupon") || (name == "thirty_year_commitment_rate") || (name == "primary_interest_rate") || (name == "libor_2") || (name == "libor_5") || (name == "libor_2") || (name == "libor_20")
      if(flag){
		sd_data_vector_stid_temp = combined_data_vector[i] - sd_data_vector_mean[i]
            names(sd_data_vector_special_temp) = name
            sd_data_vector_stid_temp = as.data.frame(sd_data_vector_stid_temp[,1][2:length(sd_data_vector_stid_temp[,1])])
            names(sd_data_vector_stid_temp) = supply_demand_data_code_vector[i]
	}else{
		sd_data_vector_stid_temp = as.data.frame(sapply(combined_data_vector[i],"get.sreturn"))
            sd_data_vector_special_temp = sd_data_vector_stid_temp
	}
      if(length(sd_data_vector_stdi)==0){
		sd_data_vector_stdi = sd_data_vector_stid_temp
	}else{
		sd_data_vector_stdi = cbind(sd_data_vector_stdi,sd_data_vector_stid_temp)
      }
   }
   return(sd_data_vector_stdi)
}