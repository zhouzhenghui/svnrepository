
"get.sd.data" = function(data_code,state = "",begin_month,begin_year,end_month,end_year)
{
	source("../source_functions/convert_data_freq.R")
	source("../source_functions/chop.R")
	no_monthly=FALSE
	cols = 1	
	master = read.csv("../../../Data/Supply Demand/Data/supply_demand_variables_complete.csv")
	code_index = which(master$Code==data_code)
	file_name = as.matrix(master$File[code_index])
	total_data = read.csv(paste("../../../Data/Supply Demand/Data/",file_name,sep=""))
	if (state=="" || (length(which(names(total_data)=="National"))>=1)){
		#must be national/regional, so look for national column
		if(length(which(names(total_data)=="National"))>=1){
			data_subset = total_data
			data_index = which(names(data_subset)=="National")
			
		}else{
			print("error")
		}
		
	}else{
		if(length(which(names(total_data)=="State"))>=1){
			#read appropriate state
			data_subset = subset(total_data, total_data$State == state)			
			data_index = which(names(data_subset)=="Data")
			
		}else{
			#states are listed accross the top
			data_subset = total_data
			data_index = which(names(total_data)==state)			
		}
	}
		if(length(which(names(data_subset)=="Month"))>=1){
				months = data_subset$Month
				cols = cols + 1
			}else{
				months = ""
				no_monthly=TRUE
			}
			if(length(which(names(data_subset)=="Year"))>=1){
				years = data_subset$Year
				cols = cols + 1
			}else{
				years = ""
			}
			if(length(which(names(data_subset)=="Quarter"))>=1){
				quarters = data_subset$Quarter
				cols = cols + 1
			}else{
				quarters = ""
			}

		columns = c(seq(1:cols),data_index)
		data = as.matrix(data_subset)[,columns]
		data = as.data.frame(data)
		
	
	#align the data here (beginning/end subset)
	if ((months[1]!="" || quarters[1]!="") && years[1]!=""){
		#must be quarterly or monthly already
		yearly = FALSE
		last_element = end_month
		if(quarters[1]!=""){
			months = quarters			
			last_element = ifelse(end_month==12,4,2) #if not year end, assume half year
		}
		begin_month_match = which(as.numeric(months )==begin_month)
		begin_year_match = which(as.numeric(as.matrix(data$Year))==begin_year)

		end_month_match = which(as.numeric(months)==last_element)
		end_year_match = which(as.numeric(as.matrix(data$Year))==end_year)
		
		
		begin_row_match = begin_month_match[match(begin_year_match,begin_month_match)[1]]-ifelse(quarters[1]!="",1,0)
		end_row_match = end_month_match[match(end_year_match,end_month_match)[last_element]]
		
		
	}else{
		#must be yearly data
		yearly=TRUE
		begin_year_match = which(as.numeric(as.matrix(data$Year))==begin_year)
		end_year_match = which(as.numeric(as.matrix(data$Year))==end_year)
		begin_row_match = begin_year_match-1 #minus one row to include extra interpolated data
		end_row_match =  end_year_match
	
	}
	
		data = data[seq(begin_row_match,end_row_match),]
	#interpolate here
		if(no_monthly){
			frequency = ifelse(quarters[1]!="","quarter","annual")[1]

			interpolated_data = convert_data_freq(as.numeric(as.matrix(data[dim(data)[2]])),frequency,"linear") 
			interpolated_data = interpolated_data[2:length(interpolated_data)]
			data = data[seq(2,dim(data)[1]),]

		}else{
			interpolated_data = as.numeric(as.matrix(data[,dim(data)[2]]))
		}
	#adjust for mid year data in annual interpolation
		if(yearly){
				 interpolated_data= chop(interpolated_data,12-end_month,"end")
			}else{
				interpolated_data =interpolated_data 
			}
	#done
	structure(list(data = data, interpolated_data = interpolated_data))

}
