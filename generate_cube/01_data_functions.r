###############################################################################################################
## 01_data_functions.r
## Amelia Bertozzi-Villa
## February 2020
## 
## Functions to accompany 01_prep_data.r

## -emplogit: 1-input empirical logit
## -emplogit2: 2-input empirical logit 
## -calc_access: calculate access metrics by household size from stock and flow outputs
## -reposition_points: assign impossibly-placed lat-longs to the nearest valid pixel

##############################################################################################################

# lifted from the docs of the empLogit function from the binomTools package
emplogit <- function (y, eps = 1e-3){
  # 1-variable logit transform
  # y: value between 0 and 1
  # eps: buffer to avoid null values
  return(log((eps + y)/(1 - y + eps)))
} 

emplogit2<-function(y, n){
  # approximation of a log odds
  # y: # of occurrences of interest
  # n: # of tries
  top=y+0.5
  bottom=n-y+0.5
  return(log(top/bottom))
}

calc_access <- function(hh_props, max_nets=40, return_mean=F){
  
  # Find the probabilities of a household of sizes 1:10 having 1:max_nets nets, 
  # assuming a zero-truncated poisson distribution with mean equal to the stock and flow mean. 
  # Multiply this by the weighted probability of having a net to get the weighted probability 
  # of an hh_sized household having a given number of nets. This is the same as the proportion of hh-sized households
  # expected to have a given number of nets-- e.g., as the access within that household size. 
  
  net_dist <- lapply(hh_props$hh_size, function(this_hh_size){
    
    these_net_stats <- hh_props[hh_size==this_hh_size]
    
    no_hh_subset <- data.table(hh_size=this_hh_size,
                               net_count=0,
                               weighted_net_prob=these_net_stats$weighted_prob_no_nets)
    
    net_hh_subset <- data.table(hh_size=this_hh_size,
                                net_count=1:max_nets,
                                weighted_net_prob=dpospois(1:max_nets, 
                                                           these_net_stats$stockflow_mean_nets_per_hh) * these_net_stats$weighted_prob_any_net
    )
    
    return(rbind(no_hh_subset, net_hh_subset))
    
  })
  
  net_dist <- rbindlist(net_dist)
  
  if (sum(net_dist$weighted_net_prob)!=1){
    if (abs(sum(net_dist$weighted_net_prob)-1)>1e-10){
      warning("Net probabilities do not sum properly!")
    }
  }
  
  # calculate access
  net_dist_test <- copy(net_dist)
  net_dist[, pop_weight:=hh_size*weighted_net_prob]
  net_dist[, prop_in_bin:=pop_weight/sum(pop_weight)] # convert from proportions among household to proportions among the population
  net_dist[, access_weight:= pmin(2*net_count/hh_size, 1)]
  net_dist[, prop_with_access:=access_weight * prop_in_bin]
  
  access_stats <- net_dist[, list(stock_and_flow_access=sum(prop_with_access)/sum(prop_in_bin)), by=hh_size] 
  all_access_stats <- sum(net_dist$prop_with_access)
  
  if(return_mean){
    return(all_access_stats)
  }else{
    return(access_stats)
  }
}


reposition_points<-function(nat_raster, points, radius=4){
  # assign impossibly-placed lat-longs to the nearest valid pixel, so long as they are sufficiently close to that pixel
  
  # nat_raster: raster layer of country boundaries
  # points: data.table including the columns "lat" and "long" for each cluster location
  # radius: cell count "radius" around which to search for a point to reposition
  
  matrix_side_len <- radius*2 + 1
  rm<-raster::extract(nat_raster, points[, list(lon,lat)])	# get all point values
  index<-which(is.na(rm))
  
  # loop through the points that need to be repositioned
  for(x in index){
    
    st<-paste0("<", "Survey-year: ", points$Survey[x],"-",points$year[x],">")
    error <- paste0("--> ", st, " Point ",  points[x,"lat"], points[x,"lon"]," is not positioned properly")
    
    # find cell "radius" rows up and "radius" columns left of the point of interest (for matrix contstruction)
    row1 =  rowFromY(nat_raster, points[[x,"lat"]])-radius 
    col1 = colFromX(nat_raster, points[[x,"lon"]])-radius 
    
    # if these are valid row values, attempt to reposition
    if(!is.na(col1) & !is.na(row1)){ 
      
      # construct a block of values with a radius of "radius" around the point to be repositioned
      mat<-raster::getValuesBlock(nat_raster, row=row1, nrows=matrix_side_len, col=col1, ncols=matrix_side_len) 

      # if any of the points *near* the original points exist, then reposition point.
      if(any(!is.na(mat))){	
        
        mat <- matrix(mat, nrow=matrix_side_len, ncol=matrix_side_len, byrow=TRUE)
        matcells<-mat
        
        # get cell counts for each point
        for(i in 1:dim(mat)[1]){
          for(j in 1:dim(mat)[2]){
            matcells[i,j]=as.numeric(cellFromRowCol(nat_raster,(row1-1+i),(col1-1+j)))	
          }
        }
        
        # get raw lat lons for each cell above
        matcoords<-xyFromCell(nat_raster,matcells) 
        matcells<-as.vector(matcells)
        
        # keep only non-null nearby pixels
        matcells<-matcells[!is.na(as.vector(mat))]
        matcoords<-matcoords[!is.na(as.vector(mat)),]
        
        # find distances between current point and non-null points 
        dist<-pointDistance(cbind(points[x,"lon"],points[x,"lat"]),matcoords,longlat=T)	# find the closest non na valid cell
        
        # assign point a new lat-long equal to the closest non-null point
        newp<-xyFromCell(nat_raster,matcells[which.min(dist)])
        points[x,"lon"]<-newp[,"x"]
        points[x,"lat"]<-newp[,"y"]
        
        print(paste("--> Point", st ,"has been repositioned at distance of", min(dist)/1000, "km", "at(",points[x,"lon"],",",points[x,"lat"],")"))  	
        
      } else {
        print(error)
      }
    } else{
      print(error)
      } 
  }
  return(points)
  
}




