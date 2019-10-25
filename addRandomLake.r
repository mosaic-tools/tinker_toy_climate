#Function to add random lake to network
#moasics working group 23 Oct 2019

addRandomLake<-function(network,order,width,depth,lake_d=0.005,verbose=FALSE){
  # make sure width and depth are the same length as order, if not repeat what is supplied to make same length
  if(length(width)<length(order)){
    width=rep(width,length.out=length(order))
  }
  if(length(depth)<length(order)){
    depth=rep(depth,length.out=length(order))
  }
  # new network dataframe to be returned with lakes added
  newNet=network
  
  # summary of network (number of reaches of each order)
  networkOrderCount=table(c(network$order,1:max(c(network$order,order))))-1
  
  # number of reaches of various orders to be changed to lakes
  toChangeCount=table(c(order,1:max(c(network$order,order))))-1
  
  # make random changes
  for(i in 1:length(toChangeCount)){
    # if we're changing any order i reaches to lakes
    if(toChangeCount[i]>0){
      # if there are enough order i reaches to make the desired number of lakes
      if(toChangeCount[i]<networkOrderCount[i]){
        # randomly select desired number of reaches of order i to turn into lakes by setting width, depth, and d to values passed as arguments
        toChange=sample((1:nrow(network))[network$order==names(toChangeCount)[i]],toChangeCount[i],replace=FALSE)
        newNet$width[toChange]=width[order==names(toChangeCount)[i]]
        newNet$depth[toChange]=depth[order==names(toChangeCount)[i]]
        newNet$d[toChange]=lake_d
        newNet$lake[toChange]=1
        if(verbose){
          print(paste(toChangeCount[i]," of ",networkOrderCount[i]," order ",names(networkOrderCount)[i]," changed to lakes.",sep=""))
        }
        # if there are NOT enough order i reaches to make the desired number of lakes, change all of them and print warning if verbose is TRUE 
      }else{
        toChange=(1:nrow(network))[network$order==names(toChangeCount)[i]]
        newNet$width[toChange]=width[order==names(toChangeCount)[i]]
        newNet$depth[toChange]=depth[order==names(toChangeCount)[i]]
        newNet$d[toChange]=lake_d
        newNet$lake[toChange]=1
        
        print(paste("ONLY ",length(toChange)," of ",networkOrderCount[i]," order ",names(networkOrderCount)[i]," changed to lakes.",sep=""))
      }
      # if we are NOT changing any order i reaches to lakes  
    }else{
      if(verbose){
        print(paste("0 of ",networkOrderCount[i]," order ",names(networkOrderCount)[i]," changed to lakes.",sep=""))
      }
    }
  }
  return(newNet)
}