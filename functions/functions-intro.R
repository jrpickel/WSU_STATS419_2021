handshake = function(n=1,plotme=F){
  if(floor(n) == n && n > 0){
    num_of_handshake = n*(n-1)/2;
    if(plotme == F){
      print(paste0("The number of handshakes is ",num_of_handshake))
    }
    else {
      m = matrix(1,n,n)
      for(i in c(1:n)){
        m[i,i] = 0
      }
      title = paste0("The number of hand shakes is ",num_of_handshake)
      network = graph.adjacency(m)
      plot(network,main = title )
    }
  }
  else{
    print("Please enter a integer greater than 0")
  }
}

letter_count = function(string){
  string = str_to_lower(string)
  dict = str_count(string,letters)
  num_punc = str_count(string,c("[:digit:]","[:punct:]"))
  names(dict) = paste(letters)
  dict$OTHER = sum(num_punc)
  df = as.data.frame(dict)
  df
}

string_difference = function(str1,str2){
  str1 = gsub("[\r\n]", " ", str1)
  str2 = gsub("[\r\n]", " ", str2)
  str1_vect = unlist(strsplit(str1,split = ' '))
  str2_vect = unlist(strsplit(str2,split = ' '))
  difference = setdiff(str1_vect,str2_vect)
  difference
}

computeDeterminant = function(m){
  if(nrow(m) != ncol(m)){
    return(print("Please enter a square matrix"))
  }
  if(nrow(m) == 2){
    res = (m[1,1] * m[2,2]) - (m[1,2] * m[2,1])
    return(res)
  }
  else{
    start = 0
    for (i in c(1:nrow(m))){
      result = (m[1,i] * computeDeterminant(m[-c(1),-c(i)]))
      if(i%%2 != 0){
        start = start + result
      }
      else{
        start = start - result
      }
    }
    return(start)
  }
}