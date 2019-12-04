add_one = function(x){
  carry = 1L
  p = length(x)
  result = vector("numeric", length = p)
  
  for(i in p:1){
    result[i] = x[i] + carry
    if(result[i] %% 10 == 0){
      result[i] = result[i] %% 10
      carry = 1L
    } else {
      carry = 0L
    }
  }
  if(result[1] == 0L){
    result = c(1L, result)
  }
  return(result)
}

add_one(x = c(1,2,4))
add_one(x = c(1,2,9))
add_one(x = c(9,9,9))