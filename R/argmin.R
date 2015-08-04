####  argmin ####
argmin = function(v, target=0)
  sapply(target, function(target)which(abs(v-target) == min(abs(v-target))[1]))
