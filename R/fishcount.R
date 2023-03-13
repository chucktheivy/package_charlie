fishsummary = function(fishVec) {
  fishFact = as.factor(fishVec)

  # max name and number
  a = names(which.max(summary(fishFact)))
  b = max(summary(fishFact))

  # min name and number
  c = names(which.min(summary(fishFact)))
  d = min(summary(fishFact))

  # calculate total number of fish
  e = length(fishVec)

  return(list(mostCommonFish = a, numberMostCommonFish = b, leastCommonFish = c, numberLeastCommonFish = d, totalFishCaught = e))

}
