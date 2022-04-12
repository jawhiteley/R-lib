# I can't remember why I wrote this exactly, but it's a simulation choosing 18 'cards' out of 22, and checking to see how many draws it takes until you see all cards?
cards <- 1:22

listi <- c()
for (n in 1:1000) {
  tracker <- c()
  for (i in 1:10) {
    x <- sample(cards, 18)
    if (is.null(tracker)) {
      tracker <- sort(x)
    } else {
      tracker <- union(tracker, x)
      tracker <- sort(tracker)
    }
    print(tracker)
    if (identical(tracker, cards) & length(listi) != n) {
      listi <- c(listi, i)
    }
  }
}

hist(listi)


