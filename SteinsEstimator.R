bball = read.table("http://www.swarthmore.edu/NatSci/peverso1/Sports%20Data/JamesSteinData/Efron-Morris%20Baseball/EfronMorrisBB.txt",
                   header=TRUE, stringsAsFactors=FALSE)
bball$js = bball$BattingAverage * .212 + .788 * (0.265)
bball$LastName[!is.na(match(bball$LastName, 
                            c("Scott","Williams", "Rodriguez", "Unser","Swaboda","Spencer")))] = ""

a = matrix(rep(1:3, nrow(bball)), 3, nrow(bball))
b = matrix(c(bball$BattingAverage, bball$SeasonAverage, bball$js),  3, nrow(bball), byrow=TRUE)
matplot(a, b, pch=" ", ylab="predicted average", xaxt="n", xlim=c(0.5, 3.1), ylim=c(0.13, 0.42))
matlines(a, b)
text(rep(0.7, nrow(bball)), bball$BattingAverage, bball$LastName, cex=0.6)
text(1, 0.14, "First 45\nat bats", cex=0.5)
text(2, 0.14, "Average\nof remainder", cex=0.5)
text(3, 0.14, "J-S\nestimator", cex=0.5)
