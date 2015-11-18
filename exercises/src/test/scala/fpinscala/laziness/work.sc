import fpinscala.laziness.Stream

Stream(1,2,3).hasSubsequence(Stream(2))
Stream(5,3,7,4,3,4,4).hasSubsequence(Stream(7,4,5))
Stream(5,3,7,4,3,4,4).hasSubsequence(Stream(7,4,3))
Stream(5,3,7,4,3,4,4).tails
Stream(5,3,7,4,3,4,4).startsWith(Stream(3))


Stream(5,3,7,4,3,4,4) zipAll Stream(1,2,3,4,5)


