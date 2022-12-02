all: day1t1 day1t2

day1t1: day1t1.hs util.hs
	ghc day1t1.hs util.hs -o day1t1

day1t2: day1t2.hs util.hs
	ghc day1t2.hs util.hs -o day1t2