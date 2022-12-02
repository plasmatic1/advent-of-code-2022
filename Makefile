all: day1t1 day1t2 day2t1 day2t2

day1t1: day1t1.hs util.hs
	ghc day1t1.hs util.hs -o day1t1

day1t2: day1t2.hs util.hs
	ghc day1t2.hs util.hs -o day1t2

day2t1: day2t1.hs util.hs
	ghc day2t1.hs util.hs -o day2t1

day2t2: day2t2.hs util.hs
	ghc day2t2.hs util.hs -o day2t2

.PHONY: clean
clean: 
	rm *.o *.hi day1t1 day1t2 day2t1 day2t2