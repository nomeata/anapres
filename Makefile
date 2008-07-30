anapres: anapres.hs AnaPresParse.hs TransMaker.hs
	ghc -O2 --make anapres.hs

AnaPresParse.hs: AnaPresParse.y
	happy $+

