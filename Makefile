anapres: anapres.hs AnaPresParse.hs TransMaker.hs
	ghc --make anapres.hs

AnaPresParse.hs: AnaPresParse.y
	happy $+

