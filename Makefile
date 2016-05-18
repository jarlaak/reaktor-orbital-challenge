default: tests route

tests: Tests.hs
	ghc Tests.hs
	./Tests

route: Main.hs
	ghc Main.hs
	./Main
