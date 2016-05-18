default: clean tests route

tests: Tests.hs
	@echo "Compiling tests..."
	ghc Tests.hs
	@./Tests

route: Main.hs
	@echo "Compiling program..."
	ghc Main.hs
	@./Main

clean:
	@echo "Cleaning..."
	rm -f *.hi *.o Main Tests
