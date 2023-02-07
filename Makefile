all: format test

format:
	for path in $$(ls -d *.cabal); do cabal-fmt -c $$path || cabal-fmt -i $$path; done
	ormolu --mode inplace -c $$(find . -name "*.hs" -not -path "./*.stack-work/*" -not -path "./dist/*" -not -path "./dist-newstyle/*" -not -path "./.git/*")

test:
	cabal test --test-show-details always -j +RTS -A128m -n2m -N -RTS -fwarn-incomplete-patterns --builddir dist/hspec-test

docs:
	cabal haddock --enable-documentation --builddir dist/docs

clean:
	rm -rf dist
