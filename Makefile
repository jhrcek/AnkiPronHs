.PHONY: format
format:
	git ls-files '*.hs' | xargs fourmolu -i
	cabal-fmt --inplace anki.cabal

.PHONY: install
install:
	stack install --pedantic --local-bin-path ~/Dropbox/Softy/AnkiPron/
