CASK=cask

.PHONY: test test-simple elc package clean

test-simple:
	$(CASK) exec ecukes

test:
	$(MAKE) clean
	$(MAKE) test-simple
	$(MAKE) elc
	$(MAKE) test-simple

elc:
	$(CASK) build

clean:
	$(CASK) clean-elc

package:
	$(CASK) package
