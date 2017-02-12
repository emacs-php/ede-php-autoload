CASK=cask

.PHONY: test test-simple elc package clean

test-unit:
	$(CASK) exec ert-runner

test-functional:
	$(CASK) exec ecukes

test-all:
	$(MAKE) test-unit
	$(MAKE) test-functional

test:
	$(MAKE) clean
	$(MAKE) test-all
	$(MAKE) elc
	$(MAKE) test-all

elc:
	$(CASK) build

clean:
	$(CASK) clean-elc

package:
	$(CASK) package
