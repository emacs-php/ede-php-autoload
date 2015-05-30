CASK=cask

.PHONY: test elc package clean

test:
	$(CASK) exec ert-runner

elc:
	$(CASK) build

clean:
	$(CASK) clean-elc

package:
	$(CASK) package
