EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q -batch -L . -L test \
	--eval '(setq linear-api-use-auth-source nil)' \
	-l linear

.PHONY: test test-unit test-integration build-server

test: build-server
	$(EMACS_BATCH) -l linear-unit-test -l linear-test \
		--eval '(ert-run-tests-batch-and-exit "linear-test-")'

test-unit:
	$(EMACS_BATCH) -l linear-unit-test \
		--eval '(ert-run-tests-batch-and-exit "linear-test-build-query")'

test-integration: build-server
	$(EMACS_BATCH) -l linear-test \
		--eval '(ert-run-tests-batch-and-exit "linear-test-")'

build-server:
	cargo build --release --manifest-path test-server/Cargo.toml
