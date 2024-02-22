zcc:
	cargo build

test: zcc
	bash test.sh

clean:
	rm -f tmp*

.PHONY: test clean