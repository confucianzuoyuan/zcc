zcc:
	cargo build --release

test: zcc
	bash test.sh

clean:
	rm -f tmp*

.PHONY: test clean