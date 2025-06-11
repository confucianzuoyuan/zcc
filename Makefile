TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

zcc:
	go build

test/macro.exe: zcc test/macro.c
	./zcc -c -o test/macro.o test/macro.c
	$(CC) -o $@ test/macro.o -xc test/common

test/%.exe: zcc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./zcc -c -o test/$*.o -
	$(CC) -o $@ test/$*.o -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh

clean:
	rm -rf zcc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean
