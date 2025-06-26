TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

zcc:
	go build

test/%.exe: clean zcc test/%.c
	./zcc -Iinclude -Itest -c -o test/$*.o test/$*.c
	$(CC) -std=c11 -pthread -Wno-psabi -o $@ test/$*.o -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	sh test/driver.sh

clean:
	rm -rf zcc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean
