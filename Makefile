TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

TEST_C23_SRCS=$(wildcard test/c23/*.c)
TESTS_C23=$(TEST_C23_SRCS:.c=.exe)

zcc:
	go build

test/%.exe: clean zcc test/%.c
	./zcc -Iinclude -Itest -c -o test/$*.o test/$*.c
	$(CC) -std=c11 -pthread -Wno-psabi -o $@ test/$*.o -xc test/common

test/c23/%.exe: clean zcc test/c23/%.c
	./zcc -std=c23 -Iinclude -Itest -c -o test/c23/$*.o test/c23/$*.c
	$(CC) -std=c11 -pthread -Wno-psabi -o $@ test/c23/$*.o -xc test/common

test: $(TESTS) $(TESTS_C23)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	bash test/driver.sh

clean:
	rm -rf zcc
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'
	find test/* -type f '(' -name '*~' -o -name '*.exe' ')' -exec rm {} ';'

.PHONY: test clean
