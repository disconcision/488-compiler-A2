# gcc -o file file.s ; time ./file ; echo $
CC = gcc
CFLAGS += 

all: test

test: test.s
	$(CC) $(CFLAGS) -o test test.s

run: all
	time ./test
	echo $?

clean:
	rm -f *.o test
