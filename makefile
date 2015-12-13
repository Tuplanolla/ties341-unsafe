CFLAGS=-O3 -std=c11
LDLIBS=-lm

build: main

clean:
	$(RM) findroot.o main

main: main.c findroot.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<
