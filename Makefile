
# path to uim.h
UIM_DIR = ../uim/uim
CFLAGS = -I$(UIM_DIR) -Wall -g -Wextra # -Werror
LIBS = $(UIM_DIR)/.libs/libuim.so

libuim-test0002.so: test0002.o
	$(CC) $(CFLAGS) -shared -o $@ $< $(LIBS)
