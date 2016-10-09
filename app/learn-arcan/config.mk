# simplified init version
VERSION = 0.0.1

# paths
PREFIX  =
MANDIR  = ${PREFIX}/usr/share/man
ETCDIR  = ${PREFIX}/etc
SBINDIR = ${PREFIX}/sbin

# cflags from $(pkg-config --cflags freeglut glew)
# libs from $(pkg-config --libs freeglut glew)
CC = gcc
LD = $(CC)
CPPFLAGS =
CFLAGS   = -Wextra -Wall -Wno-unused-result -O2 -I/home/j/dev/apps/durden-arcan/arcan/src -I/home/j/dev/apps/durden-arcan/arcan/src/shmif -I/usr/include/
LDFLAGS  = -lm -lpthread -lGLEW -lGL -lGLU -larcan_shmif -larcan_shmif_ext -lGLESv2 -L/home/j/dev/apps/durden-arcan/arcan/build/shmif -L/usr/lib64/


