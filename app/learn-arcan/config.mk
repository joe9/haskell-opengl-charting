# simplified init version
VERSION = 0.0.1

VIDEOBACKEND=SDL
# VIDEOBACKEND=EGL-DRI

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
CFLAGS   = -D VIDEOBACKEND=$(VIDEOBACKEND) -g -Wextra -Wall -Wno-unused-result -O2 -I/home/j/dev/apps/durden-arcan/arcan/src -I/home/j/dev/apps/durden-arcan/arcan/src/ -I/usr/include/
COMMONLDFLAGS  = -lm -lpthread -lGL -lEGL -larcan_shmif -larcan_shmif_ext -L/usr/lib64/

SDLLIBFOLDER = -L/home/j/dev/apps/durden-arcan/arcan/build-sdl/shmif
LIBFOLDER = -L/home/j/dev/apps/durden-arcan/arcan/build/shmif

ifeq ($(VIDEOBACKEND), SDL)
	LDFLAGS  = ${COMMONLDFLAGS} ${SDLLIBFOLDER}
else
	LDFLAGS  = ${COMMONLDFLAGS} ${LIBFOLDER}
endif


