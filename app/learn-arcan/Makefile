include config.mk

.POSIX:
.SUFFIXES: .c .o

SRC = arcan-eglintro.c
SCRIPTS =

OBJ = $(SRC:.c=.o)
BIN = $(SRC:.c=)
# DEPS = Utils.o
DEPS =

all: options $(DEPS) bin

options:
	@echo init build options:
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

bin:	$(DEPS) $(BIN)

scripts: $(SCRIPTS)

$(OBJ): config.mk

.o:
	@echo LD $@
	@echo $(LD) -o $@ $< $(DEPS) $(LDFLAGS)
	@$(LD) -o $@ $< $(DEPS) $(LDFLAGS)

.c.o:
	@echo CC $<
	@echo $(CC) -c -o $@ $< $(CFLAGS)
	@$(CC) -c -o $@ $< $(CFLAGS)

clean:
	@echo cleaning
	@rm -f $(BIN) $(OBJ) $(DEPS)
