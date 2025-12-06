OUTDIR := out
PROGS := progs

ifeq ($(shell uname), Darwin)
  LANGS_CC ?= arch -x86_64 gcc
  LANGS_AS ?= nasm -g -f macho64 --gprefix _
else
  LANGS_CC ?= gcc
  LANGS_AS ?= nasm -g -f elf64
endif

.SECONDARY:

default: mash

objs = \
	runtime/print_int.o \
	runtime/print_bool.o \
	runtime/print_char.o

mash: src runtime/runtime.o
	cargo build  --color=always --package mash --bin mash --profile dev

runtime/runtime.o: $(objs)
	ld -r $(objs) -o runtime/runtime.o

$(OUTDIR)/%: $(OUTDIR)/%.o runtime/runtime.o
	$(LANGS_CC) runtime/runtime.o $< -o $@

runtime/%.o: runtime/%.c
	$(LANGS_CC) -fPIC -c -g -o $@ $<

clean:
	@$(RM) out/* runtime/*.o

fclean:
	@$(RM) out/*

$(OUTDIR)/%.o: $(OUTDIR)/%.s
	$(LANGS_AS) $< -o $@

$(OUTDIR)/%.s: $(PROGS)/%.msh
	cargo run  --color=always --package mash --bin mash --profile dev $<






