ifeq ($(shell uname), Darwin)
  LANGS_CC ?= arch -x86_64 gcc
  LANGS_AS ?= nasm -g -f macho64 --gprefix _
else
  LANGS_CC ?= gcc
  LANGS_AS ?= nasm -g -f elf64
endif

default: mash

objs = \
	runtime/print_int.o

mash: src
	cargo build  --color=always --package mash --bin mash --profile dev

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

%.run: %.o runtime.o
	$(LANGS_CC) runtime/runtime.o $< -o $@

runtime/%.o: runtime/%.c
	$(LANGS_CC) -fPIC -c -g -o $@ $<


clean:
	@$(RM) out/*.o out/*.s out/*.run runtime/*.o


%.o: %.s
	$(LANGS_AS) out/$< -o out/$@

%.s: progs/%.msh
	cargo run  --color=always --package mash --bin mash --profile dev $<


%: %.o runtime.o
	$(LANGS_CC) out/$< runtime.o -o out/$@




