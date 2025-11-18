

default: mash


mash: src
	cargo build  --color=always --package mash --bin mash --profile dev

clean:
	@$(RM) out/*.o out/*.s out/*.run

%.o: %.s
	nasm -f elf64 out/$< -o out/$@

%.s: progs/%.msh
	cargo run  --color=always --package mash --bin mash --profile dev $<


%: %.o
	gcc out/$< -o out/$@




