ERL = $(shell find src -name "*.erl")
BEAMS = $(ERL:src/%.erl=ebin/%.beam)

all: $(BEAMS)

ebin/%.beam: src/%.erl
	erlc -W -o ./ebin $<

clean:
	rm -rf ./ebin/*.beam

run: all
	erl -pa ./ebin

.PHONY: clean run
