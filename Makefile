.DEFAULT_GOAL	:= all

.PHONY:	deps

REBAR=./rebar

all:	deps compile

deps:	rebar
	@$(REBAR) get-deps

compile:	deps rebar
	@$(REBAR) compile

clean:	rebar
	@$(REBAR) clean

distclean:	rebar clean
	@$(REBAR) delete-deps
	rm -rf rebar-git
	rm rebar

uninstall:
	bash -c "cd deps/wiringPi; sudo ./build uninstall"

rebar:	
	mkdir -p deps
	git clone https://github.com/rebar/rebar.git deps/rebar
	bash -c "cd deps/rebar; ./bootstrap"
	cp deps/rebar/rebar .

