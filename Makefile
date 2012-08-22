
PROJECT = sensors

REBAR = ./rebar
DIALYZER = dialyzer

all: compile

get-deps:
	@$(REBAR) get-deps

compile: get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -rf erl_crash.dump

docs: clean-docs
	@$(REBAR) doc skip_deps=true

clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

run:
	erl +P 65535 -sname sensors -pa ebin -pa deps/*/ebin -boot start_sasl -s sensors -config priv/sensors.config

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
