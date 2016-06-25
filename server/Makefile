# http://ninenines.eu/docs/en/cowboy/HEAD/guide/req/
# http://blog.mongodb.org/post/7270427645/design-of-the-erlang-driver-for-mongodb
# http://api.mongodb.org/erlang/
# http://www.amazon.com/gp/product/0321146530/ref=ox_sc_act_title_1?ie=UTF8&psc=1&smid=ATVPDKIKX0DER
# file:///Users/ryan/Dropbox/resume/resume_knockout.html

# http://www.deloitte.com/view/en_US/us/Services/consulting/technology-consulting/index.htm#&panel1-1

app=resume

all: compile

clean:
	@./rebar clean
	@rm -rf rel/$(app)

nuke: clean
	@rm -rf deps

check: compile
	@ERL_FLAGS="-config sasl.config" ./rebar eunit skip_deps=true

dependencies:
	@./rebar get-deps

compile: dependencies
	@./rebar compile

dist: compile
	@./rebar generate force=1
	@chmod 755 rel/$(app)/bin/$(app)
