
On a decent OS
==

brew install erlang
brew install rebar

rebar get-deps
rebar compile

On windows
==

Install erlang (see http://www.erlang.org/download.html)
Install rebar (see https://github.com/rebar/rebar/wiki)
Make sure VC++ is installed, open a shell and run vcvarsall.bat

rebar get-deps
rebar compile


tests
==

rebar eunit

or to run a single one
./eunit.sh testmodulename
