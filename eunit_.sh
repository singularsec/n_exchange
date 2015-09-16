erlc -DTEST ./test/$1.erl && erl -noshell -pa .eunit/ -eval "eunit:test($1, [verbose])" -s init stop
