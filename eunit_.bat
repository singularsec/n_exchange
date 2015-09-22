erlc +debug_info -o .eunit -DTEST ./test/%1.erl
erl -init_debug -noshell -pa .eunit/ -eval "eunit:test(%1 , [verbose])" -s init stop
