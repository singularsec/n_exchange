erl -boot start_sasl -pa ebin/ -pa deps/amqp_client/ebin -pa deps/rabbit_common/ebin -pa deps/protobuffs/ebin -eval "application:start(nexchange)." 
