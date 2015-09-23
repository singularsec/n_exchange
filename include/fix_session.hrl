-record(state, {socket,
                authenticated,
                lastheartbeat,
                username,
                password,
                account,
                prevbuffer=[],
                their_seq,
                our_seq,
                sessionid,
                timer_ref,
                sentlogonrequest=false
                }).
