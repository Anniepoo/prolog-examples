:- use_module(library(socket)).


go :-
    Adress = '127.0.0.1':6555,
    tcp_socket(Socket),
    tcp_connect(Socket, Adress, Read, Write),
    set_input(Read),
    process_input.

process_input :-
	get_char(C),
	put_char(C),
	process_input.

% we get end of file in get_char if we kill cogbot.
% so presumably we can just test for eof and die

