% this file works with the Excel spreadsheet
% example.xlsx
%
% You need to have the spreadsheet open in Excel for this to work.
%
%
% demo of using dde to query Excel
% Prolog is the DDE Client

get_cell(X) :-
	% Excel is the application name
	% [example.xlsx] is the name of the excel file
	% it's actually the name of the open document, so no path
	%
	% and humma is the workbook
	open_dde_conversation('Excel', '[example.xlsx]humma', C),
	% now we ask for a row/col cell
	dde_request(C, 'R3C1', X),
	close_dde_conversation(C).

get_cell_by_name(X) :-
	% Excel is the application name
	% [example.xlsx] is the name of the excel file
	% it's actually the name of the open document, so no path
	%
	% and humma is the workbook
	open_dde_conversation('Excel', '[example.xlsx]Sheet1', C),
	% now we ask for a row/col cell
	dde_request(C, onions, X),
	close_dde_conversation(C).

% we can get a range of cells just by asking for a reference
% They're returned as a tab separated file as 'codes'
% style string
cell_range :-
	open_dde_conversation('Excel', '[example.xlsx]humma', C),
	dde_request(C, 'R2C1:R11C6', Y),
	writeln(Y),
	close_dde_conversation(C).

%
% The system topic allows us to send commands to the system.
%
system_topic :-
	% Excel is the application name
	% System is a topic all applications must support
	open_dde_conversation('Excel', 'System', C),
	dde_request(C, 'Topics', X),
	writeln(X),
	close_dde_conversation(C).

%
% Lets set some cells
set_cell :-
	open_dde_conversation('Excel', '[example.xlsx]humma', C),
	R1 is random(10),
	dde_poke(C, 'R1C10', R1),
	R2 is random(10),
	dde_poke(C, 'R1C11', R2),
	R3 is random(10),
	dde_poke(C, 'R1C12', R3),
	R4 is random(10),
	dde_poke(C, 'R1C13', R4),
	close_dde_conversation(C).

%
% A dde server that serves prolog flags
%
flags_server :-
	dde_register_service(
	    % set our service name and topi name,
	    % get our Item name and return our value in V
	    prolog(current_prolog_flag, Item, V),
	    current_prolog_flag(Item, V)
	    % our handler is just a system pred
	).


%
% a prolog server that executes a command.
% in this case, it's just a writeln
%
% Our topic is say,
% our item is what to print
%
command_server :-
	dde_register_service(
	    prolog(say, Command), % notice arity 2
	    writeln(Command)
	).


% run this from a second instance of prolog
%
demo_command_server(What) :-
	open_dde_conversation(prolog, say, C),
	dde_execute(C, What),
	close_dde_conversation(C).

%
% This demonstrates sending a command to Excel.
% We'll just ask it to save.
% Type something in a cell in excel, run this command,
% then close the workbook, and you'll notice it doesn't
% ask you to save.
%
%
demo_commanding_excel :-
	open_dde_conversation('Excel', 'System', C),
	dde_execute(C, '[Save()]'),
	close_dde_conversation(C).

