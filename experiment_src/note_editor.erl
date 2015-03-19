-module(note_editor).
-compile(export_all).

start(Pid) ->
    io:format("note_editor:~p~n",[Pid]),
    Pid ! [#{cmd=> fill_div, 
	     id => canvas,
	     txt => <<"<svg id='one'></svg>">>},
	   #{cmd => fill_div,
	     id => one,
	     txt => <<"<circle cx='50' cy='50', r='10', fill='blue'/>">>},
	   #{cmd => send_keystrokes}
	  ],
    Mus = {music, [],[initial_note()], global(), 1},
    loop(Pid, [], Mus).


loop(Pid, State, Mus) ->
    receive
	{Pid, #{key :=_} = Key} ->
	    to_div(Pid,key,Key),
	    io:format("Key:~p~n",[Key]),
	    {State1, Mus1} = update(Key, State, Mus),
	    show(Pid, Mus1),
	    play(Mus1),
	    loop(Pid, State1, Mus1);
	Any ->
	    io:format("joe got:~p~n",[Any]),
	    loop(Pid, State, Mus)
    end.

to_div(Pid, Div, X) ->
    Pid ! [#{cmd => fill_div, id=> Div,
	     txt => list_to_binary(io_lib:format("~w", [X]))}].
    

update(#{key := 40}, State, Mus) ->
    io:format("down~n"),
    Mus1 = with_current_pitch(Mus, fun(Note) -> pitch_move(Note, -1) end),
    {State, Mus1};
update(#{key := 38}, State, Mus) ->
    %% up key
    io:format("up~n"),
    Mus1 = with_current_pitch(Mus, fun(Note) -> pitch_move(Note, 1) end),
    {State, Mus1};
update(#{key := 9}, State, Mus) ->
    io:format("rotate~n"),
    Mus1 = with_current_pitch(Mus, fun(Note) -> pitch_rotate_focus(Note) end),
    {State, Mus1};
update(#{key := 78}, State, Mus) ->
    %% add a new note to the chord
    io:format("new~n"),
    Mus1 = with_current_pitch(Mus, fun(Note) -> pitch_add_new_tone(Note) end),
    {State, Mus1};
update(#{key := 68}, State, Mus) ->
    %% delete
    io:format("new~n"),
    Mus1 = with_current_pitch(Mus, fun(Note) -> pitch_delete_tone(Note) end),
    {State, Mus1};

update(Key, State, Mus) ->
    io:format("Update:~p ~p ~p~n",[Key,State,Mus]), 
    {State, Mus}.

pitch_rotate_focus(#{pitch := {0, []}} = Note) ->
    Note;
pitch_rotate_focus(#{pitch := {K, L}} = Note) ->
    P1 = rotate_focus(K, L),
    Note#{pitch:=P1}.

rotate_focus(0, L) -> {1, L};
rotate_focus(K, L) when K < length(L) -> {K+1, L}; 
rotate_focus(K, L) -> {0, L}. 
    
with_current_pitch({music,Before,[H|T],Glob,Track}, F) ->
    {music, Before, [F(H)|T], Glob, Track}.

pitch_add_new_tone(#{pitch := {0, []}} = Note) ->
    Note#{pitch := {1,[60]}};
pitch_add_new_tone(#{pitch := {K, L}} = Note) ->
    Last = lists:last(L),
    L1 = L ++ [Last+2],
    K1 = length(L1),
    Note#{pitch := {K1, L1}}.

pitch_delete_tone(#{pitch := {0,L}} = Note) ->
    Note#{pitch := {0,[]}};
pitch_delete_tone(#{pitch := {K,L}} = Note) ->
    L1 = delete_kth(K, L, []),
    K1 = K + 1,
    K2 = if
	     K1 > length(L1) ->
		 1;
	     true ->
		 K1
	 end,
    K3 = if
	     L1 == [] ->
		 0;
	     true ->
		 K2
	 end,
    Pitch1 = {K3, L1},
    Note#{pitch := Pitch1}.

delete_kth(1, [_|T], L) -> lists:reverse(L, T);
delete_kth(N, [H|T], L) -> delete_kth(N-1, T, [H|L]).

%%----------------------------------------------------------------------
%% pitch_move(Note, Inc)

pitch_move(#{pitch := {K,L}} = Note, Inc) ->
    L1 = move_tones(K, L, Inc),
    Note#{pitch:={K,L1}}.

move_tones(0, L, Inc) ->
    P1 = [I + Inc || I <- L];
move_tones(K, L, Inc) ->
    %% move note K by Inc
    N1 = lists:nth(K, L),
    replace_nth(K, L, N1+Inc).

replace_nth(K, L, X) ->
    replace_nth(K, L, X, []).

replace_nth(1, [_|T], X, L) -> lists:reverse(L, [X|T]);
replace_nth(N, [H|T], X, L) -> replace_nth(N-1, T, X, [H|L]).

show(Pid, Mus) ->
    F = fun(#{pitch := P}) -> 
		to_div(Pid, pitch, P),
		io:format("Pitch:~w~n",[P])
	end,
    with_current_pitch(Mus, F).

play(Mus) ->
    void.

%% Note values
%% semi-demi-semi-quaver (4 bars) = 1
%% demi-semi-quaver      (3 bars) = 2
%% semi-quaver           (2 bars) = 4
%% quaver                (1 bar)  = 8
%% crotchet                       = 16 (note)
%% minim                          = 32
%% semibreve                      = 64
%% breve                          = 128

initial_note() ->
    #{pitch    => {0,[60]}, %% 0 means all notes are focused - 60 = middle C
      duration => {16,[]},  %% 16 => crotchet
      volume   => {100,[]}, %% {Volume, AfterTouch}
      score    => []}.      %% for future expansion

global() ->
    #{beats_per_second => 120,
      volume => 100, %% percentage volume multiplier
      instruments => [{1,10},{2,20}]
     }.
