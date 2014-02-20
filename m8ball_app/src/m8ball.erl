-module(m8ball).
-behavior(application).
-export([start/2, stop/1]).
-export([ask/1]).



% Callbacks

start(normal, []) ->
  m8ball_sup:start_link([]).

stop(_State) ->
  ok.



% Interface

ask(Question) ->
  m8ball_server:ask(Question).
