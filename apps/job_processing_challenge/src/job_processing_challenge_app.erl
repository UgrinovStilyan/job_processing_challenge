-module(job_processing_challenge_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jsx),
    job_processing_challenge_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
