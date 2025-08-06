-module(job_http_handler).
-author("stilyan").

-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  {ok, Body, Req2} = cowboy_req:read_body(Req),
  Json = jsx:decode(Body, [return_maps]),
  io:format("[DEBUG] Decoded JSON: ~p~n", [Json]),

  try
    SortedMap = job_sorter:sort(Json),
    #{format := Format} = cowboy_req:match_qs([{format, [], <<"json">>}], Req),

    case Format of
      <<"bash">> ->
        io:format("[DEBUG] Entered bash clause~n", []),
        Script = job_sorter:to_bash(SortedMap),
        {ok, cowboy_req:reply(200,
          #{<<"content-type">> => <<"text/x-shellscript">>},
          Script, Req2), State};
      _ ->
        io:format("[DEBUG] Entered json clause~n", []),
        ResponseJson = jsx:encode(SortedMap),
        {ok, cowboy_req:reply(200,
          #{<<"content-type">> => <<"application/json">>},
          ResponseJson, Req2), State}
    end
  catch
    error:{cycle_detected, Path} ->
      io:format("[ERROR] Cycle Detected: ~p~n", [Path]),
      {ok, cowboy_req:reply(400,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"error">> => <<"Cycle Detected!">>, <<"Path">> => Path}),
        Req2), State}
  end.
