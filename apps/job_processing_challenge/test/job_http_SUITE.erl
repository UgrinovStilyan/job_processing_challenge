-module(job_http_SUITE).
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
  application:ensure_all_started(inets),
  application:ensure_all_started(cowboy),
  application:ensure_all_started(ranch),
  application:ensure_all_started(jsx),
  application:ensure_all_started(job_processing_challenge),
  Config.

end_per_suite(_Config) ->
  application:stop(job_processing_challenge),
  application:stop(cowboy),
  application:stop(ranch),
  application:stop(jsx),
  application:stop(inets),
  ok.

all() ->
  [json_response_test, bash_response_test, cycle_detection_test].

task_json() ->
  <<"{\"tasks\":[
        {\"name\":\"task-1\",\"command\":\"touch /tmp/file1\"},
        {\"name\":\"task-2\",\"command\":\"cat /tmp/file1\",\"requires\":[\"task-3\"]},
        {\"name\":\"task-3\",\"command\":\"echo 'Hello World!' > /tmp/file1\",\"requires\":[\"task-1\"]},
        {\"name\":\"task-4\",\"command\":\"rm /tmp/file1\",\"requires\":[\"task-2\",\"task-3\"]}
    ]}">>.

parse_task_names(JsonBinary) ->
  Parsed = jsx:decode(JsonBinary, [return_maps]),
  TaskList = maps:get(<<"tasks">>, Parsed),
  [maps:get(<<"name">>, Task) || Task <- TaskList].

assert_order(TaskNames, Before, After) ->
  B = find_index(TaskNames, Before),
  A = find_index(TaskNames, After),
  ?assert(B > 0),
  ?assert(A > 0),
  ?assert(B < A).

find_index(List, Value) -> find_index(List, Value, 1).
find_index([], _, _) -> 0;
find_index([Value | _], Value, Index) -> Index;
find_index([_ | Rest], Value, Index) -> find_index(Rest, Value, Index + 1).

assert_line_order(Lines, Before, After) ->
  B = find_line_index(Lines, Before),
  A = find_line_index(Lines, After),
  ?assert(B > 0),
  ?assert(A > 0),
  ?assert(B < A).

find_line_index(Lines, Line) -> find_line_index(Lines, Line, 1).
find_line_index([], _, _) -> 0;
find_line_index([Line | _], Line, Index) -> Index;
find_line_index([_ | Rest], Line, Index) -> find_line_index(Rest, Line, Index + 1).

json_response_test(_Config) ->
  JsonBody = task_json(),
  {ok, {{_,200,_}, _Headers, Body}} =
    httpc:request(post, {"http://localhost:4000/jobs", [], "application/json", JsonBody}, [], []),
  TaskNames = parse_task_names(unicode:characters_to_binary(Body)),
  io:format("[TEST] JSON task names: ~p~n", [TaskNames]),
  assert_order(TaskNames, <<"task-1">>, <<"task-3">>),
  assert_order(TaskNames, <<"task-3">>, <<"task-2">>),
  assert_order(TaskNames, <<"task-2">>, <<"task-4">>),
  assert_order(TaskNames, <<"task-3">>, <<"task-4">>).

bash_response_test(_Config) ->
  JsonBody = task_json(),
  Url = "http://localhost:4000/jobs?format=bash",
  {ok, {{_,200,_}, _Headers, Body}} =
    httpc:request(post, {Url, [], "application/json", JsonBody}, [], []),
  ScriptLines = string:split(Body, "\n", all),
  io:format("[TEST] Bash script lines: ~p~n", [ScriptLines]),
  assert_line_order(ScriptLines, "touch /tmp/file1", "echo 'Hello World!' > /tmp/file1"),
  assert_line_order(ScriptLines, "echo 'Hello World!' > /tmp/file1", "cat /tmp/file1"),
  assert_line_order(ScriptLines, "cat /tmp/file1", "rm /tmp/file1").

cycle_detection_test(_Config) ->
  JsonBody = <<"{\"tasks\":[
        {\"name\":\"a\",\"command\":\"echo A\",\"requires\":[\"b\"]},
        {\"name\":\"b\",\"command\":\"echo B\",\"requires\":[\"a\"]}
    ]}">>,
  {ok, {{_,Status,_}, _Headers, Body}} =
    httpc:request(post, {"http://localhost:4000/jobs", [], "application/json", JsonBody}, [], []),
  io:format("[TEST] Cycle detection response: status=~p body=~s~n", [Status, Body]),
  ?assertEqual(400, Status),
  Parsed = jsx:decode(unicode:characters_to_binary(Body), [return_maps]),
  ?assertEqual(<<"Cycle Detected!">>, maps:get(<<"error">>, Parsed)),
  Path = maps:get(<<"Path">>, Parsed),
  ?assertEqual([<<"a">>, <<"b">>, <<"a">>], Path).