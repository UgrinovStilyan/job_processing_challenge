-module(job_sorter_tests).
-author("stilyan").

-include_lib("eunit/include/eunit.hrl").

%% Test data
sample_tasks() ->
  #{
    <<"tasks">> => [
      #{<<"name">> => <<"task-1">>,
        <<"command">> => <<"touch /tmp/file1">>},
      #{<<"name">> => <<"task-2">>,
        <<"command">> => <<"cat /tmp/file1">>,
        <<"requires">> => [<<"task-3">>]},
      #{<<"name">> => <<"task-3">>,
        <<"command">> => <<"echo 'Hello World!' > /tmp/file1">>,
        <<"requires">> => [<<"task-1">>]},
      #{<<"name">> => <<"task-4">>,
        <<"command">> => <<"rm /tmp/file1">>,
        <<"requires">> => [<<"task-2">>, <<"task-3">>]}
    ]
  }.

%% Test: Sorting order
sort_order_test() ->
  Input = sample_tasks(),
  Result = job_sorter:sort(Input),
  ?assertMatch(#{<<"tasks">> := _}, Result),
  SortedNames = [maps:get(<<"name">>, T) || T <- maps:get(<<"tasks">>, Result)],
  %% Expected order: 1 -> 3 -> 2 -> 4
  ?assertEqual([<<"task-1">>, <<"task-3">>, <<"task-2">>, <<"task-4">>], SortedNames).

%% Test: Bash script output
bash_script_test() ->
  Input = sample_tasks(),
  Sorted = job_sorter:sort(Input),
  Script = job_sorter:to_bash(Sorted),
  Expected =
    <<"#!/usr/bin/env bash
touch /tmp/file1
echo 'Hello World!' > /tmp/file1
cat /tmp/file1
rm /tmp/file1
">>,
  ?assertEqual(Expected, Script).

%% Test: Detect cycles
cycle_test() ->
  Input = #{
    <<"tasks">> => [
      #{<<"name">> => <<"a">>, <<"command">> => <<"echo A">>, <<"requires">> => [<<"b">>]},
      #{<<"name">> => <<"b">>, <<"command">> => <<"echo B">>, <<"requires">> => [<<"a">>]}
    ]
  },
  ?assertException(error, {cycle_detected, [<<"a">>, <<"b">>, <<"a">>]}, job_sorter:sort(Input)).
