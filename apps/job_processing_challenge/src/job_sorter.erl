-module(job_sorter).
-author("stilyan").

-export([sort/1, to_bash/1]).

sort(#{<<"tasks">> := Tasks}) ->
  Map = maps:from_list([{maps:get(<<"name">>, T), T} || T <- Tasks]),
  {Sorted, _Visited} = inner_sort(Map),
  #{<<"tasks">> => lists:reverse(Sorted)}.

to_bash(#{<<"tasks">> := Tasks}) ->
  Commands = [maps:get(<<"command">>, T) || T <- Tasks],
  <<<<"#!/usr/bin/env bash\n">>/binary, (binary:join(Commands, <<"\n">>))/binary, "\n">>.

%Internal Functions
inner_sort(Map) ->
  Names = maps:keys(Map),
  lists:foldl(fun(Name, {Acc, VisitStatus}) ->
    visit(Name, Map, Acc, VisitStatus, [])
              end, {[], #{}}, Names).

visit(TaskName, TaskMap, SortedAcc, VisitState, Path) ->
  case maps:get(TaskName, VisitState, unvisited) of
    visited ->
      {SortedAcc, VisitState};

    visiting ->
      erlang:error({cycle_detected, lists:reverse([TaskName | Path])});

    unvisited ->
      Task = maps:get(TaskName, TaskMap),
      Dependencies = maps:get(<<"requires">>, Task, []),

      {SortedAfterDeps, StateAfterDeps} =
        lists:foldl(
          fun(Dep, {AccSorted, AccState}) ->
            visit(Dep, TaskMap, AccSorted, AccState, [TaskName | Path])
          end,
          {SortedAcc, VisitState#{TaskName => visiting}},
          Dependencies
        ),

      {[Task | SortedAfterDeps], StateAfterDeps#{TaskName => visited}}
  end.
