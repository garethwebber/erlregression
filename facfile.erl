-module(pointfile).
-export([loadFile/1,test/0]).

% Read content of file passed to loadList, 
% reading an integer per line 
% return as a list

loadFile(FileName) ->
  {ok, FileHandle} = file:open(FileName, [read]),
  try readContents(FileHandle)
    after file:close(FileHandle)
  end.

readContents(FileIO) ->
  case io:get_line(FileIO, "") of
    eof -> 
       [];
    {error, Reason} ->
       io:format("File error: ~p~n", [Reason]),
       [];
    Line ->
      Text = string:trim(Line),
      {Value, _} = string:to_integer(Text), 
      [Value | readContents(FileIO)] 
  end.


test() ->
  loadFile("/Users/me/Development/erlregression/points.dat").
