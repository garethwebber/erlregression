-module(facfile).
-export([loadFile/1,test/0]).

% Read content of file passed to loadList, 
% reading an integer per line 
% return as a list

loadFile(FileName) ->
  {ok, FileHandle} = file:open(FileName, [read]),
  try readLines(FileHandle)
    after file:close(FileHandle)
  end.
  
readLines(FileHandle) ->
  case file:read_line(FileHandle) of
    {ok, Line} ->
      Text = string:trim(Line),
      {Value, Rest} = string:to_integer(Text), 
      [Value | readLines(fileHandle)];
    eof -> [];
    {error, Reason} -> 
       io:format("File error: ~p~n", [Reason]),
       []
  end.

test() ->
  loadFile("/Users/me/Development/erlregression/fac.dat").
