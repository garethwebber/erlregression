-module(regression_file).
-export([run_regression/1]).

run_regression(Filename) ->
  case code:priv_dir(my_application) of
        {error, bad_name} ->
            % This occurs when not running as a release; e.g., erl -pa ebin
            % Of course, this will not work for all cases, but should account 
            % for most
            PrivDir = "priv";
        PrivDir ->
            % In this case, we are running in a release and the VM knows
            % where the application (and thus the priv directory) resides
            % on the file system
            ok
    end,

  {ok, Terms} = file:consult(filename:join([PrivDir, Filename])),
  lists:map(fun printPoint/1, Terms),
  {regression, A, B} = regression_math:regression(Terms),
  io:format("A=~w B=~w ~n", [A, B]).

printPoint({point,PX,PY}) ->
  io:format("(~w, ~w)~n", [PX, PY]).

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
