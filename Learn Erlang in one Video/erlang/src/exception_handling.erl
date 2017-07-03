-module(exception_handling).
-export([error_stuff/2, read_text/1]).

error_stuff(Num1, Num2) ->
  % inside try we put expression we are trying to evaluate.
  % code which potentially could cause an error.
  try
    Result = Num1 div Num2,
    Result
  % inside catch we a list of errors we are expecting to catch/find.
  catch
    % bad arithmetic error, if this is the type of error we face, the code inside will be executed.
    error:badarith ->
      "No division by zero allowed!"
  end.

% we try reading a file.
read_text(File) ->
  try
    {ok, Data} = file:open(File, [read]),
    File_contents = file:read(Data, 1024*1024)
  catch
    % will catch any possible error.
    _:_ ->
      "File doesn't exist..."
  end.

% more on errors: http://erlang.org/doc/reference_manual/errors.html
