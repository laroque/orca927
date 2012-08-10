% generates a string to send to ORCA via telnet
-module(orca927_mkcmds).

-export([orca_make_cmd/4]).
-export([setUpperDiscriminator/1, setLowerDiscriminator/1, setLtPreset/1, setPresetCtrlReg/0]).
-export([clearSpectrum/0, startAcquisition/0, stopAcquisition/0, writeSpectrum/1]).
-export([setComment/1]).

%don't actually export these once it is debugged.
-export([unpack_arg_list/1]).

orca_make_cmd(TagString, Module, Method, Argslist)->
    {"send", lists:concat([TagString, " = [", Module," ",
           unpack_arg_list(Method), unpack_arg_list(Argslist),"]"])}.

%%=========================================
%% Some recursion to unpack arg:value pairs
%%=========================================
%The one you call
unpack_arg_list(Arglist)->
    unpack_arg_list(Arglist,[]).
%Base cases
unpack_arg_list([{}],Acc)->
    Acc;
unpack_arg_list([],Acc)->
    Acc;
%recurse
unpack_arg_list([{Name,Value}|T],Acc)->
    Str = lists:concat([Name,":",Value," "]),
    unpack_arg_list(T,Acc ++ Str).

%%===========================================
%% ORCA MCA methods
%%===========================================

%% Setters-----------------------------------
%% default values (only for those with an obvious default):
mca_set(upper_discrim)->
    mca_set(upper_discrim, 16383);
mca_set(lower_discrim)->
    mca_set(lower_discrim, 0);
mca_set(presetMode)->
    mca_set(presetMode, 8).
%% custom values:
mca_set(upper_discrim, Value)->
    orca_set([{setUpperDiscriminator, 0}], [{withValue, Value}]);
mca_set(lower_discrim, Value)->
    orca_set([{setLowerDiscriminator, 0}], [{withValue, Value}]);
mca_set(ltTimeout, Value)->
    orca_set([{setLtPreset, 0}], [{withValue, Value}]);
mca_set(presetMode, Value)->
    orca_set([{setPresetCtrlReg,0}, [{withValue, Value}]);
mca_set(comment, Value)->
    orca_set([{setComment, Value}], []);
orca_set(Method, Argslist)->
    orca_make_cmd("", "ORMCA927Model", Method, Argslist).

%% Doers--------------------------------------
orca_do(clearSpectrum)->
    orca_do([{clearSpectrum, 0}], []);
orca_do(startSpectrum)->
    orca_do([{startAcquisition, 0}], []);
orca_do(stopSpectrum)->
    orca_do([{stopAcquisition, 0}], []);
orca_do(saveSpectrum, Filename)->
    orca_do([{writespectrum, 0}], [{toFile, Filename}]);
orca_do(Method, Argslist)->
    orca_make_cmd("", "ORMCA927Model", Method, Argslist).
