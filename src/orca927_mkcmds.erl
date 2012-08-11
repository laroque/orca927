% generates a string to send to ORCA via telnet
-module(orca927_mkcmds).

-export([mca_set/1, mca_set/2,
        mca_do/1, mca_do/2]).

%don't actually export these once it is debugged.
-compile([debug_info, export_all]).

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
    Str = lists:concat([Name,":",Value," "]), unpack_arg_list(T,Acc ++ Str).

%%===========================================
%% ORCA MCA methods
%%===========================================

% Setters------------------------------------
% default values (where reasonable):
mca_set(upper_discrim)->
    mca_set(upper_discrim, 16383);
mca_set(lower_discrim)->
    mca_set(lower_discrim, 0);
mca_set(presetMode)->
    mca_set(presetMode, 8).
% user values:
mca_set(upper_discrim, Value)->
    mca_set([{setUpperDiscriminator, 0}], [{withValue, Value}]);
mca_set(lower_discrim, Value)->
    mca_set([{setLowerDiscriminator, 0}], [{withValue, Value}]);
mca_set(ltTimeout, Value)->
    mca_set([{setLtPreset, 0}], [{withValue, Value}]);
mca_set(presetMode, Value)->
    mca_set([{setPresetCtrlReg,0}], [{withValue, Value}]);
mca_set(comment, Value)->
    mca_set([{setComment, Value}], []);
mca_set(Method, Argslist)->
    orca_make_cmd("", "ORMCA927Model", Method, Argslist).

%% Doers-------------------------------------
% a thing
mca_do(clearSpectrum)->
    mca_do([{clearSpectrum, 0}], []);
mca_do(startSpectrum)->
    mca_do([{startAcquisition, 0}], []);
mca_do(stopSpectrum)->
    mca_do([{stopAcquisition, 0}], []).
% a thing with a param
mca_do(saveSpectrum, Filename)->
    mca_do([{writespectrum, 0}], [{toFile, Filename}]);
mca_do(Methodlist, Argslist)->
    orca_make_cmd("", "ORMCA927Model", Methodlist, Argslist).
