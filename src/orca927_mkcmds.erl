% generates a string to send to ORCA via telnet
-module(orca927_mkcmds).
-export([orca_make_cmd/4]).
-export([setUpperDiscriminator/1, setLowerDiscriminator/1, setLtPreset/1, setPresetCtrlReg/0]).
-export([clearSpectrum/0, startAcquisition/0, stopAcquisition/0, writeSpectrum/1]).
-export([setComment/1]).

orca_make_cmd(TagString, Module, Method, Argslist)->
    {"send", lists:concat([TagString, " = [", Module," ",
          unpack_arg_list(Method), unpack_arg_list(Argslist),"]"])}.

unpack_arg_list(Arglist)->
    (unpack_arg_list(Arglist,[])).

unpack_arg_list([],Acc)->
    lists:reverse(Acc);
unpack_arg_list([{Name,Value}|T],Acc)->
    Str = lists:concat([Name,":",Value," "]),
    unpack_arg_list(T,[<<Str>> | Acc]).

% ORCA methods%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Run condition presets
% Set the ULD value (2^14 - 1 is effectively no limit)
setUpperDiscriminator(Val)->
    orca_make_cmd(somerand, "ORMCA927Model", 
      [{setUpperDiscriminator, 0}], [{withValue, Val}]).
% Set the LLD value (0 is effecitvely no limit)
setLowerDiscriminator(Val)->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{setLowerDiscriminator, 0}], [{withValue, Val}]).
% Set the total live time for a run
setLtPreset(Val)->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{setLtPreset, 0}], [{withValue, Val}]).
% Enable auto-stop at live time
    % if PresetCtrlReg == 8 then livetime condition is enabled,
    %I don't what other possible values exist or what state they correspond with.
setPresetCtrlReg()->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{setPresetCtrlReg, 0}], [{withValue, 8}]).

    % Data taking control
% Clear current spectrum
clearSpectrum()->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{clearSpectrum, 0}], []).
% Start taking data
startAcquisition()->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{startAcquisition, 0}], []).
% Stop taking data
stopAcquisition()->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{stopAcquisition, 0}], []).
% Save current spectrum
writeSpectrum(FileName)->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{writeSpectrum, 0}], [{toFile,FileName}]).

    % Other
% Set the comment string
setComment(Comment)->
    orca_make_cmd(somerand, "ORMCA927Model",
      [{setComment, Comment}], []).
