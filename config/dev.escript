%% -*- mode: erlang -*-
%% run by rebar shell before starting the apps or the shell

main(_) ->
  logger:update_formatter_config(default, #{
    single_line => true,
    legacy_header => false,
    template => ["<",level,">", " ", msg,"\n"]
  }),
  logger:set_primary_config(level, debug).
