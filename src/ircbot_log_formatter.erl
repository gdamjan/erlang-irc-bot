-module(ircbot_log_formatter).

%%% Global log formatter handler.
%%% Check `config/sys.config.src` to see how its enabled.
-export([format/2]).

format(#{level := Level, msg := {report, Msg}}, _Config) ->
    format(#{level => Level, msg => {"~p", [Msg]}}, _Config);
format(#{level := Level, msg := {string, Msg}}, _Config) ->
    format(#{level => Level, msg => {"~s", [Msg]}}, _Config);
format(#{level := Level, msg := {Fmt, Args}}, _Config) ->
    Prefix = printk_prefix(Level),
    [Prefix, io_lib:format(Fmt, Args), "\n"].

%%% Loglevel formatting according to the systemd/printk convention
%%% https://www.freedesktop.org/software/systemd/man/latest/sd-daemon.html#Description
printk_prefix(emergency) -> "<0>";
printk_prefix(alert)     -> "<1>";
printk_prefix(critical)  -> "<2>";
printk_prefix(error)     -> "<3>";
printk_prefix(warning)   -> "<4>";
printk_prefix(notice)    -> "<5>";
printk_prefix(info)      -> "<6>";
printk_prefix(debug)     -> "<7>".
