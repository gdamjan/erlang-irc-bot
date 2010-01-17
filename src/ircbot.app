{application, ircbot, [
    {description, "ircbot"},
    {vsn, "0.01"},
    {modules, [
        ircbot_app, ircbot_server, ircbot_api,
        utils, pong_plugin, ctcp_plugin
    ]},
    {mod, {ircbot_app, []}},
    {applications, [kernel, stdlib]}]
}.
