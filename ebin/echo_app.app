{
    application,
    echo_app,
    [
        {description, "Echo Server with a Port"},
        {vsn, "0.0.1"},
        {modules, [echo_app, echo_sup, echo]},
        {registered, [echo]},
        {applications, [kernel, stdlib]},
        {mod, {echo_app, []}},
        {env, [
            {app, "echo.py"},
            {timeout, 3000},
            {maxline, 100}
        ]}
    ]
}.
