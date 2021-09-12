{application, atmex,
    [
        {description, "OPT bank demo."},
        {vsn, "0,1,0"},
        {registered, [
            atmex,
            atmex_sup
        ]},
        {applications, [
            kernel,
            stdlib
        ]},
        {mod, {atmex_app, []}},
        {env, []}
    ]
}.
