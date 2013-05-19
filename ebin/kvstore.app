{
    application,
    kvstore,
    [
        {description, "Simple key value store wrapper around Mnesia"},
        {vsn, "1.1.0"},
        {modules, [kvstore, kvstore_sup, kvstore_server, kvstore_server_sup]},
        {registered, [kvstore]},
        {mod, {kvstore, []}},
        {applications, [stdlib, kernel, mnesia]}
    ]
}.