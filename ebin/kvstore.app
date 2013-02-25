{
    application,
    kvstore,
    [
        {description, "Simple key value store wrapper around Mnesia"},
        {vsn, "1.0.0"},
        {modules, [kvstore, kvstore_sup]},
        {applications, [stdlib, kernel, mnesia]}
    ]
}.