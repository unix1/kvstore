%% record definition
-record (
    kvstore_record,
    {
        key,
        time_created,
        time_modified,
        time_accessed,
        value
    }
).