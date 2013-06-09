-module(kvstore_server).
-include("kvstore.hrl").
-behaviour(gen_server).
-export([start_link/1, start_link/2, init/1]).
-export([handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2,
         read/3, write/4, delete/3, delete_match/3, delete_match_spec/3]).

%%%%% Supervision functions %%%%%

%% start without name
start_link(Sup) ->
    gen_server:start_link(?MODULE, {Sup}, []).

%% start with locally registered name
start_link(Name, Sup) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, {Sup}, []).

%% init, nothing to do
init({_Sup}) ->
    % if pooling is needed, we could start a dynamic supervisor here
    {ok, 0}.

%%%%% User functions %%%%%

read(Name, Key, AccessContext) ->
    gen_server:call(Name, {read, Key, AccessContext}).

write(Name, Key, Value, AccessContext) ->
    gen_server:call(Name, {write, Key, Value, AccessContext}).

delete(Name, Key, AccessContext) ->
    gen_server:call(Name, {delete, Key, AccessContext}).

delete_match(Name, Pattern, AccessContext) ->
    gen_server:call(Name, {delete_match, Pattern, AccessContext}).

delete_match_spec(Name, Match, AccessContext) ->
    gen_server:call(Name, {delete_match_spec, Match, AccessContext}).

%%%%% Server functions %%%%%

handle_call({read, Key, AccessContext}, _From, S) ->
    {reply, libread(Key, AccessContext), S};

handle_call({write, Key, Value, AccessContext}, _From, S) ->
    {reply, libwrite(Key, Value, AccessContext), S};

handle_call({delete, Key, AccessContext}, _From, S) ->
    {reply, libdelete(Key, AccessContext), S};

handle_call({delete_match, Pattern, AccessContext}, _From, S) ->
    {reply, libdelete_match(Pattern, AccessContext), S};

handle_call({delete_match_spec, Match, AccessContext}, _From, S) ->
    {reply, libdelete_match_spec(Match, AccessContext), S}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%%% library functions %%%%%

%% reads by Key
%% allows to specify Mnesia access context
libread(Key, AccessContext) ->
    case libread_raw(Key, AccessContext) of
        [#kvstore_record{time_created = TC, time_modified = TM, time_accessed = _TA, value = V}] ->
            NewTA = os:timestamp(),
            libwrite_raw(
                #kvstore_record {
                    key = Key,
                    time_created = TC,
                    time_modified = TM,
                    time_accessed = NewTA,
                    value = V
                },
                AccessContext
            ),
            {Key, V, TC, TM, NewTA};
        [] ->
            undefined
    end.

%% writes by key
%% allows to specify Mnesia access context
libwrite(Key, Value, AccessContext) ->
    Now = os:timestamp(),
    % time_created will stay the same if modifying existing record
    ExistingRecordList = libread_raw(Key, AccessContext),
    case ExistingRecordList =:= [] of
        true ->
            TimeCreated = Now;
        false ->
            [ExistingRecord|_] = ExistingRecordList,
            TimeCreated = ExistingRecord#kvstore_record.time_created
    end,
    libwrite_raw(
        #kvstore_record {
            key = Key,
            time_created = TimeCreated,
            time_modified = Now,
            time_accessed = Now,
            value = Value
        },
        AccessContext
    ).

%% deletes by key
%% allows to specify Mnesia access context
libdelete(Key, AccessContext) ->
    libdelete_raw(Key, AccessContext).

%% deletes by match pattern
%% allows to specify Mnesia access context
libdelete_match(Pattern, AccessContext) ->
    libdelete_match_raw(Pattern, AccessContext).

%% deletes by match spec
%% allows to specify Mnesia access context
libdelete_match_spec(Match, AccessContext) ->
    libdelete_match_spec_raw(Match, AccessContext).

%%%%% Private functions %%%%%

%% wrapper around mnesia:read
libread_raw(Key, AccessContext) ->
    F = fun() ->
        mnesia:read({kvstore_record, Key})
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around mnesia:write
libwrite_raw(Record, AccessContext) ->
    F = fun() ->
        mnesia:write(Record)
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around mnesia:delete
libdelete_raw(Key, AccessContext) ->
    F = fun() ->
        mnesia:delete({kvstore_record, Key})
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around match_object and delete_object sequence
libdelete_match_raw(Pattern, AccessContext) ->
    F = fun() ->
        ListToDelete = mnesia:match_object(Pattern),
        lists:foreach(
            fun(X) ->
                mnesia:delete_object(X)
            end,
            ListToDelete
        )
    end,
    mnesia:activity(AccessContext, F).

%% wrapper around select_delete
libdelete_match_spec_raw(Match, AccessContext) ->
    F = fun() ->
        ListToDelete = mnesia:select(kvstore_record, Match),
        lists:foreach(
            fun(X) ->
                mnesia:delete({kvstore_record, X})
            end,
            ListToDelete
        )
    end,
    mnesia:activity(AccessContext, F).