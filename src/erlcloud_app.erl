-module(erlcloud_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

-define(DEFAULT_POOLS, [{erlcloud_pool, []}]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(application:start_type(), term()) -> {ok, pid()} |
                                                 {ok, pid(), term()} |
                                                 {error, term()}.
start(_StartType, _StartArgs) ->
    %% start pools
    application:ensure_all_started(hackney_pooler),
    lists:foreach(fun({PoolName, Config}) ->
                          PoolSize = proplists:get_value(workers, Config, 50),
                          MaxConn = proplists:get_value(maxconn, Config, 50),
                          Concurrency = proplists:get_value(concurrency, Config, 50),
                          PoolConfig = [{concurrency, Concurrency},
                                        {maxconn, MaxConn},
                                        {group, erlcloud},
                                        {max_count, PoolSize},
                                        {init_count, PoolSize}],
                          hackney_pooler:new_pool(PoolName, PoolConfig)
                  end, application:get_env(erlcloud, pools, ?DEFAULT_POOLS)),
    %% start the main supervisor
    case erlcloud_sup:start_link() of
        {error, {already_started, Pid}} -> {ok, Pid};
        Result -> Result
    end.

-spec stop(term()) -> no_return().
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

