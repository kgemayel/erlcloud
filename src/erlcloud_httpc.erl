%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% HTTP client abstraction for erlcloud. Simplifies changing http clients.
%% API matches lhttpc, except Config is passed instead of options for
%% future cusomizability.
%%
%% @end
-module(erlcloud_httpc).

-include("erlcloud.hrl").
-include("erlcloud_aws.hrl").
-include_lib("fusco/include/fusco.hrl").

-export([request/6, adhoc_request/6]).

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_POOL_BASE_NAME, "erlcloud_pool_").

request(URL, Method0, Hdrs, Body, Timeout, _Config) ->
    Method = normalise_method(Method0),
    FuscoURL = fusco_lib:parse_url(URL),
    case fusco:request(get_worker(FuscoURL), list_to_binary(FuscoURL#fusco_url.path),
                       Method, normalise_headers(Hdrs), Body, 0, Timeout) of
        {ok, {{Status, StatusLine}, RespHeaders, RespBody, _, _}} ->
            {ok, {{binary_to_integer(Status), StatusLine}, RespHeaders, RespBody}};
        Error ->
            Error
    end.

adhoc_request(BaseURL, Path, Method, Hdrs, Body, Timeout) ->
    {ok, ConnPid} = fusco:start(BaseURL, []),
    Result = case fusco:request(ConnPid, Path, Method, Hdrs, Body, 0, Timeout) of
                 {ok, {{Status, StatusLine}, RespHeaders, RespBody, _, _}} ->
                     {ok, {{Status, StatusLine}, RespHeaders, RespBody}};
                 Error ->
                     Error
             end,
    fusco:disconnect(ConnPid),
    Result.

get_worker(#fusco_url{ host = Host, port = Port, is_ssl = IsSSL } = FuscoURL) ->
    case get(aws_pool) of
        undefined ->
            put(aws_pool, list_to_atom(?DEFAULT_POOL_BASE_NAME ++ Host)),
            get_worker(FuscoURL);
        PoolName ->
            case ets:info(PoolName) of
                undefined -> new_pool(PoolName, {Host, Port, IsSSL});
                _ -> ok
            end,
            cuesport:get_worker(PoolName)
    end.

new_pool(PoolName, PoolBase) ->
    FuscoOpts = [{connect_timeout, 30000}],
    PoolSize = ?DEFAULT_POOL_SIZE,
    ChildMods = [fusco],
    ChildMF = {fusco, start_link},
    already_started_is_ok(
      supervisor:start_child(
        ejabberd_sup,
        {{libon_fusco_sup, PoolName},
         {cuesport, start_link,
          [PoolName, PoolSize, ChildMods, ChildMF, {for_all, [PoolBase, FuscoOpts]}]},
         transient, 2000, supervisor, [cuesport | ChildMods]})).

already_started_is_ok({ok, _Pid}) -> ok;
already_started_is_ok({error, {already_started, _}}) -> ok.

normalise_method(Method) ->
    string:to_upper(atom_to_list(Method)).

normalise_headers(Headers) ->
    Headers1 = [ {iolist_to_binary(Key), iolist_to_binary(Value)} || {Key, Value} <- Headers ],
    [ {<<"Connection">>, <<"keep-alive">>} | Headers1 ].

