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
-export([request/6]).

-define(POOL_NAME, erlcloud_pool).
-define(HIBENATE_AFTER, 200).

request(URL, Method, Hdrs, Body, Timeout, Config) ->
    %% set ssl options. We check the certificate using latest roots, and
    %% hibernate the connections after ?HIBERNATE_AFTER time to reduce the
    %% memory usage.
    CACertFile =  filename:join(hackney_util:privdir(), "ca-bundle.crt"),
    SSLOptions = [{cacertfile, CACertFile },
                  {verify, verify_peer},
                  {depth, 99},
                  {hibernate_after, ?HIBERNATE_AFTER}],

    %% connection options.
    Options = [{recv_timeout, Timeout},
               {connect_timeout, Timeout},
               {ssl_options, SSLOptions}],

    case is_async() of
        false -> do_sync_request(URL, Method, Hdrs, Body, Options);
        To -> do_async_request(To, URL, Method, Hdrs, Body, Options)
    end.


do_sync_request(URL, Method, Hdrs, Body, Options) ->
    case hackney_pooler:request(pool(), Method, URL, Hdrs, Body, Options,
                                available_worker, infinity) of
        {ok, Status, RespHeaders, RespBody} ->
            {ok, {{Status, <<>>}, RespHeaders, RespBody}};
        {ok, Status, RespHeaders} ->
            {ok, {{Status, <<>>}, RespHeaders, <<>>}};
        Error ->
            Error
    end.

do_async_request(To, URL, Method, Hdrs, Body, Options) ->
    hackney_pooler:async_request(pool(), To, Method, URL, Hdrs, Body,
                                 Options, available_worker).

is_async() ->
    case get(aws_async_request) of
        undefined -> false;
        To -> To
    end.

pool() ->
    case get(aws_pool) of
        undefined -> ?POOL_NAME;
        PoolName -> PoolName
    end.
